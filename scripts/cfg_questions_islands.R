#!/usr/bin/env Rscript
# Island biogeography questions: SAR, richness rankings, isolation, beta diversity.
# Depends on cfg_spatial_analysis.R outputs: cfg_island_areas.tsv,
#   cfg_island_species.tsv, cfg_caves_island_summary.tsv.

.libPaths(c("/workspace/.Rlib", .libPaths()))
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(scales)
library(vegan)

source("scripts/cfg_load_data.R")

# ── helpers ──────────────────────────────────────────────────────────────────
save_tsv <- function(df, name) {
    write_delim(df, file.path("results", paste0(name, ".tsv")), delim = "\t")
    invisible(df)
}
save_plot <- function(p, name, w = 18, h = 12) {
    ggsave(file.path("plots", paste0(name, ".png")),
           plot = p, width = w, height = h, units = "cm", dpi = 300)
    invisible(p)
}
theme_cfg <- function(base = 11) {
    theme_bw(base_size = base) +
    theme(panel.grid.minor  = element_blank(),
          panel.grid.major  = element_blank(),
          plot.title        = element_text(face = "bold"),
          plot.subtitle     = element_text(colour = "grey40", size = base - 1))
}

################################################################
cat("\n================================================================\n")
cat("LOADING PRECOMPUTED ISLAND DATA\n")
cat("================================================================\n")

island_areas   <- read_delim("results/cfg_island_areas.tsv",
                              delim = "\t", show_col_types = FALSE)
island_species <- read_delim("results/cfg_island_species.tsv",
                              delim = "\t", show_col_types = FALSE)
caves_island   <- read_delim("results/cfg_caves_island_summary.tsv",
                              delim = "\t", show_col_types = FALSE)

cat("Island areas:", nrow(island_areas), "rows\n")
cat("Island species:", nrow(island_species), "rows\n")

# ── build island-level summary ────────────────────────────────────────────────
island_sp_counts <- island_species |>
    filter(is_island) |>
    left_join(species |> select(Species_Full_Name, Classification, Distribution),
              by = c("Species" = "Species_Full_Name")) |>
    group_by(NAME_2, NAME_3) |>
    summarise(
        n_species     = n_distinct(Species),
        n_endemic     = n_distinct(Species[Distribution == "Endemic to Greece"]),
        n_obligate    = n_distinct(Species[Classification %in% c("Troglobiont","Stygobiont")]),
        n_troglobiont = n_distinct(Species[Classification == "Troglobiont"]),
        .groups       = "drop"
    )

island_cave_counts <- caves_island |>
    filter(is_island) |>
    group_by(NAME_2, NAME_3) |>
    summarise(n_caves = n_distinct(Cave_ID), .groups = "drop")

island_summary <- island_areas |>
    left_join(island_sp_counts,   by = c("NAME_2", "NAME_3")) |>
    left_join(island_cave_counts, by = c("NAME_2", "NAME_3")) |>
    mutate(across(c(n_species, n_caves, n_endemic, n_obligate, n_troglobiont),
                  ~ replace_na(.x, 0L)),
           area_island_km2 = as.numeric(area_island_km2),
           species_per_km2  = round(n_species / pmax(area_island_km2, 0.01), 5),
           endemic_per_km2  = round(n_endemic / pmax(area_island_km2, 0.01), 5)) |>
    filter(n_caves > 0) |>
    arrange(desc(n_species))

cat("Islands with ≥ 1 cave:", nrow(island_summary), "\n")

################################################################
cat("\n================================================================\n")
cat("ISLANDS — SUMMARY TABLE\n")
cat("================================================================\n")

print(island_summary |> select(NAME_2, NAME_3, area_island_km2,
                                n_caves, n_species, n_endemic, n_obligate, n_troglobiont))
save_tsv(island_summary, "q_islands_summary")

p_bubble <- ggplot(island_summary |> filter(area_island_km2 > 0, n_species > 0),
                   aes(x = area_island_km2, y = n_species,
                       size = n_caves, colour = NAME_2,
                       label = NAME_3)) +
    geom_point(alpha = 0.75) +
    geom_text(aes(label = NAME_3), size = 2.5, vjust = -1, colour = "grey20") +
    scale_x_log10(labels = label_comma()) +
    scale_y_log10() +
    scale_size_continuous(name = "Caves", range = c(2, 12)) +
    scale_colour_brewer(palette = "Set1", name = "Island group") +
    labs(title    = "Island summary: area vs cave species richness",
         subtitle = "Bubble size = number of caves; log–log axes",
         x = "Island area (km², log scale)", y = "Total species (log scale)") +
    theme_cfg() + theme(legend.position = "right")
save_plot(p_bubble, "q_islands_summary", w = 24, h = 16)

################################################################
cat("\n================================================================\n")
cat("ISLANDS — SPECIES–AREA RELATIONSHIPS\n")
cat("================================================================\n")

sar_data <- island_summary |>
    filter(area_island_km2 > 0)

# SAR for total, endemic, and obligate species
run_sar <- function(df, y_col, label) {
    y_vals      <- df[[y_col]]
    df_fit      <- df[y_vals > 0, ]
    df_fit$ysar <- df_fit[[y_col]]
    if (nrow(df_fit) < 3) return(NULL)
    fit <- lm(log10(ysar) ~ log10(area_island_km2), data = df_fit)
    s   <- summary(fit)
    tibble(
        type        = label,
        n_islands   = nrow(df_fit),
        slope_z     = round(coef(fit)["log10(area_island_km2)"], 3),
        intercept_c = round(coef(fit)["(Intercept)"], 3),
        r_squared   = round(s$r.squared, 3),
        p_value     = round(coef(s)["log10(area_island_km2)", "Pr(>|t|)"], 5)
    )
}

sar_results <- bind_rows(
    run_sar(sar_data, "n_species",     "All species"),
    run_sar(sar_data, "n_endemic",     "Endemic to Greece"),
    run_sar(sar_data, "n_obligate",    "Obligate (Troglo+Stygo)"),
    run_sar(sar_data, "n_troglobiont", "Troglobiont only")
)
print(sar_results)
save_tsv(sar_results, "q_islands_sar_results")

# Plot: log-log SAR
sar_long <- sar_data |>
    pivot_longer(cols = c(n_species, n_endemic, n_obligate),
                 names_to = "type", values_to = "richness") |>
    mutate(type = recode(type,
                         n_species  = "All species",
                         n_endemic  = "Endemic to Greece",
                         n_obligate = "Obligate")) |>
    filter(richness > 0)

p_sar <- ggplot(sar_long,
                aes(x = area_island_km2, y = richness, colour = type)) +
    geom_point(alpha = 0.7, size = 2.5) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.9,
                aes(fill = type), alpha = 0.15) +
    scale_x_log10(labels = label_comma()) +
    scale_y_log10() +
    scale_colour_brewer(palette = "Dark2", name = NULL) +
    scale_fill_brewer(palette   = "Dark2", name = NULL) +
    labs(title    = "Species–area relationship (SAR) for Greek cave fauna",
         subtitle = "log–log regression; one point per island with ≥ 1 cave",
         x = "Island area (km², log scale)", y = "Species richness (log scale)") +
    theme_cfg() +
    theme(legend.position = c(0.18, 0.85),
          legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))
save_plot(p_sar, "q_islands_sar", w = 20, h = 14)

# Caves–area relationship
p_car <- ggplot(sar_data |> filter(n_caves > 0),
                aes(x = area_island_km2, y = n_caves)) +
    geom_point(aes(colour = NAME_2), size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "black", linewidth = 0.8) +
    scale_x_log10(labels = label_comma()) +
    scale_y_log10() +
    scale_colour_brewer(palette = "Set1", name = "Island group") +
    labs(title    = "Caves–area relationship across Greek islands",
         subtitle = "log–log axes",
         x = "Island area (km², log scale)", y = "Number of caves (log scale)") +
    theme_cfg()
save_plot(p_car, "q_islands_caves_area", w = 20, h = 13)

save_tsv(sar_data |>
             select(NAME_2, NAME_3, area_island_km2, n_caves,
                    n_species, n_endemic, n_obligate, n_troglobiont,
                    species_per_km2, endemic_per_km2),
         "q_islands_sar_data")

################################################################
cat("\n================================================================\n")
cat("ISLANDS — RICHNESS RANKINGS\n")
cat("================================================================\n")

cat("\n--- Q: Island richness rankings ---\n")

# Top by total species
cat("Top 10 by total species:\n")
print(island_summary |> head(10) |> select(NAME_2, NAME_3, area_island_km2, n_caves, n_species))

# Top by species per km²
cat("\nTop 10 by species density (per km²):\n")
print(island_summary |> arrange(desc(species_per_km2)) |>
          head(10) |> select(NAME_2, NAME_3, area_island_km2, n_species, species_per_km2))

# Top by endemic count
cat("\nTop 10 by endemic species:\n")
print(island_summary |> arrange(desc(n_endemic)) |>
          head(10) |> select(NAME_2, NAME_3, area_island_km2, n_endemic, endemic_per_km2))

# Top by troglobiont count
cat("\nTop 10 by troglobiont species:\n")
print(island_summary |> arrange(desc(n_troglobiont)) |>
          head(10) |> select(NAME_2, NAME_3, area_island_km2, n_troglobiont))

save_tsv(island_summary |>
             mutate(rank_total    = rank(-n_species,     ties.method = "min"),
                    rank_density  = rank(-species_per_km2, ties.method = "min"),
                    rank_endemic  = rank(-n_endemic,     ties.method = "min"),
                    rank_troglo   = rank(-n_troglobiont, ties.method = "min")) |>
             arrange(rank_total),
         "q_islands_richness_rank")

# Richness ranking plot (top 15 by total species)
top15 <- island_summary |>
    head(15) |>
    mutate(NAME_3 = fct_reorder(NAME_3, n_species))

rank_long <- top15 |>
    pivot_longer(cols = c(n_species, n_endemic, n_troglobiont),
                 names_to = "type", values_to = "n") |>
    mutate(type = recode(type,
                         n_species     = "Total species",
                         n_endemic     = "Endemic",
                         n_troglobiont = "Troglobiont"))

p_rank <- ggplot(rank_long,
                 aes(x = NAME_3, y = n, fill = type)) +
    geom_col(position = "dodge", width = 0.75) +
    geom_text(aes(label = n), position = position_dodge(0.75),
              hjust = -0.2, size = 2.8) +
    scale_fill_manual(values = c("Total species" = "#2c7bb6",
                                 "Endemic"        = "#d73027",
                                 "Troglobiont"    = "#1a9641"),
                      name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    coord_flip() +
    labs(title    = "Top 15 islands by species richness",
         subtitle = "Total species, endemics, and troglobionts",
         x = NULL, y = "Number of species") +
    theme_cfg() +
    theme(legend.position = "bottom")
save_plot(p_rank, "q_islands_richness_rank", w = 22, h = 16)

# Species density plot (top 15 by density)
top15_density <- island_summary |>
    filter(area_island_km2 > 1) |>  # exclude tiny islets
    arrange(desc(species_per_km2)) |>
    head(15) |>
    mutate(NAME_3 = fct_reorder(NAME_3, species_per_km2))

p_density <- ggplot(top15_density,
                    aes(x = NAME_3, y = species_per_km2, fill = NAME_2)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = round(species_per_km2, 3)), hjust = -0.2, size = 3) +
    scale_fill_brewer(palette = "Set1", name = "Island group") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    coord_flip() +
    labs(title    = "Top 15 islands by cave species density",
         subtitle = "Species per km² (islands > 1 km²)",
         x = NULL, y = "Species per km²") +
    theme_cfg() + theme(legend.position = "bottom")
save_plot(p_density, "q_islands_density", w = 20, h = 14)

# Q: Island group comparison
cat("\n--- Q: Island group comparison ---\n")
island_group_comp <- island_summary |>
    group_by(NAME_2) |>
    summarise(
        n_islands      = n(),
        mean_caves     = round(mean(n_caves), 1),
        mean_species   = round(mean(n_species), 1),
        mean_endemic   = round(mean(n_endemic), 1),
        mean_obligate  = round(mean(n_obligate), 1),
        total_species  = sum(n_species),
        total_endemic  = sum(n_endemic),
        .groups        = "drop"
    )
print(island_group_comp)
save_tsv(island_group_comp, "q_islands_group_comparison")

p_group <- ggplot(island_summary,
                  aes(x = NAME_2, y = n_species, fill = NAME_2)) +
    geom_boxplot(outlier.alpha = 0.5, width = 0.5, show.legend = FALSE) +
    scale_fill_brewer(palette = "Set1") +
    labs(title    = "Cave species richness by island group",
         subtitle = "Each box = one island in that group",
         x = NULL, y = "Total cave species per island") +
    theme_cfg()
save_plot(p_group, "q_islands_group_comparison", w = 16, h = 10)

################################################################
cat("\n================================================================\n")
cat("ISLANDS — BIOGEOGRAPHIC PATTERNS\n")
cat("================================================================\n")

# Q: Jaccard overlap between each island and the mainland fauna
cat("\n--- Q: Species overlap (Jaccard) between islands and mainland ---\n")

mainland_spp <- island_species |>
    filter(region_type == "Mainland") |>
    distinct(Species) |>
    pull(Species)

island_jaccard <- island_species |>
    filter(is_island) |>
    group_by(NAME_3) |>
    summarise(island_spp = list(unique(Species)), .groups = "drop") |>
    mutate(
        n_island   = lengths(island_spp),
        n_shared   = sapply(island_spp, function(s) sum(s %in% mainland_spp)),
        n_mainland = length(mainland_spp)
    ) |>
    mutate(
        jaccard_sim  = round(n_shared / (n_island + n_mainland - n_shared), 4),
        jaccard_dist = round(1 - jaccard_sim, 4)
    ) |>
    select(-island_spp) |>
    arrange(desc(jaccard_sim))

cat("Islands with highest Jaccard similarity to mainland:\n")
print(head(island_jaccard, 10))
save_tsv(island_jaccard, "q_islands_jaccard")

p_jacc <- ggplot(island_jaccard |>
                     filter(n_island > 0) |>
                     mutate(NAME_3 = fct_reorder(NAME_3, jaccard_sim)),
                 aes(x = NAME_3, y = jaccard_sim)) +
    geom_col(aes(fill = jaccard_sim), width = 0.75, show.legend = FALSE) +
    scale_fill_gradient(low = "#fee090", high = "#4575b4") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip() +
    labs(title    = "Jaccard similarity of island cave fauna to mainland",
         subtitle = "Higher = more species shared with the mainland",
         x = NULL, y = "Jaccard similarity (0–1)") +
    theme_cfg()
save_plot(p_jacc, "q_islands_jaccard", w = 18, h = 16)

# Q: Strict island endemics — species on only 1 island
cat("\n--- Q: Strict island endemics (1 island only) ---\n")
strict_island_endemic <- island_species |>
    filter(is_island) |>
    group_by(Species) |>
    summarise(
        n_islands  = n_distinct(NAME_3),
        islands    = paste(sort(unique(NAME_3)), collapse = "|"),
        NAME_3     = first(NAME_3),
        .groups    = "drop"
    ) |>
    filter(n_islands == 1) |>
    filter(!Species %in% (island_species |>
                              filter(region_type == "Mainland") |>
                              pull(Species))) |>
    left_join(species |> select(Species_Full_Name, Classification, Distribution),
              by = c("Species" = "Species_Full_Name")) |>
    arrange(NAME_3, Species)

cat("Strict single-island endemic species:", nrow(strict_island_endemic), "\n")
cat("Distribution by island:\n")
print(strict_island_endemic |> count(NAME_3, sort = TRUE))
save_tsv(strict_island_endemic, "q_islands_strict_endemic")

p_strict <- strict_island_endemic |>
    count(NAME_3, Classification) |>
    mutate(NAME_3 = fct_reorder(NAME_3, n, sum)) |>
    ggplot(aes(x = NAME_3, y = n, fill = Classification)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5),
              size = 3, colour = "white", fontface = "bold") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip() +
    labs(title    = "Strict single-island endemic species by island",
         subtitle = "Species with records on only one island and no mainland record",
         x = NULL, y = "Number of species") +
    theme_cfg() + theme(legend.position = "bottom")
save_plot(p_strict, "q_islands_strict_endemic", w = 20, h = 14)

# Q: Beta diversity — pairwise Jaccard among island groups
cat("\n--- Q: Beta diversity (Jaccard) among island groups ---\n")

group_species <- island_species |>
    filter(is_island) |>
    distinct(NAME_2, Species)

# Presence/absence matrix: rows = island groups, columns = species
island_groups <- sort(unique(group_species$NAME_2))
all_spp       <- sort(unique(group_species$Species))

pa_matrix <- matrix(0L, nrow = length(island_groups), ncol = length(all_spp),
                    dimnames = list(island_groups, all_spp))
for (grp in island_groups) {
    spp <- group_species |> filter(NAME_2 == grp) |> pull(Species)
    pa_matrix[grp, spp] <- 1L
}

# Include mainland as a group
mainland_spp_vec <- intersect(mainland_spp, all_spp)
mainland_row <- matrix(0L, nrow = 1, ncol = length(all_spp),
                       dimnames = list("Mainland", all_spp))
mainland_row[1, mainland_spp_vec] <- 1L
pa_full <- rbind(pa_matrix, mainland_row)

jacc_dist <- vegan::vegdist(pa_full, method = "jaccard")
jacc_mat  <- as.matrix(jacc_dist)

jacc_df <- as.data.frame(jacc_mat) |>
    tibble::rownames_to_column("group_from") |>
    pivot_longer(-group_from, names_to = "group_to", values_to = "jaccard_dist")

save_tsv(jacc_df, "q_islands_beta")

# Heatmap
p_heat <- ggplot(jacc_df,
                 aes(x = group_to, y = group_from, fill = jaccard_dist)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = round(jaccard_dist, 2)), size = 3.5) +
    scale_fill_gradient(low = "#fee090", high = "#d73027",
                        name = "Jaccard\ndistance") +
    labs(title    = "Beta diversity among island groups and mainland",
         subtitle = "Pairwise Jaccard distances (0 = identical, 1 = no shared species)",
         x = NULL, y = NULL) +
    theme_cfg() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_plot(p_heat, "q_islands_beta", w = 16, h = 12)

cat("\n================================================================\n")
cat("DONE — results/ and plots/ updated\n")
cat("================================================================\n")
