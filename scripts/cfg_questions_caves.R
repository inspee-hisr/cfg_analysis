#!/usr/bin/env Rscript
# Cave-level questions: inventory, hotspots, sampling effort, protection, geology.
# Outputs: results/q_caves_*.tsv, plots/q_caves_*.png
# Run after cfg_spatial_analysis.R (needs caves_geology.tsv) and cfg_geology.R.

.libPaths(c("/workspace/.Rlib", .libPaths()))
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(scales)
library(ggrepel)

source("scripts/cfg_load_data.R")
source("scripts/cfg_plot_style.R")

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

cat("\n================================================================\n")
cat("LOADING SPATIAL DATA\n")
cat("================================================================\n")

greece_regions <- sf::st_read("spatial_data/gadm41_GRC_shp/gadm41_GRC_2.shp", quiet = TRUE) |>
    sf::st_transform(crs = 3035)

caves_sf <- caves |>
    filter(!is.na(Longitude), !is.na(Latitude)) |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) |>
    st_transform(crs = 3035)

natura2000 <- sf::st_read(
    "spatial_data/N2000_spatial_GR_2021_12_09_v32/N2000_spatial_GR_2021_12_09_v32.shp",
    quiet = TRUE) |>
    sf::st_transform(crs = 3035)

geopark <- sf::st_read("spatial_data/geopark_borders_mod/geopark_borders_mod.shp", quiet = TRUE) |>
    sf::st_transform(crs = 3035)

caves_in_n2000 <- sf::st_join(caves_sf, natura2000, join = sf::st_intersects, left = FALSE) |>
    sf::st_drop_geometry() |>
    distinct(Cave_ID, Cave_Name, SITECODE, SITETYPE)

caves_in_geopark <- sf::st_join(caves_sf, geopark, join = sf::st_intersects, left = FALSE) |>
    sf::st_drop_geometry() |>
    distinct(Cave_ID, Cave_Name)

cat("Natura2000 join: done\n")
cat("Geopark join: done —", nrow(caves_in_geopark), "caves in geopark\n")

# ── precompute per-cave summaries ─────────────────────────────────────────────
cave_species_count <- census_all_species |>
    filter(!is.na(Species), !grepl("\\bsp\\.$", Species)) |>
    distinct(Cave_ID, Species) |>
    left_join(species |> select(Species_Full_Name, Classification, Distribution),
              by = c("Species" = "Species_Full_Name")) |>
    group_by(Cave_ID) |>
    summarise(
        n_species     = n_distinct(Species),
        n_endemic     = sum(Distribution == "Endemic to Greece", na.rm = TRUE),
        n_obligate    = sum(Classification %in% c("Troglobiont", "Stygobiont"), na.rm = TRUE),
        n_troglobiont = sum(Classification == "Troglobiont", na.rm = TRUE),
        .groups       = "drop"
    )

cave_refs_count <- census_long_man |>
    distinct(Cave_ID, Reference_ID) |>
    count(Cave_ID, name = "n_refs")

caves_full <- caves |>
    left_join(cave_species_count, by = "Cave_ID") |>
    left_join(cave_refs_count,    by = "Cave_ID") |>
    mutate(across(c(n_species, n_refs, n_endemic, n_obligate, n_troglobiont),
                  ~ replace_na(.x, 0L)))

################################################################
cat("\n================================================================\n")
cat("CAVES — INVENTORY\n")
cat("================================================================\n")

# Q: How many caves per region?
cat("\n--- Q: Caves per region ---\n")
caves_per_region <- caves_full |>
    count(Region, name = "n_caves") |>
    arrange(desc(n_caves))
print(caves_per_region)
save_tsv(caves_per_region, "q_caves_per_region")

p <- ggplot(caves_per_region,
            aes(x = fct_reorder(Region, n_caves), y = n_caves, fill = Region)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n_caves), hjust = -0.2, size = 3.5, colour = "#333333") +
    scale_fill_viridis_d(option = "D") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    coord_flip() +
    labs(title = "Number of caves per region", x = NULL, y = "Number of caves") +
    theme_cfg_bar()
save_plot(p, "q_caves_per_region")

# Q: Cave type distribution
cat("\n--- Q: Cave type distribution ---\n")
caves_type <- caves_full |>
    count(Cave_Type, name = "n_caves") |>
    arrange(desc(n_caves))
print(caves_type)
save_tsv(caves_type, "q_caves_type")

p <- ggplot(caves_type,
            aes(x = fct_reorder(Cave_Type, n_caves), y = n_caves, fill = Cave_Type)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(aes(label = n_caves), hjust = -0.2, size = 4.5, colour = "#333333") +
    scale_fill_viridis_d(option = "D") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    coord_flip() +
    labs(title = "Cave type distribution", x = NULL, y = "Number of caves") +
    theme_cfg_bar()
save_plot(p, "q_caves_type", w = 14, h = 7)

# Q: Altitudinal distribution
cat("\n--- Q: Altitude distribution of caves ---\n")
caves_altitude <- caves_full |>
    filter(!is.na(Altitude)) |>
    select(Cave_ID, Cave_Name, Region, Altitude, Cave_Type)
save_tsv(caves_altitude, "q_caves_altitude")

alt_summary <- caves_altitude |>
    group_by(Region) |>
    summarise(median_alt = round(median(Altitude), 0), .groups = "drop")

p <- ggplot(caves_altitude, aes(x = Altitude, fill = Region)) +
    geom_histogram(binwidth = 100, colour = "white", linewidth = 0.2) +
    scale_fill_viridis_d(option = "D") +
    scale_x_continuous(breaks = seq(0, 2500, 250)) +
    labs(title    = "Altitudinal distribution of caves",
         subtitle = paste0(nrow(caves_altitude), " caves with altitude data"),
         x = "Altitude (m a.s.l.)", y = "Number of caves") +
    theme_cfg() +
    theme(legend.position = "right", legend.text = element_text(size = 8))
save_plot(p, "q_caves_altitude", w = 22, h = 13)

# Q: Natural vs artificial species richness
cat("\n--- Q: Artificial vs natural species richness ---\n")
caves_type_richness <- caves_full |>
    filter(!is.na(Cave_Type), n_species > 0) |>
    select(Cave_ID, Cave_Name, Cave_Type, Region, n_species)
save_tsv(caves_type_richness, "q_caves_type_richness")

type_med <- caves_type_richness |>
    group_by(Cave_Type) |>
    summarise(med = median(n_species), n = n(), .groups = "drop")

p <- ggplot(caves_type_richness, aes(x = Cave_Type, y = n_species, fill = Cave_Type)) +
    geom_boxplot(outlier.alpha = 0.35, width = 0.5, show.legend = FALSE) +
    geom_text(data = type_med,
              aes(x = Cave_Type, y = med, label = paste0("n=", n, "\nmed=", med)),
              size = 3, vjust = -1.2, inherit.aes = FALSE) +
    scale_fill_viridis_d(option = "D") +
    scale_y_log10() +
    labs(title    = "Species richness by cave type",
         subtitle = "Caves with ≥ 1 species; log10 y-axis",
         x = NULL, y = "Number of species (log scale)") +
    theme_cfg()
save_plot(p, "q_caves_type_richness", w = 14, h = 10)

################################################################
cat("\n================================================================\n")
cat("CAVES — BIODIVERSITY HOTSPOTS\n")
cat("================================================================\n")

# Q: Region with highest total and mean species richness
cat("\n--- Q: Region total and mean species richness ---\n")
caves_region_richness <- caves_full |>
    group_by(Region) |>
    summarise(
        n_caves        = n(),
        total_species  = sum(n_species),
        mean_species   = round(mean(n_species), 2),
        median_species = round(median(n_species), 2),
        .groups        = "drop"
    ) |>
    arrange(desc(total_species))
print(caves_region_richness)
save_tsv(caves_region_richness, "q_caves_region_richness")

p_total <- ggplot(caves_region_richness,
                  aes(x = fct_reorder(Region, total_species), y = total_species, fill = Region)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = total_species), hjust = -0.2, size = 3.5, colour = "#333333") +
    scale_fill_viridis_d(option = "D") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    coord_flip() +
    labs(title = "Total cave species richness per region", x = NULL, y = "Total species") +
    theme_cfg_bar()
save_plot(p_total, "q_caves_region_total_richness")

p_mean <- ggplot(caves_region_richness,
                 aes(x = fct_reorder(Region, mean_species), y = mean_species, fill = Region)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = mean_species), hjust = -0.2, size = 3.5, colour = "#333333") +
    scale_fill_viridis_d(option = "D") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    coord_flip() +
    labs(title = "Mean species richness per cave by region",
         x = NULL, y = "Mean species per cave") +
    theme_cfg_bar()
save_plot(p_mean, "q_caves_region_mean_richness")

# Q: Caves with highest proportion of endemic species
cat("\n--- Q: Caves with highest endemic proportion ---\n")
caves_endemic_prop <- caves_full |>
    filter(n_species >= 3) |>
    mutate(prop_endemic = round(n_endemic / n_species, 3)) |>
    arrange(desc(prop_endemic)) |>
    select(Cave_ID, Cave_Name, Region, n_species, n_endemic, prop_endemic)
print(head(caves_endemic_prop, 10))
save_tsv(caves_endemic_prop, "q_caves_endemic_proportion")

p <- ggplot(caves_endemic_prop |> head(20) |>
                mutate(Cave_Name = fct_reorder(Cave_Name, prop_endemic)),
            aes(x = Cave_Name, y = prop_endemic, fill = Region)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = percent(prop_endemic, accuracy = 1)), hjust = -0.2, size = 3,
              colour = "#333333") +
    scale_fill_viridis_d(option = "D") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35)),
                       labels = percent_format()) +
    coord_flip() +
    labs(title    = "Caves with highest proportion of endemic-to-Greece species",
         subtitle = "Caves with ≥ 3 species",
         x = NULL, y = "Proportion endemic") +
    theme_cfg_bar() + theme(legend.position = "bottom")
save_plot(p, "q_caves_endemic_proportion", w = 22, h = 16)

# Q: Caves with highest proportion of obligate species (Troglobiont + Stygobiont)
cat("\n--- Q: Caves with highest obligate proportion ---\n")
caves_obligate_prop <- caves_full |>
    filter(n_species >= 3) |>
    mutate(prop_obligate = round(n_obligate / n_species, 3)) |>
    arrange(desc(prop_obligate)) |>
    select(Cave_ID, Cave_Name, Region, n_species, n_obligate, prop_obligate)
print(head(caves_obligate_prop, 10))
save_tsv(caves_obligate_prop, "q_caves_obligate_proportion")

p <- ggplot(caves_obligate_prop |> head(20) |>
                mutate(Cave_Name = fct_reorder(Cave_Name, prop_obligate)),
            aes(x = Cave_Name, y = prop_obligate, fill = Region)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = percent(prop_obligate, accuracy = 1)), hjust = -0.2, size = 3,
              colour = "#333333") +
    scale_fill_viridis_d(option = "D") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35)),
                       labels = percent_format()) +
    coord_flip() +
    labs(title    = "Caves with highest proportion of obligate species",
         subtitle = "Caves with ≥ 3 species; obligate = Troglobiont + Stygobiont",
         x = NULL, y = "Proportion obligate") +
    theme_cfg_bar() + theme(legend.position = "bottom")
save_plot(p, "q_caves_obligate_proportion", w = 22, h = 16)

################################################################
cat("\n================================================================\n")
cat("CAVES — SAMPLING EFFORT\n")
cat("================================================================\n")

# Q: Distribution of references per cave
cat("\n--- Q: References per cave distribution ---\n")
caves_refs_dist <- caves_full |>
    select(Cave_ID, Cave_Name, Region, n_refs, n_species)

single_ref <- sum(caves_refs_dist$n_refs == 1, na.rm = TRUE)
zero_ref   <- sum(caves_refs_dist$n_refs == 0, na.rm = TRUE)
cat("Single-reference caves:", single_ref, "\n")
cat("Zero-reference caves:", zero_ref, "\n")
print(summary(caves_refs_dist$n_refs))

save_tsv(caves_refs_dist |>
             mutate(ref_class = case_when(
                 n_refs == 0 ~ "0 refs",
                 n_refs == 1 ~ "1 ref",
                 n_refs <= 5 ~ "2–5 refs",
                 TRUE        ~ "6+ refs"
             )),
         "q_caves_refs_per_cave")

p <- ggplot(caves_refs_dist |> filter(n_refs > 0),
            aes(x = n_refs)) +
    geom_histogram(bins = 30, fill = "#2c7bb6", colour = "white", linewidth = 0.2) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    labs(title    = "Distribution of references per cave",
         subtitle = paste0(single_ref, " single-reference caves; ",
                           zero_ref, " caves with no references"),
         x = "Number of references", y = "Number of caves") +
    theme_cfg()
save_plot(p, "q_caves_refs_distribution", w = 16, h = 10)

# Q: Correlation between references and species per cave
cat("\n--- Q: Refs vs species richness correlation ---\n")
refs_species_df <- caves_refs_dist |>
    filter(n_refs > 0, n_species > 0)

cor_val <- round(cor(refs_species_df$n_refs, refs_species_df$n_species,
                     method = "spearman"), 3)
cat("Spearman r (refs vs species):", cor_val, "\n")

save_tsv(refs_species_df |> mutate(spearman_r = cor_val),
         "q_caves_refs_species_scatter")

p <- ggplot(refs_species_df, aes(x = n_refs, y = n_species)) +
    geom_point(aes(colour = Region), alpha = 0.6, size = 1.5) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "black",
                se = TRUE, linewidth = 0.8) +
    scale_colour_viridis_d(option = "D") +
    labs(title    = "Sampling effort vs species richness per cave",
         subtitle = paste0("Spearman r = ", cor_val,
                           " (caves with ≥ 1 ref and ≥ 1 species)"),
         x = "Number of references (effort proxy)", y = "Number of species") +
    theme_cfg() + theme(legend.position = "right")
save_plot(p, "q_caves_refs_species_scatter", w = 22, h = 14)

# Q: Under-sampled regions
cat("\n--- Q: Under-sampled regions ---\n")
caves_undersampled <- caves_full |>
    group_by(Region) |>
    summarise(
        n_caves      = n(),
        mean_species = round(mean(n_species), 2),
        mean_refs    = round(mean(n_refs), 2),
        .groups      = "drop"
    ) |>
    arrange(desc(n_caves))
print(caves_undersampled)
save_tsv(caves_undersampled, "q_caves_undersampled_regions")

p <- ggplot(caves_undersampled,
            aes(x = n_caves, y = mean_species, colour = mean_refs)) +
    geom_point(size = 5) +
    geom_text(aes(label = Region), vjust = -1, size = 2.8, colour = "grey20") +
    scale_colour_gradient(low = seq_lo, high = seq_hi, name = "Mean refs\nper cave") +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
    labs(title    = "Under-sampled regions: many caves, few species per cave",
         subtitle = "Colour = mean references per cave (survey effort)",
         x = "Number of caves in region", y = "Mean species per cave") +
    theme_cfg()
save_plot(p, "q_caves_undersampled", w = 20, h = 13)

################################################################
cat("\n================================================================\n")
cat("CAVES — PROTECTION\n")
cat("================================================================\n")

# Q: Caves in Geopark
cat("\n--- Q: Caves in Geopark ---\n")
cat("Caves overlapping Geopark boundaries:", nrow(caves_in_geopark), "\n")
geopark_summary <- tibble(
    total_caves_in_geopark = nrow(caves_in_geopark),
    cave_ids = paste(caves_in_geopark$Cave_ID, collapse = "|"),
    cave_names = paste(caves_in_geopark$Cave_Name, collapse = "|")
)
save_tsv(geopark_summary, "q_caves_geopark")

# Q: Proportion of troglobiont caves inside Natura2000
cat("\n--- Q: Troglobiont caves in Natura2000 ---\n")
troglo_caves <- cave_species_count |>
    filter(n_troglobiont > 0) |>
    pull(Cave_ID)

n_troglo_in_n2000 <- sum(troglo_caves %in% caves_in_n2000$Cave_ID)
troglo_n2000_overlap <- tibble(
    total_troglo_caves   = length(troglo_caves),
    troglo_in_n2000      = n_troglo_in_n2000,
    troglo_outside_n2000 = length(troglo_caves) - n_troglo_in_n2000,
    prop_protected       = round(n_troglo_in_n2000 / length(troglo_caves), 3)
)
print(troglo_n2000_overlap)
save_tsv(troglo_n2000_overlap, "q_caves_natura2000_troglo")

p <- ggplot(
        tibble(status = c("Inside N2000", "Outside N2000"),
               n      = c(troglo_n2000_overlap$troglo_in_n2000,
                          troglo_n2000_overlap$troglo_outside_n2000)),
        aes(x = status, y = n, fill = status)) +
    geom_col(width = 0.5, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.4, size = 5) +
    scale_fill_manual(values = prot_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title    = "Caves with troglobiont species: Natura2000 coverage",
         subtitle = paste0(troglo_n2000_overlap$total_troglo_caves,
                           " caves with ≥ 1 troglobiont species; ",
                           percent(troglo_n2000_overlap$prop_protected), " protected"),
         x = NULL, y = "Number of caves") +
    theme_cfg()
save_plot(p, "q_caves_natura2000_troglo", w = 13, h = 10)

# Q: Geological substrate composition
cat("\n--- Q: Geological substrate of caves ---\n")
caves_geology_df <- read_delim("results/caves_geology.tsv",
                               delim = "\t", show_col_types = FALSE) |>
    select(Cave_ID, geology_group) |>
    distinct()

caves_geology_summary <- caves_geology_df |>
    group_by(geology_group) |>
    summarise(n_caves = n_distinct(Cave_ID), .groups = "drop") |>
    filter(!is.na(geology_group)) |>
    arrange(desc(n_caves))
print(caves_geology_summary)
save_tsv(caves_geology_summary, "q_caves_geology_substrate")

p <- ggplot(caves_geology_summary,
            aes(x = fct_reorder(geology_group, n_caves), y = n_caves,
                fill = geology_group)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n_caves), hjust = -0.2, size = 3.5, colour = "#333333") +
    scale_fill_viridis_d(option = "D") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    coord_flip() +
    labs(title = "Cave geological substrate (from GeologicUnitView.gpkg)",
         x = NULL, y = "Number of caves (distinct Cave_ID)") +
    theme_cfg_bar()
save_plot(p, "q_caves_geology_substrate", w = 18, h = 10)

################################################################
cat("\n================================================================\n")
cat("BETA DIVERSITY — TROGLOBIONT SPECIES BETWEEN REGIONS\n")
cat("================================================================\n")

# Q: Troglobiont beta diversity between regions (Jaccard dissimilarity)
cat("\n--- Q: Troglobiont species turnover between regions (Jaccard) ---\n")
library(vegan)

troglo_spp <- species |>
    filter(Classification == "Troglobiont") |>
    pull(Species_Full_Name)

region_spp_mat <- census_all_species_all_caves |>
    filter(!grepl("\\bsp\\.$", Species),
           Species %in% troglo_spp,
           !is.na(Region)) |>
    distinct(Region, Species) |>
    mutate(presence = 1L) |>
    pivot_wider(names_from = Species, values_from = presence, values_fill = 0L)

regions_vec <- region_spp_mat$Region
mat <- as.matrix(region_spp_mat |> select(-Region))
rownames(mat) <- regions_vec

jacc_dist <- vegan::vegdist(mat, method = "jaccard", binary = TRUE)
jacc_mat  <- as.matrix(jacc_dist)

jacc_df <- as.data.frame(jacc_mat) |>
    tibble::rownames_to_column("region1") |>
    pivot_longer(-region1, names_to = "region2", values_to = "jaccard_dist")
save_tsv(jacc_df, "q_caves_troglo_beta_diversity")

p <- ggplot(jacc_df, aes(x = region1, y = region2, fill = jaccard_dist)) +
    geom_tile() +
    scale_fill_gradient2(low = "#0072B2", mid = "#f7f7f7", high = "#D55E00",
                         midpoint = 0.5, limits = c(0, 1),
                         name = "Jaccard\ndissimilarity") +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(title    = "Troglobiont species turnover between regions",
         subtitle = "Jaccard dissimilarity (1 = no shared species)",
         x = NULL, y = NULL) +
    coord_fixed() +
    theme_cfg() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "q_caves_troglo_beta_diversity", w = 20, h = 18)

cat("\n================================================================\n")
cat("DONE — results/ and plots/ updated\n")
cat("================================================================\n")
