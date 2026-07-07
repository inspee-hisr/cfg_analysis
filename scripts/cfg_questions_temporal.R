#!/usr/bin/env Rscript
# Reference and temporal-dynamics questions: inventory, discovery curves,
# author/journal contributions, sampling completeness (Chao1).
# No spatial dependencies.

.libPaths(c("/workspace/.Rlib", .libPaths()))
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(scales)
library(stringr)
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

# ── join references to census long ───────────────────────────────────────────
census_long_ref <- census_long_man |>
    left_join(Census_references, by = c("Reference_ID" = "ID"))

################################################################
cat("\n================================================================\n")
cat("REFERENCES — INVENTORY\n")
cat("================================================================\n")

# Q: Total references; single-cave vs multi-cave
cat("\n--- Q: Reference inventory ---\n")
ref_cave_counts <- census_long_ref |>
    filter(!is.na(Reference_ID)) |>
    distinct(Reference_ID, Cave_ID) |>
    count(Reference_ID, name = "n_caves")

ref_inv <- tibble(
    metric = c("Total references",
               "Single-cave references (1 cave)",
               "Multi-cave references (≥2 caves)"),
    count  = c(nrow(Census_references),
               sum(ref_cave_counts$n_caves == 1),
               sum(ref_cave_counts$n_caves >= 2))
)
print(ref_inv)
save_tsv(ref_inv, "q_refs_inventory")

p <- ggplot(ref_inv |> tail(2),
            aes(x = metric, y = count, fill = metric)) +
    geom_col(width = 0.5, show.legend = FALSE) +
    geom_text(aes(label = count), vjust = -0.4, size = 5) +
    scale_fill_manual(values = c("Single-cave references (1 cave)"     = "#2c7bb6",
                                  "Multi-cave references (≥2 caves)"    = "#d73027")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title    = "Reference inventory: single-cave vs multi-cave",
         subtitle = paste0(ref_inv$count[1], " total references"),
         x = NULL, y = "Number of references") +
    theme_cfg()
save_plot(p, "q_refs_inventory", w = 14, h = 9)

# Q: References covering the most caves
cat("\n--- Q: References covering most caves ---\n")
refs_most_caves <- ref_cave_counts |>
    left_join(Census_references |> select(ID, Short, Title, Year),
              by = c("Reference_ID" = "ID")) |>
    arrange(desc(n_caves)) |>
    head(20) |>
    mutate(label = paste0(Short, " (", Year, ")"))
print(refs_most_caves |> select(Short, Year, n_caves))
save_tsv(refs_most_caves, "q_refs_most_caves")

p <- ggplot(refs_most_caves |>
                mutate(label = fct_reorder(label, n_caves)),
            aes(x = label, y = n_caves, fill = Year)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n_caves), hjust = -0.2, size = 3.5) +
    scale_fill_gradient(low = "#a8d8ea", high = "#154360", name = "Year") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title    = "Top 20 references by number of caves covered",
         x = NULL, y = "Number of caves") +
    theme_cfg()
save_plot(p, "q_refs_most_caves", w = 24, h = 14)

################################################################
cat("\n================================================================\n")
cat("TEMPORAL — SPECIES DISCOVERY\n")
cat("================================================================\n")

# First record per species (earliest year it appears in any reference)
first_per_species <- census_long_ref |>
    filter(!is.na(Year), !grepl("\\bsp\\.$", Species)) |>
    distinct(Species, Year) |>
    group_by(Species) |>
    summarise(first_year = min(Year), .groups = "drop")

# First record per cave
first_per_cave <- census_long_ref |>
    filter(!is.na(Year)) |>
    distinct(Cave_ID, Year) |>
    group_by(Cave_ID) |>
    summarise(first_year = min(Year), .groups = "drop")

# Q: New species records per decade
cat("\n--- Q: Species first records per decade ---\n")
species_per_decade <- first_per_species |>
    mutate(decade = floor(first_year / 10) * 10) |>
    count(decade, name = "n_new_species") |>
    arrange(decade) |>
    mutate(cumulative = cumsum(n_new_species))
print(species_per_decade)
save_tsv(species_per_decade, "q_species_per_decade")

# Dual-axis: bars = new per decade, line = cumulative
max_new  <- max(species_per_decade$n_new_species)
max_cum  <- max(species_per_decade$cumulative)
scale_f  <- max_cum / max_new

p <- ggplot(species_per_decade, aes(x = decade)) +
    geom_col(aes(y = n_new_species), fill = "#2c7bb6", width = 8, alpha = 0.8) +
    geom_line(aes(y = cumulative / scale_f), colour = "#d73027", linewidth = 1.2) +
    geom_point(aes(y = cumulative / scale_f), colour = "#d73027", size = 2.5) +
    scale_x_continuous(breaks = seq(1860, 2030, 10)) +
    scale_y_continuous(
        name     = "New species per decade",
        sec.axis = sec_axis(~ . * scale_f, name = "Cumulative species",
                            breaks = seq(0, max_cum, 100))
    ) +
    labs(title    = "Cave species discovery by decade",
         subtitle = "Blue bars: new first records; red line: cumulative total",
         x        = "Decade") +
    theme_cfg() +
    theme(axis.text.x      = element_text(angle = 45, hjust = 1),
          axis.title.y      = element_text(colour = "#2c7bb6"),
          axis.title.y.right = element_text(colour = "#d73027"))
save_plot(p, "q_species_per_decade", w = 26, h = 14)

# Q: Cumulative species discovery curve
cat("\n--- Q: Cumulative species discovery curve ---\n")
accum_all <- first_per_species |>
    arrange(first_year) |>
    mutate(cumulative = row_number()) |>
    group_by(first_year) |>
    slice_max(cumulative, n = 1) |>
    ungroup()

accum_endemic <- first_per_species |>
    left_join(species |> select(Species_Full_Name, Distribution),
              by = c("Species" = "Species_Full_Name")) |>
    filter(Distribution == "Endemic to Greece") |>
    arrange(first_year) |>
    mutate(cumulative = row_number()) |>
    group_by(first_year) |>
    slice_max(cumulative, n = 1) |>
    ungroup()

accum_data <- bind_rows(
    accum_all    |> mutate(type = "All species"),
    accum_endemic |> mutate(type = "Endemic to Greece")
)
save_tsv(accum_data |> select(first_year, cumulative, type),
         "q_species_accumulation")

p <- ggplot(accum_data, aes(x = first_year, y = cumulative, colour = type)) +
    geom_line(linewidth = 1.2) +
    geom_point(data = accum_data |> filter(first_year %% 10 == 0), size = 2) +
    scale_colour_manual(values = c("All species" = "#2c7bb6",
                                   "Endemic to Greece" = "#d73027"),
                        name = NULL) +
    scale_x_continuous(breaks = seq(1860, 2030, 10)) +
    labs(title    = "Cumulative cave species discovery over time",
         x = "Year of first record", y = "Cumulative number of species") +
    theme_cfg() +
    theme(legend.position   = c(0.18, 0.85),
          axis.text.x        = element_text(angle = 45, hjust = 1))
save_plot(p, "q_species_accumulation", w = 26, h = 14)

# Q: Caves first described per decade
cat("\n--- Q: Caves first described per decade ---\n")
caves_per_decade <- first_per_cave |>
    mutate(decade = floor(first_year / 10) * 10) |>
    count(decade, name = "n_new_caves") |>
    arrange(decade) |>
    mutate(cumulative = cumsum(n_new_caves))
print(caves_per_decade)
save_tsv(caves_per_decade, "q_caves_per_decade")

p <- ggplot(caves_per_decade, aes(x = decade, y = n_new_caves)) +
    geom_col(fill = "#1a9641", width = 8, alpha = 0.85) +
    geom_text(aes(label = n_new_caves), vjust = -0.3, size = 3.2) +
    scale_x_continuous(breaks = seq(1860, 2030, 10)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title    = "Caves with first fauna records per decade",
         x = "Decade", y = "Number of caves") +
    theme_cfg() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "q_caves_per_decade", w = 22, h = 12)

# Q: Records per decade (all census records, not just firsts)
cat("\n--- Q: Records per decade ---\n")
records_per_decade <- census_long_ref |>
    filter(!is.na(Year)) |>
    distinct(Cave_ID, Species, Reference_ID, Year) |>
    mutate(decade = floor(Year / 10) * 10) |>
    count(decade, name = "n_records") |>
    arrange(decade)
print(records_per_decade)
save_tsv(records_per_decade, "q_records_per_decade")

p <- ggplot(records_per_decade, aes(x = decade, y = n_records)) +
    geom_col(fill = "#6a3d9a", width = 8, alpha = 0.85) +
    geom_text(aes(label = n_records), vjust = -0.3, size = 3.2) +
    scale_x_continuous(breaks = seq(1860, 2030, 10)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title    = "Cave fauna records (distinct cave × species × reference) per decade",
         x = "Decade", y = "Number of records") +
    theme_cfg() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "q_records_per_decade", w = 22, h = 12)

# Q: Post-2000 species
cat("\n--- Q: Species first recorded after 2000 ---\n")
post2000 <- first_per_species |>
    filter(first_year >= 2000) |>
    left_join(species |> select(Species_Full_Name, Classification, Order, Distribution),
              by = c("Species" = "Species_Full_Name")) |>
    arrange(first_year)
cat("Species first recorded ≥ 2000:", nrow(post2000), "\n")
cat("By classification:\n")
print(post2000 |> count(Classification, sort = TRUE))
save_tsv(post2000, "q_species_post2000")

################################################################
cat("\n================================================================\n")
cat("REFERENCES — AUTHOR AND JOURNAL CONTRIBUTIONS\n")
cat("================================================================\n")

# Q: Author contributions (top surnames from Short)
cat("\n--- Q: Author contributions (top 20) ---\n")
author_counts <- Census_references |>
    filter(!is.na(Short)) |>
    mutate(surname = str_extract(Short, "^[A-Za-zÀ-ÖØ-öø-ÿ''-]+")) |>
    count(surname, name = "n_refs") |>
    arrange(desc(n_refs)) |>
    filter(!is.na(surname))
print(head(author_counts, 20))
save_tsv(author_counts, "q_author_contributions")

p <- ggplot(author_counts |> head(20) |>
                mutate(surname = fct_reorder(surname, n_refs)),
            aes(x = surname, y = n_refs, fill = n_refs)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n_refs), hjust = -0.2, size = 3.5) +
    scale_fill_gradient(low = "#a8d8ea", high = "#154360") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title    = "Top 20 authors by number of references",
         subtitle = "Author surname extracted from Reference_Short",
         x = NULL, y = "Number of references") +
    theme_cfg()
save_plot(p, "q_author_contributions", w = 18, h = 14)

# Q: Journal contributions — extract journal from Section
cat("\n--- Q: Journal contributions (top 20) ---\n")
journal_counts <- Census_references |>
    filter(!is.na(Section), nchar(Section) > 2) |>
    mutate(journal = str_extract(Section, "^[A-Za-zÀ-ÿ .,'()-]+") |>
               str_trim() |>
               str_replace(",$", "") |>
               str_replace("\\.$", "")) |>
    filter(!is.na(journal), nchar(journal) > 2) |>
    count(journal, name = "n_refs") |>
    arrange(desc(n_refs))
print(head(journal_counts, 20))
save_tsv(journal_counts, "q_journal_contributions")

p <- ggplot(journal_counts |> head(20) |>
                mutate(journal = fct_reorder(journal, n_refs)),
            aes(x = journal, y = n_refs, fill = n_refs)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n_refs), hjust = -0.2, size = 3) +
    scale_fill_gradient(low = "#a8d8ea", high = "#154360") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title    = "Top 20 publication venues by number of references",
         subtitle = "Extracted from Section field (journal/book citation line)",
         x = NULL, y = "Number of references") +
    theme_cfg()
save_plot(p, "q_journal_contributions", w = 22, h = 14)

# Q: Species per reference (effort ratio histogram)
cat("\n--- Q: Species per reference ---\n")
refs_species_count <- census_long_ref |>
    filter(!is.na(Reference_ID), !grepl("\\bsp\\.$", Species)) |>
    distinct(Reference_ID, Species) |>
    count(Reference_ID, name = "n_species_per_ref")

cat("Mean species per reference:", round(mean(refs_species_count$n_species_per_ref), 1), "\n")
cat("Median species per reference:", median(refs_species_count$n_species_per_ref), "\n")
save_tsv(refs_species_count, "q_refs_species_per_ref")

p <- ggplot(refs_species_count, aes(x = n_species_per_ref)) +
    geom_histogram(bins = 40, fill = "#6a3d9a", colour = "white", linewidth = 0.2) +
    scale_x_continuous(breaks = seq(0, 300, 25)) +
    labs(title    = "Distribution of species per reference",
         subtitle = paste0("Mean = ", round(mean(refs_species_count$n_species_per_ref), 1),
                           "; Median = ", median(refs_species_count$n_species_per_ref)),
         x = "Species per reference", y = "Number of references") +
    theme_cfg()
save_plot(p, "q_refs_species_per_ref", w = 18, h = 10)

################################################################
cat("\n================================================================\n")
cat("SAMPLING COMPLETENESS (Chao1 per region)\n")
cat("================================================================\n")

# Q: Chao1 estimators per region
cat("\n--- Q: Chao1 completeness estimates per region ---\n")

# Build species × cave incidence matrix per region
# Region comes from caves data, joined via census
cave_region_map <- caves |>
    select(Cave_ID, Region) |>
    filter(!is.na(Region))

census_region <- census_all_species |>
    filter(!grepl("\\bsp\\.$", Species)) |>
    distinct(Cave_ID, Species) |>
    inner_join(cave_region_map, by = "Cave_ID")

regions <- sort(unique(census_region$Region))

chao1_results <- lapply(regions, function(reg) {
    spp_per_cave <- census_region |>
        filter(Region == reg) |>
        select(Cave_ID, Species) |>
        distinct()

    if (n_distinct(spp_per_cave$Cave_ID) < 3) return(NULL)

    # Incidence matrix: rows = caves, columns = species (presence = 1)
    inc_wide <- spp_per_cave |>
        mutate(present = 1L) |>
        pivot_wider(names_from = Species, values_from = present, values_fill = 0L)

    mat <- as.matrix(inc_wide |> select(-Cave_ID))

    # estimateR expects species × sites (columns = sites)
    # summing presences across caves gives frequency-based abundance for Chao1
    # Use column sums as species abundance vector (number of caves each species occurs in)
    col_sums <- colSums(mat)

    est <- vegan::estimateR(col_sums)

    tibble(
        Region         = reg,
        n_caves        = n_distinct(spp_per_cave$Cave_ID),
        observed_sp    = as.integer(est["S.obs"]),
        chao1          = round(est["S.chao1"], 1),
        chao1_se       = round(est["se.chao1"], 1),
        prop_observed  = round(est["S.obs"] / est["S.chao1"], 3)
    )
})

chao1_df <- bind_rows(Filter(Negate(is.null), chao1_results)) |>
    arrange(prop_observed)
print(chao1_df)
save_tsv(chao1_df, "q_sampling_completeness")

p <- ggplot(chao1_df |>
                mutate(Region = fct_reorder(Region, prop_observed)),
            aes(x = Region)) +
    geom_col(aes(y = chao1), fill = "#d3e5f5", width = 0.6) +
    geom_col(aes(y = observed_sp), fill = "#2c7bb6", width = 0.6) +
    geom_text(aes(y = chao1, label = round(prop_observed, 2)),
              hjust = -0.2, size = 3.2) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title    = "Sampling completeness by region (Chao1 estimator)",
         subtitle = "Dark blue = observed species; light blue = Chao1 estimate; label = proportion observed",
         x = NULL, y = "Number of species") +
    theme_cfg()
save_plot(p, "q_sampling_completeness", w = 20, h = 13)

# Q: Are discovery rates levelling off (open frontier vs near-complete)?
cat("\n--- Q: Is species discovery levelling off? ---\n")
# Look at rate of increase in recent decades
recent_rate <- species_per_decade |>
    filter(decade >= 1970) |>
    mutate(delta = c(NA, diff(cumulative)),
           pct_increase = round(delta / lag(cumulative) * 100, 1))
print(recent_rate)
save_tsv(recent_rate, "q_sampling_discovery_rate")

cat("\n================================================================\n")
cat("DONE — results/ and plots/ updated\n")
cat("================================================================\n")
