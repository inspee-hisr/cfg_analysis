#!/usr/bin/env Rscript
# Cave-specific analysis for Spilaio Evripidi (Cave ID 117).
# Loads results/cfg_data_long.tsv directly — no spatial dependencies.
# Outputs: results/evripidi_*.tsv  |  plots/evripidi_*.png

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
library(stringr)
library(scales)

source("scripts/cfg_plot_style.R")

# ── helpers ───────────────────────────────────────────────────────────────────
save_tsv  <- function(df, name) {
    write_delim(df, file.path("results", paste0(name, ".tsv")), delim = "\t")
    invisible(df)
}
save_plot <- function(p, name, w = 18, h = 12) {
    ggsave(file.path("plots", paste0(name, ".png")),
           plot = p, width = w, height = h, units = "cm", dpi = 300)
    invisible(p)
}

# ── load & filter ─────────────────────────────────────────────────────────────
cat("\nLoading cfg_data_long.tsv …\n")
data_long <- read_delim("results/cfg_data_long.tsv", delim = "\t",
                        show_col_types = FALSE)

evripidi <- data_long |>
    filter(CaveName == "Spilaio Evripidi")

cat(sprintf("Spilaio Evripidi: %d rows (species × reference combinations)\n",
            nrow(evripidi)))

# One row per species (deduplicate multi-reference rows)
evripidi_sp <- evripidi |>
    distinct(Species, .keep_all = TRUE)

# ── cave metadata ─────────────────────────────────────────────────────────────
cave_meta <- evripidi_sp |>
    select(Cave_ID, Cave_Name, Region, Municipality, Altitude,
           Longitude, Latitude, Cave_Type) |>
    slice(1)

cat("\n--- Cave metadata ---\n")
print(t(cave_meta))

################################################################
cat("\n================================================================\n")
cat("SPECIES SUMMARIES\n")
cat("================================================================\n")

n_species      <- n_distinct(evripidi$Species)
n_endemic      <- evripidi_sp |> filter(Distribution == "Endemic to Greece") |> nrow()
n_cave_endemic <- evripidi_sp |>
    filter(Locus_Typicus_Cave == "Spilaio Evripidi") |> nrow()
n_troglo       <- evripidi_sp |> filter(Classification == "Troglobiont")  |> nrow()
n_stygo        <- evripidi_sp |> filter(Classification == "Stygobiont")   |> nrow()
n_obligate     <- n_troglo + n_stygo

species_summary <- tibble(
    metric = c(
        "Total species",
        "Endemic to Greece",
        "Cave-endemic (described from Evripidi)",
        "Troglobionts",
        "Stygobionts",
        "Obligate cave species (troglo + stygo)"
    ),
    count = c(n_species, n_endemic, n_cave_endemic,
              n_troglo, n_stygo, n_obligate)
)
print(species_summary)
save_tsv(species_summary, "evripidi_species_summary")

# ── species by ecological classification ─────────────────────────────────────
clf_counts <- evripidi_sp |>
    count(Classification, name = "n_species") |>
    mutate(Classification = fct_reorder(Classification, n_species))

cat("\n--- Species by classification ---\n")
print(clf_counts)
save_tsv(clf_counts, "evripidi_classification")

p_clf <- ggplot(clf_counts, aes(x = Classification, y = n_species,
                                 fill = Classification)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = n_species), hjust = -0.3, size = 4,
              colour = "#333333") +
    scale_fill_manual(values = clf_colours,
                      na.value = "#bbbbbb") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    coord_flip() +
    labs(title    = "Spilaio Evripidi — species by ecological classification",
         subtitle = sprintf("%d species total", n_species),
         x = NULL, y = "Number of species") +
    theme_cfg_bar()
save_plot(p_clf, "evripidi_classification", w = 18, h = 10)

# ── species by taxonomic order ────────────────────────────────────────────────
order_counts <- evripidi_sp |>
    count(Order, name = "n_species") |>
    mutate(Order = fct_reorder(Order, n_species))

cat("\n--- Species by Order ---\n")
print(order_counts)
save_tsv(order_counts, "evripidi_orders")

p_ord <- ggplot(order_counts, aes(x = Order, y = n_species)) +
    geom_col(fill = okabe["blue"], width = 0.65) +
    geom_text(aes(label = n_species), hjust = -0.3, size = 3.5,
              colour = "#333333") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    coord_flip() +
    labs(title    = "Spilaio Evripidi — species by taxonomic order",
         subtitle = sprintf("%d orders represented", nrow(order_counts)),
         x = NULL, y = "Number of species") +
    theme_cfg_bar()
save_plot(p_ord, "evripidi_orders", w = 18, h = 12)

# ── conservation status ───────────────────────────────────────────────────────
iucn_counts <- evripidi_sp |>
    mutate(IUCN_short = str_extract(IUCN_Red_List, "^[A-Z]+")) |>
    count(IUCN_short, name = "n_species") |>
    filter(!is.na(IUCN_short)) |>
    mutate(IUCN_short = fct_relevel(IUCN_short,
                                    "CR", "EN", "VU", "NT", "LC", "DD", "NE"))

cat("\n--- IUCN Red List status ---\n")
print(iucn_counts)
save_tsv(iucn_counts, "evripidi_iucn")

iucn_colours <- c(
    CR = "#d7191c", EN = "#f46d43", VU = "#fdae61",
    NT = "#fee08b", LC = "#a6d96a", DD = "#aaaaaa", NE = "#dddddd"
)

p_iucn <- ggplot(iucn_counts, aes(x = IUCN_short, y = n_species,
                                    fill = IUCN_short)) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = n_species), vjust = -0.4, size = 4,
              colour = "#333333") +
    scale_fill_manual(values = iucn_colours, na.value = "#cccccc") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title    = "Spilaio Evripidi — IUCN Red List status of species",
         subtitle = "CR=Critically Endangered · EN=Endangered · VU=Vulnerable · NT=Near Threatened · LC=Least Concern · NE=Not Evaluated",
         x = "IUCN category", y = "Number of species") +
    theme_cfg()
save_plot(p_iucn, "evripidi_iucn", w = 16, h = 10)

################################################################
cat("\n================================================================\n")
cat("REFERENCE SUMMARIES\n")
cat("================================================================\n")

refs <- evripidi |>
    filter(!is.na(Short)) |>
    distinct(Reference_ID, Short, Title, Year)

n_refs       <- nrow(refs)
year_first   <- min(refs$Year, na.rm = TRUE)
year_last    <- max(refs$Year, na.rm = TRUE)
most_recent  <- refs |> filter(Year == year_last) |> pull(Short) |> paste(collapse = "; ")

ref_summary <- tibble(
    metric = c("Total references", "First publication year",
               "Most recent publication year", "Most recent reference(s)"),
    value  = c(as.character(n_refs), as.character(year_first),
               as.character(year_last), most_recent)
)
print(ref_summary)
save_tsv(ref_summary, "evripidi_ref_summary")

# species per reference
ref_species <- evripidi |>
    filter(!is.na(Short)) |>
    distinct(Reference_ID, Short, Year, Species) |>
    count(Reference_ID, Short, Year, name = "n_species") |>
    arrange(desc(n_species)) |>
    mutate(label = paste0(Short, " (", Year, ")"),
           label = fct_reorder(label, n_species))

cat("\n--- Species per reference ---\n")
print(ref_species |> select(Short, Year, n_species))
save_tsv(ref_species, "evripidi_ref_species")

p_refs <- ggplot(ref_species, aes(x = label, y = n_species)) +
    geom_col(fill = okabe["orange"], width = 0.65) +
    geom_text(aes(label = n_species), hjust = -0.3, size = 3.5,
              colour = "#333333") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    coord_flip() +
    labs(title    = "Spilaio Evripidi — species documented per reference",
         subtitle = sprintf("%d references total", n_refs),
         x = NULL, y = "Number of species") +
    theme_cfg_bar(base = 10)
save_plot(p_refs, "evripidi_ref_species", w = 20, h = 14)

# references per year (timeline bar)
refs_per_year <- refs |>
    count(Year, name = "n_refs")

p_refsyr <- ggplot(refs_per_year, aes(x = Year, y = n_refs)) +
    geom_col(fill = okabe["sky_blue"], width = 0.8) +
    geom_text(aes(label = n_refs), vjust = -0.4, size = 3.5,
              colour = "#333333") +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(title    = "Spilaio Evripidi — publications per year",
         subtitle = sprintf("Span: %d – %d (%d references total)",
                            year_first, year_last, n_refs),
         x = "Year", y = "Number of references") +
    theme_cfg()
save_plot(p_refsyr, "evripidi_refs_per_year", w = 18, h = 10)

################################################################
cat("\n================================================================\n")
cat("SPECIES ACCUMULATION CURVE\n")
cat("================================================================\n")

# First year each species was recorded in the cave
first_record <- evripidi |>
    filter(!is.na(Year)) |>
    group_by(Species) |>
    summarise(first_year = min(Year, na.rm = TRUE), .groups = "drop")

accum <- first_record |>
    count(first_year, name = "new_species") |>
    arrange(first_year) |>
    mutate(cumulative = cumsum(new_species))

cat("\n--- Species accumulation by year ---\n")
print(accum)
save_tsv(accum, "evripidi_species_accumulation")

p_accum <- ggplot(accum, aes(x = first_year)) +
    geom_col(aes(y = new_species), fill = okabe["green"],
             width = 0.8, alpha = 0.7) +
    geom_line(aes(y = cumulative), colour = okabe["blue"],
              linewidth = 1.1) +
    geom_point(aes(y = cumulative), colour = okabe["blue"],
               size = 2.5) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title    = "Spilaio Evripidi — species accumulation over time",
         subtitle = "Bars = new species per year · Line = cumulative total",
         x = "Year of first record", y = "Number of species") +
    theme_cfg()
save_plot(p_accum, "evripidi_species_accumulation", w = 18, h = 11)

# accumulation coloured by classification
first_record_clf <- evripidi |>
    filter(!is.na(Year)) |>
    group_by(Species) |>
    summarise(first_year    = min(Year, na.rm = TRUE),
              Classification = first(Classification),
              .groups = "drop")

accum_clf <- first_record_clf |>
    count(first_year, Classification, name = "new_species") |>
    arrange(first_year) |>
    group_by(Classification) |>
    mutate(cumulative = cumsum(new_species)) |>
    ungroup()

p_accum_clf <- ggplot(accum_clf,
                       aes(x = first_year, y = cumulative,
                           colour = Classification)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2) +
    scale_colour_manual(values = clf_colours, na.value = "#bbbbbb") +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title    = "Spilaio Evripidi — cumulative species by classification",
         subtitle = "Each line = running total for one ecological category",
         x = "Year of first record", y = "Cumulative species",
         colour = "Classification") +
    theme_cfg()
save_plot(p_accum_clf, "evripidi_accumulation_by_classification", w = 20, h = 12)

################################################################
cat("\n================================================================\n")
cat("CAVE-ENDEMIC SPECIES DETAIL\n")
cat("================================================================\n")

cave_endemics <- evripidi_sp |>
    filter(Locus_Typicus_Cave == "Spilaio Evripidi") |>
    select(Species, Order, Class, Classification, Distribution,
           IUCN_Red_List, Greek_Red_Data_Book)

cat("\n--- Species described from Spilaio Evripidi ---\n")
print(cave_endemics)
save_tsv(cave_endemics, "evripidi_cave_endemics")

cat("\n================================================================\n")
cat("DONE — outputs written to results/evripidi_*.tsv and plots/evripidi_*.png\n")
cat("================================================================\n")
