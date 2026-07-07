#!/usr/bin/env Rscript
# Answers to QUESTIONS.md — saves TSVs to results/ and plots to plots/.
# All spatial operations use EPSG:3035 (LAEA).

.libPaths(c("/workspace/.Rlib", .libPaths()))
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
library(RColorBrewer)

source("scripts/cfg_load_data.R")

# ── shared helpers ─────────────────────────────────────────────────────────────
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
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title   = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey40", size = base - 1))
}

clf_colours <- c(
    "Troglobiont"  = "#1f78b4",
    "Stygobiont"   = "#33a02c",
    "Troglophile"  = "#a6cee3",
    "Stygophile"   = "#b2df8a",
    "Trogloxene"   = "#ff7f00",
    "Stygoxene"    = "#fdbf6f",
    "Accidental"   = "#e31a1c"
)

cat("\n\n================================================================\n")
cat("LOADING SPATIAL DATA\n")
cat("================================================================\n")

greece_regions <- sf::st_read("spatial_data/gadm41_GRC_shp/gadm41_GRC_2.shp",
                              quiet = TRUE) |>
    sf::st_transform(crs = 3035)

caves_sf <- caves |>
    filter(!is.na(Longitude), !is.na(Latitude)) |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) |>
    st_transform(crs = 3035)

natura2000 <- sf::st_read(
    "spatial_data/N2000_spatial_GR_2021_12_09_v32/N2000_spatial_GR_2021_12_09_v32.shp",
    quiet = TRUE) |>
    sf::st_transform(crs = 3035)

# Caves in Natura2000 (spatial join)
caves_in_n2000 <- sf::st_join(caves_sf, natura2000, join = sf::st_intersects, left = FALSE) |>
    sf::st_drop_geometry() |>
    distinct(Cave_ID, Cave_Name, SITECODE, SITETYPE)

cat("Caves spatially joined to Natura2000: done\n")


################################################################
cat("\n================================================================\n")
cat("CAVES\n")
cat("================================================================\n")

# Q1: How many caves
cat("\n--- Q: How many caves? ---\n")
cat("Total caves in database:", nrow(caves), "\n")
cat("Caves with coordinates: ", nrow(caves_sf), "\n")

save_tsv(
    tibble(metric = c("Total caves", "Caves with coordinates"),
           count  = c(nrow(caves), nrow(caves_sf))),
    "q_caves_count"
)

# Q2: Cave with most species
cat("\n--- Q: Which cave has the most species? ---\n")
cave_species_count <- census_all_species |>
    filter(!is.na(Species), !grepl("\\bsp\\.$", Species)) |>
    distinct(Cave_ID, Species) |>
    group_by(Cave_ID) |>
    summarise(n_species = n(), .groups = "drop") |>
    arrange(desc(n_species)) |>
    left_join(caves |> select(Cave_ID, Cave_Name, Region, Longitude, Latitude), by = "Cave_ID")

print(head(cave_species_count, 10))
save_tsv(cave_species_count, "q_caves_most_species")

p <- ggplot(cave_species_count |> head(20) |>
                mutate(Cave_Name = fct_reorder(Cave_Name, n_species)),
            aes(x = Cave_Name, y = n_species, fill = Region)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n_species), hjust = -0.2, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    scale_fill_brewer(palette = "Set3") +
    coord_flip() +
    labs(title    = "Top 20 caves by number of species",
         subtitle = "Excluding open-ended identifications (sp.)",
         x = NULL, y = "Number of species") +
    theme_cfg() + theme(legend.position = "bottom", legend.title = element_text(size = 9))
save_plot(p, "q_caves_most_species", w = 22, h = 16)

# Q3: Caves in Natura2000 areas
cat("\n--- Q: Which caves are in Natura2000 areas? ---\n")
cat("Number of caves overlapping Natura2000 polygons:", length(unique(caves_in_n2000$Cave_ID)), "\n\n")
caves_n2000_summary <- caves_in_n2000 |>
    group_by(Cave_ID, Cave_Name) |>
    summarise(
        n2000_sites = n(),
        site_types  = paste(sort(unique(SITETYPE)), collapse = "|"),
        site_codes  = paste(sort(unique(SITECODE)), collapse = "|"),
        .groups     = "drop"
    ) |>
    arrange(Cave_Name)
print(caves_n2000_summary, n = 30)
save_tsv(caves_n2000_summary, "q_caves_in_natura2000")

# map: all caves grey, N2000 caves coloured by site type
n2000_colours <- c(
    "SCI"    = "#2ecc71", "SPA"    = "#3498db",
    "SCISPA" = "#9b59b6", "SCI|SPA" = "#e67e22",
    "Outside N2000" = "grey72"
)
caves_sf_plot <- caves_sf |>
    left_join(caves_n2000_summary |> select(Cave_ID, site_types), by = "Cave_ID") |>
    mutate(n2000_status = if_else(is.na(site_types), "Outside N2000", site_types))

p_map <- ggplot() +
    geom_sf(data = greece_regions, fill = "grey95", colour = "white", linewidth = 0.2) +
    geom_sf(data = caves_sf_plot |> filter(n2000_status == "Outside N2000"),
            colour = "grey72", size = 0.9, alpha = 0.7) +
    geom_sf(data = caves_sf_plot |> filter(n2000_status != "Outside N2000"),
            aes(colour = n2000_status), size = 1.8, alpha = 0.9) +
    scale_colour_manual(values = n2000_colours, name = "Natura2000 type",
                        guide = guide_legend(override.aes = list(size = 3))) +
    labs(title    = "Caves inside Natura2000 protected areas",
         subtitle = paste0(nrow(caves_n2000_summary), " of ", nrow(caves_sf),
                           " georeferenced caves overlap a Natura2000 polygon"),
         caption  = "Grey points: outside N2000  |  EPSG:3035 LAEA") +
    theme_cfg() +
    theme(panel.grid = element_blank(),
          legend.position   = c(0.82, 0.72),
          legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))
save_plot(p_map, "q_caves_natura2000_map", w = 22, h = 22)

# barplot: N2000 caves by region
caves_n2000_region <- caves_in_n2000 |>
    left_join(caves |> select(Cave_ID, Region), by = "Cave_ID") |>
    distinct(Cave_ID, Region) |>
    count(Region, name = "n_caves") |>
    arrange(desc(n_caves))

p_reg <- ggplot(caves_n2000_region,
                aes(x = fct_reorder(Region, n_caves), y = n_caves, fill = Region)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n_caves), hjust = -0.2, size = 3.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    scale_fill_brewer(palette = "Set3") +
    coord_flip() +
    labs(title = "Caves in Natura2000 areas by region",
         x = NULL, y = "Number of caves") +
    theme_cfg()
save_plot(p_reg, "q_caves_natura2000_by_region", w = 18, h = 12)

# Q4: Caves with only troglobiont AND endemic AND single-cave species
cat("\n--- Q: Caves with only troglobiont + endemic + single-cave species? ---\n")

# Step 1: species that are troglobiont, endemic, and occur in exactly 1 cave
species_cave_counts <- census_all_species |>
    filter(!grepl("\\bsp\\.$", Species)) |>
    distinct(Cave_ID, Species) |>
    group_by(Species) |>
    summarise(n_caves = n(), .groups = "drop")

strict_species <- species |>
    filter(Classification == "Troglobiont",
           Distribution  == "Endemic to Greece") |>
    left_join(species_cave_counts, by = c("Species_Full_Name" = "Species")) |>
    filter(n_caves == 1)

cat("Troglobiont + endemic + single-cave species:", nrow(strict_species), "\n")

# Step 2: caves where ALL their species (excluding sp.) belong to strict_species
cave_species_strict <- census_all_species |>
    filter(!grepl("\\bsp\\.$", Species)) |>
    distinct(Cave_ID, Species) |>
    group_by(Cave_ID) |>
    summarise(
        n_total  = n(),
        n_strict = sum(Species %in% strict_species$Species_Full_Name),
        .groups  = "drop"
    ) |>
    filter(n_total > 0, n_total == n_strict) |>
    left_join(caves |> select(Cave_ID, Cave_Name, Region), by = "Cave_ID") |>
    arrange(Cave_Name)

cat("Caves where ALL species are troglobiont + endemic + single-cave:\n")
print(cave_species_strict)
save_tsv(cave_species_strict, "q_caves_strict")

# also save which species are in each strict cave
strict_cave_species <- census_all_species |>
    filter(Cave_ID %in% cave_species_strict$Cave_ID,
           !grepl("\\bsp\\.$", Species)) |>
    distinct(Cave_ID, Species) |>
    left_join(cave_species_strict |> select(Cave_ID, Cave_Name, Region), by = "Cave_ID") |>
    left_join(species |> select(Species_Full_Name, Order), by = c("Species" = "Species_Full_Name"))
save_tsv(strict_cave_species, "q_caves_strict_species")

p_strict <- ggplot(strict_cave_species,
    aes(x = fct_reorder(Cave_Name, Cave_ID), y = Species, colour = Order)) +
    geom_point(size = 4) +
    scale_colour_brewer(palette = "Dark2") +
    labs(title    = "Caves with only troglobiont + endemic + single-cave species",
         subtitle = "Each dot is one species; all species in these caves meet all three criteria",
         x = NULL, y = NULL) +
    theme_cfg() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 9),
          axis.text.y = element_text(size = 8),
          legend.position = "bottom")
save_plot(p_strict, "q_caves_strict", w = 20, h = 10)


################################################################
cat("\n================================================================\n")
cat("SPECIES\n")
cat("================================================================\n")

# Q1: How many species
cat("\n--- Q: How many species? ---\n")
cat("Total species in database:", nrow(species), "\n")
cat("Distinct species in census:", length(unique(census$Species)), "\n")

# Q2: Species with most occurrences (= most distinct caves)
cat("\n--- Q: Which species has the most occurrences (caves)? ---\n")
species_occ <- census_all_species |>
    filter(!grepl("\\bsp\\.$", Species)) |>
    distinct(Cave_ID, Species) |>
    group_by(Species) |>
    summarise(n_caves = n(), .groups = "drop") |>
    arrange(desc(n_caves)) |>
    left_join(species |> select(Species_Full_Name, Classification, Distribution, Order, Class),
              by = c("Species" = "Species_Full_Name"))
print(head(species_occ, 10))
save_tsv(species_occ, "q_species_most_occurrences")

p <- ggplot(species_occ |> head(20) |>
                mutate(Species = fct_reorder(Species, n_caves)),
            aes(x = Species, y = n_caves, fill = Classification)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n_caves), hjust = -0.2, size = 3) +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title    = "Top 20 species by number of caves",
         subtitle = "Distinct cave records per species (excluding sp.)",
         x = NULL, y = "Number of caves") +
    theme_cfg() + theme(legend.position = "bottom")
save_plot(p, "q_species_most_occurrences", w = 24, h = 16)

# Q3: Troglobiont species with most occurrences
cat("\n--- Q: Which troglobiont species has the most occurrences? ---\n")
species_occ_trogl <- species_occ |>
    filter(Classification == "Troglobiont")
print(head(species_occ_trogl, 10))
save_tsv(species_occ_trogl, "q_species_troglobiont_occurrences")

p <- ggplot(species_occ_trogl |> head(20) |>
                mutate(Species = fct_reorder(Species, n_caves)),
            aes(x = Species, y = n_caves, fill = Order)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n_caves), hjust = -0.2, size = 3) +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title    = "Top 20 troglobiont species by number of caves",
         subtitle = "Obligate cave-dwellers; coloured by order",
         x = NULL, y = "Number of caves") +
    theme_cfg() + theme(legend.position = "bottom")
save_plot(p, "q_species_troglobiont_occurrences", w = 24, h = 16)

# Q4: Genus with most endemic-to-Greece species
cat("\n--- Q: Which genus has the most endemic-to-Greece species? ---\n")
genus_endemic <- species |>
    filter(Distribution == "Endemic to Greece",
           !grepl("\\bsp\\.$", Species_Full_Name)) |>
    group_by(Genus) |>
    summarise(n_endemic = n(), .groups = "drop") |>
    arrange(desc(n_endemic))
print(head(genus_endemic, 10))
save_tsv(genus_endemic, "q_genus_endemic_species")

p <- ggplot(genus_endemic |> head(20) |>
                mutate(Genus = fct_reorder(Genus, n_endemic)),
            aes(x = Genus, y = n_endemic, fill = n_endemic)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n_endemic), hjust = -0.2, size = 3.5) +
    scale_fill_gradient(low = "#a8d8ea", high = "#1a3a5c") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title    = "Top 20 genera by number of endemic-to-Greece species",
         x = NULL, y = "Number of endemic species") +
    theme_cfg()
save_plot(p, "q_genus_endemic_species", w = 18, h = 14)

# Q5: Species only in islands
cat("\n--- Q: Which species are only in islands? ---\n")
island_only_species <- read_delim("results/cfg_species_distribution.tsv",
                                   delim = "\t", show_col_types = FALSE) |>
    filter(distribution == "Island only") |>
    left_join(species |> select(Species_Full_Name, Classification, Distribution, Order),
              by = c("Species" = "Species_Full_Name"))
cat("Species only on islands:", nrow(island_only_species), "\n")
print(island_only_species)
save_tsv(island_only_species, "q_island_only_species")

island_clf <- island_only_species |>
    count(Classification, name = "n") |>
    mutate(Classification = factor(Classification, levels = names(clf_colours)))
p <- ggplot(island_clf,
            aes(x = fct_reorder(Classification, n), y = n, fill = Classification)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n), hjust = -0.2, size = 4) +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    coord_flip() +
    labs(title    = "Island-only species by ecological classification",
         subtitle = paste0(nrow(island_only_species), " species with no mainland records"),
         x = NULL, y = "Number of species") +
    theme_cfg()
save_plot(p, "q_island_only_species_classification", w = 16, h = 10)

# Q6: Endemic-to-Greece species only in islands
cat("\n--- Q: Which endemic-to-Greece species are only in islands? ---\n")
endemic_island_only <- island_only_species |>
    filter(Distribution == "Endemic to Greece")
cat("Endemic + island-only species:", nrow(endemic_island_only), "\n")
print(endemic_island_only)
save_tsv(endemic_island_only, "q_endemic_island_only_species")

endemic_island_order <- endemic_island_only |>
    count(Order, Classification, name = "n") |>
    mutate(Classification = factor(Classification, levels = names(clf_colours)))
p <- ggplot(endemic_island_order,
            aes(x = fct_reorder(Order, n, sum), y = n, fill = Classification)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n, group = Classification),
              position = position_stack(vjust = 0.5),
              size = 3, colour = "white", fontface = "bold") +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip() +
    labs(title    = "Endemic-to-Greece island-only species by Order",
         subtitle = paste0(nrow(endemic_island_only),
                           " species endemic to Greece found only on islands"),
         x = NULL, y = "Number of species") +
    theme_cfg() + theme(legend.position = "bottom")
save_plot(p, "q_endemic_island_only_species", w = 18, h = 14)

# Q7: Endemic-to-Greece species only in islands EXCLUDING Crete
cat("\n--- Q: Endemic-to-Greece species only on islands excluding Crete? ---\n")
island_species_detail <- read_delim("results/cfg_island_species.tsv",
                                     delim = "\t", show_col_types = FALSE)

species_no_crete <- island_species_detail |>
    filter(region_type == "Island") |>
    group_by(Species) |>
    summarise(
        on_crete      = any(NAME_2 == "Crete"),
        only_on_crete = all(NAME_2 == "Crete"),
        .groups       = "drop"
    )

# Species with records only on non-Crete islands (no mainland, no Crete)
mainland_species <- island_species_detail |>
    filter(region_type == "Mainland") |>
    distinct(Species) |>
    pull(Species)

endemic_island_no_crete <- species_no_crete |>
    filter(!Species %in% mainland_species,   # not on mainland
           !on_crete) |>                     # not on Crete at all
    left_join(species |> select(Species_Full_Name, Classification, Distribution, Order),
              by = c("Species" = "Species_Full_Name")) |>
    filter(Distribution == "Endemic to Greece")

cat("Endemic + island-only (excl. Crete) species:", nrow(endemic_island_no_crete), "\n")
print(endemic_island_no_crete)
save_tsv(endemic_island_no_crete, "q_endemic_island_excl_crete")

no_crete_order <- endemic_island_no_crete |>
    count(Order, Classification, name = "n") |>
    mutate(Classification = factor(Classification, levels = names(clf_colours)))
p <- ggplot(no_crete_order,
            aes(x = fct_reorder(Order, n, sum), y = n, fill = Classification)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n, group = Classification),
              position = position_stack(vjust = 0.5),
              size = 3, colour = "white", fontface = "bold") +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                       breaks = scales::breaks_pretty()) +
    coord_flip() +
    labs(title    = "Endemic-to-Greece species on islands only (excluding Crete)",
         subtitle = paste0(nrow(endemic_island_no_crete),
                           " species — by order and ecological classification"),
         x = NULL, y = "Number of species") +
    theme_cfg() + theme(legend.position = "bottom")
save_plot(p, "q_endemic_island_excl_crete", w = 18, h = 12)

# Q8: Species in Natura2000 areas — summary and endemic
cat("\n--- Q: Species in Natura2000 areas — summary and endemic subset ---\n")
species_in_n2000 <- census_all_species |>
    filter(Cave_ID %in% caves_in_n2000$Cave_ID,
           !grepl("\\bsp\\.$", Species)) |>
    distinct(Species) |>
    left_join(species |> select(Species_Full_Name, Classification, Distribution, Order, Class),
              by = c("Species" = "Species_Full_Name"))

cat("Total species in Natura2000 caves:", nrow(species_in_n2000), "\n")
cat("\nBy classification:\n")
print(species_in_n2000 |> count(Classification, sort = TRUE))
cat("\nEndemic to Greece in Natura2000:\n")
endemic_n2000 <- species_in_n2000 |> filter(Distribution == "Endemic to Greece")
cat(nrow(endemic_n2000), "endemic species\n")
print(endemic_n2000 |> count(Classification, sort = TRUE))
save_tsv(species_in_n2000, "q_natura2000_species")

n2000_clf <- bind_rows(
    species_in_n2000 |> count(Classification, name = "n") |> mutate(subset = "All species"),
    endemic_n2000    |> count(Classification, name = "n") |> mutate(subset = "Endemic to Greece")
) |> mutate(Classification = factor(Classification, levels = names(clf_colours)))
p <- ggplot(n2000_clf,
            aes(x = fct_reorder(Classification, n, sum), y = n, fill = Classification)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    facet_wrap(~subset, scales = "free_y") +
    labs(title    = "Species in Natura2000 caves by ecological classification",
         subtitle = paste0(nrow(species_in_n2000), " total species; ",
                           nrow(endemic_n2000), " endemic to Greece"),
         x = NULL, y = "Number of species") +
    theme_cfg() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          strip.text   = element_text(face = "bold"))
save_plot(p, "q_natura2000_species_classification", w = 22, h = 12)

# Q9: Species ONLY in Natura2000 areas
cat("\n--- Q: Species only in Natura2000 areas? ---\n")
all_cave_ids_per_species <- census_all_species |>
    filter(!grepl("\\bsp\\.$", Species)) |>
    distinct(Cave_ID, Species)

n2000_cave_ids <- caves_in_n2000$Cave_ID

species_only_n2000 <- all_cave_ids_per_species |>
    group_by(Species) |>
    summarise(
        all_in_n2000 = all(Cave_ID %in% n2000_cave_ids),
        n_caves      = n(),
        .groups      = "drop"
    ) |>
    filter(all_in_n2000) |>
    left_join(species |> select(Species_Full_Name, Classification, Distribution, Order, Class),
              by = c("Species" = "Species_Full_Name")) |>
    arrange(Classification, Species)

cat("Species found exclusively in Natura2000 caves:", nrow(species_only_n2000), "\n")
cat("\nBy classification:\n")
print(species_only_n2000 |> count(Classification, sort = TRUE))
cat("\nEndemic subset:\n")
print(species_only_n2000 |> filter(Distribution == "Endemic to Greece") |>
      select(Species, Classification, Order, n_caves))
save_tsv(species_only_n2000, "q_species_only_in_natura2000")

only_n2000_clf <- bind_rows(
    species_only_n2000 |> count(Classification, name = "n") |> mutate(subset = "All species"),
    species_only_n2000 |> filter(Distribution == "Endemic to Greece") |>
        count(Classification, name = "n") |> mutate(subset = "Endemic to Greece")
) |> mutate(Classification = factor(Classification, levels = names(clf_colours)))
p <- ggplot(only_n2000_clf,
            aes(x = fct_reorder(Classification, n, sum), y = n, fill = Classification)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    facet_wrap(~subset, scales = "free_y") +
    labs(title    = "Species found EXCLUSIVELY in Natura2000 caves",
         subtitle = paste0(nrow(species_only_n2000), " total; ",
                           sum(species_only_n2000$Distribution == "Endemic to Greece"),
                           " endemic to Greece"),
         x = NULL, y = "Number of species") +
    theme_cfg() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          strip.text   = element_text(face = "bold"))
save_plot(p, "q_species_only_natura2000", w = 22, h = 12)


################################################################
cat("\n================================================================\n")
cat("REFERENCES\n")
cat("================================================================\n")

census_long_ref <- census_long_man |>
    left_join(Census_references, by = c("Reference_ID" = "ID"))

# Q1: Oldest reference
cat("\n--- Q: Oldest reference of cave fauna in Greece? ---\n")
oldest <- Census_references |>
    filter(!is.na(Year)) |>
    arrange(Year) |>
    select(ID, Short, Title, Year, Section) |>
    head(10)
print(oldest)
save_tsv(oldest, "q_references_oldest")

# Q2: First reference that described a new species (= earliest first-occurrence record)
cat("\n--- Q: First reference to introduce a new species? ---\n")
first_occurrence_per_species <- census_long_ref |>
    filter(!is.na(Year),
           !grepl("\\bsp\\.$", Species)) |>
    distinct(Species, Reference_ID, Year, Short, Title) |>
    arrange(Year) |>
    mutate(is_first = !duplicated(Species)) |>
    filter(is_first) |>
    arrange(Year)

# Earliest reference that first introduced any species
cat("Earliest 10 first-species introductions:\n")
print(head(first_occurrence_per_species |>
           select(Year, Short, Species, Reference_ID), 10))

first_new_species_ref <- first_occurrence_per_species |>
    group_by(Reference_ID, Short, Title, Year) |>
    summarise(n_new_species = n(), .groups = "drop") |>
    arrange(Year)

cat("\nFirst reference that described a new species:\n")
print(head(first_new_species_ref, 1))
save_tsv(first_new_species_ref, "q_references_first_new_species_per_ref")
save_tsv(first_occurrence_per_species |> select(Year, Short, Title, Species, Reference_ID),
         "q_references_first_occurrence_per_species")

# Q3: Top 5 references that first introduced the most species
cat("\n--- Q: Top 5 references that first introduced most species? ---\n")
top5_refs <- first_new_species_ref |>
    arrange(desc(n_new_species)) |>
    head(5)
print(top5_refs)
save_tsv(top5_refs, "q_references_top5")

top20_refs <- first_new_species_ref |>
    arrange(desc(n_new_species)) |>
    head(20) |>
    mutate(label = paste0(Short, " (", Year, ")"),
           label = fct_reorder(label, n_new_species))
p <- ggplot(top20_refs,
            aes(x = label, y = n_new_species, fill = Year)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n_new_species), hjust = -0.2, size = 3.5) +
    scale_fill_gradient(low = "#a8d8ea", high = "#154360", name = "Year") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title    = "Top 20 references by number of new species introduced",
         subtitle = "Each species counted once, at its earliest reference in the database",
         x = NULL, y = "Number of new species introduced") +
    theme_cfg() + theme(legend.position = "right")
save_plot(p, "q_references_top20_new_species", w = 24, h = 16)

# species knowledge accumulation curve
accum_all <- first_occurrence_per_species |>
    arrange(Year) |>
    mutate(cumulative = row_number()) |>
    group_by(Year) |> slice_max(cumulative, n = 1) |> ungroup() |>
    mutate(type = "All species")

accum_endemic <- first_occurrence_per_species |>
    left_join(species |> select(Species_Full_Name, Distribution),
              by = c("Species" = "Species_Full_Name")) |>
    filter(Distribution == "Endemic to Greece") |>
    arrange(Year) |>
    mutate(cumulative = row_number()) |>
    group_by(Year) |> slice_max(cumulative, n = 1) |> ungroup() |>
    mutate(type = "Endemic to Greece")

p_accum <- ggplot(bind_rows(accum_all, accum_endemic),
                  aes(x = Year, y = cumulative, colour = type)) +
    geom_line(linewidth = 1) +
    geom_point(data = top5_refs,
               aes(x = Year, y = 0), inherit.aes = FALSE,
               shape = 21, fill = "#e74c3c", colour = "white", size = 3) +
    geom_text(data = top5_refs,
              aes(x = Year, y = 20, label = Short), inherit.aes = FALSE,
              angle = 90, hjust = 0, size = 2.6, colour = "#c0392b") +
    scale_colour_manual(values = c("All species" = "#2c7bb6",
                                   "Endemic to Greece" = "#d73027"),
                        name = NULL) +
    scale_x_continuous(breaks = seq(1860, 2030, 10)) +
    scale_y_continuous(breaks = seq(0, 1000, 100)) +
    labs(title    = "Cumulative species knowledge in Greek caves",
         subtitle = "Red markers: top 5 references by new species introduced",
         x = "Year", y = "Cumulative number of species") +
    theme_cfg() +
    theme(legend.position = c(0.18, 0.85),
          axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p_accum, "q_references_accumulation_curve", w = 26, h = 15)

cat("\n================================================================\n")
cat("DONE — results saved to results/ and plots saved to plots/\n")
cat("================================================================\n")
