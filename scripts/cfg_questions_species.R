#!/usr/bin/env Rscript
# Species questions: taxonomy, macroecology, conservation status, shortfalls.
# Outputs: results/q_species_*.tsv, plots/q_species_*.png
# Requires cfg_spatial_analysis.R outputs for island distribution.

library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(scales)

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
cat("LOADING SPATIAL DATA FOR CHOROPLETH MAPS\n")
cat("================================================================\n")

greece_regions <- sf::st_read("spatial_data/gadm41_GRC_shp/gadm41_GRC_2.shp", quiet = TRUE) |>
    sf::st_transform(crs = 3035)

caves_sf <- caves |>
    filter(!is.na(Longitude), !is.na(Latitude)) |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) |>
    st_transform(crs = 3035)

# Assign GADM NAME_2 to each cave via spatial join
caves_region <- sf::st_join(caves_sf, greece_regions |> select(NAME_2),
                             join = sf::st_intersects) |>
    sf::st_drop_geometry() |>
    select(Cave_ID, NAME_2) |>
    distinct()

# Load precomputed island/mainland distribution
island_species_df <- read_delim("results/cfg_island_species.tsv",
                                 delim = "\t", show_col_types = FALSE)
species_dist_df   <- read_delim("results/cfg_species_distribution.tsv",
                                 delim = "\t", show_col_types = FALSE)

natura2000 <- sf::st_read(
    "spatial_data/N2000_spatial_GR_2021_12_09_v32/N2000_spatial_GR_2021_12_09_v32.shp",
    quiet = TRUE) |>
    sf::st_transform(crs = 3035)

caves_in_n2000 <- sf::st_join(caves_sf, natura2000, join = sf::st_intersects, left = FALSE) |>
    sf::st_drop_geometry() |>
    distinct(Cave_ID)

cat("Spatial joins done\n")

################################################################
cat("\n================================================================\n")
cat("SPECIES — TAXONOMY\n")
cat("================================================================\n")

# Q: Orders, families, genera counts
cat("\n--- Q: Taxonomic inventory ---\n")
tax_summary <- tibble(
    level  = c("Orders", "Families", "Genera", "Species"),
    count  = c(n_distinct(species$Order), n_distinct(species$Family),
               n_distinct(species$Genus), nrow(species))
)
print(tax_summary)
save_tsv(tax_summary, "q_species_taxonomy_summary")

# Species per order (top 10)
sp_per_order <- species |>
    filter(!grepl("\\bsp\\.$", Species_Full_Name)) |>
    count(Order, Classification, name = "n") |>
    mutate(Classification = factor(Classification, levels = names(clf_colours)))

order_totals <- sp_per_order |>
    group_by(Order) |> summarise(total = sum(n), .groups = "drop") |>
    arrange(desc(total))

cat("Top 10 orders by species count:\n")
print(head(order_totals, 10))
save_tsv(sp_per_order, "q_species_taxonomy")

p <- ggplot(sp_per_order |>
                semi_join(order_totals |> head(15), by = "Order") |>
                mutate(Order = factor(Order, levels = head(order_totals$Order, 15))),
            aes(x = Order, y = n, fill = Classification)) +
    geom_col(width = 0.75) +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip() +
    labs(title    = "Species by order and ecological classification (top 15 orders)",
         x = NULL, y = "Number of species") +
    theme_cfg_bar() + theme(legend.position = "bottom")
save_plot(p, "q_species_taxonomy", w = 22, h = 16)

################################################################
cat("\n================================================================\n")
cat("SPECIES — CLASSIFICATION\n")
cat("================================================================\n")

# Q: Classification breakdown + obligate proportion
cat("\n--- Q: Classification breakdown ---\n")
clf_breakdown <- species |>
    count(Classification, name = "n") |>
    mutate(prop = round(n / sum(n), 3),
           is_obligate = Classification %in% c("Troglobiont", "Stygobiont")) |>
    arrange(desc(n))
print(clf_breakdown)
cat("Obligate proportion:",
    round(sum(clf_breakdown$n[clf_breakdown$is_obligate]) / sum(clf_breakdown$n), 3), "\n")
save_tsv(clf_breakdown, "q_species_classification")

p <- ggplot(clf_breakdown |>
                mutate(Classification = factor(Classification, levels = names(clf_colours))),
            aes(x = fct_reorder(Classification, n), y = n, fill = Classification)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = paste0(n, " (", percent(prop, accuracy = 0.1), ")")),
              hjust = -0.1, size = 3.5) +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    coord_flip() +
    labs(title    = "Species by ecological classification",
         x = NULL, y = "Number of species") +
    theme_cfg_bar()
save_plot(p, "q_species_classification", w = 18, h = 10)

# Q: Single-cave species
cat("\n--- Q: Single-cave species ---\n")
species_cave_counts <- census_all_species |>
    filter(!grepl("\\bsp\\.$", Species)) |>
    distinct(Cave_ID, Species) |>
    count(Species, name = "n_caves")

single_cave <- species_cave_counts |>
    filter(n_caves == 1) |>
    left_join(species |> select(Species_Full_Name, Order, Classification, Distribution),
              by = c("Species" = "Species_Full_Name"))
cat("Single-cave species:", nrow(single_cave), "\n")
cat("By order:\n")
print(single_cave |> count(Order, sort = TRUE) |> head(10))
save_tsv(single_cave, "q_species_singlecave")

p <- single_cave |>
    count(Order, name = "n") |>
    arrange(desc(n)) |>
    head(15) |>
    mutate(Order = fct_reorder(Order, n)) |>
    ggplot(aes(x = Order, y = n, fill = Order)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n), hjust = -0.2, size = 3.5, colour = "#333333") +
    scale_fill_viridis_d(option = "D", direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    coord_flip() +
    labs(title    = "Single-cave species by order (top 15)",
         subtitle = paste0(nrow(single_cave), " species recorded in only 1 cave"),
         x = NULL, y = "Number of species") +
    theme_cfg_bar()
save_plot(p, "q_species_singlecave", w = 18, h = 12)

# Q: Stygobiont species with most occurrences
cat("\n--- Q: Stygobiont species occurrences ---\n")
species_occ <- census_all_species |>
    filter(!grepl("\\bsp\\.$", Species)) |>
    distinct(Cave_ID, Species) |>
    count(Species, name = "n_caves") |>
    left_join(species |> select(Species_Full_Name, Classification, Order),
              by = c("Species" = "Species_Full_Name"))

stygo_occ <- species_occ |>
    filter(Classification == "Stygobiont") |>
    arrange(desc(n_caves))
cat("Stygobiont species:\n")
print(head(stygo_occ, 10))
save_tsv(stygo_occ, "q_species_stygobiont")

p <- ggplot(stygo_occ |> head(20) |>
                mutate(Species = fct_reorder(Species, n_caves)),
            aes(x = Species, y = n_caves, fill = Order)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n_caves), hjust = -0.2, size = 3, colour = "#333333") +
    scale_fill_viridis_d(option = "D", direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    coord_flip() +
    labs(title    = "Top 20 stygobiont species by cave occurrences",
         x = NULL, y = "Number of caves") +
    theme_cfg_bar() + theme(legend.position = "bottom")
save_plot(p, "q_species_stygobiont", w = 24, h = 14)

# Q: Broad-distribution species (mainland + island)
cat("\n--- Q: Broad-distribution species (mainland + island) ---\n")
broad_dist <- species_dist_df |>
    filter(distribution == "Both") |>
    left_join(species |> select(Species_Full_Name, Classification, Distribution, Order),
              by = c("Species" = "Species_Full_Name"))
cat("Species found on both mainland and islands:", nrow(broad_dist), "\n")
print(broad_dist |> count(Classification, sort = TRUE))
save_tsv(broad_dist, "q_species_broad_dist")

################################################################
cat("\n================================================================\n")
cat("SPECIES — MACROECOLOGICAL PATTERNS\n")
cat("================================================================\n")

# Q: Regional species richness (total, endemic, troglobiont) + choropleth map
cat("\n--- Q: Regional species richness ---\n")
cave_species_region <- census_all_species_all_caves |>
    filter(!grepl("\\bsp\\.$", Species)) |>
    left_join(caves_region |> select(Cave_ID, NAME_2), by = "Cave_ID") |>
    filter(!is.na(NAME_2)) |>
    group_by(NAME_2) |>
    summarise(
        n_species     = n_distinct(Species),
        n_endemic     = n_distinct(Species[Distribution == "Endemic to Greece"], na.rm = TRUE),
        n_troglobiont = n_distinct(Species[Classification == "Troglobiont"], na.rm = TRUE),
        n_caves       = n_distinct(Cave_ID),
        prop_endemic  = round(n_endemic / n_species, 3),
        .groups       = "drop"
    )
print(cave_species_region |> arrange(desc(n_species)))
save_tsv(cave_species_region, "q_species_region_richness")

# Choropleth map
region_sf <- greece_regions |>
    left_join(cave_species_region, by = "NAME_2")

map_opts <- list(
    geom_sf(data = region_sf, colour = "white", linewidth = 0.3),
    labs(x = NULL, y = NULL, caption = "EPSG:3035 LAEA"),
    theme_cfg(),
    theme(legend.position   = "right",
          axis.text         = element_text(size = 7),
          panel.border      = element_blank())
)

p_total <- ggplot(region_sf, aes(fill = n_species)) + map_opts +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Total\nspecies",
                         na.value = "grey90") +
    labs(title = "Total cave species richness per region")
save_plot(p_total, "q_species_region_total_richness", w = 20, h = 18)

p_endemic <- ggplot(region_sf, aes(fill = n_endemic)) + map_opts +
    scale_fill_distiller(palette = "Reds", direction = 1, name = "Endemic\nspecies",
                         na.value = "grey90") +
    labs(title = "Endemic-to-Greece cave species per region")
save_plot(p_endemic, "q_species_region_endemic_richness", w = 20, h = 18)

p_troglo <- ggplot(region_sf, aes(fill = n_troglobiont)) + map_opts +
    scale_fill_distiller(palette = "Blues", direction = 1, name = "Troglobiont\nspecies",
                         na.value = "grey90") +
    labs(title = "Troglobiont species per region")
save_plot(p_troglo, "q_species_region_troglobiont_richness", w = 20, h = 18)

# Endemic proportion per region bar chart
p_eprop <- ggplot(cave_species_region |>
                      mutate(NAME_2 = fct_reorder(NAME_2, prop_endemic)),
                  aes(x = NAME_2, y = prop_endemic, fill = NAME_2)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = percent(prop_endemic, accuracy = 1)), hjust = -0.2, size = 3.5,
              colour = "#333333") +
    scale_fill_viridis_d(option = "D", direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35)),
                       labels = percent_format()) +
    coord_flip() +
    labs(title    = "Endemic species proportion per region",
         subtitle = "Proportion of cave species endemic to Greece",
         x = NULL, y = "Proportion endemic") +
    theme_cfg_bar()
save_plot(p_eprop, "q_species_region_endemic_prop", w = 18, h = 12)

# Q: Altitudinal gradient
cat("\n--- Q: Altitudinal gradient ---\n")
altitude_df <- census_all_species_all_caves |>
    filter(!grepl("\\bsp\\.$", Species), !is.na(Altitude)) |>
    distinct(Cave_ID, Species, Altitude) |>
    group_by(Cave_ID, Altitude) |>
    summarise(n_species = n_distinct(Species), .groups = "drop")

cat("Spearman r (altitude vs richness):",
    round(cor(altitude_df$Altitude, altitude_df$n_species, method = "spearman"), 3), "\n")
save_tsv(altitude_df, "q_species_altitude_gradient")

p <- ggplot(altitude_df, aes(x = Altitude, y = n_species)) +
    geom_point(alpha = 0.4, colour = "#0072B2", size = 1.5) +
    geom_smooth(method = "loess", formula = y ~ x, colour = "#D55E00",
                se = TRUE, linewidth = 1) +
    scale_x_continuous(breaks = seq(0, 2500, 250)) +
    labs(title    = "Altitudinal gradient in cave species richness",
         subtitle = "Each point = one cave; red curve = LOESS smoother",
         x = "Altitude (m a.s.l.)", y = "Number of species") +
    theme_cfg()
save_plot(p, "q_species_altitude_gradient", w = 20, h = 12)

# Q: Latitudinal gradient
cat("\n--- Q: Latitudinal gradient ---\n")
latitude_df <- census_all_species_all_caves |>
    filter(!grepl("\\bsp\\.$", Species), !is.na(Latitude)) |>
    distinct(Cave_ID, Species, Latitude) |>
    group_by(Cave_ID, Latitude) |>
    summarise(n_species = n_distinct(Species), .groups = "drop")

cat("Spearman r (latitude vs richness):",
    round(cor(latitude_df$Latitude, latitude_df$n_species, method = "spearman"), 3), "\n")
save_tsv(latitude_df, "q_species_latitude_gradient")

p <- ggplot(latitude_df, aes(x = Latitude, y = n_species)) +
    geom_point(alpha = 0.4, colour = "#009E73", size = 1.5) +
    geom_smooth(method = "loess", formula = y ~ x, colour = "#D55E00",
                se = TRUE, linewidth = 1) +
    scale_x_continuous(breaks = seq(34, 42, 1)) +
    labs(title    = "Latitudinal gradient in cave species richness",
         subtitle = "Each point = one cave; red curve = LOESS smoother",
         x = "Latitude (°N)", y = "Number of species") +
    theme_cfg()
save_plot(p, "q_species_latitude_gradient", w = 20, h = 12)

################################################################
cat("\n================================================================\n")
cat("SPECIES — CONSERVATION STATUS\n")
cat("================================================================\n")

threatened_cats <- c("CR - Critically Endangered", "EN - Endangered", "VU - Vulnerable")

# Q: IUCN Red List breakdown
cat("\n--- Q: IUCN Red List categories ---\n")
iucn_breakdown <- species |>
    count(IUCN_Red_List, Classification, name = "n") |>
    mutate(Classification = factor(Classification, levels = names(clf_colours))) |>
    arrange(IUCN_Red_List)
print(iucn_breakdown |> group_by(IUCN_Red_List) |> summarise(n = sum(n)))
save_tsv(iucn_breakdown, "q_species_iucn")

# Order of IUCN categories for plotting
iucn_order <- c("CR - Critically Endangered", "EN - Endangered",
                 "VU - Vulnerable", "NT - Near Threatened",
                 "LC - Least Concern", "DD - Data Deficient",
                 "NE - Not Evaluated")

p <- ggplot(iucn_breakdown |>
                mutate(IUCN_Red_List = factor(IUCN_Red_List, levels = iucn_order)),
            aes(x = IUCN_Red_List, y = n, fill = Classification)) +
    geom_col(width = 0.75) +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip() +
    labs(title    = "IUCN Red List categories by ecological classification",
         x = NULL, y = "Number of species") +
    theme_cfg_bar() + theme(legend.position = "bottom")
save_plot(p, "q_species_iucn", w = 22, h = 12)

# Q: Greek Red Data Book
cat("\n--- Q: Greek Red Data Book categories ---\n")
grdb_breakdown <- species |>
    count(Greek_Red_Data_Book, Classification, name = "n") |>
    mutate(Classification = factor(Classification, levels = names(clf_colours)))
print(grdb_breakdown |> group_by(Greek_Red_Data_Book) |> summarise(n = sum(n)))
save_tsv(grdb_breakdown, "q_species_greek_rdb")

p <- ggplot(grdb_breakdown |>
                mutate(Greek_Red_Data_Book = factor(Greek_Red_Data_Book, levels = iucn_order)),
            aes(x = Greek_Red_Data_Book, y = n, fill = Classification)) +
    geom_col(width = 0.75) +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip() +
    labs(title    = "Greek Red Data Book categories by ecological classification",
         x = NULL, y = "Number of species") +
    theme_cfg_bar() + theme(legend.position = "bottom")
save_plot(p, "q_species_greek_rdb", w = 22, h = 12)

# Q: IUCN Threatened troglobionts
cat("\n--- Q: Threatened troglobiont species ---\n")
threatened_troglo <- species |>
    filter(IUCN_Red_List %in% threatened_cats,
           Classification == "Troglobiont") |>
    select(Species_Full_Name, Order, Family, IUCN_Red_List, Greek_Red_Data_Book,
           Distribution) |>
    arrange(IUCN_Red_List, Species_Full_Name)
cat("Threatened (CR+EN+VU) troglobiont species:", nrow(threatened_troglo), "\n")
print(threatened_troglo)
save_tsv(threatened_troglo, "q_species_threatened_troglo")

# Q: Threatened endemics
cat("\n--- Q: Threatened endemic species ---\n")
threatened_endemic <- species |>
    filter(IUCN_Red_List %in% threatened_cats,
           Distribution == "Endemic to Greece") |>
    select(Species_Full_Name, Order, Classification, IUCN_Red_List,
           Greek_Red_Data_Book) |>
    arrange(IUCN_Red_List, Classification)
cat("Threatened endemic species:", nrow(threatened_endemic), "\n")
save_tsv(threatened_endemic, "q_species_threatened_endemic")

p <- ggplot(threatened_endemic |>
                mutate(IUCN_Red_List = factor(IUCN_Red_List, levels = threatened_cats)),
            aes(x = fct_rev(Species_Full_Name), y = IUCN_Red_List,
                colour = Classification)) +
    geom_point(size = 3) +
    scale_colour_manual(values = clf_colours) +
    coord_flip() +
    labs(title = "Threatened endemic-to-Greece cave species",
         subtitle = paste0(nrow(threatened_endemic), " species with IUCN CR/EN/VU status"),
         x = NULL, y = "IUCN category") +
    theme_cfg() + theme(legend.position = "bottom",
                        axis.text.y = element_text(size = 8))
save_plot(p, "q_species_threatened_endemic", w = 18, h = 14)

# Q: Habitats Directive species
cat("\n--- Q: Habitats Directive species ---\n")
habitats_directive <- species |>
    filter(grepl("Habitats Directive", Protection_Status, ignore.case = TRUE)) |>
    mutate(
        annex_ii  = grepl("Appendix II|Annex II", Protection_Status),
        annex_iv  = grepl("Appendix IV|Annex IV", Protection_Status)
    ) |>
    select(Species_Full_Name, Order, Classification, Distribution,
           IUCN_Red_List, annex_ii, annex_iv)
cat("Species listed under EU Habitats Directive:", nrow(habitats_directive), "\n")
cat("Annex II:", sum(habitats_directive$annex_ii), "\n")
cat("Annex IV:", sum(habitats_directive$annex_iv), "\n")
save_tsv(habitats_directive, "q_species_habitats_directive")

# Q: Linnaean shortfall — orders with highest proportion of unassessed species
cat("\n--- Q: Linnaean shortfall (unassessed species) ---\n")
linnaean <- species |>
    group_by(Order) |>
    summarise(
        n_total      = n(),
        n_unassessed = sum(IUCN_Red_List == "NE - Not Evaluated", na.rm = TRUE),
        prop_unasses = round(n_unassessed / n_total, 3),
        .groups      = "drop"
    ) |>
    arrange(desc(prop_unasses))
cat("Total unassessed species:",
    sum(species$IUCN_Red_List == "NE - Not Evaluated", na.rm = TRUE), "\n")
print(head(linnaean, 10))
save_tsv(linnaean, "q_species_linnaean_shortfall")

p <- ggplot(linnaean |> head(15) |>
                mutate(Order = fct_reorder(Order, prop_unasses)),
            aes(x = Order, y = prop_unasses, fill = n_total)) +
    geom_col(width = 0.75, show.legend = TRUE) +
    geom_text(aes(label = paste0(percent(prop_unasses, accuracy = 1),
                                  " (n=", n_total, ")")),
              hjust = -0.1, size = 3) +
    scale_fill_gradient(low = seq_lo, high = seq_hi, name = "Total\nspecies") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35)),
                       labels = percent_format()) +
    coord_flip() +
    labs(title    = "Linnaean shortfall: orders with most unassessed species",
         subtitle = "NE = Not Evaluated on IUCN Red List",
         x = NULL, y = "Proportion unassessed (IUCN NE)") +
    theme_cfg_bar()
save_plot(p, "q_species_linnaean_shortfall", w = 20, h = 13)

# Q: Threatened × Natura2000 overlap
cat("\n--- Q: Threatened species in Natura2000 caves ---\n")
n2000_cave_ids <- caves_in_n2000$Cave_ID

threatened_spp <- species |>
    filter(IUCN_Red_List %in% threatened_cats) |>
    pull(Species_Full_Name)

spp_in_n2000 <- census_all_species |>
    filter(Cave_ID %in% n2000_cave_ids,
           !grepl("\\bsp\\.$", Species)) |>
    distinct(Species) |>
    pull(Species)

threatened_n2000 <- tibble(
    metric                 = c("Total threatened species",
                                "Threatened in ≥1 N2000 cave",
                                "Threatened NOT in any N2000 cave"),
    count                  = c(length(threatened_spp),
                                sum(threatened_spp %in% spp_in_n2000),
                                sum(!threatened_spp %in% spp_in_n2000))
)
print(threatened_n2000)
save_tsv(threatened_n2000, "q_species_threatened_n2000")

p <- ggplot(threatened_n2000 |> tail(2),
            aes(x = metric, y = count, fill = metric)) +
    geom_col(width = 0.5, show.legend = FALSE) +
    geom_text(aes(label = count), vjust = -0.4, size = 5) +
    scale_fill_manual(values = c("Threatened in ≥1 N2000 cave"      = "#0072B2",
                                  "Threatened NOT in any N2000 cave" = "#D55E00")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title    = "Threatened cave species: Natura2000 coverage",
         subtitle = paste0(threatened_n2000$count[1],
                           " IUCN CR+EN+VU species in the database"),
         x = NULL, y = "Number of species") +
    theme_cfg()
save_plot(p, "q_species_threatened_n2000", w = 16, h = 10)

################################################################
cat("\n================================================================\n")
cat("LOCUS TYPICUS, DATABASE LINKS, OCCURRENCE DISTRIBUTIONS\n")
cat("================================================================\n")

# Q: Locus Typicus — caves that are type localities for the most species
cat("\n--- Q: Locus Typicus caves ---\n")
locus_typicus_caves <- species |>
    filter(!is.na(Locus_Typicus_Cave)) |>
    distinct(Species_Full_Name, Class, Locus_Typicus_Cave, Locus_Typicus_Cave_ID) |>
    group_by(Locus_Typicus_Cave_ID, Locus_Typicus_Cave) |>
    summarise(n_species = n(), .groups = "drop") |>
    arrange(desc(n_species))
cat("Caves with Locus Typicus records:", nrow(locus_typicus_caves), "\n")
cat("Total species with Greek cave type locality:",
    sum(!is.na(species$Locus_Typicus_Cave)), "\n")
print(head(locus_typicus_caves, 15))
save_tsv(locus_typicus_caves, "q_species_locus_typicus_caves")

locus_typicus_class <- species |>
    filter(!is.na(Locus_Typicus_Cave)) |>
    distinct(Species_Full_Name, Class) |>
    count(Class, name = "n_species") |>
    arrange(desc(n_species))
save_tsv(locus_typicus_class, "q_species_locus_typicus_class")

p <- ggplot(locus_typicus_caves |>
                head(20) |>
                mutate(Locus_Typicus_Cave = fct_reorder(Locus_Typicus_Cave, n_species)),
            aes(x = Locus_Typicus_Cave, y = n_species, fill = n_species)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = n_species), hjust = -0.2, size = 3, colour = "#333333") +
    scale_fill_gradient(low = seq_lo, high = seq_hi) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    coord_flip() +
    labs(title    = "Top caves as type localities (Locus Typicus)",
         subtitle = paste0(sum(!is.na(species$Locus_Typicus_Cave)),
                           " species described from Greek cave type localities"),
         x = NULL, y = "Number of species") +
    theme_cfg_bar()
save_plot(p, "q_species_locus_typicus", w = 20, h = 13)

# Q: External database link coverage (GBIF, IUCN, PESI, NCBI, Fauna Europaea)
cat("\n--- Q: External database links coverage ---\n")
db_links <- tibble(
    database = c("GBIF", "IUCN", "PESI", "NCBI Taxonomy", "Fauna Europaea"),
    n_linked = c(
        sum(!is.na(species$Link_GBIF)),
        sum(!is.na(species$Link_IUCN)),
        sum(!is.na(species$Link_PESI)),
        sum(!is.na(species$Link_NCBI)),
        sum(!is.na(species$Link_Fauna_Europaea))
    )
) |>
    mutate(
        n_missing   = nrow(species) - n_linked,
        prop_linked = round(n_linked / nrow(species), 3)
    )
print(db_links)
save_tsv(db_links, "q_species_database_links")

p <- ggplot(db_links,
            aes(x = fct_reorder(database, prop_linked), y = prop_linked)) +
    geom_col(fill = "#0072B2", width = 0.65) +
    geom_text(aes(label = paste0(percent(prop_linked, accuracy = 1),
                                  " (n=", n_linked, ")")),
              hjust = -0.1, size = 3.2, colour = "#333333") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35)),
                       labels = percent_format()) +
    coord_flip() +
    labs(title    = "External database link coverage",
         subtitle = paste0("Total species in database: ", nrow(species)),
         x = NULL, y = "Proportion of species with link") +
    theme_cfg_bar()
save_plot(p, "q_species_database_links", w = 18, h = 10)

# Q: Taxon occurrence distribution (hollow curve — most species found in few caves)
cat("\n--- Q: Taxon occurrence distributions (hollow curve) ---\n")
census_no_sp <- census_all_species |>
    filter(!grepl("\\bsp\\.$", Species))

taxon_occ <- bind_rows(
    census_no_sp |> distinct(Cave_ID, Species) |>
        count(Species, name = "n_caves") |>
        count(n_caves, name = "n_taxa") |>
        mutate(rank = "Species"),
    census_no_sp |> distinct(Cave_ID, Genus) |>
        count(Genus, name = "n_caves") |>
        count(n_caves, name = "n_taxa") |>
        mutate(rank = "Genus"),
    census_no_sp |> distinct(Cave_ID, Family) |>
        count(Family, name = "n_caves") |>
        count(n_caves, name = "n_taxa") |>
        mutate(rank = "Family"),
    census_no_sp |> distinct(Cave_ID, Order) |>
        count(Order, name = "n_caves") |>
        count(n_caves, name = "n_taxa") |>
        mutate(rank = "Order")
) |>
    rename(n_occurrences = n_caves)
cat("Singleton species (found in 1 cave):",
    taxon_occ$n_taxa[taxon_occ$rank == "Species" & taxon_occ$n_occurrences == 1], "\n")
save_tsv(taxon_occ, "q_species_occurrence_distribution")

p <- ggplot(taxon_occ, aes(x = n_occurrences, y = n_taxa)) +
    geom_line(colour = "#0072B2") +
    geom_point(colour = "#0072B2", size = 1) +
    facet_wrap(~ factor(rank, levels = c("Species", "Genus", "Family", "Order")),
               scales = "free", ncol = 2) +
    labs(title    = "Taxon occurrence distributions (hollow curve)",
         subtitle = "Number of taxa found in exactly n caves",
         x = "Number of cave occurrences", y = "Number of taxa") +
    theme_cfg()
save_plot(p, "q_species_occurrence_distribution", w = 20, h = 16)

# Q: Altitude gradient by ecological classification
cat("\n--- Q: Altitude gradient by ecological classification ---\n")
altitude_clf <- census_all_species_all_caves |>
    filter(!grepl("\\bsp\\.$", Species), !is.na(Altitude), !is.na(Classification)) |>
    distinct(Species, Classification, Altitude) |>
    mutate(alt_bin = cut(Altitude, breaks = seq(0, 2400, by = 100))) |>
    group_by(alt_bin, Classification) |>
    summarise(n_species   = n_distinct(Species),
              mean_alt    = mean(Altitude),
              .groups     = "drop")
save_tsv(altitude_clf, "q_species_altitude_by_classification")

p <- ggplot(altitude_clf, aes(x = mean_alt, y = n_species, colour = Classification)) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = clf_colours) +
    scale_x_continuous(breaks = seq(0, 2400, 200)) +
    labs(title    = "Species richness along altitude gradient by ecological classification",
         x = "Altitude (m a.s.l.)", y = "Number of species per 100 m bin",
         colour = NULL) +
    theme_cfg() +
    theme(legend.position = "bottom")
save_plot(p, "q_species_altitude_by_classification", w = 22, h = 13)

cat("\n================================================================\n")
cat("DONE — results/ and plots/ updated\n")
cat("================================================================\n")
