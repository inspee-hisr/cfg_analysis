#!/usr/bin/env Rscript
# Cave geology analysis: assign geology to each cave via spatial join,
# then produce a map and barplot of caves per geological category.
# All spatial operations use EPSG:3035 (LAEA Europe).
#
# Outputs:
#   plots/geology_map.png
#   plots/geology_barplot.png
#   results/caves_geology.tsv

library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

source("scripts/cfg_load_data.R")

TARGET_CRS <- 3035

################################ Load base layers ##############################

print("Loading spatial data")

greece_regions <- sf::st_read("spatial_data/gadm41_GRC_shp/gadm41_GRC_2.shp",
                              quiet = TRUE) |>
    sf::st_transform(crs = TARGET_CRS)

print("Loading geology (this may take a moment — 243k features)")

geology_raw <- sf::st_read("spatial_data/GeologicUnitView.gpkg",
                           layer = "GeologicUnitView", quiet = TRUE) |>
    dplyr::filter(country == "GR-IGME") |>
    sf::st_transform(crs = TARGET_CRS) |>
    dplyr::select(representativelithology_title, representativeage_title, name)

################################ Geology grouping ##############################

# Simplified geological categories for karst/cave context.
# Carbonate rocks (limestone, marble) are the primary cave-forming lithologies.
lithology_groups <- tribble(
    ~representativelithology_title,               ~geology_group,
    "limestone",                                  "Carbonate",
    "marble",                                     "Carbonate",
    "clasticSediment",                            "Clastic Sedimentary",
    "clasticSedimentaryRock",                     "Clastic Sedimentary",
    "conglomerate",                               "Clastic Sedimentary",
    "sandstone",                                  "Clastic Sedimentary",
    "shale",                                      "Clastic Sedimentary",
    "mudstone",                                   "Clastic Sedimentary",
    "siltstone",                                  "Clastic Sedimentary",
    "claystone",                                  "Clastic Sedimentary",
    "schist",                                     "Metamorphic",
    "gneiss",                                     "Metamorphic",
    "glaucophaneLawsoniteEpidoteMetamorphicRock", "Metamorphic",
    "amphibolite",                                "Metamorphic",
    "migmatite",                                  "Metamorphic",
    "fineGrainedIgneousRock",                     "Volcanic / Igneous",
    "pyroclasticRock",                            "Volcanic / Igneous",
    "granitoid",                                  "Volcanic / Igneous",
    "ultramaficIgneousRock",                      "Ultramafic",
    "biogenicSilicaSedimentaryRock",              "Siliceous / Chert",
    "gravel",                                     "Unconsolidated",
    "diamicton",                                  "Unconsolidated",
    "organicRichSedimentaryMaterial",             "Unconsolidated"
)

geology <- geology_raw |>
    dplyr::left_join(lithology_groups, by = "representativelithology_title") |>
    dplyr::mutate(geology_group = dplyr::coalesce(geology_group, "Other"))

################################ Caves as sf ###################################

caves_sf <- caves |>
    dplyr::filter(!is.na(Longitude), !is.na(Latitude)) |>
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs = 4326,
                 remove = FALSE) |>
    sf::st_transform(crs = TARGET_CRS)

################################ Spatial join ##################################

print("Joining caves to geology polygons")

caves_geology <- sf::st_join(caves_sf,
                             geology |> dplyr::select(geology_group,
                                                      representativelithology_title,
                                                      representativeage_title),
                             join = sf::st_intersects,
                             left = TRUE) |>
    # When a cave falls in multiple polygons take the first match
    dplyr::group_by(Cave_ID) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(geology_group = dplyr::coalesce(geology_group, "No data"))

cat("Geology assignment summary:\n")
print(table(caves_geology$geology_group, useNA = "always"))

# Export TSV (drop geometry)
caves_geology_df <- caves_geology |>
    sf::st_drop_geometry()

readr::write_delim(caves_geology_df, "results/caves_geology.tsv", delim = "\t")

################################ Colour palette ################################

geology_levels <- c("Carbonate", "Clastic Sedimentary", "Metamorphic",
                    "Volcanic / Igneous", "Ultramafic",
                    "Siliceous / Chert", "Unconsolidated", "No data")

geology_colors <- c(
    "Carbonate"          = "#4E9CC8",   # blue  — karst/cave classic
    "Clastic Sedimentary"= "#D4A55A",   # tan
    "Metamorphic"        = "#8B6BAE",   # purple
    "Volcanic / Igneous" = "#C0392B",   # red
    "Ultramafic"         = "#2ECC71",   # green
    "Siliceous / Chert"  = "#F39C12",   # orange
    "Unconsolidated"     = "#BDC3C7",   # light grey
    "No data"            = "#7F8C8D"    # dark grey
)

caves_geology <- caves_geology |>
    dplyr::mutate(geology_group = factor(geology_group, levels = geology_levels))

################################ Map ##########################################

print("Plotting map")

map_plot <- ggplot() +
    geom_sf(data = greece_regions,
            fill = "grey92", colour = "white", linewidth = 0.25) +
    geom_sf(data = caves_geology,
            aes(colour = geology_group),
            size = 1.4, alpha = 0.85) +
    scale_colour_manual(
        name   = "Geology",
        values = geology_colors,
        drop   = FALSE,
        guide  = guide_legend(override.aes = list(size = 3))
    ) +
    labs(
        title   = "Geology of Greek cave sites",
        caption = "Source: OneGeology Europe / IGME; spatial analysis in EPSG:3035 LAEA"
    ) +
    theme_bw(base_size = 12) +
    theme(
        panel.grid       = element_blank(),
        legend.position  = c(0.85, 0.72),
        legend.background = element_rect(fill = alpha("white", 0.8), colour = NA),
        legend.key.size  = unit(0.45, "cm"),
        legend.text      = element_text(size = 9),
        legend.title     = element_text(size = 10, face = "bold"),
        plot.title       = element_text(size = 14, face = "bold"),
        plot.caption     = element_text(size = 7, colour = "grey50")
    )

ggsave("plots/geology_map.png",
       plot   = map_plot,
       width  = 22, height = 22, units = "cm", dpi = 300)

print("Saved plots/geology_map.png")

################################ Barplot ######################################

print("Plotting barplot")

caves_per_geology <- caves_geology_df |>
    dplyr::count(geology_group, name = "n_caves") |>
    dplyr::mutate(geology_group = factor(geology_group, levels = geology_levels)) |>
    dplyr::arrange(geology_group)

barplot_plot <- ggplot(caves_per_geology,
                       aes(x    = reorder(geology_group, n_caves),
                           y    = n_caves,
                           fill = geology_group)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = n_caves),
              hjust = -0.2, size = 3.5) +
    scale_fill_manual(values = geology_colors, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(
        title   = "Number of caves per geological category",
        x       = NULL,
        y       = "Number of caves",
        caption = "Source: OneGeology Europe / IGME"
    ) +
    theme_bw(base_size = 12) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(size = 13, face = "bold"),
        axis.text.y        = element_text(size = 11),
        plot.caption       = element_text(size = 7, colour = "grey50")
    )

ggsave("plots/geology_barplot.png",
       plot   = barplot_plot,
       width  = 18, height = 12, units = "cm", dpi = 300)

print("Saved plots/geology_barplot.png")
print("Done.")
