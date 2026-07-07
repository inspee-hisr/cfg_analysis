#!/usr/bin/env Rscript

library(sf)
library(units)
library(ggplot2)
library(ggnewscale)
library(tidyr)
library(RColorBrewer)

source("scripts/cfg_load_data.R")

################################ load spatial data ##############################

print("Loading spatial data")

greece_regions <- sf::st_read("spatial_data/gadm41_GRC_shp/gadm41_GRC_2.shp", quiet = TRUE) |>
    sf::st_transform(crs = 3035)
greece_municipalities <- sf::st_read("spatial_data/gadm41_GRC_shp/gadm41_GRC_3.shp", quiet = TRUE) |>
    sf::st_transform(crs = 3035)

# Natura2000 v32 (2021-12-09)
natura2000 <- sf::st_read("spatial_data/N2000_spatial_GR_2021_12_09_v32/N2000_spatial_GR_2021_12_09_v32.shp",
                           quiet = TRUE) |>
    sf::st_transform(crs = 3035)

# Geoparks
geopark <- sf::st_read("spatial_data/geopark_borders_mod/geopark_borders_mod.shp", quiet = TRUE) |>
    sf::st_transform(crs = 3035)

####################### caves sf ###################
caves_sf <- caves |>
    filter(!(is.na(Longitude))) |>
    st_as_sf(coords=c("Longitude","Latitude"),
             remove=F,
             crs = 4326) |>
    sf::st_transform(crs = 3035)

locations_inland <- census_all_species_all_caves |>
    filter(!(is.na(Longitude))) |>
    st_as_sf(coords=c("Longitude","Latitude"),
             remove=F,
             crs = 4326) |>
    sf::st_transform(crs = 3035)

grid_10k <- st_read("spatial_data/Greece_shapefile/gr_10km.shp", quiet = TRUE) |>
    st_transform(crs = 3035)

#grid_10k_shapefile_dataframe <- broom::tidy(grid_10k_shapefile_wgs84)

locations_10_grid_species <- st_join(grid_10k, locations_inland, left=F) |>
    distinct(geometry,CELLCODE, Latitude, Longitude, Species) |>
    group_by(geometry,CELLCODE) |>
    summarise(n_species=n(),.groups="keep")

locations_10_grid_samples <- st_join(grid_10k, locations_inland, left=F) |>
    distinct(geometry,CELLCODE, Latitude, Longitude) |>
    group_by(geometry,CELLCODE) |>
    summarise(n_samples=n(),.groups="keep")

grid_10k_species_abundance_plot <- ggplot()+
    geom_sf(data = greece_regions,
                 mapping= aes(fill = NAME_2),
                 color="white",
                 lwd=0.2,
                 show.legend = F,
                 alpha=0.35)+
    new_scale_fill() +
    geom_sf(data = locations_10_grid_species,
            mapping=aes(fill=n_species),
                 lwd=0.082,
                 alpha=0.83)+
    scale_fill_gradientn(n.breaks=4, 
                         colours = c("gray100",
                                     "gray40",
                                     "gray35",
                                     "gray20",
                                     "gray10",
                                     "gray0"),
                         name="Number of species")+
    geom_sf(data = caves_sf,
            aes(color=Cave_Type),
            size = 0.9)+
    labs(x="Easting (m, EPSG:3035)",y="Northing (m, EPSG:3035)")+
    ggtitle("Species richness")+
    scale_color_manual(name="Cave Types",
                       values = c("Natural"="red",
                                  "Artificial"="black",
                                  "Natural Modified"="orange"))+
    theme_bw()+
    guides(colour = guide_legend(order = 1), 
              fill = guide_legend(order = 2))+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = c(0.85, 0.80),
          legend.text = element_text(size=16,hjust = 0.5),
          legend.title = element_text(size=18,hjust = 0.5),
          axis.text=element_text(size = 16),
          plot.title = element_text(size=22),
          axis.title = element_text(size = 18),
          legend.title.align = 0.5,legend.box = "vertical")

ggsave("map_greece_plot_lines_grid_species.png",
       plot = grid_10k_species_abundance_plot,
       device = "png",
       width = 30,height = 30,units = "cm",dpi = 300 ,path = "plots/")

###########################################################
# -------------------- Census Long ---------------------- #
###########################################################

census_long <- census_long_man |>
    left_join(species, by=c("Species"="Species_Full_Name")) |>
    left_join(caves %>% dplyr::select(Cave_ID,
                                      Cave_Name,
                                      Longitude,
                                      Latitude,
                                      Cave_Synonyms,
                                      Cave_Type,
                                      Region,
                                      Municipality,
                                      Altitude),
              by=c("Cave_ID"="Cave_ID")) |>
    left_join(Census_references, by=c("Reference_ID"="ID"))

# number of caves
length(unique(census_long$Cave_ID))
# number of caves
length(unique(census_long$Cave_ID))
# number of species
length(unique(census_long$Species))
# number of references
length(unique(census_long$Reference_ID))
# number of occurrences
census_long |> distinct(Cave_ID,Species) |> nrow()

write_delim(census_long, "results/cfg_data_long.tsv",delim="\t")


############################## araneae ########################

species_araneae <- species |>
    filter(Order=="Araneae")

census_araneae <- census |>
    filter(Species_ID %in% species_araneae$`Species ID`)

caves_araneae <- caves |>
    filter(Cave_ID %in% unique(census_araneae$Cave_ID)) |>
    dplyr::select(Cave_ID,Cave_Name,Longitude,Latitude, Cave_Synonyms, Cave_Type, Region, Municipality, Altitude)

#census_references_araneae <- Census_references |> 
#    filter(ID %in% unique())

census_araneae_long <- census_long_man |>
    filter(Species %in% species_araneae$Species_Full_Name) |>
    left_join(species_araneae, by=c("Species"="Species_Full_Name")) |>
    left_join(caves_araneae) |>
    left_join(Census_references, by=c("Reference_ID"="ID")) |>
    dplyr::select(-n)

# number of caves
length(unique(census_araneae_long$Cave_ID))
# number of caves
length(unique(census_araneae_long$Cave_ID))
# number of species
length(unique(census_araneae_long$Species))
# number of references
length(unique(census_araneae_long$Reference_ID))
# number of occurrences
census_araneae_long |> distinct(Cave_ID,Species) |> nrow()

write_delim(census_araneae_long, "results/cfg_araneae_data_long.tsv",delim="\t")


################################## islands #############################
# Keep municipalities as MULTIPOLYGON — st_cast("POLYGON", do_split=TRUE) on
# an sf object only takes the FIRST part of each MULTIPOLYGON (warns "polygon
# from first part only"), which truncates large islands like Lesbos and Rhodes
# to their smallest satellite islet.  Connectivity and area checks work
# correctly on MULTIPOLYGON geometries, so no explosion is needed.

gadm_single <- greece_municipalities |>
    st_make_valid() |>
    mutate(id          = row_number(),
           area_island = round(set_units(st_area(geometry), km^2), 4))

connectivity <- st_intersects(gadm_single, gadm_single)
gadm_single$is_island <- sapply(connectivity, function(x) length(x) == 1)

greece_islands <- gadm_single |>
    mutate(is_island = ifelse(NAME_2 %in% c("North Aegean","Crete","South Aegean","Ionian Islands"),
                              TRUE, is_island)) |>
    mutate(region_type = ifelse(is_island, "Island", "Mainland")) |>
    dplyr::select(is_island, region_type, NAME_2, NAME_3, id, area_island)

# Crete: union all main-island municipalities (area > 100 km²) into one polygon.
# Gavdos (≈ 35 km²) is correctly excluded and kept as a separate island feature.
crete_only <- greece_islands |>
    filter(NAME_2 == "Crete") |>
    arrange(desc(area_island)) |>
    filter(area_island > set_units(100, km^2))

crete_only_one <- st_union(crete_only) |>
    sf::st_as_sf() |>
    rename("geometry" = "x") |>
    mutate(is_island = TRUE, region_type = "Island",
           NAME_2 = "Crete", NAME_3 = "All Crete", id = 0L) |>
    mutate(area_island = round(set_units(st_area(geometry), km^2), 4))

# Evia: cast the individual sfc geometry (not the sf data frame) so the
# MULTIPOLYGON is properly split into its constituent polygons.  The mainland
# polygon of Central Greece is ≈ 11 600 km²; Evia is ≈ 3 676 km².
cg_sfc   <- st_cast(st_geometry(greece_regions |>
                                    filter(NAME_2 == "Central Greece") |>
                                    st_make_valid()), "POLYGON")
cg_areas <- set_units(st_area(cg_sfc), km^2)
evia_idx <- which(cg_areas > set_units(1000, km^2) & cg_areas < set_units(4000, km^2))
evia <- st_sf(
    geometry    = st_sfc(cg_sfc[[evia_idx]], crs = 3035),
    is_island   = TRUE,
    region_type = "Island",
    NAME_2      = "Central Greece",
    NAME_3      = "Evia",
    id          = 1000L,
    area_island = round(cg_areas[evia_idx], 4)
)

# Remove individual Crete municipality polygons, add merged Crete + Evia
greece_islands_final <- greece_islands |>
    filter(!(id %in% crete_only$id)) |>
    bind_rows(crete_only_one) |>
    bind_rows(evia)

islands_gr <- ggplot() +
  geom_sf(data = greece_islands_final, mapping = aes(fill = region_type)) +
  theme_bw() +
  labs(title = "Islands vs Mainland in GADM Data")

ggsave("islands_gr.png",
       plot = islands_gr,
       device = "png",
       width = 20,
       height = 20,
       units = "cm",
       dpi = 300,
       path = "plots/")

############ island species analysis (all species) ##########

# Join all species locations to island/mainland polygons
all_species_islands <- sf::st_join(locations_inland,
                                   greece_islands_final,
                                   join = sf::st_intersects) |>
    sf::st_drop_geometry() |>
    distinct(Cave_ID, Species, region_type, NAME_2, NAME_3, is_island) |>
    filter(!is.na(region_type))

# Classify each species by whether it occurs on islands, mainland, or both
species_island_mainland <- all_species_islands |>
    group_by(Species) |>
    summarise(
        on_island   = any(region_type == "Island"),
        on_mainland = any(region_type == "Mainland"),
        .groups = "drop"
    ) |>
    mutate(distribution = dplyr::case_when(
        on_island & !on_mainland ~ "Island only",
        !on_island & on_mainland ~ "Mainland only",
        on_island & on_mainland  ~ "Both"
    ))

# Cave-level island summary
caves_island_summary <- all_species_islands |>
    group_by(Cave_ID, NAME_2, NAME_3, region_type, is_island) |>
    summarise(n_species = n_distinct(Species), .groups = "drop")

# Island areas (km²) for SAR analysis in cfg_questions_islands.R
island_areas <- greece_islands_final |>
    sf::st_drop_geometry() |>
    dplyr::select(NAME_2, NAME_3, area_island) |>
    dplyr::group_by(NAME_2, NAME_3) |>
    dplyr::summarise(area_island_km2 = sum(as.numeric(area_island), na.rm = TRUE),
                     .groups = "drop")

caves_island_summary <- caves_island_summary |>
    left_join(island_areas, by = c("NAME_2", "NAME_3"))

write_delim(all_species_islands,      "results/cfg_island_species.tsv",      delim = "\t")
write_delim(species_island_mainland,  "results/cfg_species_distribution.tsv", delim = "\t")
write_delim(caves_island_summary,     "results/cfg_caves_island_summary.tsv", delim = "\t")
write_delim(island_areas,             "results/cfg_island_areas.tsv",         delim = "\t")


