#!/usr/bin/env Rscript

# Load packages
library(readr)
library(dplyr)
library(tibble)
library(sf)
library(ggplot2)
library(ggnewscale)
library(tidyr)
library(RColorBrewer)

# Load data
#setwd("../") # remove # when running from interactively
print("Loading data")
data_files <- list.files(path = "data")
 
# Data import from Database Export
# the files are choosen automatically based on their name.
# The folder data must contain only the latest data files.
Cave_References <- read_delim(file = paste0("data/",grep("Cave_References",data_files,value = TRUE)),delim = "\t")
 
caves <- read_delim(file = paste0("data/",grep("Caves",data_files,value = TRUE)),delim = "\t")
caves$Longitude <- as.numeric(caves$Longitude)
caves$Latitude <- as.numeric(caves$Latitude)

census <- read_delim(file = paste0("data/",grep("Census_\\d",data_files,value = TRUE)),delim = "\t")
 
Census_references <- read_delim(file = paste0("data/",grep("Census_references",data_files,value = TRUE)),delim = "\t")
 
species <- read_delim(file = paste0("data/",
                                    grep("Species_",data_files,value = TRUE)),
                      delim = "\t") |>
    mutate(Classification=gsub(pattern="\\?",replacement = "",x = Classification))# Data import from Database Export


census$species_epithet <- as.character(lapply(strsplit(as.character(census$Species), split=" "), "[", n=2))

census_all_species <- census |> left_join(species,by=c("Species"="Species_Full_Name"))

# ------------------ master table ----------------- #
census_all_species_all_caves <- census_all_species |> dplyr::select(-Cave_Name) |> left_join(caves, by=c("Cave_ID"="Cave_ID"))

census_long_str_man <- strsplit(x = census_all_species$Reference_Short,split = "|",fixed=TRUE)
census_long_str_man_id <- strsplit(x = census_all_species$Reference_ID,split = "|",fixed=TRUE)

census_long_man <- tibble(ReferenceShort=unlist(census_long_str_man),
                              Reference_ID=unlist(census_long_str_man_id),
                              CaveName=rep.int(census_all_species$Cave_Name,times = sapply(census_long_str_man,length)),
                              Cave_ID=rep.int(census_all_species$Cave_ID,times = sapply(census_long_str_man,length)),
                              Census_id=rep.int(census_all_species$Census_ID,times = sapply(census_long_str_man,length)),
                              Species=rep.int(census_all_species$Species,times = sapply(census_long_str_man,length))) |> 
    group_by(Reference_ID,Cave_ID,CaveName,Species,Census_id) |> 
    summarise(n=n(), .groups="keep") |> 
    ungroup() |> 
    mutate(Species=trimws(Species,"r"))

census_long_man$Reference_ID <- as.numeric(census_long_man$Reference_ID)

################################ load spatial data ##############################

print("Loading spatial data")

greece_regions <- sf::st_read("spatial_data/gadm41_GRC_shp/gadm41_GRC_2.shp")
####################### caves sf ###################
caves_sf <- caves |>
    filter(!(is.na(Longitude))) |>
    st_as_sf(coords=c("Longitude","Latitude"),
             remove=F,
             crs="WGS84")

locations_inland <- census_all_species_all_caves |>
    filter(!(is.na(Longitude))) |>
    st_as_sf(coords=c("Longitude","Latitude"),
             remove=F,
             crs="WGS84")
#st_write(caves_sf, "results/caves.geojson", delete_dsn = TRUE)

grid_10k_shapefile_wgs84 <- st_read("spatial_data/Greece_shapefile/gr_10km.shp") |>
    st_transform(., crs="WGS84")

#grid_10k_shapefile_dataframe <- broom::tidy(grid_10k_shapefile_wgs84)

locations_10_grid_species <- st_join(grid_10k_shapefile_wgs84, locations_inland, left=F) |>
    distinct(geometry,CELLCODE, Latitude, Longitude, Species) |>
    group_by(geometry,CELLCODE) |>
    summarise(n_species=n(),.groups="keep")

locations_10_grid_samples <- st_join(grid_10k_shapefile_wgs84, locations_inland, left=F) |>
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
    geom_point(data = caves,
               aes(x=Longitude,
                   y=Latitude,
                   color=Cave_Type),
               size = 0.9)+
    labs(x="Longitude",y="Latitude")+
    ggtitle("Species richness")+
    scale_color_manual(name="Cave Types",
                       values = c("Natural"="red",
                                  "Artificial"="black",
                                  "Natural Modified"="orange"))+  
    scale_x_continuous(breaks = seq(19,30,1),limits = c(20,30))+
    scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
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
