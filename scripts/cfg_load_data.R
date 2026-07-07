#!/usr/bin/env Rscript
# Shared data loader — source this at the top of any CFG analysis script.
# Requires: readr, dplyr, tibble
# Working directory must be the repo root (/workspace).

library(readr)
library(dplyr)
library(tibble)

print("Loading CFG database exports")

data_files <- list.files(path = "data")

Cave_References <- read_delim(
    file  = paste0("data/", grep("Cave_References", data_files, value = TRUE)),
    delim = "\t", show_col_types = FALSE)

caves <- read_delim(
    file  = paste0("data/", grep("Caves", data_files, value = TRUE)),
    delim = "\t", show_col_types = FALSE) |>
    mutate(Longitude = as.numeric(Longitude),
           Latitude  = as.numeric(Latitude))

census <- read_delim(
    file  = paste0("data/", grep("Census_\\d", data_files, value = TRUE)),
    delim = "\t", show_col_types = FALSE) |>
    mutate(species_epithet = as.character(
        lapply(strsplit(as.character(Species), split = " "), "[", n = 2)))

Census_references <- read_delim(
    file  = paste0("data/", grep("Census_references", data_files, value = TRUE)),
    delim = "\t", show_col_types = FALSE)

species <- read_delim(
    file  = paste0("data/", grep("Species_", data_files, value = TRUE)),
    delim = "\t", show_col_types = FALSE) |>
    mutate(Classification = gsub(pattern = "\\?", replacement = "", x = Classification))

# Master joined tables
census_all_species <- census |>
    left_join(species, by = c("Species" = "Species_Full_Name"))

census_all_species_all_caves <- census_all_species |>
    dplyr::select(-Cave_Name) |>
    left_join(caves, by = "Cave_ID")

# Long-format reference pivot (one row per census record × reference)
census_long_str_man    <- strsplit(x = census_all_species$Reference_Short, split = "|", fixed = TRUE)
census_long_str_man_id <- strsplit(x = census_all_species$Reference_ID,    split = "|", fixed = TRUE)

census_long_man <- tibble(
    ReferenceShort = unlist(census_long_str_man),
    Reference_ID   = unlist(census_long_str_man_id),
    CaveName       = rep.int(census_all_species$Cave_Name,  times = sapply(census_long_str_man, length)),
    Cave_ID        = rep.int(census_all_species$Cave_ID,    times = sapply(census_long_str_man, length)),
    Census_id      = rep.int(census_all_species$Census_ID,  times = sapply(census_long_str_man, length)),
    Species        = rep.int(census_all_species$Species,    times = sapply(census_long_str_man, length))
) |>
    group_by(Reference_ID, Cave_ID, CaveName, Species, Census_id) |>
    summarise(n = n(), .groups = "keep") |>
    ungroup() |>
    mutate(Species      = trimws(Species, "r"),
           Reference_ID = as.numeric(Reference_ID))

print(paste("Loaded:", nrow(caves), "caves |",
            nrow(species), "species |",
            nrow(census), "census records |",
            nrow(census_long_man), "long-format rows"))
