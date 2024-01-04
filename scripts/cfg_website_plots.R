#!/usr/bin/env Rscript

# Load packages
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(kableExtra)
library(RColorBrewer)

# Load data
# setwd("../") # remove # when running from interactively
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

# main website plots

# First plot, 1, caves per species region

caves_Region <- caves |>
    distinct(Cave_ID, Region) |>
    group_by(Region) |> summarize(number_of_caves=n()) |>
    na.omit() |>
    mutate(color_manual=colorRampPalette(c("orangered2","palegreen3","skyblue1","slateblue1","pink2","sienna3"),space="Lab")( 14 ))

species_region_endemic <- census_all_species_all_caves |>
    filter(species_epithet!="sp.") |>
    filter(Distribution=="Endemic to Greece") |>
    dplyr::select(Species,Distribution, Region) |>
    distinct() |>
    group_by(Region) |>
    summarise(number_of_endemic_species=n())

species_Region <- census_all_species_all_caves |>
    filter(species_epithet!="sp.") |>
    dplyr::select(Species,Region) |>
    distinct() |>
    group_by(Region) |>
    summarise(number_of_species=n()) |>
    na.omit()

species_troglobiont_Region <- census_all_species_all_caves |>
    filter(species_epithet!="sp.") |>
    dplyr::select(Species,Region, Classification) |>
    distinct() |>
    filter(Classification=="Troglobiont") |>
    group_by(Region) |> 
    summarise(number_of_troglobiont_species=n()) |>
    na.omit()

caves_species_region <- species_Region |>
    left_join(caves_Region, by=c("Region"="Region")) |>
    left_join(species_region_endemic, by=c("Region"="Region")) |>
    left_join(species_troglobiont_Region, by=c("Region"="Region")) |>
    pivot_longer(-c(Region,color_manual),
                 names_to = "Variable",
                 values_to = "number")


caves_species_region$number[is.na(caves_species_region$number)] <- 0

caves_species_region$Variable <- factor(caves_species_region$Variable,
                                        levels = c("number_of_caves","number_of_species","number_of_endemic_species","number_of_troglobiont_species"))

### plot

print("plot 1")
caves_species_region_plot <- ggplot()+
  geom_col(data = caves_species_region,
           aes(x=Region, y= number, fill=Variable),
           width=0.82,
           position = position_dodge(width = 0.82),
           show.legend = T)+
  geom_text(data = caves_species_region,
            aes(x =Region,y= number, label=number,group=Variable),
            position=position_dodge(width = 0.87),
            vjust=-0.25,
            size=5)+
  scale_y_continuous(breaks = seq(0,280,20),
                     limits = c(0,285),
                     expand = c(0.01,0.4))+
  scale_x_discrete(expand = c(0.01,0.4))+
  scale_fill_manual(label=c("Caves","All species","Species endemic to Greece","Troglobiont species"),
                    values = c("coral1","lightgoldenrod2","lightpink1","lightblue1"),
                    name="")+
  labs(x="Region", y= "Count")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 18),
        axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 18),
        axis.title = element_text(size=22),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = 'black', linewidth = 0.3),
        axis.line.y = element_line(colour = 'black', linewidth = 0.3),
        legend.position = c(0.13,0.87),
        legend.key.size = unit(1, "cm"))

ggsave("1_caves_species_region.png",
       plot = caves_species_region_plot,
       device = "png",
       width = 50,
       height = 36,
       units = "cm",
       dpi = 300,
       path = "website_plots/")

# Second plot, 2, species class barplot

species_class <- species |>
    dplyr::select(Species_Full_Name,Class) |>
    distinct() |>
    group_by(Class) |>
    summarise(number_of_species=n()) |>
    na.omit() |>
    mutate(color_manual=colorRampPalette(c("orangered2","palegreen3","skyblue1","slateblue1","pink2","goldenrod1","slategray2"),space="Lab")( 20 ))

print("plot 2")

species_class_barplot <- ggplot()+
  geom_col(data = species_class,
           aes(x=Class, y= number_of_species, fill=Class),show.legend = F)+
  geom_text(data = species_class,
            aes(x =Class,y= number_of_species, label=number_of_species),
            position=position_dodge(width=0.7), vjust=-0.25,size=6)+
  scale_y_continuous(breaks = seq(0,300,25),limits = c(0,300))+
  labs(x="Class", y= "Number of species")+
  scale_fill_manual(values = species_class$color_manual)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 22),
        axis.text.y=element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 18),
        axis.title = element_text(size=22),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = 'black', linewidth = 0.3),
        axis.line.y = element_line(colour = 'black', linewidth = 0.3),
        legend.position = c(0.15,0.91),
        legend.key.size = unit(1, "cm"))


ggsave("2_species_class_barplot.png",
       plot = species_class_barplot,
       device = "png",
       width = 50,
       height = 40,
       units = "cm",
       dpi = 300,
       path = "website_plots/")

# Third plot, 3, species knowledge accumulation
census_long_man_reference <- census_long_man |>
    left_join(Census_references,by=c("Reference_ID"="ID"))

species_references_spreaded_arranged <- census_long_man_reference |> 
    distinct(Species,Reference_ID, Year) |>
    arrange(Year) |>
    mutate(Duplicates=duplicated(Species), 
           species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) |> 
    filter(species_epithet!="sp.") |> 
    mutate(First_occurance=if_else(Duplicates=="FALSE",1,0)) |> 
    na.omit() |> 
    mutate(Cumulative_occurance= cumsum(First_occurance)) |> 
    mutate(Classification="All species") |> 
    dplyr::select(-c(Species,species_epithet,First_occurance)) |>
    distinct()

endemic_cumulative_species <- census_long_man_reference |> 
    left_join(species,by=c("Species"="Species_Full_Name")) |> 
    distinct(Species,Distribution,Reference_ID, Year) |>
    arrange(Year) |>
    mutate(Duplicates=duplicated(Species), 
           species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) |> 
    filter(species_epithet!="sp.") |> 
    mutate(First_occurance=if_else(Duplicates=="FALSE",1,0)) |>
    na.omit() |> 
    filter(First_occurance==1) |>
    group_by(Year,Distribution) |> 
    summarise(Occurance_species_year= n(), .groups="keep") |>
    group_by(Distribution) |> 
    mutate(Cumulative_occurance= cumsum(Occurance_species_year)) |> 
    filter(Distribution=="Endemic to Greece") |> 
    mutate(Classification="Endemic species to Greece") |> 
    ungroup() |>
    dplyr::select(-Occurance_species_year,-Distribution)

census_long_man_reference_all_species_classification <- census_long_man_reference |> 
    left_join(species,by=c("Species"="Species_Full_Name")) |> 
    distinct(Species,Classification,Reference_ID, Year) |>
    ungroup() |>
    arrange(Year) |> 
    mutate(Duplicates=duplicated(Species), 
           species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) |> 
    filter(species_epithet!="sp.") |> 
    mutate(First_occurance=if_else(Duplicates=="FALSE",1,0)) |> 
    na.omit() |>
    filter(First_occurance==1) |> 
    mutate(Classification=gsub("Troglobiont|Stygobiont", "Troglobiont + Stygobiont",Classification)) |>
    mutate(Classification=gsub("Troglophile|Stygophile", "Troglophile + Stygophile",Classification)) |>
    group_by(Year,Classification) |>
    summarise(Occurance_species_year= n(), .groups="keep") |>
    ungroup() |>
    group_by(Classification) |> 
    mutate(Cumulative_occurance= cumsum(Occurance_species_year)) |> 
    dplyr::select(-Occurance_species_year) |> 
    bind_rows(species_references_spreaded_arranged) |> 
    filter(Classification %in% c("Troglobiont + Stygobiont","Troglophile + Stygophile","All species")) |>
    bind_rows(endemic_cumulative_species) |>
    ungroup()

print("plot 3")
species_occurrence_accumulation_classification_plot <- ggplot()+
    geom_line(data=census_long_man_reference_all_species_classification,
              aes(x=Year,
                  y= Cumulative_occurance,color=Classification),
              linewidth=1,
              show.legend = T)+
    #ggtitle("Class")+
    scale_x_continuous(breaks = seq(1860,2030,10),
                       limits = c(1860,2030),
                       expand=c(0.015,0))+
    scale_y_continuous(breaks = seq(0,1000,100),
                       limits = c(0,1000),
                       expand = c(0.01,0))+
    scale_color_manual(values =c("Endemic species to Greece"="lightcoral",
                                 "Troglophile + Stygophile"="lightgoldenrod2",
                                 "Troglobiont + Stygobiont"="darkolivegreen3",
                                 "All species"="deepskyblue"))+
    labs(x="Years",
         y="Cumulative number of species")+
    theme_bw()+
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          legend.text = element_text(size = 18),
          axis.text.y=element_text(margin = margin(t = 0, r = 5, b = 0, l = 15,unit = "pt"),size = 18),
          axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 15, l = 0,unit = "pt"),size = 18),
          axis.title = element_text(size=22),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = 'black', linewidth = 0.3), 
          axis.line.y = element_line(colour = 'black', linewidth = 0.3),
          legend.position = c(0.13,0.87), 
          legend.key.size = unit(1.5, "cm"), 
          legend.title = element_blank())
  
ggsave("3_species_occurrence_accumulation_classification.png",
       plot = species_occurrence_accumulation_classification_plot,
       device = "png",
       width = 50,
       height = 36,
       units = "cm",
       dpi = 300,
       path = "website_plots/")


# Forth plot, 4, for endemic species ecological classification

species_classification_categories <- data.frame(Classification=unique(species$Classification))

species_classification_summary <- species |>
    group_by(Classification) |>
    summarise(number_of_species=n()) |>
    mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) |>
    mutate(Species_status="All species")

species_classification_summary_endemic <- species |>
    filter(Distribution=="Endemic to Greece") |>
    group_by(Classification) |>
    summarise(number_of_species=n()) |>
    mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) |>
    mutate(Species_status="Endemic to Greece")

species_classification_summary_endemic_all_categories <- species_classification_categories |>
    left_join(species_classification_summary_endemic,by=c("Classification"="Classification")) |>
    mutate(number_of_species=if_else(is.na(number_of_species),0,as.numeric(number_of_species))) |>
    mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) |>
    mutate(Species_status="Endemic to Greece")

classification_all_and_endemic <- rbind(species_classification_summary_endemic_all_categories,species_classification_summary)


classification_all_and_endemic$Classification <- factor(classification_all_and_endemic$Classification,
                                                        levels = c("Accidental","Trogloxene","Stygoxene","Stygophile","Troglophile","Stygobiont","Troglobiont"))

print("plot 4")

species_ecological_classification_all_and_endemic <- ggplot()+
  geom_col(data = classification_all_and_endemic,
           aes(x=Classification,
               y= number_of_species,
               fill=Species_status),
           position = position_dodge(width = 0.8),
           show.legend = T)+
  geom_text(data = classification_all_and_endemic,
            aes(x =Classification,
                y= number_of_species,
                label=number_of_species,
                group=Species_status), 
            position=position_dodge(width=0.9),
            vjust=-0.25,
            size=6)+
  scale_y_continuous(breaks = seq(0,375,25),
                     limits = c(0,375),
                     expand = c(0.01,0.4))+
  scale_fill_manual(labels=c("All species","Species endemic to Greece"),
                    values = c("lightgoldenrod2","lightpink1"),
                    name="")+
  labs(x="Classification", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 18),
        axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),
        axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 12, l = 0,unit = "pt"),
                                   angle = 45,
                                   hjust = 1,
                                   size = 18),
        axis.title = element_text(size=22),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = 'black', linewidth = 0.3),
        axis.line.y = element_line(colour = 'black', linewidth = 0.3),
        legend.position = c(0.132,0.93),
        legend.key.size = unit(1.5, "cm"))

ggsave("4_species_ecological_classification_all_and_endemic.png",
       plot = species_ecological_classification_all_and_endemic,
       device = "png",
       width = 50,
       height = 36,
       units = "cm",
       dpi = 300,
       path = "website_plots/")

# Fifth plot, 5, species red list and greed red data book 

iucn_species <- species |>
    group_by(IUCN_Red_List) |>
    summarise(number_of_species=n()) |>
    mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) |>
    mutate(Red_List="IUCN Red List") |>
    dplyr::rename(Categories=IUCN_Red_List) |>
    rbind(tibble(Categories="EN - Endangered",
                       number_of_species=0,
                       frequency=0,
                       Red_List="IUCN Red List"))

greek_red_data_species <- species |>
    group_by(Greek_Red_Data_Book) |>
    summarise(number_of_species=n()) |>
    mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) |>
    mutate(Red_List="Greece's Red Data Book") |>
    dplyr::rename(Categories=Greek_Red_Data_Book)

red_lists_species <- rbind(greek_red_data_species,iucn_species)

red_lists_species$Categories <- factor(red_lists_species$Categories,
                                       levels = c("NE - Not Evaluated","DD - Data Deficient","LC - Least Concern","NT - Near Threatened","VU - Vulnerable","EN - Endangered","CR - Critically Endangered"))

print("plot 5")

red_lists_data_species_plot <- ggplot()+
    geom_col(data = red_lists_species,
           aes(x=Categories,
               y= number_of_species,
               fill=Red_List),
             position = position_dodge(width = 0.9),
             show.legend = T)+ 
    geom_text(data = red_lists_species,
              aes(x =Categories,
                  y= number_of_species,
                  label=number_of_species,
                  group=Red_List),
              position=position_dodge(width=0.9),
              vjust=-0.25,
              size=6)+
    scale_y_continuous(breaks = seq(0,800,50),
                       limits = c(0,800),
                       expand = c(0.01,0.4))+
    scale_fill_manual(values = c("lightpink1","firebrick1"),name="")+
    labs(x="Categories", y= "Number of species")+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.text = element_text(size = 18),
          axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1,size = 18),
          axis.title = element_text(size=22),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = 'black', linewidth = 0.3),
          axis.line.y = element_line(colour = 'black', linewidth = 0.3),
          legend.position = c(0.83,0.89),
          legend.key.size = unit(1.5, "cm"))

ggsave("5_red_lists_data_species.png",
       plot = red_lists_data_species_plot,
       device = "png",
       width = 50,
       height = 36,
       units = "cm",
       dpi = 300,
       path = "website_plots/")

# Sixth plot, 6, Protection status species
species_protection <- strsplit(x = species$Protection_Status,split = "|",fixed=TRUE)

species_protection_data <- tibble(Species_Protection=unlist(species_protection),
                                      Species=rep.int(species$Species_Full_Name,
                                                      times = sapply(species_protection,length)),
                                      Class=rep.int(species$Class,
                                                    times = sapply(species_protection,length)),
                                      Classification=rep.int(species$Classification,
                                                             times = sapply(species_protection,length)))

species_protection_data_classification <- species_protection_data |>
    mutate(Protection_Status=if_else(is.na(Species_Protection)==TRUE,"Not protected","Protected")) |>
    distinct(Classification,Protection_Status,Species) |>
    group_by(Classification,Protection_Status) |>
    summarise(number_of_species=n(), .groups="keep") |>
    ungroup() |>
    pivot_wider(names_from=Protection_Status,
                values_from=number_of_species,
                values_fill=0) |>
    pivot_longer(-Classification,
                 names_to="Protection_Status",
                 values_to="number_of_species")

species_protection_data_classification$Classification <- factor(species_protection_data_classification$Classification,
                                                                levels = c("Accidental","Trogloxene","Stygoxene","Stygophile","Troglophile","Stygobiont","Troglobiont"))

print("plot 6")

species_protection_data_classification_plot <- ggplot()+
    geom_col(data = species_protection_data_classification,
             aes(x=Classification,
                 y= number_of_species,
                 fill=Protection_Status,
                 group=Protection_Status),
             position="dodge",
             show.legend = T)+
    geom_text(data = species_protection_data_classification,
              aes(x=Classification,
                  y= number_of_species,
                  label=number_of_species,
                  group=Protection_Status),
              position=position_dodge(width=0.93),
              vjust=-0.25,
              size=6)+
    scale_y_continuous(breaks = seq(0,400,25),
                       limits = c(0,400),
                       expand = c(0.01,0.4))+
    scale_fill_manual(values = c("lightcoral","lightgreen"),name="")+
    labs(x="Classification", y= "Number of species")+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.text = element_text(size = 18),
          axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),
          axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0,unit = "pt"),
                                     angle = 45, hjust = 1,size = 18),
          axis.title = element_text(size=22),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = 'black', linewidth = 0.3),
          axis.line.y = element_line(colour = 'black', linewidth = 0.3),
          legend.position = c(0.09,0.90),
          legend.key.size = unit(1.5, "cm"))
  
ggsave("6_species_protection_data_classification.png",
       plot = species_protection_data_classification_plot,
       device = "png",
       width = 50,
       height = 36,
       units = "cm",
       dpi = 300,
       path = "website_plots/")

# Seventh plot, 7, Protection status caves

caves_protection <- strsplit(x = caves$Protection_Status,split = "|",fixed=TRUE)

caves_protection_data <- tibble(Caves_Protection=unlist(caves_protection),
                                    CaveName=rep.int(caves$Cave_Name,
                                                     times = sapply(caves_protection,length)),
                                    Cave_ID=rep.int(caves$Cave_ID,
                                                    times = sapply(caves_protection,length)),
                                    Region=rep.int(caves$Region,
                                                   times = sapply(caves_protection,length)),
                                    Altitude=rep.int(caves$Altitude,
                                                     times = sapply(caves_protection,length))) |> 
    mutate(Protection_Type_ab=substr(x = Caves_Protection,start = 1,stop = 1)) |>
    group_by(Protection_Type_ab) |>
    left_join(tibble(ab=c("G","K",NA,"H","A","L"),
                         Protection_Type=c("Natura2000","Wildlife Refuge","Not Protected","Historical Monument","Archaeological Site","Landscape of Outstanding Natural Beauty")),
              by=c("Protection_Type_ab"="ab")) |>
    ungroup() |>
    dplyr::select(-Protection_Type_ab)

caves_protection_data_summary_type <- caves_protection_data |>
    group_by(Protection_Type) |>
    summarise(number_of_caves=n()) |>
    mutate(frequency=round(number_of_caves/sum(number_of_caves),digits = 3))

# Define the order of the columns but simultaniously wrap
# the text of the labels of the columns by replacing space with \n

caves_protection_data_summary_type$Protection_Type_l <- factor(gsub(pattern = " ",
                                                                    replacement = "\n",
                                                                    x = caves_protection_data_summary_type$Protection_Type),
                                                               levels = c("Archaeological\nSite","Historical\nMonument","Landscape\nof\nOutstanding\nNatural\nBeauty","Natura2000","Wildlife\nRefuge","Not\nProtected"))

print("plot 7")

caves_protection_data_type <- ggplot()+
    geom_col(data = caves_protection_data_summary_type,
             aes(x=Protection_Type_l,
                 y= number_of_caves,
                 fill=Protection_Type_l),
             width=0.8,
             show.legend = F)+ 
    geom_text(data = caves_protection_data_summary_type,
              aes(x =Protection_Type_l,
                  y= number_of_caves,
                  label=number_of_caves),
              position=position_dodge(width=0.7),
              vjust=-0.25,
              size=5)+
    scale_y_continuous(breaks = seq(0,450,50),limits = c(0,450))+
    labs(x="Protection", y= "Number of caves")+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.text = element_text(size = 18),
          axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),
          axis.text.x = element_text(size = 18,angle = 45, hjust = 1),
          axis.title = element_text(size=22),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = 'black', linewidth = 0.3),
          axis.line.y = element_line(colour = 'black', linewidth = 0.3),
          legend.position = c(0.132,0.93),
          legend.key.size = unit(1.5, "cm"))

ggsave("7_caves_protection_data_type.png",
       plot = caves_protection_data_type,
       device = "png",
       width = 50,
       height = 36,
       units = "cm",
       dpi = 300,
       path = "website_plots/")

# Table for statistics tab on the website:
# for each phylum, for each class, for each order count number of families, genera, species
species_na_order <- species |> filter(is.na(Order))

species_taxonomy_table <- species |>
    dplyr::select(Species_Full_Name,Genus,Family,Order,Class,Phylum) |>
    group_by(Genus,Family,Order,Class,Phylum) |>
    summarise(Species=n(), .groups="keep") |>
    group_by(Family,Order,Class,Phylum) |>
    summarise(Genera=n(),Species=sum(Species), .groups="keep") |>
    group_by(Order,Class,Phylum) |>
    summarise(Families=n(),Genera=sum(Genera),Species=sum(Species), .groups="keep") |>
    group_by(Class,Phylum) |>
    mutate(Orders=n()) |>
    ungroup()

# Create Taxonomic Summary Table for The Database Statistics

database_taxonomic_summary <- as_tibble(matrix(ncol = 5))
colnames(database_taxonomic_summary)<- c("TAXA","Orders","Families","Genera","Species")
phyla <- unique(species_taxonomy_table$Phylum)

for(i in 1:length(phyla)) {

    phylum <- as_tibble(matrix(c(paste0("Phylum ",phyla[i]),NA,NA,NA,NA),ncol = 5))
    colnames(phylum)<- c("TAXA","Orders","Families","Genera","Species")
    
    database_taxonomic_summary <- rbind(database_taxonomic_summary,phylum)
    
    class <- species_taxonomy_table |>
        filter(Phylum==phyla[i]) |>
        group_by(Class,Orders) |>
        summarise(Families=sum(Families),
                  Genera=sum(Genera),
                  Species=sum(Species),
                  .groups="keep") |> 
          dplyr::rename(TAXA=Class) |>
          ungroup()
    
    for(j in 1:nrow(class)) {
  
      database_taxonomic_summary <- rbind(database_taxonomic_summary,class[j,])
      
      orders <- species_taxonomy_table |>
          filter(Class==as.character(class[j,1])) |>
          dplyr::select(Order,Orders,Families,Genera,Species) |>
          dplyr::rename(TAXA=Order) |>
          mutate(Orders=NA) |>
          ungroup()
        
      database_taxonomic_summary <- rbind(database_taxonomic_summary,orders)
    }
}

# Create summary, totals
TOTAL <- as_tibble(matrix(c(NA,NA,NA,NA,NA,"TOTAL",
                            length(unique(species$Order)),
                            length(unique(species$Family)),
                            length(unique(species$Genus)),
                            length(unique(species$Species_Full_Name))),
                          ncol = 5,
                          nrow = 2,
                          byrow = T),
                   .name_repair = 'unique')

colnames(TOTAL)<- c("TAXA","Orders","Families","Genera","Species")

database_taxonomic_summary <- rbind(database_taxonomic_summary[-1,], TOTAL)

#write_delim(database_taxonomic_summary,
#            delim = "\t",
#            col_names = T,
#            file = "database_taxonomic_summary.tsv",
#            na = " ")

database_taxonomic_summary[is.na(database_taxonomic_summary)] <- " " # replace NA with space


# HTML formatting and exporting
print("export HTML table")

html_taxonomic_summary <- database_taxonomic_summary |> 
    mutate(TAXA = cell_spec(TAXA,
                            "html",
                            bold = ifelse(grepl(pattern = "^Phylum",x = database_taxonomic_summary$TAXA),
                                          "TRUE",
                                          ifelse(database_taxonomic_summary$Orders!=" ",
                                                 "TRUE",
                                                 "FALSE")),
                            underline = ifelse(grepl(pattern = "^Phylum",
                                                     x = database_taxonomic_summary$TAXA),
                                               "TRUE",
                                               "FALSE"))) |>
    kable(format = "html", escape = F) |>
    kable_styling(bootstrap_options = c("hover","condensed"),font_size = 15)
    
cat(html_taxonomic_summary, file = "website_plots/database_taxonomic_summary.html")

# check the font type
# import in Website: paste the html code in source code of the edit stats page, 
## Regions

print("All website statistics plots were generated")
