#!/usr/bin/env Rscript
# Data manipulation packages
library(readxl)
library(readr)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(reshape2)
library(dplyr)
library(knitr)
library(tidyr)
library(httr)
library(broom)
library(stringr)
library(RColorBrewer)

## Data for species

library(rredlist)
library(taxize)
library(rgbif)
library(ISOcodes)
library(spocc)

packages <- c("readxl","readr","ggplot2","scales","gridExtra","dplyr", "knitr", "tidyr","RColorBrewer","ggmap","rgdal","rgeos","maptools","tmap","Rcpp","sp","raster","broom")

write_bib(x = packages,file = "packages_used.bib")


#' # Species Data
#' 

# load the file with species occurences

species_occurencies <- read_xlsx(path = "Cave_Fauna_of_Greece_Census_v5_24_01_2018.xlsx",col_names = T)
species_occurencies[species_occurencies=="NA"] <- NA

species_occurencies$Genus <- as.character(lapply(strsplit(as.character(species_occurencies$Species), split=" "), "[", n=1))

species_occurencies$species_epithet <- as.character(lapply(strsplit(as.character(species_occurencies$Species), split=" "), "[", n=2))

species_occurencies$Species_bare_name <- do.call(paste, c(species_occurencies[c("Genus", "species_epithet")], sep = " ")) 

species_occurencies_unique_species <- species_occurencies %>% dplyr::select(Phylum, Class,Order,Family,Genus,Species,species_epithet,Species_bare_name) %>% distinct(.) %>% filter(species_epithet!="sp.")

species_occurencies_unique_species_all <- species_occurencies %>% dplyr::select(Phylum, Class,Order,Family,Genus,Species,species_epithet,Species_bare_name) %>% distinct(.)


species_occurencies_only_sp <- species_occurencies %>% dplyr::select(Species, species_epithet) %>% distinct() %>% filter(species_epithet=="sp.")

species_occurencies_unique_species_only_species <- species_occurencies_unique_species %>% dplyr::select(Species) %>% distinct()



#' 
#' There are `r nrow(species_occurencies_only_sp)` occurencies that only the genus is known.
#' 
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------
#Count species and caves per taxon and department, respectively.

species_occurencies_unique_caves2 <- species_occurencies %>% group_by(Cave_ID,CaveName) %>% summarise(No_species_per_cave=n()) %>% ungroup() %>% mutate(duplicates=duplicated(Cave_ID,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Cave_ID,fromLast = T)) %>% mutate(duplicatesAll=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE")))

# species_occurencies_unique_caves3 <- species_occurencies %>% distinct(CaveName,Cave_ID,NearestVillage, Municipality,Island, County,Altitude,Cave_References)
# species_occurencies_unique_caves32 <- species_occurencies_unique_caves3[,c(8,6,5,4,3,2,1,7)]

species_occurencies_unique_caves <- species_occurencies %>% group_by(CaveName,Cave_ID, NearestVillage, Municipality,Island, County,Region,Cave_Comment,Latitude, Longitude,Altitude,Location_Status,Cave_References) %>% summarise(No_species_per_cave=n()) %>% ungroup()

species_occurencies_unique_caves$Latitude <- as.numeric(species_occurencies_unique_caves$Latitude)
species_occurencies_unique_caves$Longitude <- as.numeric(species_occurencies_unique_caves$Longitude)
#write_delim(species_occurencies_unique_caves,"Caves_INFO_10_12_2017.txt",delim = "\t",col_names = T)

# Caves
caves_municipality <- species_occurencies %>% dplyr::select(CaveName, Municipality) %>% distinct() %>% group_by(Municipality) %>% summarize(number_of_caves=n()) %>% na.omit()

#write_delim(caves_municipality,path="INSPEE_database_caves_municipality.txt",delim = "\t",col_names = TRUE)

caves_county <- species_occurencies %>% dplyr::select(CaveName, County) %>% distinct() %>% group_by(County) %>% summarize(number_of_caves=n()) %>% na.omit()

#write_delim(caves_county,path="INSPEE_database_caves_county.txt",delim = "\t",col_names = TRUE)

caves_Region <- species_occurencies %>% dplyr::select(CaveName, Region) %>% distinct() %>% group_by(Region) %>% summarize(number_of_caves=n()) %>% na.omit()

#write_delim(caves_Region,path="INSPEE_database_species_Region.txt",delim = "\t",col_names = TRUE)

#Species

species_genus <- species_occurencies_unique_species %>% dplyr::select(Species,Genus) %>% distinct() %>% group_by(Genus) %>% summarise(number_of_species=n()) %>% na.omit()

#write_delim(species_genus,path="INSPEE_database_species_per_genus.txt",delim = "\t",col_names = TRUE)

species_family <- species_occurencies_unique_species %>% dplyr::select(Species,Family) %>% distinct() %>% group_by(Family) %>% summarise(number_of_species=n()) %>% na.omit()

#write_delim(species_family,path="INSPEE_database_species_per_family.txt",delim = "\t",col_names = TRUE)

species_order <- species_occurencies_unique_species %>% dplyr::select(Species,Order) %>% distinct() %>% group_by(Order) %>% summarise(number_of_species=n()) %>% na.omit()

#write_delim(species_order,path="INSPEE_database_species_per_order.txt",delim = "\t",col_names = TRUE)

species_class <- species_occurencies_unique_species %>% dplyr::select(Species,Class) %>% distinct() %>% group_by(Class) %>% summarise(number_of_species=n()) %>% na.omit()

#write_delim(species_class,path="INSPEE_database_species_per_class.txt",delim = "\t",col_names = TRUE)

species_Region <- species_occurencies %>% dplyr::select(Species,Region) %>% distinct() %>% group_by(Region) %>% summarise(number_of_species=n()) %>% na.omit()

#write_delim(species_class,path="INSPEE_database_species_Region.txt",delim = "\t",col_names = TRUE)

species_municipality <- species_occurencies %>% dplyr::select(Species, Municipality) %>% distinct() %>% group_by(Municipality) %>% summarize(number_of_species=n()) %>% na.omit()

#' 
#' There are 2545 species occurencies in `r nrow(species_occurencies_unique_caves)` caves of Greece with `r nrow(species_occurencies_unique_species)` different species. There are `r length(unique(species_occurencies_unique_species$Species))` different species in `r length(unique(species_occurencies_unique_species$Genus))` genera, in `r length(unique(species_occurencies_unique_species$Family))` families, in `r nrow(species_order)` orders, in `r length(unique(species_occurencies_unique_species$Class))` classes, in `r length(unique(species_occurencies_unique_species$Phylum))` phyla.
#' 
#' ## Test export
#' 
## ------------------------------------------------------------------------

#export inspee

# species <- read_delim(file = "C:/Users/inikoloudakis/Downloads/Species_15-Feb-2018_165202.tsv",delim = "\t")
# census <- read_delim(file = "C:/Users/inikoloudakis/Downloads/Census_15-Feb-2018_165101.tsv",delim = "\t") %>% unite(col = Merged2,Species,Cave_Name,sep = ";",remove = F)
# caves <- read_delim(file = "C:/Users/inikoloudakis/Downloads/Caves_15-Feb-2018_094551.tsv",delim = "\t")
# Census_references <- read_delim(file = "C:/Users/inikoloudakis/Downloads/Census_references_15-Feb-2018_093414.tsv",delim = "\t")
# Cave_References <- read_delim(file = "C:/Users/inikoloudakis/Downloads/Cave_References_15-Feb-2018_093419.tsv",delim = "\t")

cave_references_import <- read_excel(path = "Cave_Fauna_Greece_Database_24_01_2018/Cave_references_FOR_DATABASE_09_01_2018.xlsx",col_names = T)

## mac

species <- read_delim(file = "/Users/savasp/Downloads/Species_16-Feb-2018_104829.tsv",delim = "\t")
census <- read_delim(file = "/Users/savasp/Downloads/Census_16-Feb-2018_104810.tsv",delim = "\t") %>% unite(col = Merged2,Species,Cave_Name,sep = ";",remove = F)
caves <- read_delim(file = "/Users/savasp/Downloads/Caves_16-Feb-2018_104856.tsv",delim = "\t")
Census_references <- read_delim(file = "/Users/savasp/Downloads/Census_references_16-Feb-2018_104900.tsv",delim = "\t")
Cave_References <- read_delim(file = "/Users/savasp/Downloads/Cave_References_16-Feb-2018_104904.tsv",delim = "\t")


species_occurencies4 <- species_occurencies %>% dplyr::select(Species,CaveName,Cave_ID, Locus_Typicus,Species_references,Locus_Typicus) %>% unite(col = Merged2,Species,CaveName,sep = ";",remove = F)

species_sav <- unique(species_occurencies4$Species)
species_man_census <- unique(census$Species)
species_man <- unique(species$`Species Full Name`)

ww_census <- species_sav[which(!(species_sav %in% species_man_census))]
ww_species <- species_sav[which(!(species_sav %in% species_man))]


dd_census <- species_occurencies4[which(!(species_occurencies4$Merged2 %in% census$Merged2)),]

species_occurencies_unique_caves <- species_occurencies %>% group_by(CaveName,Cave_ID, NearestVillage, Municipality,Island, County,Region,Cave_Comment,Latitude, Longitude,Altitude,Location_Status,Cave_References) %>% summarise(No_species_per_cave=n()) %>% ungroup()

species_occurencies_unique_caves_long_str <- strsplit(x = species_occurencies_unique_caves$Cave_References,split = ";")

species_occurencies_unique_caves_long <- data_frame(Cave_References=unlist(species_occurencies_unique_caves_long_str), Cave_ID=rep.int(species_occurencies_unique_caves$Cave_ID,times = sapply(species_occurencies_unique_caves_long_str,length)),CaveName=rep.int(species_occurencies_unique_caves$CaveName,times = sapply(species_occurencies_unique_caves_long_str,length))) %>% group_by(Cave_ID,CaveName,Cave_References) %>% summarise(n=n())

species_occurencies_unique_caves_long_str_man <- strsplit(x = caves$Cave_short_references,split = "|",fixed=TRUE)

species_occurencies_unique_caves_long_man <- data_frame(Cave_References=unlist(species_occurencies_unique_caves_long_str_man), Cave_ID=rep.int(caves$Cave_ID,times = sapply(species_occurencies_unique_caves_long_str_man,length)),CaveName=rep.int(caves$Cave_Name,times = sapply(species_occurencies_unique_caves_long_str_man,length))) %>% group_by(Cave_ID,CaveName,Cave_References) %>% summarise(n=n())


census_long_str_man <- strsplit(x = census$Reference_Short,split = "|",fixed=TRUE)
census_long_str_man_id <- strsplit(x = census$Reference_ID,split = "|",fixed=TRUE)

census_long_man <- data_frame(ReferenceShort=unlist(census_long_str_man),reference_id=unlist(census_long_str_man_id), Merged=rep.int(census$Merged2,times = sapply(census_long_str_man,length)),CaveName=rep.int(census$Cave_Name,times = sapply(census_long_str_man,length)),Cave_ID=rep.int(census$Cave_ID,times = sapply(census_long_str_man,length)),Census_id=rep.int(census$Census_ID,times = sapply(census_long_str_man,length)),Species=rep.int(census$Species,times = sapply(census_long_str_man,length))) %>% group_by(ReferenceShort,Cave_ID,CaveName,Species,Census_id) %>% summarise(n=n()) %>% ungroup() %>% mutate(Species=trimws(Species,"r")) %>% unite(col = Merged2,Species,CaveName,ReferenceShort,sep = ";",remove = F) %>% unite(col = MergedCave_spece,Species,CaveName,sep = ";",remove = F)


species_occurencies4_str <- strsplit(x = species_occurencies4$Species_references,split = ";")
species_occurencies4_long <- data_frame(ReferenceShort=unlist(species_occurencies4_str), Merged=rep.int(species_occurencies4$Merged2,times = sapply(species_occurencies4_str,length)),CaveName=rep.int(species_occurencies4$CaveName,times = sapply(species_occurencies4_str,length)),Cave_ID=rep.int(species_occurencies4$Cave_ID,times = sapply(species_occurencies4_str,length)),Species=rep.int(species_occurencies4$Species,times = sapply(species_occurencies4_str,length))) %>% mutate(CaveName=gsub(" or .*","",CaveName)) %>% group_by(ReferenceShort,Cave_ID,CaveName,Species) %>% summarise(n=n()) %>% ungroup() %>% unite(col = Merged2,Species,CaveName,ReferenceShort,sep = ";",remove = F) %>% unite(col = MergedCave_spece,Species,CaveName,sep = ";",remove = F)

# Locus typicus

census_inner <- census_long_man[which(census_long_man$Merged2 %in% species_occurencies4_long$Merged2),]
census_out <- census_long_man[which(!(census_long_man$Merged2 %in% species_occurencies4_long$Merged2)),]



occurencies_outcave <- species_occurencies4_long[which(!(species_occurencies4_long$CaveName %in% census_long_man$CaveName)),] %>% distinct(CaveName)

occurencies_outsp <- species_occurencies4_long[which(!(species_occurencies4_long$Species %in% census_long_man$Species)),] %>% distinct(Species)

occurencies_outcave_sp <- species_occurencies4_long[which(!(species_occurencies4_long$MergedCave_spece %in% census_long_man$MergedCave_spece)),] %>% distinct(Species,CaveName) %>% filter(!(Species %in% occurencies_outsp$Species), !(CaveName %in% occurencies_outcave$CaveName))

occurencies_out <- species_occurencies4_long[which(!(species_occurencies4_long$Merged2 %in% census_long_man$Merged2)),] %>% filter(!(Species %in% occurencies_outsp$Species), !(CaveName %in% occurencies_outcave$CaveName)) %>% group_by(Species,CaveName) %>% summarise(ss=toString(ReferenceShort))


species_cens <- species[which(!(species$Species_Full_Name %in% unique(species_occurencies4_long$Species))),]
#27367	924	Minotauria fagei (Kratochvil, 1970)	Spilaio Agias Sofias	22	Beron 2016|Fage 1945|Bosmans et al. 2013|Bosmans & Chatzaki 2005|Fage 1945|Bosmans et al. 2013|Bosmans & Chatzaki 2005	706|110|678|579|110|678|579


jj<- unique(grep("sp.", species_occurencies_unique_species_all$Species,fixed = T,value = T))
dd<- trimws(x = unique(grep("sp.", census$Species,fixed = T,value = T)),"r")

sp_niot <- dd[which(!(dd %in% jj))]

species_na <- species[which(is.na(species$Species_Full_Name)),]
species_sp <- species[grep("sp.", species$Species_Full_Name,fixed = T),]


#' 
#' 
#' ## Resolve species names
#' 
#' With the taxize package we can resolve mispellings of species scientific names using the Global Names Resolver (GNR) service.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## 
## #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4609752/
## #https://ropensci.org/tutorials/taxize_tutorial/
## 
## library('taxizesoap')
## 
## species_occurencies_unique_species_resolve <- gnr_resolve(names = species_occurencies_unique_species$Species,best_match_only = TRUE)
## 
## species_occurencies_unique_species_resolve_sum <- species_occurencies_unique_species_resolve %>% mutate(match=if_else(user_supplied_name==matched_name,paste0("TRUE"),paste0("FALSE")))
## 
## #pesi_search()
## aa <- get_pesiid((head(species_occurencies_unique_species$Species)))
## 
## species_resolve_names_Global_Name <- species_occurencies_unique_species %>% left_join(species_occurencies_unique_species_resolve_sum, by=c("Species"="user_supplied_name")) %>% dplyr::select(-species_epithet,-Species_bare_name,-submitted_name)
## 
## # write_delim(species_resolve_names_Global_Name,"species_resolve_names_Global_Names_Resolver.txt",delim = "\t",col_names = T)
## 

#' 
#' 
#' ## Higher Taxonomy with R
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## library(taxize)
## library(httr)
## 
## species_names <- as.vector(species_occurencies_unique_species$Species_bare_name)
## 
## species_uid <- sapply(species_names, function(x) get_uid(x,verbose = T,ask=F))
## 
## species_uid_df <- data.frame(Species_bare_name=names(species_uid), species_uid=species_uid)
## 
## aa <- get_nbnid(tail(species_occurencies_unique_species$Species_bare_name))
## 
## classification_species_uid <- classification("Acanthocreagris lycaonis",db = 'ncbi')
## 
## ss <- length(which(is.na(names(classification_species_uid))))
## 
## 

#' 
#' ## Species occurencies-distributions from databases
#' 
#' ### From GBIF
#' 
#' 
#' Get the species ids from Gbif with rgbif package.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## library(rgbif)
## library(ISOcodes)
## library(spocc)
## 
## splist <- species_occurencies_unique_species$Species
## keys_gbif <- sapply(splist, function(x) name_backbone(x,rank = "species"))
## 
## 
## keys_gbif_df <- data.frame(Data=unlist(keys_gbif), stringsAsFactors=FALSE) %>% mutate(Columns=rownames(.)) %>%
## mutate(Species=gsub("(.*)\\.(.*)", "\\1",Columns), Variables=gsub("(.*)\\.(.*)", "\\2",Columns)) %>% dplyr::select(-Columns) %>% spread(Variables,Data)
## 
## #write_delim(keys_gbif_df,"Species_with_Gbif_ids.txt",delim = "\t")
## 
## 

#' 
#' Get the occurencies data from Gbif.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## keys_gbif_df <- read_delim("Species_with_Gbif_ids.txt",delim = "\t",col_names = T)
## 
## 
## start_time <- Sys.time()
## 
## gbif <- occ_data(taxonKey = na.omit(unique(keys_gbif_df$speciesKey)))
## 
## end_time <- Sys.time()
## 
## end_time-start_time
## #Time difference of 9.739269 mins
## 
## 
## list_dataframes <- list()
## 
## for(i in 1:length(gbif)){
## 
##   list_dataframes[[i]] <- gbif[[i]]$data
## 
## }
## 
## df <- do.call("bind_rows", list_dataframes)
## 
## df_in_our_database <- df %>% filter(scientificName %in% species_occurencies_unique_species$Species)
## 
## df_in_our_database_countries <- df %>% filter(scientificName %in% species_occurencies_unique_species$Species) %>% distinct(scientificName, country)%>% na.omit() %>% group_by(scientificName) %>% summarise(Countries_occurrences_GBIF=toString(country))
## 
## #write_delim(df_in_our_database_countries,"Species_with_Gbif_Occurencies.txt",delim = "\t")
## 
## sss <- species_occurencies %>% filter(species_epithet!="sp.") %>% distinct(Species,Distribution_IUCN)
## 
## sss2 <- sss %>% left_join(df_in_our_database_countries, by=c("Species"="scientificName")) %>% left_join(catalogue_life_taxa_in_Cave_fauna_Greece_dist,by=c("Species"="scientificName"))
## 
## sss3 <- sss2 %>% filter(!is.na(Countries_occurrences_GBIF) | !is.na(Distribution_IUCN) | !is.na(Distribution_CoT))
## 
## #write_delim(sss2,"Species_with_Distributions.txt",delim = "\t")
## 
## 

#' 
#' 
#' ### Species distributions IUCN
#' 
## ------------------------------------------------------------------------

species_occurencies_unique_species$speciesGSUB <- gsub(pattern = " ",replacement ="##",x = species_occurencies_unique_species$Species)



#' 
#' 
#' We downloaded the distribution countries for each species in our database from IUCN using the rredlist package.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## 
## IUCN_token <- "0ec43516c25669cfd876387ab7f755d1d3745a6f39f4ee5a00b96bac1579cb42"
## 
## library(rredlist)
## library(taxize)
## 
## #Time difference of 16.67948 mins
## start_time <- Sys.time()
## 
## sss <- species_occurencies_unique_species$Species_bare_name
## 
## species_IUCN <- lapply(sss,function(x) rl_search(name = x,key = IUCN_token))
## 
## 
## #iucn_species_summary <- sapply(sss,function(x) iucn_summary(sciname = x,key = IUCN_token))
## 
## #iucn_species_summary2 <- iucn_summary(sciname = sss,key = IUCN_token)
## 
## iucn_species_summary_dataframe <- as.data.frame(c())
## 
## to_df_iucn <- function(x){
## 
## y <- data.frame(Names=as.character(),Status=as.character(),Distribution=as.character(),Population_trend=as.character(),stringsAsFactors=FALSE)
## 
## for(i in 1:length(x)) {
## 
##   y[i,1] <- as.character(names(x[i]))
##   y[i,2] <- paste(as.character(x[[i]][1]),collapse = ";")
##   y[i,3] <- paste(as.character(x[[i]][3]),collapse = ";")
##   y[i,4] <- paste(as.character(x[[i]][4]),collapse = ";")
## }
##   y
## }
## 
## #iucn_species_summary_dataframe <- to_df_iucn(iucn_species_summary)
## 
## 
## x <- species_IUCN
## 
##   #y <- data.frame(Names=as.character(),Status=as.character(),Distribution=as.character(),Population_trend=as.character(),stringsAsFactors=FALSE)
## 
## dat <- data.frame(i=as.numeric(),names=as.character(),status=as.character(),stringsAsFactors=FALSE)
## 
## 
## for(i in 1:length(x)) {
## 
##   dat[i,1] <- i
##   dat[i,2] <- paste(as.character(x[[i]][1]))
##   dat[i,3] <- class(x[[i]]$result)=="data.frame"
## 
##   # y[i,1] <- as.character(names(x[i]))
##   # y[i,2] <- paste(as.character(x[[i]][1]),collapse = ";")
##   # y[i,3] <- paste(as.character(x[[i]][3]),collapse = ";")
##   # y[i,4] <- paste(as.character(x[[i]][4]),collapse = ";")
## 
## }
## 
##   #l <- unlist(x,recursive = F)
##   list_dataframes <- list(c())
##   list_with_true <- x[which(dat$status=="TRUE")]
## 
##   for (j in 1:length(list_with_true)) {
## 
##     list_dataframes[[j]] <- as.data.frame(list_with_true[[j]][2])
## 
##   }
## 
## Species_in_IUCN <- do.call("rbind",list_dataframes)
## 
## 
## end_time <- Sys.time()
## 
## end_time - start_time
## 
## 
## #write_delim(Species_in_IUCN,path = "Species_in_IUCN.txt",delim = "\t",col_names = T)
## 

#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
iucn_species_summary_dataframe <- read_csv(file = "iucn_species_summary_dataframe.csv",col_names = T) %>% distinct()
colnames(iucn_species_summary_dataframe)<-c("IUCN_Status","Distribution_IUCN","Population_trend_IUCN","Species_Name")

species_IUCN_enrichment <- species_occurencies_unique_species %>% left_join(.,iucn_species_summary_dataframe,by=c("Species_bare_name"="Species_Name")) %>% dplyr::select(Species,IUCN_Status,Distribution_IUCN,Population_trend_IUCN)

species_occurencies_unique_species_in_IUCN <- species_IUCN_enrichment %>% na.omit(IUCN_Status)

species_occurencies_unique_species_in_IUCN_table <- species_occurencies_unique_species_in_IUCN %>% left_join(species_occurencies_unique_species, by=c("Species"="Species")) %>% group_by(Order) %>% summarise(n=n())



#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
species_occurencies_unique_species_in_IUCN_table_caption <- "Only 43 species of Cave Fauna of Greece database are evaluated in IUCN database"
kable(species_occurencies_unique_species_in_IUCN_table,caption=species_occurencies_unique_species_in_IUCN_table_caption, align = 'l')


#' 
#' 
#' IUCN database has `r nrow(species_occurencies_unique_species_in_IUCN)` species from our database.  
#' 
#' ### Species distributions from Catalogue of Life
#' 
#' From the Catalogue of Life.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## ## Load the data Catalogue of Life
## 
## 
## catalogue_life_description <- read_delim(file = "/Users/savasp/Desktop/archive-kingdom-animalia-bl3-2/description.txt",delim = "\t",col_names = TRUE)
## 
## catalogue_life_reference <- read_delim(file = "/Users/savasp/Desktop/archive-kingdom-animalia-bl3-2/reference.txt",delim = "\t",col_names = TRUE)
## 
## catalogue_life_distribution <- read_delim(file = "/Users/savasp/Desktop/archive-kingdom-animalia-bl3-2/distribution.txt",delim = "\t",col_names = TRUE)
## 
## catalogue_life_taxa <- read_delim(file = "/Users/savasp/Desktop/archive-kingdom-animalia-bl3-2/taxa.txt",delim = "\t",col_names = TRUE)
## 
## ## From our database
## 
## catalogue_life_taxa_in_Cave_fauna_Greece <- catalogue_life_taxa %>% filter(scientificName %in% unique(species_occurencies_unique_species$Species))
## 
## 
## catalogue_life_taxa_in_Cave_fauna_Greece_dist <- catalogue_life_distribution %>% filter(taxonID %in% catalogue_life_taxa_in_Cave_fauna_Greece$taxonID) %>% left_join(catalogue_life_taxa_in_Cave_fauna_Greece,by=c("taxonID"="taxonID")) %>% group_by(taxonID) %>% summarise(Distribution_CoT=toString(locality)) %>% left_join(catalogue_life_taxa_in_Cave_fauna_Greece,by=c("taxonID"="taxonID")) %>% distinct(scientificName,Distribution_CoT)
## 
## catalogue_life_taxa_in_Cave_fauna_Greece_reference <- catalogue_life_reference %>% filter(taxonID %in% catalogue_life_taxa_in_Cave_fauna_Greece$taxonID) %>% left_join(catalogue_life_taxa_in_Cave_fauna_Greece,by=c("taxonID"="taxonID"))
## 
## catalogue_life_taxa_in_Cave_fauna_Greece_description <- catalogue_life_description %>% filter(taxonID %in% catalogue_life_taxa_in_Cave_fauna_Greece$taxonID) %>% left_join(catalogue_life_taxa_in_Cave_fauna_Greece,by=c("taxonID"="taxonID"))
## 
## 

#' 
#' 
#' ## Species protection status
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## species_protection_status <- read_xlsx(path = "Species_Protection_Status_for_Kaloust_Working_13.12.2107.xlsx",col_names = T,sheet = "SpeciesPS") %>% dplyr::select(-c(PhylumOrDivision, Class,Order,Family,Genus,species_epithet,Species_bare_name,Protected,H))
## 
## species_protection_status[species_protection_status=="NA"] <- NA
## 
## species_protection_status_categories <- t(read_xlsx(path = "Species_Protection_Status_for_Kaloust_Working_13.12.2107.xlsx",col_names = F,sheet = "Apend"))
## 
## species_protection_status$A <- with(species_protection_status,ifelse(is.na(A),NA_character_,species_protection_status_categories[3,1]))
## 
## species_protection_status$B <- with(species_protection_status,ifelse(is.na(B),NA_character_,species_protection_status_categories[3,2]))
## 
## species_protection_status$C <- with(species_protection_status,ifelse(is.na(C),NA_character_,ifelse(C=="IV",species_protection_status_categories[3,4],species_protection_status_categories[3,3])))
## 
## species_protection_status$D <- with(species_protection_status,ifelse(is.na(D),NA_character_,species_protection_status_categories[3,5]))
## 
## species_protection_status$E <- with(species_protection_status,ifelse(is.na(E),NA_character_,ifelse(E=="II",species_protection_status_categories[3,6],species_protection_status_categories[3,7])))
## 
## species_protection_status$F <- with(species_protection_status,ifelse(is.na(F),NA_character_,species_protection_status_categories[3,8]))
## 
## species_protection_status$G <- with(species_protection_status,ifelse(is.na(G),NA_character_,ifelse(G=="I/A",species_protection_status_categories[3,9],species_protection_status_categories[3,10])))
## 
## #species_protection_status$H <- with(species_protection_status,ifelse(is.na(H),NA_character_,species_protection_status_categories[3,11]))
## 
## #species_protection_status[is.na(species_protection_status)] <- "There is no species-specific legal framework for protection"
## 
## colnames(species_protection_status) <- c("Species","Protection_Law_86/1969","Protection_Presidential_Decree 67/1981","Protection_Habitats_Directive_(92/43/EEC)","Protection_Birds_Directive_(79/409/EEC)","Protection_Bern_Convection","Protection_Bonn_Convection","Protection_CITES")
## 
## #species_occurencies2 <- species_occurencies %>% left_join(species_protection_status, by=c("Species"="Species"))
## 
## #write_delim(x = species_occurencies2,path = "Cave_Fauna_of_Greece_Census_14_12_2017.txt",delim = "\t",col_names = T)
## 

#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
species_protection_status <- species_occurencies %>% dplyr::select("Species","Protection_Law_86/1969","Protection_Presidential_Decree 67/1981","Protection_Habitats_Directive_(92/43/EEC)","Protection_Birds_Directive_(79/409/EEC)","Protection_Bern_Convection","Protection_Bonn_Convection","Protection_CITES") %>% distinct(.)


#' 
#' 
#' ## Troglofauna characterisation
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## 
## Troglofauna_characterisation <- data_frame(Troglofauna_characterisation_AB=c("tx","tx?","tph","tph?","tb","tb?","ac","ac?"),Troglofauna_characterisation=c("Trogloxene","Trogloxene?","Troglophile", "Troglophile?","Troglobiont", "Troglobiont?","Accidental","Accidental?") )
## 
## 
## species_troglofauna_characterisation <- species_occurencies %>% dplyr::select(Species,species_epithet,Type) %>% distinct(.) %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% dplyr::select(-duplicatesLast,-duplicates) %>% filter(!(duplicatesAll=="TRUE" & is.na(Type))) %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% dplyr::select(-duplicatesLast,-duplicates) %>% mutate(Troglofauna_characterisation_AB=if_else(duplicatesAll=="TRUE",NA_character_,Type)) %>% distinct(Species,Troglofauna_characterisation_AB) %>% left_join(Troglofauna_characterisation, by=c("Troglofauna_characterisation_AB"="Troglofauna_characterisation_AB"))
## 
## 
## #species_occurencies_unique_species_all
## #%>% filter(!(is.na(Type) & duplicatesAll=="TRUE")) %>% distinct() %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll_misspell=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE")))
## 
## # species_occurencies_unique_species2 <- species_occurencies %>% dplyr::select(Phylum, Class,Order,Family,Genus,Species,species_epithet,Species_bare_name,Synonymes,Type,Speciment_Abbreviations,Species_comment,Comments) %>% distinct(.) %>% filter(species_epithet!="sp.") %>% dplyr::select(Species,Type)
## #
## # species_occurencies_unique_species222 <- species_occurencies %>% dplyr::select(Species,species_epithet,Type) %>% distinct(.)
## # #%>% filter(species_epithet!="sp.") %>% dplyr::select(Species,Type)   Synonymes,Type,Speciment_Abbreviations,Species_comment,Comments
## #
## 
## ########
## 
## # species_occurencies_unique_species2_type <- species_occurencies_unique_species2 %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% dplyr::select(-duplicatesLast,-duplicates) %>% filter(!(is.na(Type) & duplicatesAll=="TRUE")) %>% distinct() %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll_misspell=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE")))
## #
## # species_occurencies_unique_species2_type_ok <- species_occurencies_unique_species2_type %>% mutate(., Type2 = if_else(duplicatesAll_misspell=="TRUE","NA",Type)) %>% filter(!(Type2=="NA" & duplicates=="TRUE")) %>% dplyr::select(Species, Type2)
## #
## # species_occurencies_unique_species2_type_ok[species_occurencies_unique_species2_type_ok=="NA"] <- NA
## # colnames(species_occurencies_unique_species2_type_ok) <- c("Species", "Troglofauna_characterisation_AB")
## #
## #
## # species_occurencies_unique_species1 <- species_occurencies_unique_species %>% left_join(.,species_occurencies_unique_species2_type_ok, by=c("Species"="Species")) %>% left_join(.,Troglofauna_characterisation, by=c("Troglofauna_characterisation_AB"="Troglofauna_characterisation_AB"))
## 
## 
## 

#' 
#' Summary table with the species characterisations. 
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
species_troglofauna_characterisation <- species_occurencies %>% distinct(Species,species_epithet,Troglofauna_characterisation)

species_occurencies_unique_species_troglofauna_summary <- species_troglofauna_characterisation %>% group_by(Troglofauna_characterisation) %>% summarise(number_of_species=n())

species_occurencies_unique_species_troglofauna_summary_captio <- "Number of species per troglofauna characterisation"
kable(species_occurencies_unique_species_troglofauna_summary,caption=species_occurencies_unique_species_troglofauna_summary_captio, align = 'l')

#' 
#' 
#' ## Species hyperlinks in databases
#' 
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE----------------
## 
## species_Conservation_links_file <-  species_occurencies %>% dplyr::select(Species,CheckIUCNRedList,CheckPESI,CheckFaunaEuropaea,CheckGBIF,CheckNCBI) %>% distinct(.)
## 
## 
## species_Conservation_links <- species_occurencies_unique_species %>% dplyr::select(Species) %>% left_join(.,species_Conservation_links_file, by=c("Species"="Species"))
## 
## species_links_summary <- data_frame(Source=c("CheckFaunaEuropaea","CheckNCBI","CheckGBIF","CheckPESI","CheckIUCNRedList"), Number_of_links=c(sum(!is.na(species_Conservation_links$CheckFaunaEuropaea)),sum(!is.na(species_Conservation_links$CheckNCBI)),sum(!is.na(species_Conservation_links$CheckGBIF)),sum(!is.na(species_Conservation_links$CheckPESI)),sum(!is.na(species_Conservation_links$CheckIUCNRedList))), Missing_links=c(nrow(species_occurencies_unique_species)-sum(!is.na(species_Conservation_links$CheckFaunaEuropaea)),nrow(species_occurencies_unique_species)-sum(!is.na(species_Conservation_links$CheckNCBI)),nrow(species_occurencies_unique_species)-sum(!is.na(species_Conservation_links$CheckGBIF)),nrow(species_occurencies_unique_species)-sum(!is.na(species_Conservation_links$CheckPESI)),nrow(species_occurencies_unique_species)-sum(!is.na(species_Conservation_links$CheckIUCNRedList))))
## 
## species_links_summary_caption <- "Links to different databases for individual species"
## kable(species_links_summary,caption=species_links_summary_caption, align = 'l')
## 
## ####
## 
## # links_Giannis <- read_xlsx("species_for_links_OK_15_01_2018_GIANNIS.xlsx",col_names = T) %>% dplyr::select(Species, CheckGBIF)
## #
## # links_Kaloust <- read_xlsx("species_for_links_OK_15_01_2018_Kaloust.xlsx",col_names = T) %>% dplyr::select(Species,CheckFaunaEuropaea,CorrectSpeciesName)
## # links_Savvas <- read_xlsx("species_for_links_OK_15_01_2018_Savvas.xlsx",col_names = T) %>% dplyr::select(Species,CheckPESI)
## #
## # species_links <- links_Kaloust %>% left_join(links_Giannis, by=c("Species"="Species")) %>% left_join(links_Savvas, by=c("Species"="Species")) %>% mutate(CorrectSpeciesName=if_else(is.na(CorrectSpeciesName),Species,CorrectSpeciesName))
## #
## # species_links[species_links=="NA"] <- NA
## #
## # colnames(species_links)[1] <- "Species_2018_2"
## # colnames(species_links)[3] <- "Species"
## 
## #colnames(species_occurencies)[38] <- "Species_2018_2"
## 
## #species_occurencies_links <- species_occurencies %>% dplyr::select(-c(CheckPESI,CheckFaunaEuropaea,CheckGBIF)) %>% left_join(species_links,by=c("Species_2018_2"="Species_2018_2"))
## 
## #write_delim(species_occurencies_links,path = "Cave_Fauna_of_Greece_Census_v4_17_01_2018.txt",col_names = T,delim = "\t")
## 
## # NCBI LINKS
## # first we find the NCBI ids with the taxise package and afterwards we create the links. According to their site in this link https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/index.cgi?chapter=howlink we use the ids and a url prefix to create the urls.
## 
## species_names <- as.vector(species_occurencies_unique_species$Species_bare_name)
## 
## species_uid <- sapply(species_names, function(x) get_uid(x,verbose = T,ask=F))
## 
## species_uid_df <- data.frame(Species_bare_name=names(species_uid), species_uid=as.numeric(species_uid),row.names = NULL)
## 
## species_uid_df_no_na <-  species_uid_df %>% na.omit() %>% mutate(NCBI_links=paste("https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",species_uid,sep = "")) %>% left_join(species_occurencies_unique_species_all, by=c("Species_bare_name"="Species_bare_name"))
## 
## write_delim(species_uid_df_no_na,path = "species_uid_df_no_na.txt",delim = "\t")
## 
## # Open all tabs
## 
## #sapply(species_uid_df_no_na$NCBI_links,FUN = browseURL)
## 
## species_occurencies2 <- species_occurencies %>% left_join(species_uid_df_no_na, by=c("Species_bare_name"="Species_bare_name"))
## 
## species_Conservation_linksNCBI <- species_Conservation_links %>% dplyr::select(Species,CheckNCBI) %>% na.omit
#' 
#' In the case of IUCN database there are missing links because these species are not included in the databese. This has to be clarified for the rest databases. 
#' 
#' 
#' Get ready the species data.
## ------------------------------------------------------------------------

# species_Data_Cave_Fauna_Greece <- species_occurencies_unique_species %>% left_join(species_troglofauna_characterisation, by=c("Species"="Species")) %>% left_join(species_IUCN_enrichment, by=c("Species"="Species")) %>% left_join(species_Conservation_links, by=c("Species"="Species"))

#write_delim(x = species_Data_Cave_Fauna_Greece,path = "Cave_Fauna_of_Greece_Species_10_12_2017.txt",delim = "\t",col_names = T)


