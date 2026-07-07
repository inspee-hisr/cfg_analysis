#!/usr/bin/env Rscript

library(tidyverse)
# Load packages
#' 
#' # Caves in database
#' 
#' 
# Get ready the cave references file

# cave_references$Year <- as.numeric(gsub(pattern = "\\.",replacement = "",x = cave_references$Year))
# 
# cave_references$Journal <- gsub(pattern = "\\,",replacement = "",x = cave_references$Journal)
# 
# cave_references$Pages <- gsub(pattern = "\\.",replacement = "",x = cave_references$Pages)
# 
# cave_references$Title <- gsub(pattern = "\\.",replacement = "",x = cave_references$Title)
# cave_references$Title <- paste0(cave_references$Title,".")
# 
# write_delim(cave_references,path = "Cave references FOR DATABASE_09_01_2018.txt",delim = "\t",col_names = T)


#' 
#' 
#' We are using `r length(unique(species_occurencies$Cave_References))` cave references. There are `r length(which(is.na(unique(species_occurencies$Cave_References))))` without reference.
#' 
#' 
#' # References
#' 
#' ## References from Mendeley
#' Get ready the references for the database.
#' 
## #Transform the .ris file to txt

#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# Instead of using JabRef!!! Do this:
# From txt long to dataframe wide.
references_from_ris <- read_delim(file = "Cave_Fauna_References_23_01_2018.txt",delim = "@",col_names = FALSE)

references_from_ris$X1 <- trimws(references_from_ris$X1,which = "right")

references_from_ris <- references_from_ris %>% mutate(id=cumsum(if_else(X1=="ER",1,0))) %>% group_by(id,X1) %>% summarise(Value=paste0(X2,collapse = ";")) %>% ungroup() 

references_from_ris <- references_from_ris[!references_from_ris$id==max(references_from_ris$id),]

references_from_ris_wide <- references_from_ris %>% spread(X1,Value)


## colnames

colnames(references_from_ris_wide) <- c("Id","Author","Place_Published","DOI","Editor","End_Page","ER","Edition","Issue","Journal","Keywords", "Link_to_PDF","Notes","Abstract","Publisher","ISBN","Start_Page","Title","Type","URL","Volume","Year")

#sss <- c("id" ,"A1", "CY", "DO", "ED", "EP", "ER", "ET", "IS", "JF", "KW", "L1", "N1", "N2", "PB", "SN", "SP", "T1","TY" ,"UR", "VL", "Y1")

references_from_ris_wide <- references_from_ris_wide[,c(1,2,18,22,10,21,9,17,6,5,8,15,3,16,4,19,20,12,13,14,11,7)]

references_from_ris_wide$Year <- trimws(gsub("///","",references_from_ris_wide$Year),which = "left")



#' 
#' ## Creation of short references
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
## Create unique short references

references_from_ris_wide$n_authors <- str_count(references_from_ris_wide$Author, pattern = ";") +str_count(references_from_ris_wide$Author, pattern = "&") +1

references_from_ris_wide$first_author <- gsub("^(.*?),.*", "\\1", references_from_ris_wide$Author)

references_from_ris_wide$second_author <- gsub(pattern = ".*;(.+?),.*",replacement = "\\1",references_from_ris_wide$Author)  #"; *([^.,]*)"


references_from_ris_wide$Pages <- with(references_from_ris_wide, ifelse(Start_Page==End_Page,paste0(Start_Page),paste0(Start_Page,"-",End_Page)))

references_from_ris_wide$short_reference <- with(references_from_ris_wide,ifelse(n_authors==1,paste0(first_author," ",Year),ifelse(n_authors==2,paste0(first_author," & ",second_author," ",Year),paste0(first_author," et al. ",Year))))

species_references_ready_ris <- references_from_ris_wide %>% group_by(short_reference) %>% mutate(n_dupl_citation=letters[1:n()]) %>% mutate(n_dupl_citation_number=n()) %>% mutate(short_reference_ready=if_else(n_dupl_citation_number==1,paste0(short_reference),paste0(short_reference,n_dupl_citation))) %>% ungroup() %>% dplyr::select(-short_reference,-second_author,-Start_Page,-End_Page,-n_authors,-first_author,-second_author,-n_dupl_citation,-n_dupl_citation_number,-ER)

## Initials of Authors 

species_references_author <- strsplit(x = as.character(species_references_ready_ris$Author), "[;]")

species_references_author_spreaded <- data.frame(short_reference_ready = rep.int(species_references_ready_ris$short_reference_ready,sapply(species_references_author, length)),Author = rep.int(species_references_ready_ris$Author,sapply(species_references_author, length)), Author_spread = gsub("^\\s|\\s$", "",unlist(species_references_author))) 


Last_names <- gsub(",.*", "\\1", species_references_author_spreaded$Author_spread,perl = TRUE)

sort_names_initials <- iconv(gsub("[:a-z:]|\\s|\\.|[[:punct:]]", "", gsub(".*,", "\\1", species_references_author_spreaded$Author_spread,perl = TRUE)), "UTF-8", "ASCII", sub = "")
 #\\B\\w

sort_names_initials_ready <- trimws(gsub("([A-Z])", "\\1. ", sort_names_initials),which = "right")

species_references_author_spreaded$Author_spread_Initials <- paste0(Last_names,", ",sort_names_initials_ready)

species_references_author <- species_references_author_spreaded %>% group_by(short_reference_ready) %>% summarise(Author=paste0(Author_spread_Initials,collapse="; ")) %>% mutate(Author=gsub(";",",",gsub("(.*)\\;(.*)", "\\1 &\\2",Author)))


## Combine and Ready

species_references_ready_ris_final <- species_references_ready_ris %>% dplyr::select(-Author) %>% left_join(.,species_references_author,by=c("short_reference_ready"="short_reference_ready"))

species_references_ready_ris_final$Year <- as.numeric(species_references_ready_ris_final$Year)


species_references <- species_references_ready_ris_final
