# install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("dplyr")
library(dplyr)
#install.packages("stringi")
library(stringr)
#install.packages("VIM")
library(VIM)
library(stringi)

setwd("/Users/miquelrodoreda/uni/MD")

filename <- "dataset/preprocessed.csv"
file.exists(filename)
dd <- read.csv(filename)

# ------------------------------------- original_location name shortening -------------------------------------
unique(dd$original_location)

dd <- dd %>%
  mutate(original_location = case_when(
    original_location == "Vallès Occidental" ~ "VOc",
    original_location == "Maresme" ~ "Mar",
    original_location == "NotFound" ~ "NF",
    original_location == "Osona" ~ "Os",
    original_location == "Vallès Oriental"   ~ "VOr",
    original_location == "Berguedà" ~ "Ber",
    original_location == "Bages" ~ "Bag",
    original_location == "Alt Penedès" ~ "APe",
    original_location == "Baix Llobregat" ~ "BLl",
    original_location == "Barcelonès" ~ "Bar",
    original_location == "Garraf" ~ "Ga",
    original_location == "Moianès" ~ "Mo",
    original_location == "Lluçanès" ~ "Llu",
    original_location == "Anoia" ~ "An",
    TRUE ~ "UNK"
  ))

unique(dd$original_location)

# ------------------------------------- awards name shortening -------------------------------------
unique(dd$awards)

dd <- dd %>%
  mutate(awards = case_when(
    awards == "Certificate of Excellence 2011" ~ "CoE11",
    awards == "Certificate of Excellence 2012" ~ "CoE12",
    awards == "Certificate of Excellence 2013" ~ "CoE13",
    awards == "Certificate of Excellence 2014" ~ "CoE14",
    awards == "Certificate of Excellence 2015" ~ "CoE15",
    awards == "Certificate of Excellence 2016" ~ "CoE16",
    awards == "Certificate of Excellence 2017" ~ "CoE17",
    awards == "Certificate of Excellence 2018" ~ "CoE18",
    awards == "Certificate of Excellence 2019" ~ "CoE19",
    awards == "Certificate of Excellence 2020" ~ "CoE20",
    awards == "Not Awarded" ~ "NotAw",
    TRUE ~ "UNK"
  ))

unique(dd$awards)

# ------------------------------------- cuisines name shortening -------------------------------------
unique(dd$cuisines)

dd <- dd %>%
  mutate(cuisines = case_when(
    cuisines == "European" ~ "EU",
    cuisines == "Asian" ~ "AS",
    cuisines == "Latin American" ~ "LA",
    cuisines == "American" ~ "AM",
    cuisines == "Healthy" ~ "HE",
    cuisines == "Fusion / International" ~ "F/I",
    cuisines == "Others" ~ "O",
    TRUE ~ "UNK"
  ))

unique(dd$cuisines)

# ------------------------------------- saving -------------------------------------

write.table(dd, file = "dataset/renamed.csv", sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
head(dd)

