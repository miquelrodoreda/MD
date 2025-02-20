setwd("/Users/miquelrodoreda/uni/MD")

library(codebookr)
library(dplyr)

before <- read.csv("dataset/only_columns.csv")
after <- read.csv("dataset/columnrefactor.csv")

glimpse(before)
glimpse(after)

print(x = codebook(before), target = "before_preprocessing.docx")
print(x = codebook(after), target = "after_preprocessing.docx")
