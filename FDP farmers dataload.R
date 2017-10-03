
# Import data to R ---------------------------------------------------------

# Create list with files to load
setwd("../To be loaded")
to.load <- list.files()

# Import files to load
library(xlsx)
for (i in 1:length(to.load)) {
     file.name <- paste("file", i, sep = "")
     ifelse(i == 1,
            data <- read.xlsx(to.load[i], sheetName = "Template for data load"),
            assign(file.name, read.xlsx(to.load[i], 
                                        sheetName = "Template for data load")))
     data <- rbind(data, file.name)
}

# Change variable names by short names
# Obtain questions coding from Google Drive spreadsheet
library(googlesheets)
library(dplyr)
(my_sheets <- gs_ls())
template.info <- gs_title("FDP - Data load template")
questions.coding <- template.info %>% gs_read(ws = "Questions coding", 
        range = "A1:C57" , colnames =TRUE)
# Change names for shorter names
names(data) <- questions.coding$short.name

# Clean data --------------------------------------------------------------

# Check that all required variables have information for all observations


# Check that farmer codes are unique

# Check that field officers exist in Salesforce

# Check that year variables contain the year only


# Load data ---------------------------------------------------------------




# Movel loaded files to 'Previously loaded' folder