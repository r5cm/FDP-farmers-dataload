
# Import load to R ---------------------------------------------------------

# Create list with files to load
setwd("../To be loaded")
to.load <- list.files()

# Import files to load
library(xlsx)
for (i in 1:length(to.load)) {
     file.name <- paste("file", i, sep = "")
     ifelse(i == 1,
            load <- read.xlsx(to.load[i], sheetName = "Template for data load"),
            assign(file.name, read.xlsx(to.load[i], 
                                        sheetName = "Template for data load")))
     load <- rbind(load, file.name)
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
names(load) <- questions.coding$short.name


# Clean data --------------------------------------------------------------

# Check variables class
sapply(load, class)

# Check that all required variables have information for all observations
# farmer name
nb.farmer.name <- length(load$famer.name[!is.na(load$famer.name)])
ifelse(nrow(load) == nb.farmer.name, "No blank fields",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))
# Assigned to
nb.assigned.to <- length(load$assigned.to[!is.na(load$assigned.to)])
ifelse(nrow(load) == nb.assigned.to, "No blank fields",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))
# Farmer code
nb.farmer.code <- length(load$farmer.code[!is.na(load$farmer.code)])
ifelse(nrow(load) == nb.farmer.code, "No blank fields",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))

# Check that farmer codes in the files are unique
unique.codes <- length(unique(load$farmer.code))
ifelse(nrow(load) == unique.codes, "All codes in files to load are unique",
       warning("Duplicate codes found. Make sure that farmer codes are unique",
               call. = FALSE))

# Check that farmer codes are not already assigned to farmers in Salesforce
# Salesforce login
library(RForcecom)
username <- "admin@utzmars.org"
password <- "gfutzmars2017**oyCqeCwuJQCCfOBACJKmKOIr8"
session <- rforcecom.login(username, password)
# Retrieve existing farmer codes and check if exist in data to load
sf.codes <- rforcecom.retrieve(session, "FDP_submission__c", "farmerCode__c")
repeat.codes <- load$farmer.code %in% sf.codes$farmerCode__c
ifelse(length(repeat.codes[repeat.codes == TRUE]) == 0,
                   "No repeated codes found in Salesforce",
                   warning("At least one code already found in Salesforce",
                           call. = FALSE))

# Check that field officers exist in Salesforce
# Retrieve field officers names
# 
# Check that names are unique

# Check that year variables contain the year only


# Minimum and maximum values in date fields


# Load data to SF ----------------------------------------------------------




# Move loaded files to 'Previously loaded' folder