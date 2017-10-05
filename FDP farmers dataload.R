
# Initial inspection of Excel files ---------------------------------------

# - Make sure the sheet that contains data is named 'Template for data load'
# - Make sure that there are no old files in the 'To be loaded' folder
# - Do a quick visual inspection of the file
# - Make sure there are no hidden rows or columns
# - Make sure that data start in cell A1


# Import load to R ---------------------------------------------------------

# Create list with files to load
setwd("../To be loaded")
to.load <- list.files(pattern = ".xlsx")

# Import metadata of data to import
library(googlesheets)
library(dplyr)
(my_sheets <- gs_ls())
template.info <- gs_title("FDP - Data load template")
questions.coding <- template.info %>% gs_read(ws = "Questions coding", 
                                              range = "A1:D57" , colnames =TRUE)

# Import files to load
for (i in 1:length(to.load)) {
     temp.load <- read.xlsx(to.load[i], sheetName = "Template for data load",
                          colClasses = questions.coding$r.class,
                          stringsAsFactors = FALSE)
     names(temp.load) <- questions.coding$short.name
     temp.load <- temp.load[rowSums(is.na(temp.load)) != ncol(temp.load), ]
     temp.load <- data.frame(file = rep(to.load[i], nrow(temp.load)), temp.load)
     ifelse(i == 1, load <- temp.load, load <- rbind(load, temp.load))
}


# Clean data --------------------------------------------------------------

# Check variables class
sapply(load, class)
# Check variables summary
library(Hmisc)
sapply(load, summary)

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

# Check that field officers exist in Salesforce (FIX THIS WITH UNIQUE FIELD)
# Retrieve field officers names
fo.sf.fields <- c("Name", "Id", "IsActive")
fo.sf <- rforcecom.retrieve(session, "User", fo.sf.fields)
# Check that Field officers emails are unique
ifelse(length(fo.sf$Name) == length(unique(fo.sf$Name)),
       "No duplicated Field officers",
       warning("There are duplicated Field officers in Salesforce. Please check",
               call. = FALSE))
# Check the FOs assigned to farmers that will be loaded exist in Salesforce
fo.load <- unique(load$assigned.to)
fo.load.in.sf <- fo.load %in% fo.sf$Name
ifelse(length(fo.load.in.sf[fo.load.in.sf == FALSE]) == 0,
       "All field officers assigned exist in Salesforce",
       warning("There are field officers that were not found in Salesforce",
               call. = FALSE))

# Assign field officers ID in assigned to
load <- left_join(load, select(fo.sf, Name, Id),
                  by = c("assigned.to" = "Name"))
assigned <- is.na(load$Id)
ifelse(length(assigned[assigned == FALSE]) == nrow(load),
       "All assigned.to successfully assigned",
       warning("There are assigned.to blank. Please check.", call. = FALSE))
# Change name to assign.to Id and replace assigned.to for this variable
load <- select(load, farmer.name, Id, farmer.code:renovation.ha)
names(load)[names(load) == "Id"] <- "assigned.to"

# Check that year variables contain only the year
date.vars <- c("farmer.birthday", "spouse.bday", "year.relationship.start")
# farmer.birthday
fb.chars <- nchar(as.character(load$farmer.birthday))
ifelse(max(fb.chars) == min(fb.chars), 
       paste("All fields have ", max(fb.chars), " characters.", sep = ""),
       warning(paste("Values with different characters. Max = ", max(fb.chars),
             ", Min = ", min(fb.chars), ".", sep = ""), call. = FALSE))
load$farmer.birthday <- as.integer(substr(as.character(load$farmer.birthday),
                                          1, 4))
# spouse.bday
sb.chars <- nchar(as.character(load$spouse.bday))
ifelse(max(sb.chars, na.rm = TRUE) == min(sb.chars,  na.rm = TRUE), 
       paste("All fields have ", max(sb.chars,  na.rm = TRUE), " characters.",
             sep = ""),
       warning(paste("Values with different characters. Max = ",
                     max(sb.chars,  na.rm = TRUE), ", Min = ",
                     min(sb.chars,  na.rm = TRUE), ".", sep = ""),
               call. = FALSE))
load$spouse.bday <- as.integer(substr(as.character(load$spouse.bday),
                                          1, 4))
# year.relationship.start
yrs.chars <- nchar(as.character(load$years.relationship.start))
ifelse(max(yrs.chars, na.rm = TRUE) == min(yrs.chars,  na.rm = TRUE), 
       paste("All fields have ", max(yrs.chars,  na.rm = TRUE), " characters.",
             sep = ""),
       warning(paste("Values with different characters. Max = ",
                     max(yrs.chars,  na.rm = TRUE), ", Min = ",
                     min(yrs.chars,  na.rm = TRUE), ".", sep = ""),
               call. = FALSE))
load$years.relationship.start <- as.integer(substr(as.character(load$years.relationship.start),
                                      1, 4))

# Minimum and maximum values in date fields
# Farmer birthday
min(load$farmer.birthday, na.rm = TRUE)
max(load$farmer.birthday, na.rm = TRUE)
load$farmer.birthday <- ifelse(load$farmer.birthday == 0, NA,
                               load$farmer.birthday)
# Spouse birthday
min(load$spouse.bday, na.rm = TRUE)
max(load$spouse.bday, na.rm = TRUE)
load$spouse.bday <- ifelse(load$spouse.bday == 0, NA, load$spouse.bday)
# Year relationship start
min(load$year.relationship.start)
max(load$year.relationship.start)
load$year.relationship.start <- ifelse(load$year.relationship.start == 0, NA,
                                       load$year.relationship.start)


# Load data to SF ----------------------------------------------------------

# Remove file variables
select(load, -file)

# Change variable names to API names
names(load) <- questions.coding$api.name

# Load data to Salesforce
# Create job
job_info <- rforcecom.createBulkJob(session, 
                                    operation='insert', 
                                    object='FDP_submission__c')
# Load data
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          load, 
                                          multiBatch = TRUE, 
                                          batchSize = 50)

# Batches status
batches_status <- lapply(batches_info, 
                         FUN=function(x){
                              rforcecom.checkBatchStatus(session, 
                                                         jobId=x$jobId, 
                                                         batchId=x$id)
                         })
status <- c()
records.processed <- c()
records.failed <- c()
for(i in 1:length(batches_status)) {
     status[i] <- batches_status[[i]]$state
     records.processed[i] <- batches_status[[i]]$numberRecordsProcessed
     records.failed[i] <- batches_status[[i]]$numberRecordsFailed
}
data.frame(status, records.processed, records.failed)
# Details of each batch
batches_detail <- lapply(batches_info, 
                         FUN=function(x){
                              rforcecom.getBatchDetails(session, 
                                                        jobId=x$jobId, 
                                                        batchId=x$id)
                         })
# Close job
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)


# Test data loaded --------------------------------------------------------



# Organize ----------------------------------------------------------------

# Logout from Salesforce
rforcecom.logout(session)

# Move loaded files to 'Previously loaded' folder
file.copy()
file.remove()