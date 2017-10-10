
# Initial inspection of Excel files ---------------------------------------

# - Make sure the sheet that contains data is named 'Template for data to.load'
# - Make sure that there are no old files in the 'To be to.loaded' folder
# - Do a quick visual inspection of the file
# - Make sure there are no hidden rows or columns
# - Make sure that data start in cell A1


# Import to.load to R ---------------------------------------------------------

# Create list with files to load
setwd("../2. To be loaded")
f.to.load <- list.files(pattern = ".xlsx")
# Recommended to load files one by one (change index below)
f.to.load <- f.to.load[1]


# Import metadata of data to import
library(googlesheets)
library(dplyr)
(my_sheets <- gs_ls())
template.info <- gs_title("FDP - Data load template")
# Import Questions coding
q.coding <- template.info %>% gs_read(ws = "Questions coding", 
                                              range = "A1:D57" , colnames =TRUE)
# Options
q.options <- template.info %>% gs_read(ws = "Questions options",
                                       range = "A1:B25", colnames = TRUE)
# Import table for FDP to data sent values equivalence
values.equivs <- template.info %>% gs_read(ws = "Values equivalences",
                                           colnames =TRUE)

# Import files to to.load
library(xlsx)
for (i in 1:length(f.to.load)) {
     temp.to.load <- read.xlsx(f.to.load[i], sheetName = "Template for data load",
                          colClasses = q.coding$r.class,
                          stringsAsFactors = FALSE)
     names(temp.to.load) <- q.coding$short.name
     temp.to.load <- temp.to.load[rowSums(is.na(temp.to.load)) != ncol(temp.to.load), ]
     temp.to.load <- data.frame(file = rep(f.to.load[i], nrow(temp.to.load)), temp.to.load)
     ifelse(i == 1, to.load <- temp.to.load, to.load <- rbind(to.load, temp.to.load))
     rm(temp.to.load)
}


# Clean data --------------------------------------------------------------


# Check that all required variables have information for all observations
# farmer name
nb.farmer.name <- length(!is.na(to.load$farmer.name))
ifelse(nrow(to.load) == nb.farmer.name, "No blank farmer names",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))
View(to.load[is.na(to.load$farmer.name), ]) # See blanks
# Assigned to
nb.assigned.to <- length(!is.na(to.load$assigned.to))
ifelse(nrow(to.load) == nb.assigned.to, "No blank assigned to's",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))
# Farmer code
nb.farmer.code <- length(!is.na(to.load$farmer.code))
ifelse(nrow(to.load) == nb.farmer.code, "No blank farmer codes",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))

# Check that farmer codes in the files are unique
unique.codes <- length(unique(to.load$farmer.code))
ifelse(nrow(to.load) == unique.codes, "All farmer codes are unique",
       warning("Duplicate codes found. Make sure that farmer codes are unique",
               call. = FALSE))
# List duplicated codes
dup.codes <- to.load$farmer.code[duplicated(to.load$farmer.code)]
View(to.load[to.load$farmer.code %in% dup.codes, ])
# Drop the duplicated (those with less information)
to.load <- to.load[order(to.load$farmer.birthday), ] # first those with bday
to.load <- to.load[!duplicated(to.load$farmer.code), ]

# Check that farmer codes are not already assigned to farmers in Salesforce
# Salesforce login
library(RForcecom)
username <- "admin@utzmars.org"
password <- "gfutzmars2017**oyCqeCwuJQCCfOBACJKmKOIr8"
session <- rforcecom.login(username, password)
# Retrieve existing farmer codes and check if exist in data to to.load
sf.codes <- rforcecom.retrieve(session, "FDP_submission__c", "farmerCode__c")
ifelse(sum(to.load$farmer.code %in% sf.codes$farmerCode__c) == 0,
       "No repeated farmer codes found in Salesforce",
       warning("At least one farmer code already in Salesforce", call. = FALSE))

# Check that field officers exist in Salesforce (FIX THIS WHEN UNIQUE FIELD!!!)
# Retrieve field officers names
fo.sf.fields <- c("Name", "Id", "IsActive", "Email")
fo.sf <- rforcecom.retrieve(session, "User", fo.sf.fields)
# Check that Field officers names are unique
ifelse(length(fo.sf$Name) == length(unique(fo.sf$Name)),
       "No duplicated Field officers",
       warning("There are duplicated Field officers in Salesforce. Please check.",
               call. = FALSE))
fo.sf[duplicated(fo.sf$Name), ] # Find duplicated field officer names
fo.sf[fo.sf$Name == "Hasdir Hasdir", ] # List duplicated field officer
fo.sf <- fo.sf[fo.sf$Id != "00528000004BKRFAA4", ] # Remove a FO from list
# Run again first line above (ifelse(length(fo.sf$...)))

# Check the FOs assigned to farmers that will be loaded exist in Salesforce
fo.to.load <- unique(to.load$assigned.to) # list of FO in farmers to load
fo.to.load.in.sf <- fo.to.load %in% fo.sf$Name # check if exist in Salesforce
ifelse(length(fo.to.load.in.sf[fo.to.load.in.sf == FALSE]) == 0,
       "All field officers assigned exist in Salesforce",
       warning("There are field officers that were not found in Salesforce",
               call. = FALSE))
# If there are missing FOs in Salesforce
fo.to.load[fo.to.load.in.sf == FALSE] # List FOs not found
View(fo.sf[order(as.character(fo.sf$Name)), ]) # List FOs in SF

# Assign field officers ID in assigned to
to.load <- left_join(to.load, select(fo.sf, Name, Id),
                  by = c("assigned.to" = "Name"))
assigned <- is.na(to.load$Id)
ifelse(length(assigned[assigned == FALSE]) == nrow(to.load),
       "All farmers to load successfully assigned to Field officers",
       warning("There are assigned.to blank. Please check.", call. = FALSE))
# Change name to assign.to Id and replace assigned.to for this variable
to.load <- select(to.load, farmer.name, Id, farmer.code:plots)
names(to.load)[names(to.load) == "Id"] <- "assigned.to"

# farmer.birthday
to.load$farmer.birthday #visual inspection
# If date imported as integer (integer with 5 digits), run:
to.load$farmer.birthday <- as.Date(to.load$farmer.birthday, 
                                   origin = "1899-12-30")
# CAUTION: only for files with different origin:
origin <- "1970-01-01"
to.load$farmer.birthday <- as.Date(to.load$farmer.birthday, origin = origin)
# If date contains day and month, remove:
to.load$farmer.birthday <- as.integer(substr(as.character(to.load$farmer.birthday),
                                          1, 4))
summary(to.load$farmer.birthday) # summary
# Remove data for farmers born before 1930 and after 2002
to.load$farmer.birthday <- ifelse(to.load$farmer.birthday < 1930 | 
                                       to.load$farmer.birthday > 2002, 
                                  NA, to.load$farmer.birthday)
# Summarize (last step to check)
summary(to.load$farmer.birthday)

# spouse.bday
to.load$spouse.bday #visual inspection
# If date imported as integer (integer with 5 digits), run:
to.load$spouse.bday <- as.Date(to.load$spouse.bday, origin = "1899-12-30")
# If date contains day and month, remove:
to.load$spouse.bday <- as.integer(substr(as.character(to.load$spouse.bday),
                                             1, 4))
summary(to.load$spouse.bday) # summary
# Remove data for spouses born before 1930
to.load$spouse.bday <- ifelse(to.load$spouse.bday < 1930 | 
                                   to.load$spouse.bday > 2010, 
                              NA, to.load$spouse.bday)
# Summarize (last step to check)
summary(to.load$spouse.bday)

# year.relationship.start
to.load$year.relationship.start #visual inspection
# If date imported as integer (integer with 5 digits), run:
to.load$year.relationship.start <- as.Date(to.load$year.relationship.start, origin = "1899-12-30")
# If date contains day and month, remove:
to.load$year.relationship.start <- as.integer(substr(as.character(to.load$year.relationship.start),
                                         1, 4))
summary(to.load$year.relationship.start) # summary
# Remove data for farmers born before 1930
to.load$year.relationship.start <- ifelse(to.load$year.relationship.start < 1930, 
                              NA, to.load$year.relationship.start)
# Summarize (last step to check)
summary(to.load$year.relationship.start)

# Gender
table(to.load$gender)
# Only Male and Female
wrong.male <- c("L", "Laki-laki")
wrong.fem <- c("P", "Famale")
to.load$gender <- ifelse(to.load$gender %in% wrong.male, "Male",
                         ifelse(to.load$gender %in% wrong.fem, "Female",
                                to.load$gender))
table(to.load$gender)

# Educational level
table(to.load$educational.level)
# Educational levels valid values
educ.valid <- q.options$option[q.options$short.name == "educational.level"]
educ.equiv <- values.equivs[values.equivs$question == "educational.level",]
# FARMER
table(to.load$educational.level %in% educ.valid) #false: non valid values
# Create variable with correct values
educ.correct <- c()
for(i in 1:nrow(to.load)) {
     educ.correct[i] <- ifelse(to.load$educational.level[i] %in% educ.valid,
                               to.load$educational.level[i], 
                               ifelse(to.load$educational.level[i] %in% educ.equiv$value,
                                      educ.equiv$fdp.value[educ.equiv$value == to.load$educational.level[i]],
                                      NA))
}
View(table(data.frame(educ.correct, to.load$educational.level)))
to.load$educational.level <- educ.correct
# SPOUSE
table(to.load$spouse.education)
sp.educ.equiv <- values.equivs[values.equivs$question == "spouse.education",]
table(to.load$spouse.education %in% educ.valid) #false: non valid values
# Create variable with correct values
sp.educ.correct <- c()
for(i in 1:nrow(to.load)) {
     sp.educ.correct[i] <- ifelse(to.load$spouse.education[i] %in% educ.valid,
                               to.load$spouse.education[i], 
                               ifelse(to.load$spouse.education[i] %in% sp.educ.equiv$value,
                                      sp.educ.equiv$fdp.value[sp.educ.equiv$value == to.load$spouse.education[i]],
                                      NA))
}
View(table(data.frame(sp.educ.correct, to.load$spouse.education)))
to.load$spouse.education <- sp.educ.correct

# Have spouse
table(to.load$spouse)
# Correct different options (Duda, Janda = Widow)
wrong.yes <- c("yes")
wrong.no <- c("no", "Duda", "Janda")
to.load$spouse <- ifelse(to.load$spouse %in% wrong.yes, "Yes",
                         ifelse(to.load$spouse %in% wrong.no, "No",
                                to.load$spouse))
table(to.load$spouse)

# Farmer group (check if there seem  to be misspellings -i.e. very similarly
# spelled groups)
groups <- as.data.frame(table(to.load$farmer.group))
groups[order(groups$Var1), ]

# Farm certifications
table(to.load$certifications, useNA = 'always')

# Farm area in cocoa
table(to.load$cocoa.cultivation.ha, useNA = 'always')
to.load$cocoa.cultivation.ha <- ifelse(to.load$cocoa.cultivation.ha > 49,
                                       NA, to.load$cocoa.cultivation.ha)

# Final visual inspection to all data
sapply(to.load, table)


# to.load data to SF ----------------------------------------------------------


# Change variable names to API names
names(to.load) <- q.coding$api.name

# to.load data to Salesforce
# Create job
job_info <- rforcecom.createBulkJob(session, 
                                    operation='insert', 
                                    object='FDP_submission__c')
# to.load data
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          to.load, 
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



# Save copy of data loaded ------------------------------------------------

copy.name <- paste("../3. Previously loaded/data_loaded_",
                   substr(Sys.time(), 1, 10), ".csv",
                   sep = "")
copy <- rforcecom.retrieve(session, "FDP_submission__c", q.coding$api.name)

write.csv(copy, copy.name)


# Organize ----------------------------------------------------------------

# Logout from Salesforce
rforcecom.logout(session)

# Move to.loaded files to 'Previously to.loaded' folder
file.copy(f.to.load,
          paste("../3. Previously loaded/", f.to.load, sep = ""))
file.remove(f.to.load)

