# Initial inspection of Excel files ---------------------------------------

# - Make sure the sheet that contains data is named 'Template for data load'
# - Make sure that the file has the same columns that the Data load template (56)
# - Make sure that there are no old files in the 'To be loaded' folder
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
template.info <- gs_title("FDP - Data load references")
# Import Questions coding
q.coding <- template.info %>% gs_read(ws = "Questions coding", 
                                      range = "A1:D57" , colnames =TRUE)
# Options
q.options <- template.info %>% gs_read(ws = "Questions options",
                                       range = "A1:B25", colnames = TRUE)
# Import table for FDP to data sent values equivalence
values.equivs <- template.info %>% gs_read(ws = "Values equivalences",
                                           colnames =TRUE)

# Import files to load
library(xlsx)
for (i in 1:length(f.to.load)) {
     temp.to.load <- read.xlsx(f.to.load[i], sheetName = "Template for data load",
                               colClasses = q.coding$r.class,
                               stringsAsFactors = FALSE)
     names(temp.to.load) <- q.coding$short.name
     temp.to.load <- temp.to.load[rowSums(is.na(temp.to.load)) != ncol(temp.to.load), ]
     ifelse(i == 1, to.load <- temp.to.load, to.load <- rbind(to.load, temp.to.load))
     rm(temp.to.load)
}


# Clean data --------------------------------------------------------------


# Check that all required variables have information for all observations
# farmer name
nb.farmer.name <- sum(!is.na(to.load$farmer.name))
ifelse(nrow(to.load) == nb.farmer.name, "No blank farmer names",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))
View(to.load[is.na(to.load$farmer.name), ]) # See blanks
# Assigned to
nb.assigned.to <- sum(!is.na(to.load$assigned.to))
ifelse(nrow(to.load) == nb.assigned.to, "No blank assigned to's",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))
# Farmer code
nb.farmer.code <- sum(!is.na(to.load$farmer.code))
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
password <- "gfutzmars2018**rVT2RIYCePpAUf6zhiMIejCH"
session <- rforcecom.login(username, password)
# Retrieve existing farmer codes and check if exist in data to to.load
sf.codes <- rforcecom.retrieve(session, "fdp_farmer__c", "farmerCode__c")
ifelse(sum(to.load$farmer.code %in% sf.codes$farmerCode__c) == 0,
       "No repeated farmer codes found in Salesforce",
       warning("At least one farmer code already in Salesforce", call. = FALSE))
# List farmers to load found in Salesforce
found.sf <- to.load[to.load$farmer.code %in% sf.codes$farmerCode__c, ]

# Check that field officers exist in Salesforce (FIX THIS WHEN UNIQUE FIELD!!!)
# Retrieve field officers names
fo.sf.fields <- c("Name", "Id", "IsActive", "Email", 
                  "Mars_role__c", "Contact.District__r.Name")
fo.sf <- rforcecom.retrieve(session, "User", fo.sf.fields)
# Leave only FCs
fo.sf <- fo.sf[grepl("Field Coordinator", fo.sf$Mars_role__c), ]
# Check that Field officers names are unique
ifelse(length(fo.sf$Name) == length(unique(fo.sf$Name)),
       "No duplicated Field officers",
       warning("There are duplicated Field officers in Salesforce. Please check.",
               call. = FALSE))
fo.sf[duplicated(fo.sf$Name), ] # Find duplicated field officer names
fo.sf[fo.sf$Name == "Hasdir Hasdir", ] # List duplicated field officer
fo.sf <- fo.sf[fo.sf$Id != "00528000004BKRFAA4", ] # Remove a FO from list
# Run again first line above (ifelse(length(fo.sf$...)))

# Fix FCs names to corresponding SF names
fc.correct.names <- gs_read(ss = template.info, ws = "FC equivalences", colnames = TRUE)
# Check if all names contained in to.load are in fc.correct.names
ifelse(sum(!(to.load$assigned.to %in% fc.correct.names$Sent | 
                  to.load$assigned.to %in% fc.correct.names$Salesforce)) == 0,
       "All sent names in correct SF names table",
       warning("There are FC names not contained in correct SF names table",
               call. = FALSE))
# If there are missing names above, list them
unique(to.load$assigned.to[!(to.load$assigned.to %in% fc.correct.names$Sent | 
                           to.load$assigned.to %in% fc.correct.names$Salesforce)])
# Add SF names
for(i in 1:nrow(to.load)) {
     # Find position of sent name in equivalences table and replace with SF name
     if(to.load$assigned.to[i] %in% fc.correct.names$Sent) {
          temp.equiv.pos <- grep(to.load$assigned.to[i], fc.correct.names$Sent)
          to.load$assigned.to[i] <- fc.correct.names$Salesforce[temp.equiv.pos]
          rm(temp.equiv.pos)
     }
}
# Check that FOs assigned to farmers that will be loaded exist in Salesforce
fo.to.load <- unique(to.load$assigned.to) # list of FO in farmers to load
ifelse(sum(!(fo.to.load %in% fo.sf$Name)) == 0,
       "All field officers assigned exist in Salesforce",
       warning("There are field officers that were not found in Salesforce",
               call. = FALSE))
# If there are missing FOs in Salesforce
fo.to.load.in.sf <- to.load$assigned.to %in% fo.sf$Name
unique(fo.to.load[fo.to.load.in.sf == FALSE]) # List FOs not found
# fo.to.load[fo.to.load.in.sf == FALSE] # List FOs not found
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


# VILLAGES

# No missing villages (required) - list them
na.villages.ind <- is.na(to.load$village)
table(na.villages.ind) #all should be FALSE
missing.villages <- to.load[na.villages.ind, c("farmer.name", "farmer.code")]
# Remove space from end of village names
for(i in 1:nrow(to.load)) {
     vill.nchar <- nchar(to.load$village[i])
     if(substr(to.load$village[i], vill.nchar, vill.nchar) == " ") {
          to.load$village[i] <- substr(to.load$village[i], 1, vill.nchar - 1)
     }
     rm(vill.nchar)
}
# Correct names using the equivalences table
village.equivs <- gs_read(template.info, ws = "Village equivalences")
for(i in 1:nrow(to.load)) {
     if(to.load$village[i] %in% village.equivs$Incorrect) {
          temp.pos <- grep(to.load$village[i], village.equivs$Incorrect)
          to.load$village[i] <- village.equivs$Correct[temp.pos]
          rm(temp.pos)
     }
}
# check that villages exist in Salesforce
villages.sf <- rforcecom.retrieve(session, "fpd_village__c", "Name")
ifelse(sum(!(to.load$village %in% villages.sf$Name)) > 0,
       warning("There are villages in the file that do not exist in SF", call. = FALSE),
       "All villages in the file exist in Salesforce")
# IF there are missing villages, list them:
villages.notsf <- !(to.load$village %in% villages.sf$Name)
View(data.frame(unique(to.load$village[villages.notsf])))
write.csv(unique(to.load[villages.notsf, c("assigned.to", "village")]),
          "missing_villages.csv")

# Create missing villages

# Get villages that are not in Salesforce, and their FCs
new_vill <- unique(to.load[villages.notsf, c("assigned.to", "village")])
# Remove/REPLACE special characters (TEST FIRST TIME WITH REAL DATA)
library(stringi)
new_vill <- stringi::stri_trans_general(test, "latin-ascii")
new_vill <- gsub("\'", '', new_vill)

# Add district name to villages
new_vill <- left_join(new_vill, fo.sf[, c("Id", "Contact.District__r.Name")],
                      by=c("assigned.to" = "Id"))
# Add district id to villages
sf.districts <- rforcecom.retrieve(session, "fpd_district__c", c("Id", "Name"))
new_vill <- left_join(new_vill, sf.districts, 
                      by=c("Contact.District__r.Name" = "Name"))
# Create job
new_vill <- data.frame(Name=new_vill$village, district__c=new_vill$Id)
job_info <- rforcecom.createBulkJob(session, 
                                    operation='insert', 
                                    object='fpd_village__c')
# Load data
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          new_vill, 
                                          multiBatch = TRUE, 
                                          batchSize = 50)
# Batches status
batches_status <- lapply(batches_info, 
                         FUN=function(x){
                              rforcecom.checkBatchStatus(session, 
                                                         jobId=x$jobId, 
                                                         batchId=x$id)
                         })
# Close job
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)
# Export villages created
write.csv(new_vill$Name, "Villages_created_2.csv")

# Leave phone number as only number
to.load$phone.number <- gsub(" ", "", to.load$phone.number)

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
table(to.load$gender, useNA = 'always')

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
table(to.load$spouse, useNA = 'always')
# Correct different options (Duda, Janda = Widow)
wrong.yes <- c("yes", "Ya")
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
wrong.certifications <- c("Certified")
to.load$certifications <- ifelse(to.load$certifications %in% wrong.certifications,
                                 "RA", to.load$certifications)

# Farm area in cocoa
dist.farm.area <- as.data.frame(table(to.load$cocoa.cultivation.ha, useNA = 'always'))
to.load$cocoa.cultivation.ha <- ifelse(to.load$cocoa.cultivation.ha > 49,
                                       NA, to.load$cocoa.cultivation.ha)

# Final visual inspection to all data
sapply(to.load, table)



# Create dataframe per object -------------------------------------------------------

v1_v2 <- gs_read(ss = template.info, ws = "v1_to_v2", range = "A1:C58", colnames = T)
v2_objects <- unique(v1_v2$Object)
v2_objects <- v2_objects[!is.na(v2_objects)]

load_date = strtrim(Sys.Date(), 10)
load.ids = c()
for(i in 1:nrow(to.load)) {
     load.ids[i] = paste(load_date, sprintf("%08d", i), sep = "")
}
na.rows <- rep(NA, nrow(to.load))

# Submission
tl.submission = data.frame(Id_v1__c = load.ids,
                           Surveyor__c = to.load$Assigned_to__c,
                           Respondent__c = na.rows,
                           Respondent_v1__c = load.ids
                           )

# Farmer
tl.farmer = data.frame(Id_v1__c = load.ids,
                       FDP_submission__c = na.rows,
                       FDP_submission_v1__c = load.ids,
                       FDP_Farm__c = na.rows,
                       FDP_Farm_v1__c = load.ids,
                       fullName__c = to.load$fullName__c,
                       farmerCode__c = to.load$farmerCode__c,
                       birthday__c = to.load$birthday__c,
                       gender__c = to.load$gender__c,
                       educationalLevel__c = to.load$educationalLevel__c,
                       village__c = to.load$village__c,
                       gps__c = to.load$gps__c,
                       householdAddress__c = to.load$householdAddress__c,
                       Phone_number__c = to.load$phoneNumber__c,
                       yearsRelationshipWithMars__c = to.load$yearsRelationshipWithMars__c,
                       Children_between_5_17__c = to.load$under17__c,
                       Children_5_17_enrolled_in_school__c = to.load$under17InSchool__c,
                       Family_members__c = to.load$familyMembers__c,
                       Farmer_group__c = to.load$farmerGroup__c
                       )

# Farm
tl.farm = data.frame(Id_v1__c = load.ids,
                     FDP_submission__c = na.rows,
                     FDP_submission_v1__c = load.ids,
                     farmer__c = na.rows,
                     farmer_v1__c = load.ids,
                     farmCertifications__c = to.load$farmCertifications__c
                     )

# Farm baseline
tl.farmbl = data.frame(Id_v1__c = load.ids,
                       FDP_submission__c = na.rows,
                       FDP_submission_v1__c = load.ids,
                       farm__c = na.rows,
                       farm_v1__c = load.ids,
                       totalCocoaArea__c = to.load$totalCocoaArea__c
                       )



# to.load data to SF ----------------------------------------------------------

source("../FDP-farmers-dataload/load_sf.R")


# Submissions 1 (respondent blank)

load.sf('insert', 'fpd_Submission__c', tl.submission, 1)
batch.status(batches_info)
batch.detail(batches_info)
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)


# Farmers 1 (farm blank)

# Add village Ids
sf.vill <- rforcecom.retrieve(session, "fpd_village__c", c("Id", "Name"))
sf.vill <- sf.vill[!duplicated(sf.vill$Name), ]
tl.farmer$village__c <- left_join(tl.farmer, sf.vill, by = c("village__c" = "Name"))[["Id"]]
# Add submission Id
sub.sql <- "SELECT Id, Id_v1__c FROM fpd_Submission__c WHERE CreatedDate >= 2018-09-07T00:00:00Z AND CreatedDate < 2018-09-09T00:00:00Z"
sf.sub <- rforcecom.query(session, sub.sql) # retrieve submissions from SF
tl.farmer$FDP_submission__c <- left_join(tl.farmer, sf.sub, 
                                         by = c("FDP_submission_v1__c" = "Id_v1__c"))[["Id"]]
# Drop gps field (no permission, fix if data exist)
tl.farmer <- subset(tl.farmer, select = -gps__c)
# Load farmers
load.sf('insert', 'fdp_farmer__c', tl.farmer, 1)
batch.status(batches_info)
batch.detail(batches_info)
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)


# Submission 2 (fill respondent)

# Add farmers
farmer.sql <- "SELECT Id, Id_v1__c FROM fdp_farmer__c WHERE CreatedDate >= 2018-09-07T00:00:00Z AND CreatedDate < 2018-09-09T00:00:00Z"
sf.farmer <- rforcecom.query(session, farmer.sql)
tu.submission <- left_join(sf.sub, sf.farmer, by = "Id_v1__c")
tu.submission <- data.frame(Id = tu.submission$Id.x, Respondent__c = tu.submission$Id.y)
# Update farmers
load.sf('update', 'fpd_Submission__c', tu.submission, 1)
batch.status(batches_info)
batch.detail(batches_info)
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)

# Farm

# Add submission
tl.farm$FDP_submission__c <- left_join(tl.farm, sf.sub, 
                                       by = c("FDP_submission_v1__c" = "Id_v1__c"))[["Id"]]
# Add farmer
tl.farm$farmer__c <- left_join(tl.farm, sf.farmer,
                               by = c("farmer_v1__c" = "Id_v1__c"))[["Id"]]
# Load farms
load.sf('insert', 'fdp_Farm__c', tl.farm, 1)
batch.status(batches_info)
batch.detail(batches_info)
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)


# Farmer 2 (fill farm)

# Add farm
farm.sql <- "SELECT Id, Id_v1__c FROM fdp_Farm__c WHERE CreatedDate >= 2018-09-07T00:00:00Z AND CreatedDate < 2018-09-09T00:00:00Z"
sf.farm <- rforcecom.query(session, farm.sql)
tu.farmer <- left_join(sf.farmer, sf.farm, by = "Id_v1__c")
tu.farmer <- data.frame(Id = tu.farmer$Id.x, FDP_Farm__c = tu.farmer$Id.y)
# Update farmer
load.sf('update', 'fdp_farmer__c', tu.farmer, 1)
batch.status(batches_info)
batch.detail(batches_info)
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)


# Farm baseline

# Add submission
tl.farmbl$FDP_submission__c <- left_join(tl.farmbl, sf.sub, 
                                       by = c("FDP_submission_v1__c" = "Id_v1__c"))[["Id"]]
# Add farm
tl.farmbl$farm__c <- left_join(tl.farmbl, sf.farm,
                               by = c("farm_v1__c" = "Id_v1__c"))[["Id"]]
# Load farm baseline
load.sf('insert', 'fdp_Farm_BL__c', tl.farmbl, 1)
batch.status(batches_info)
batch.detail(batches_info)
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)


### MODIFY BELOW TO GROUP EACH LOAD INTO FOLDER

save(tl.submission, tl.farmer, tl.farm, tl.farmbl, file = 'data_loaded.RData')
write.csv(tl.submission, 'submission_loaded.csv')
write.csv(tl.farmer, 'farmer_loaded.csv')
write.csv(tl.farm, 'farm_loaded.csv')
write.csv(tl.farmbl, 'farmbaseline_loaded.csv')

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