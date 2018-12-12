# 1. Initial inspection of Excel files ---------------------------------------

# - Make sure the sheet that contains data is named 'Template for data load'
# - Make sure that the file has the same columns that the Data load template (56)
# - Make sure that there are no old files in the 'To be loaded' folder
# - Do a quick visual inspection of the file
# - Make sure there are no hidden rows or columns
# - Make sure that data start in cell A1


# 2. Import to.load to R ---------------------------------------------------------


# Create list with files to load

setwd("../2_To_be_loaded")
f.to.load <- list.files(pattern = ".xlsx")[1]


# Import metadata and equivalence tables

library(googlesheets)
library(dplyr)
my_sheets <- gs_ls()
template.info <- gs_title("FDP - Data load references")
# Import Questions coding
q.coding <- template.info %>% gs_read(ws = "Questions coding", 
                                      range = "A1:D57" , colnames =TRUE)
# Import question Option
q.options <- template.info %>% gs_read(ws = "Questions options",
                                       range = "A1:B25", colnames = TRUE)
# Import common errors table
values.equivs <- template.info %>% gs_read(ws = "Values equivalences",
                                           colnames =TRUE)

# Import files to load

library(xlsx)
to.load <- read.xlsx(f.to.load, sheetName = "Template for data load",
                          colClasses = q.coding$r.class,
                          stringsAsFactors = FALSE)
names(to.load) <- q.coding$short.name


# 3. Clean data --------------------------------------------------------------


# farmer name: no blank observations

nb.farmer.name <- sum(!is.na(to.load$farmer.name))
ifelse(nrow(to.load) == nb.farmer.name, "No blank farmer names",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))
View(to.load[is.na(to.load$farmer.name), ]) # See blank observations


# Assigned to: no blank observations

nb.assigned.to <- sum(!is.na(to.load$assigned.to))
ifelse(nrow(to.load) == nb.assigned.to, "No blank assigned to's",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))


# Farmer code: no blank observations

nb.farmer.code <- sum(!is.na(to.load$farmer.code))
ifelse(nrow(to.load) == nb.farmer.code, "No blank farmer codes",
       warning("Encountered blank fields. Make sure there are no missing fields.",
               call. = FALSE))


# Farmer code: no repeated values in file

unique.codes <- length(unique(to.load$farmer.code))
ifelse(nrow(to.load) == unique.codes, "All farmer codes are unique",
       warning("Duplicate codes found. Make sure that farmer codes are unique",
               call. = FALSE))
# List duplicated codes
dup.codes <- to.load$farmer.code[duplicated(to.load$farmer.code)]
View(to.load[to.load$farmer.code %in% dup.codes, ])


# Farmer code: no values already in Salesforce

# Salesforce login
library(RForcecom)
username <- "admin@utzmars.org"
password <- "gfutzmars2019**dsxcAt9SEvg9ktmMBCH8lFOU"
session <- rforcecom.login(username, password)

# Retrieve existing farmer codes and check if exist in data to to.load
sf.codes <- rforcecom.retrieve(session, "fdp_farmer__c", "farmerCode__c")
ifelse(sum(to.load$farmer.code %in% sf.codes$farmerCode__c) == 0,
       "No repeated farmer codes found in Salesforce",
       warning("At least one farmer code already in Salesforce", call. = FALSE))

# List farmers to load found in Salesforce
View(to.load[to.load$farmer.code %in% sf.codes$farmerCode__c, ])


# Assigned to: user names in Salesforce should be unique

# Retrieve Salesforce users
fo.sf.fields <- c("Name", "Id", "IsActive", "Email", 
                  "Mars_role__c", "Contact.District__r.Name")
fo.sf <- rforcecom.retrieve(session, "User", fo.sf.fields)

# Leave only users with role "Field Coordinator"
fo.sf <- fo.sf[grepl("Field Coordinator", fo.sf$Mars_role__c), ]

# Check that user names are unique
ifelse(length(fo.sf$Name) == length(unique(fo.sf$Name)),
       "No duplicated Field officers",
       warning("There are duplicated Field officers in Salesforce. Please check.",
               call. = FALSE))
fo.sf[duplicated(fo.sf$Name), ] # Print duplicated field officer names


# Assigned to: users in file should exist in Salesforce

# Fix FCs names to corresponding SF names
fc.correct.names <- gs_read(ss = template.info, ws = "FC equivalences", colnames = TRUE)

# Check if all names in the file are in fc.correct.names
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

# Check that "Assigned to" in file exist as Salesforce Users
fo.to.load <- unique(to.load$assigned.to) # list of FO in farmers to load
ifelse(sum(!(fo.to.load %in% fo.sf$Name)) == 0,
       "All field officers assigned exist in Salesforce",
       warning("There are field officers that were not found in Salesforce",
               call. = FALSE))

# If there are missing FOs in Salesforce, list them
unique(fo.to.load[!(to.load$assigned.to %in% fo.sf$Name)]) # List Assigned to, not in SF
View(fo.sf[order(as.character(fo.sf$Name)), ]) # List all User in SF, for reference

# Assign field officers ID in assigned to
to.load <- left_join(to.load, select(fo.sf, Name, Id),
                     by = c("assigned.to" = "Name"))
assigned <- is.na(to.load$Id)
ifelse(length(assigned[assigned == FALSE]) == nrow(to.load),
       "All farmers to load successfully assigned to Field officers",
       warning("There are assigned.to blank. Please check.", call. = FALSE))

# Change field name from Id to assigned.to
to.load <- select(to.load, farmer.name, Id, farmer.code:plots)
names(to.load)[names(to.load) == "Id"] <- "assigned.to"


# Villages: no blanks

na.village.ind <- is.na(to.load$village)
ifelse(sum(na.village.ind) == 0, "No blank villages",
       warning("There are blank villages", call. = FALSE))
# List missing villages
missing.villages <- to.load[na.village.ind, c("farmer.name", "farmer.code")]


# Villages: should exist in Salesforce

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
# Append villages that will be created
write.csv(unique(to.load[villages.notsf, c("assigned.to", "village")]),
          "../3_previously_loaded/missing_villages.csv", append = TRUE)

# Create missing villages

# Get villages that are not in Salesforce, and the corresponding SF user
new.vill <- unique(to.load[villages.notsf, c("assigned.to", "village")])

# Remove/replace special characters (TEST FIRST TIME WITH REAL DATA)
library(stringi)
new.vill$village <- stringi::stri_trans_general(new.vill, "latin-ascii")
new.vill <- gsub("\'", '', new.vill)

# Add district name to villages
new_vill <- left_join(new_vill, fo.sf[, c("Id", "Contact.District__r.Name")],
                      by=c("assigned.to" = "Id"))

# Add district id to villages
sf.districts <- rforcecom.retrieve(session, "fpd_district__c", c("Id", "Name"))
new_vill <- left_join(new_vill, sf.districts, 
                      by=c("Contact.District__r.Name" = "Name"))

# Create data load job
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
write.csv(new_vill$Name, "Villages_created.csv", append = TRUE)



# Phon number: remove all non-numbers

to.load$phone.number <- gsub(" ", "", to.load$phone.number)


# farmer.birthday: correct format and range

to.load$farmer.birthday #visual inspection

# If date imported as integer (integer with 5 digits), run next line
to.load$farmer.birthday <- as.Date(to.load$farmer.birthday, 
                                   origin = "1899-12-30")

# CAUTION: run this only for files with different origin
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

# Visual inspection, again
summary(to.load$farmer.birthday)


# spouse.bday: same validations as farmer bday

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


# Gender: correct different values for a given option

table(to.load$gender)
# Only Male and Female
wrong.male <- c("L", "Laki-laki")
wrong.fem <- c("P", "Famale")
to.load$gender <- ifelse(to.load$gender %in% wrong.male, "Male",
                         ifelse(to.load$gender %in% wrong.fem, "Female",
                                to.load$gender))
table(to.load$gender, useNA = 'always')


# Educational level: correct values different than FDP options

table(to.load$educational.level)

# Educational levels valid values
educ.valid <- q.options$option[q.options$short.name == "educational.level"]
educ.equiv <- values.equivs[values.equivs$question == "educational.level",]

# Farmer educaional level
table(to.load$educational.level %in% educ.valid) #false: non valid values
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

# Spouse educational level
table(to.load$spouse.education)
sp.educ.equiv <- values.equivs[values.equivs$question == "spouse.education",]
table(to.load$spouse.education %in% educ.valid) #false: non valid values
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


# Have spouse: correct values not included in FDP options

table(to.load$spouse, useNA = 'always')

# Correct different options (Duda, Janda = Widow)
wrong.yes <- c("yes", "Ya")
wrong.no <- c("no", "Duda", "Janda")
to.load$spouse <- ifelse(to.load$spouse %in% wrong.yes, "Yes",
                         ifelse(to.load$spouse %in% wrong.no, "No",
                                to.load$spouse))
table(to.load$spouse)


# Farmer group: check if there seem to be misspellings

groups <- as.data.frame(table(to.load$farmer.group))
groups[order(groups$Var1), ]


# Farm certifications: correct values not included in FDP options

table(to.load$certifications, useNA = 'always')
wrong.certifications <- c("Certified")
to.load$certifications <- ifelse(to.load$certifications %in% wrong.certifications,
                                 "RA", to.load$certifications)


# Farm area in cocoa: check value ranges

dist.farm.area <- as.data.frame(table(to.load$cocoa.cultivation.ha, useNA = 'always'))
to.load$cocoa.cultivation.ha <- ifelse(to.load$cocoa.cultivation.ha > 49,
                                       NA, to.load$cocoa.cultivation.ha)


# Final visual inspection to all data
sapply(to.load, table)



# 4. Create dataframe per object -------------------------------------------------------

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



# 5. to.load data to SF ----------------------------------------------------------

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

# 6. Save copy of data loaded ------------------------------------------------

copy.name <- paste("../3_Previously_loaded/data_loaded_",
                   substr(Sys.time(), 1, 10), ".csv",
                   sep = "")
copy <- rforcecom.retrieve(session, "FDP_submission__c", q.coding$api.name)

write.csv(copy, copy.name)


# 7. Organize ----------------------------------------------------------------

# Logout from Salesforce
rforcecom.logout(session)

# Move to.loaded files to 'Previously to.loaded' folder
file.copy(f.to.load,
          paste("../3_Previously_loaded/", f.to.load, sep = ""))
file.remove(f.to.load)