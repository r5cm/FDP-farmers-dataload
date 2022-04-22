
# Salesforce login ------------------------------------------------------------------

library(RForcecom)
username <- readline(prompt = "Enter username: ")
password <- readline(prompt = "Enter password: ")
session <- rforcecom.login(username, password)


# Download FDP submissions ----------------------------------------------------------

query.fdpSub <- "SELECT fullName__c, farmerCode__c, Id, village__c FROM FDP_submission__c WHERE plot1Area__c != null AND plot1Area__c != '0' AND Assigned_to__r.Profile.Name = 'FDP users'"
fdpSub <- rforcecom.query(session, query.fdpSub)


# 1. Create farmers -----------------------------------------------------------------

# Create data frame with data to load
farmers <- fdpSub
names(farmers) <- c("FDP_submission__c", "fullName__c", "Name", "village__c")

# Add village
# Retrieve villages
villages <- rforcecom.retrieve(session, "village__c", c("Name", "Id"))
# Add village Id to farmers
library(dplyr)
farmers <- left_join(farmers, villages, by = c("village__c" = "Name"))
# Remove village name and rename
farmers <- select(farmers, -village__c)
names(farmers)[4] <- "village__c"

# Insert job
job_info <- rforcecom.createBulkJob(session, 
                                    operation='insert', 
                                    object='farmer__c')
# Insert data
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          farmers, 
                                          multiBatch = TRUE, 
                                          batchSize=500)
# Close job
rforcecom.closeBulkJob(session, jobId = job_info$id)



# 2. Create farms -------------------------------------------------------------------

# Download loaded farms above
query.farmers <- "SELECT Id, Name FROM farmer__c WHERE CreatedDate = TODAY"
farmer.sf <- rforcecom.query(session, query.farmers)

# Select data to load
farms <- select(farmers, Name, FDP_submission__c)
# Add relationship to farmer
farms <- left_join(farms, farmer.sf, by = "Name")
names(farms)[3] <- "farmer__c"

# Insert job
job_info <- rforcecom.createBulkJob(session, 
                                    operation='insert', 
                                    object='Farm__c')
# Insert data
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          farms, 
                                          multiBatch = TRUE, 
                                          batchSize=500)
# Close job
rforcecom.closeBulkJob(session, jobId = job_info$id)



# 3. Update FDP submissions ---------------------------------------------------------

# Add farmer field to FDP submission
upd.fdpSub <- left_join(fdpSub, farmer.sf, by = c("farmerCode__c" = "Name"))
# Select fields to update 
upd.fdpSub <- select(upd.fdpSub, Id.x, Id.y)
# Change field names to API names
names(upd.fdpSub) <- c("Id", "farmer__c")

# Insert job
job_info <- rforcecom.createBulkJob(session, 
                                    operation='update', 
                                    object='FDP_submission__c')
# Insert data
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          upd.fdpSub, 
                                          multiBatch = TRUE, 
                                          batchSize=500)
# Close job
rforcecom.closeBulkJob(session, jobId = job_info$id)
