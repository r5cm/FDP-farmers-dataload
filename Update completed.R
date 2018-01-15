# Salesforce login ------------------------------------------------------------------

library(RForcecom)
username <- "admin@utzmars.org"
password <- "gfutzmars2017**oyCqeCwuJQCCfOBACJKmKOIr8"
session <- rforcecom.login(username, password)


# Download and update FDP submissions ------------------------------------------------

# Download FDP submissions
query.fdpSub <- "SELECT Id FROM FDP_submission__c WHERE plot1Area__c != null AND plot1Area__c != '0' AND Assigned_to__r.Profile.Name = 'FDP users'"
fdpSub <- rforcecom.query(session, query.fdpSub)

# Add Plot10 area with blank value
fdpSub <- data.frame(fdpSub, plot10Area__c = rep("N/A", nrow(fdpSub)))

# Insert job
job_info <- rforcecom.createBulkJob(session, 
                                    operation='update', 
                                    object='FDP_submission__c')
# Insert data
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          fdpSub, 
                                          multiBatch = TRUE, 
                                          batchSize=1)
# Close job
rforcecom.closeBulkJob(session, jobId = job_info$id)
