# Salesforce login ------------------------------------------------------------------

library(RForcecom)
username <- "admin@utzmars.org"
password <- "gfutzmars2018*hn5OC4tzSecOhgHKnUtZL05C"
session <- rforcecom.login(username, password)


# Download and update FDP submissions ------------------------------------------------

# Download FDP submissions
query.fdpSub <- "SELECT Id, plot10Area__c FROM FDP_submission__c WHERE plot1Area__c != null AND plot1Area__c != '0' AND Assigned_to__r.Profile.Name = 'FDP users'"
fdpSub <- rforcecom.query(session, query.fdpSub)

# # Add Plot10 area with blank value
# fdpSub <- data.frame(fdpSub, plot10Area__c = rep("N/A", nrow(fdpSub)))

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

# Find failed records
# Get details on each batch
batches_detail <- lapply(batches_info, 
                         FUN=function(x){
                              rforcecom.getBatchDetails(session, 
                                                        jobId=x$jobId, 
                                                        batchId=x$id)
                         })
# Get id of failed records
library(dplyr)
status_id <- c()
for(i in seq_along(batches_detail)) {
     status_id[i] <- batches_detail[[i]]$Id
}
status_id <- status_id[!is.na(status_id)]
failed <- fdpSub[!(fdpSub$Id %in% status_id), ]

# Close job
close_job_info <- rforcecom.closeBulkJob(session, jobId = job_info$id)

# Update again
# Insert job
job_info <- rforcecom.createBulkJob(session, 
                                    operation='update', 
                                    object='FDP_submission__c')
# Insert data
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          failed, 
                                          multiBatch = TRUE, 
                                          batchSize=1)
# Close job
close_job_info <- rforcecom.closeBulkJob(session, jobId = job_info$id)
