# Salesforce login
library(RForcecom)
username <- readline(prompt = "Enter username: ")
password <- readline(prompt = "Enter password: ")
session <- rforcecom.login(username, password)

# Retrieve records to delete
delete <- rforcecom.retrieve(session, "FDP_submission__c", 
                             c("Id", "CreatedDate"))
delete$CreatedDate <- substr(as.character(delete$CreatedDate), 1, 10)
delete <- delete[delete$CreatedDate == "2018-02-21", ]
delete <-  data.frame(delete[1])

# Delete records
job_info <- rforcecom.createBulkJob(session, 
                                    operation='delete', 
                                    object='FDP_submission__c')

batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          delete, 
                                          multiBatch = TRUE, 
                                          batchSize = 500)

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


# Batches details
batches_detail <- lapply(batches_info, 
                         FUN=function(x){
                              rforcecom.getBatchDetails(session, 
                                                        jobId=x$jobId, 
                                                        batchId=x$id)
                         })

# Close job
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)

# Logout from Salesforce
rforcecom.logout(session)
