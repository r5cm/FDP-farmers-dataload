# Load data to Salesforce
load.sf = function(operation, object, source_data, batch_size = 50) {
     
     # Create job
     job_info <- rforcecom.createBulkJob(session, 
                                         operation = operation, 
                                         object = object)
     job_info <<- job_info
     
     # Load data
     batches_info <- rforcecom.createBulkBatch(session, 
                                               jobId=job_info$id, 
                                               source_data, 
                                               multiBatch = TRUE, 
                                               batchSize = batch_size)
     batches_info <<- batches_info
}

# Get batches status
batch.status = function(batches_info) {
     
     # Get batch status
     batches_status <- lapply(batches_info, 
                              FUN=function(x){
                                   rforcecom.checkBatchStatus(session, 
                                                              jobId=x$jobId, 
                                                              batchId=x$id)
                              })
     
     # Format batch stats
     status <- c()
     records.processed <- c()
     records.failed <- c()
     message <- c()
     for(i in 1:length(batches_status)) {
          status[i] <- batches_status[[i]]$state
          records.processed[i] <- batches_status[[i]]$numberRecordsProcessed
          records.failed[i] <- batches_status[[i]]$numberRecordsFailed
     }
     data.frame(status, records.processed, records.failed)
}

# Get batches status details
batch.detail <- function(batches_info) {
     batches_detail <- lapply(batches_info, 
                              FUN=function(x){
                                   rforcecom.getBatchDetails(session, 
                                                             jobId=x$jobId, 
                                                             batchId=x$id)
                              })
     batches_detail
}
