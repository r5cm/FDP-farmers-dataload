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