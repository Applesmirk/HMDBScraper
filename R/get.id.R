#'
#' Get ID
#'
#' This function adds missing HMDB IDs depending on a given matching criteria.
#'
#' @param data Data.frame with column HMDB where the indices are stored
#' @param hmdb_indices Data.frame created by create.index.list
#' @param name_data name of column from data by which should be matched
#' @param name_hmdb_indices name of column from hmdb_indices by which should be matched
#' @return Data.frame data with added HMDB indices
#' @export


get.id <- function (data,hmdb_indices, name_data, name_hmdb_indices,verbose =T){
  hmdb_indices<- hmdb_indices[!duplicated(hmdb_indices[,which(colnames(hmdb_indices)%in%name_hmdb_indices)]),]
  data<- merge(data,hmdb_indices,by.x= name_data,by.y = name_hmdb_indices, all.x=T, all.y= F)
  data$HMDB <- dplyr::coalesce(data$HMDB,data$accession)
  data[,which(colnames(data)%in%colnames(hmdb_indices))]<-NULL
  if(verbose){
    print(paste("Stats after merging by ",name_data))
    print("HMDB ID  available")
    print(table(!is.na(data$HMDB)))
  }

  return(data)
}
