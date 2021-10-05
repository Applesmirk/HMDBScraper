#'
#' Get ID
#'
#' This function adds missing HMDB IDs to a data frame depending on a matching criteria given by the user.
#'
#' @param data data.frame with column HMDB where the indices are stored
#' @param name_data name of column from data by which should be matched
#' @param name_hmdb_indices name of column from hmdb_indices by which should be matched Can be on of the following:
#' - "name"
#' - "cas_registry_number"
#' - "smiles"
#' - "inchi"
#' - "inchikey"
#' - "chemspider_id"
#' - "chebi_id"
#' - "pubchem_compound_id"
#' - "metlin_id"
#' - "kegg_id"
#' - "canonical.smiles"
#' @return Data.frame data with added HMDB indices
#' @export


get.id <- function (data, name_data, name_hmdb_indices,verbose =T){

  hmdb_indices<- HMDBScraper::hmdb_indices
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
