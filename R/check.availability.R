#'
#' Check availability
#'
#' This function checks if a HMDB ID has been revoked
#'
#' @param id HMDB ID
#' @param hmdb_prefix = "http://www.hmdb.ca/metabolites/"
#' @return TRUE/FALSE
#' @export


check.availability<- function(id, hmdb_prefix = "http://www.hmdb.ca/metabolites/"){

  link <- paste(hmdb_prefix,id,sep= "")

  webpage <- rvest::read_html(link)
  webpage<- rvest::html_text(webpage)
  available <- !grepl("has been revoked",webpage)
  return(available)
}
