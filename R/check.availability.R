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

  webpage <- try(rvest::read_html(link))

  #error handling code, just try another 3 times
  i<-1
  while(inherits(webpage, "try-error"))
  {
    print(paste("Checking for availibility has failed  ",i,". time",sep=""))
    webpage<- try(rvest::read_html(link))

    i= i+1
    if(i>3) stop("Could not find Server or ID")
  }

  webpage<- rvest::html_text(webpage)
  available <- !grepl("has been revoked",webpage)


  return(available)
}
