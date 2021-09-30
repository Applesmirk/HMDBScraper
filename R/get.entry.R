#'
#' Get Entry
#'
#' This function downloads the XML Entry of a specific HMDB ID.
#'
#'
#'
#'
#' @param id HMDB ID by which the entry should be access through
#' @param prefix = "http://www.hmdb.ca/metabolites/"
#' @param check_availability = T Option whether it should be check if entry has been revoked
#' @return A List of all available properties
#' @export


get.entry<- function(id, prefix= "http://www.hmdb.ca/metabolites/",check_availability=T){


  if(check_availability){
    if(!check.availability(id)){ stop(paste(id, " id could not be found"))}
  }
  #create link
  link<- paste(prefix,id,".xml",sep= "")





  #download data
  txt<- try(readLines(link))

  #error handling code, just try another 3 times
  i<-1
  while(inherits(txt, "try-error"))
  {
    txt<- try(readLines(link))

    i= i+1
    if(i>3) stop("Could not find Server or ID")
  }


  #process data to be usable
  data<- XML::xmlTreeParse(txt,asText= T)
  data<- XML::xmlToList(data)

  return (data)
}


