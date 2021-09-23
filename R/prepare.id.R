#' Prepare ID
#'
#' This Function will append any ID given to it until it has reached length 7
#' and afterwards combines it with "HMDB".
#' This allows the access to the right HMDB Hyperlink
#'
#'
#'
#' @param id Number or HMDB ID
#' @return HMDB ID of length 7
#' @export

prepare.id<- function (id){

  id <- gsub("HMDB","",id)
  len<- nchar(id)
  while(len!=7){
    id<- paste("0",id,sep="")
    len<- nchar(id)
  }

  return(paste("HMDB",id,sep= ""))
}
