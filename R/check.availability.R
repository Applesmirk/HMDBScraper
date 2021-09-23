check.availability<- function(id, hmdb_prefix = "http://www.hmdb.ca/metabolites/"){ 
  
  link <- paste(hmdb_prefix,id,sep= "")
  
  webpage <- rvest::read_html(link) 
  webpage<- rvest::html_text(webpage)
  available <- !grepl("has been revoked",webpage) 
  return(available)
}
