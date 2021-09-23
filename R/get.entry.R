get.entry<- function(id, prefix= "http://www.hmdb.ca/metabolites/",check_availability=T){

  
  if(check_availability){
    if(!hmdb.check.availability(id)){ stop(paste(id, " id could not be found"))}
  }
  #create link
  link<- paste(prefix,id,".xml",sep= "")
  
  #download data
  txt<- readLines(link)
  
  #process data to be usable 
  data<- XML::xmlTreeParse(txt,asText= T)
  data<- XML::xmlToList(data)
  
  return (data)
}


