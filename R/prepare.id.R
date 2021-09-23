prepare.id<- function (id){ 
  
  id <- gsub("HMDB","",id)
  len<- nchar(id)
  while(len!=7){ 
    id<- paste("0",id,sep="")
    len<- nchar(id)
  }
  
  return(paste("HMDB",id,sep= ""))
}
