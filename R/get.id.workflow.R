get.id.workflow<- function(data,hmdb_indices,verbose =T, by_cas=T,
                                by_inchikey=T, by_pc_id=T, by_chebi_id=T,
                                by_metlin_id=T ,by_smiles=F, by_name=F){
  
  
  #search for fitting CAS 
  if(by_cas){
    data<- hmdb.get.id(data = raw_data, hmdb_indices= hmdb_indices, verbose, 
                          name_data = "CAS",name_hmdb_indices = "cas_registry_number")
  }
  
  #adding INCHI and searching for that 
  if(by_inchikey){
    data$InChIKey <- sapply(data$SMILES,FUN = rinchi::get.inchi.key  )
    data<- hmdb.get.id(data = data, hmdb_indices= hmdb_indices, verbose,
                          name_data = "InChIKey",name_hmdb_indices = "inchikey")
  }
  
  
  #Merge by PubChem CID 
  if(by_pc_id){
    data<- hmdb.get.id(data = data, hmdb_indices= hmdb_indices, verbose,
                          name_data = "PC_CID",name_hmdb_indices = "pubchem_compound_id")
  }
  
  #Match by CHEBI id 
  if(by_chebi_id){
    data<- hmdb.get.id(data = data, hmdb_indices= hmdb_indices, verbose,
                          name_data = "CHEBI",name_hmdb_indices = "chebi_id")
  }
  
  #Merge by Metlin ID 
  if(by_metlin_id){
    data<- hmdb.get.id(data = data, hmdb_indices= hmdb_indices, verbose,
                          name_data = "METLIN_ID",name_hmdb_indices = "metlin_id")
  }
  
  #merge by canonical SMILES
  if(by_smiles){
    data$canonical.smiles<-0
    for (i in 1:nrow(data)) {
      smi <- rcdk::parse.smiles(as.character(data$SMILES[i]))[[1]]
      smi1 <- rcdk::get.smiles(smi, rcdk::smiles.flavors(c("Canonical")))
      data$canonical.smiles[i] <- smi1
    }
    data<- hmdb.get.id(data = data, hmdb_indices = hmdb_indices, verbose,
                            name_data = "canonical.smiles",name_hmdb_indices = "canonical.smiles")
  }
  
  #prepare names and match them (not really usefull yet) : 
  if(by_name){
    data$CNAME<- gsub(" ","",data$CNAME)
    data$CNAME<- tolower(data$CNAME)
    
    hmdb_indices$name<- gsub(" ","",hmdb_indices$name)
    hmdb_indices$name<- tolower(hmdb_indices$name)
    
    data<- hmdb.get.id(data = data, hmdb_indices= hmdb_indices, verbose,
                            name_data = "CNAME",name_hmdb_indices = "name")
  }
  
  #reduce on data where HMDB accession is available  
  data <- data[!is.na(data$HMDB),]
  
  return(data)
}