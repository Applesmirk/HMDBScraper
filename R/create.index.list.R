create.index.list<- function(path){
  
  hmdb_data<- XML::xmlToDataFrame(path)

  hmdb_indices<-c("accession", "name", "cas_registry_number", "smiles", "inchi", "inchikey", 
                  "chemspider_id", "chebi_id", "pubchem_compound_id",
                  "metlin_id")
    
  
  hmdb_indices$canonical.smiles<-0
  for (i in 1:nrow(hmdb_indices)) {
    smi <- rcdk::parse.smiles(as.character(hmdb_indices$smiles[i]))[[1]]
    smi1 <- rcdk::get.smiles(smi, rcdk::smiles.flavors(c("Canonical")))
    hmdb_indices$canonical.smiles[i] <- smi1
  }
    return(hmdb_indices)
}