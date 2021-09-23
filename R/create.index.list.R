#' Create index list
#'
#' This Function is used to generate initial hmdb_indices data.frame
#' which can be used to match CAS, InChI, InChIKey, chemspider ID, chebi ID,
#' Pubchem ID, metlin ID, Name or SMILES to the HMDB ID using function
#' get.id.
#'
#' Function expects local path to the downloaded XML file from HMDB.
#'
#'
#' @param path Path to the downloaded HMDB XML database
#' @return A data.frame with indices
#' @export

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
