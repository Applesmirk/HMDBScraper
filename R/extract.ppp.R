extract.ppp<- function(entry){ 
  
  #extract predicted properties 
  pred_properties<- entry$predicted_properties
  
  #reformat 
  pred_properties<- matrix(unlist(pred_properties),nrow=3)
  colnames(pred_properties)<-pred_properties[1,] 
  pred_properties<- pred_properties[2:3,]
  rownames(pred_properties)<- c("value", "origin")
  
  #delete unnecessary information 
  pred_properties<- pred_properties[,-which(colnames(pred_properties)%in% 
                                    c("bioavailability", "rule_of_five",
                                      "ghose_filter", "veber_rule", "mddr_like_rule",
                                      "inchikey","smiles","formula","inchi","iupac"))]
  pred_properties<- as.data.frame(pred_properties)
  
  if ("logp.1"%in% colnames(pred_properties)){
    pred_properties$logp<- pred_properties$logp.1
    pred_properties$logp.1<- NULL
  }
  if("solubility"%in% colnames(pred_properties)){ 
    pred_properties$solubility<- gsub(" g/L","",pred_properties$solubility)
    
  }
  pred_properties<- pred_properties[,!duplicated(colnames(pred_properties),
                                                 fromLast=T)]
  pred_properties[1,]<- as.numeric(pred_properties[1,])
  return(pred_properties[1,])
}
    