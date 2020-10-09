## functions to clean Mayo Clinic CCLE datasets

cleanDrugResp <- function(data) {
  # Arguments:
  #   data: drug response data frame in the format of the output of the 
  #     loadDrugResp() function
  # Returns:
  #   drug_resp: data.frame of 504 cell lines by 24 drugs; drug responses
  
  drug_resp <- data %>% 
    column_to_rownames("drug") %>% 
    t() %>%   # transpose to cell lines by drug
    as.data.frame()
  
  # sum(is.na(drug_resp_orig)) = 426
  
  return(drug_resp)
}


cleanMiRNA <- function(data) {
  # Arguments:
  #   data: mirna data frame in the format of the output of the loadMiRNA()
  #     function
  # Returns:
  #   mirna: data.frame of 954 cell lines by 734 miRNAs; microRNA expressions

  mirna <- data %>% 
    t() %>%   # transpose to cell lines by mirna
    as.data.frame()
  
  return(mirna)
}


cleanRNASeq <- function(data) {
  # Arguments:
  #   data: rnaseq data frame in the format of the output of the loadRNASeq()
  #     function  
  # Returns:
  #   rnaseq: data.frame of 1156 cell lines by 56318 gene transcripts; RPKM
  #     values measured via RNA Sequencing
  
  rnaseq <- data %>%
    t() %>%   # transpose to cell lines by rnaseq
    as.data.frame()
  
  # remove features with 0 variance
  zero_var <- which(apply(rnaseq, 2, var) == 0)
  rnaseq <- rnaseq[, -zero_var]
  
  return(rnaseq)
}


cleanMethyl <- function(data) {
  # Arguments:
  #   data: methyl data frame in the format of the output of the loadMethyl()
  #     function
  # Returns:
  #   methyl: data.frame of 843 cell lines by 20193 TSS (transcription start
  #     sites) regions; methylation values (beta-values)
  
  methyl <- data %>%
    t() %>%   # transpose to cell lines by methyl
    as.data.frame()
  
  # notes: need to remove NAs...
  # sum(is.na(data)) = 686017
  
  return(methyl)
}


cleanProtein <- function(data) {
  # Arguments:
  #   data: protein data frame in the format of the output of the loadProtein()
  #     function
  # Returns:
  #   prot: data.frame of 899 cell lines by 214 anti-bodies; protein expression
  
  prot <- data %>%
    t() %>%   # transpose to cell lines by methyl
    as.data.frame()
  
  return(prot)
}

