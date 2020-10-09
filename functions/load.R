## functions to load in Mayo Clinic CCLE data sets

loadDrugResp <- function(path = "data/raw/") {
  # Arguments:
  #   path: the path indicating the location of the `CCLE_drug_ActArea.txt` file
  # Returns:
  #   drug_resp: data.frame of 24 drugs by 504 cell lines; drug responses
  
  drug_resp <- read.table(paste0(path, "CCLE_drug_ActArea.txt"), 
                          header = T, sep = "")
  return(drug_resp)
}


loadMiRNA <- function(path = "data/raw/") {
  # Arguments:
  #   path: path indicating the location of the `ccle_miRNA_expression.txt` file
  # Returns:
  #   mirna: data.frame of 734 miRNAs by 954 cell lines; microRNA expressions

  mirna <- read.table(paste0(path, "ccle_miRNA_expression.txt"),
                      header = T, sep = "")
  return(mirna)
}


loadRNASeq <- function(path = "data/raw/") {
  # Arguments:
  #   path: path indicating the location of the `ccle_RNAseq_RPKM.txt` file
  # Returns:
  #   rnaseq: data.frame of 56318 gene transcripts by 1156 cell lines; RPKM
  #     values measured via RNA Sequencing
  
  rnaseq <- read.table(paste0(path, "ccle_RNAseq_RPKM.txt"),
                       header = T, sep = "")
  return(rnaseq)
}


loadMethyl <- function(path = "data/raw/") {
  # Arguments:
  #   path: path indicating the location of the `ccle_RRBS_TSS_1kb.txt` file
  # Returns:
  #   methyl: data.frame of 20193 TSS (transcription start sites) regions by 843
  #     cell lines; methylation values (beta-values)
  
  methyl <- read.table(paste0(path, "ccle_RRBS_TSS_1kb.txt"),
                       header = T, sep = "")
  return(methyl)
}


loadProtein <- function(path = "data/raw/") {
  # Arguments:
  #   path: path indicating the location of the `ccle_RPPA.txt` file
  # Returns:
  #   prot: data.frame of 214 anti-bodies by 899 cell lines; protein expression
  
  prot <- read.table(paste0(path, "ccle_RPPA.txt"), header = T, 
                     sep = "\t", fill = TRUE)
  
  return(prot)
}

