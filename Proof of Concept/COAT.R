#Test the possibility of COAT with 4 example DOIs
# This script is a first implementation of the Classification of Open Access Tupples (COAT) 
# for Open Access Monitoring. See also https://doi.org/10.5281/zenodo.1244154
# The script could be improved with better knowledge of R and other data sources

devtools::install_github("ropensci/rcrossref")
require(tidyverse)
require(rcrossref)
require(roadoi)

# the script require the dump from DOAJ which you find at
# https://doaj.org/faq#metadata
# the file name has to be adjusted to the newest version

DOAJ = read_csv("Raw Data/doaj_20180801_1230_utf8.csv")
# Also the dump from the OpenAPC project is used which can be found at
# Later Version can contain a direct download from Github
#https://github.com/OpenAPC/openapc-de/tree/master/data
openapc = read_csv("Raw Data/apc_de.csv.txt")

#Open Access Color definitons based on COAT. Script can work with up to 4 colors if less colors should be 
#calculated repeat same values for folowing colors
color1 <- c("gold",1,4,1,1,3)
color2 <- c("green",2,4,4,4,1)
color3 <- c("closed",4,4,4,4,4)
color4 <- c("closed",4,4,4,4,4)

# 4 Test Cases from the publication database from IST Austria
doi.restricted = "10.1038/s41586-018-0277-x"
# Gold OA not in Open APC listed
doi.gold ="10.1038/s41467-017-02159-y"
# Hybrid OA in Open APC listed
doi.hybrid ="10.1016/j.molp.2016.08.010" 
# Preprint or other Version in Archive
doi.green.preprint ="10.1103/PhysRevFluids.3.054401"
# Postprint in IST Austria Repository
doi.green.postprint ="10.1093/pcp/pcx118"
#Bronze Article 
doi.bronze ="10.1073/pnas.1501343112"

# Cases for testing, pick one
#doi.test <-  doi.restricted
#doi.test <-  doi.gold
#doi.test <-  doi.green.postprint
#doi.test <-  doi.green.preprint
#doi.test <- doi.bronze
doi.test <-  doi.hybrid
# defining licence types
# at the moment only links from CC 4.0 are supported. 
# TODO check if this information can be stored in a external files
licence.open <- c("CC BY","CC BY SA","http://creativecommons.org/licenses/by/4.0/","https://creativecommons.org/licenses/by/4.0/","https://creativecommons.org/licenses/by-sa/4.0/","https://creativecommons.org/licenses/by-sa/4.0/")
licence.free <- c("CC BY NC","CC BY NC SA","CC BY ND","CC BY ND NC","http://creativecommons.org/licenses/by-nc-sa/4.0/","http://creativecommons.org/licenses/by-nc-nd/4.0/")   


# initialization of some values
colnames(DOAJ) <- make.names(colnames(DOAJ))
doi.j.licence <- NA
doi.free <- F
journal.doaj <-  F
# initialization COAT with lowest levels
doi.coat.place <- 4
doi.coat.license <- 4
doi.coat.version <- 4
doi.coat.embargo <- 4
doi.coat.conditions <- 4

# reading data from Crossref and DOAJ
CrossRef.data = cr_works(dois = doi.test) %>% .$data
crossref.issns =  str_split(CrossRef.data$issn,",")
doi.licence= CrossRef.data$license[[1]]
doi.licence= doi.licence %>% filter (content.version == "vor") %>% .$URL
for (i in 1:length(crossref.issns)) {
  journal.doaj <- (crossref.issns[i] %in% DOAJ$Journal.ISSN..print.version. | crossref.issns[i] %in% DOAJ$Journal.EISSN..online.version.| journal.doaj)   
}

if (journal.doaj) {
  oajournal <- DOAJ %>% filter(Journal.EISSN..online.version. == crossref.issns[[1]])
  if (is.na(doi.licence)) {doi.j.licence <- oajournal$Journal.license}
  #todo check if no submission fee is asked for
  if (oajournal$APC.amount>0) doi.free = F else doi.free = T
}
# reading data from unpaywall

oadoi.data <- oadoi_fetch(dois = doi.test, email = "patrick.danowski@ist.ac.at") 
if (oadoi.data$is_oa) { 
  # reads data from best OA location 
doi.oalocations <- oadoi.data$best_oa_location[[1]]
doi.hosttype <- doi.oalocations$host_type
doi.oaurl <- doi.oalocations$url_for_landing_page
doi.oaversion <- doi.oalocations$version
}
# Data from OpenAPC
if (doi.test %in% openapc$doi) {
  doi.free = F  
  doi.hybrid = openapc$is_hybrid
}
# Calculation of publication COAT based on the retrieved values
# embargo period is based on host type, better would be to retrieve publication.data and embargo.date 
# but selected data providers are not supporting this metadata fields. 

# Evaluating place of OA and setting of implicit minimal levels
if (doi.hosttype == "journal" | doi.hosttype == "publisher" ) {
  doi.coat.place <- 1
  doi.coat.version <- 1
  #TODO check how values look for bronze article, maybe set of embargo is not right 
  # doi.coat.embargo <- 1 
  }
if (doi.hosttype == "repository") {
  doi.coat.place <- 2
  doi.free <- T 
  doi.coat.embargo = 3
  doi.coat.version = 3
  }

# Evaluating Condition of OA  
if (doi.free)  doi.coat.conditions <- 1
if (!doi.free & !doi.hybrid) doi.coat.conditions <- 2
if (doi.hybrid) doi.coat.conditions <- 3

# Evaluation of the licence
# First from DOAJ  
if (!is.na(doi.j.licence)) {
  if (doi.j.licence == "CC BY" | doi.j.licence == "CC BY SA") {
    doi.coat.license <-  1
    doi.coat.version <- 1
  } else { 
    if (doi.j.licence == "CC BY NC" | doi.j.licence == "CC BY NC ND") { 
      doi.coat.license <- 2
      doi.coat.version <- 2 
    } else {
      if (is.na(doi.j.licence) & is.na(doi.licence)) doi.coat.license <- 3
    }
  }
}

 
  
# calculation based on licence ref from crossref

if (doi.licence %in% licence.open) {
  doi.coat.license <-  1
  doi.coat.version <- 1
  if (doi.hosttype == "journal" | doi.hosttype == "publisher" ) {
    doi.coat.embargo <- 1
  }
} else { 
  if (doi.licence %in% licence.free) {
    doi.coat.license <- 2
    doi.coat.version <- 1
    if (doi.hosttype == "journal" | doi.hosttype == "publisher" ) {
      doi.coat.embargo <- 1
    }
  } else {
    if (!(is.na(doi.licence))) { doi.coat.license <- 3 }
  }
}

# TODO calculation based on licence ref from OpenApc(not sure if needed)
# TODO analyse oaversion data 


coat.result <- c(doi.coat.place,doi.coat.license, doi.coat.version, doi.coat.embargo, doi.coat.conditions)
coat.result

if (all(coat.result <= color1[-1])) {result.color = color1[1]} else {
  if (all(coat.result <= color2[-1])) {result.color = color2[1]} else
  if (all(coat.result <= color3[-1])) {result.color = color3[1]} else {
    result.color <-  color4[1]
  }
}

doi.coat.place
doi.coat.license
doi.coat.version
doi.coat.embargo
doi.coat.conditions
coat.result
result.color



