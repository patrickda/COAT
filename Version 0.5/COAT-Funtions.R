# This script is a first implementation of the Classification of Open Access Tupples (COAT) 
# for Open Access Monitoring. See also https://doi.org/10.5281/zenodo.1244154
# The script could be improved with better knowledge of R and other data sources
#
# Step one: Reading Data from external Sources (this script)
# Step two: Joining all information in one table
# Step three: Analayse the table and creat a COAT for every DOI on base of the data
# Step four: Defining labels (e.g. colors) for minimum levels and alalyse the COATs for creating a list of labels
# step five: Creating a visualisation based on the labels


# This script gets Open Access informations from the following sources
# * CrossRef
# * OpenAPC
# * DOAJ
# * Unpaywall
# Plane improvements
# * Sherpa Romeo
 
require(dplyr)
require(tidyverse)
require(rcrossref)
require(roadoi)
require(xml2)
require(httr)

# Download  last version of required data 
download.file("https://doaj.org/csv", destfile = "Data/DOAJ.csv")
download.file("https://github.com/OpenAPC/openapc-de/raw/master/data/apc_de.csv", destfile = "Data/OpenAPC.csv")
download.file("https://github.com/OpenAPC/openapc-de/raw/master/data/offsetting/offsetting.csv", destfile = "Data/Offsetting.csv")

# Input data
# Requires a csv file with the following information
# DOI: mandatory
# Year: optional
# Insitution: optional
# Country: optional

input.data <- read_csv("Data/inputdata.csv")

# function to extract OA parameters from DOAJ for an ISSN
# Returns tibble with 4 elements
# - ISSN (same as the function got called with)
# - DOAJ result (True if issn is in DOAJ else False)
# - Licence information for journal as stored in DOAJ
# - OA Fee information as information stored in DOAJ

DOAJoa <- function(issn) {
  if (journal.doaj <- (issn %in% DOAJ$Journal.ISSN..print.version. | issn %in% DOAJ$Journal.EISSN..online.version.)  ) {
    DOAJ.result <- T
    oajournal <- DOAJ %>% filter(Journal.EISSN..online.version. == issn)
    DOAJ.licence <- oajournal$Journal.license
    oa.fee <- oajournal$APC.amount
  } else {
    DOAJ.result <- F
    DOAJ.licence <- NA
    oa.fee <- NA
  }
  
  tibble(issn,DOAJ.result,DOAJ.licence,oa.fee)
}

# funtion to extract OA parameters from OpenAPC for an DOI
# Return a tibble with 3 Elements 
# - DOI (same as the function got called with)
# - inOpenAPC (True if DOI is found in OpenAPC Data, False if DOI is not found)
# - Amount of costs for the DOI, value is 0 if no information is found

OpenAPCOA <- function(doi.input) {
 in.openapc <- F
 amount <- 0
 hybrid <- F
 if (doi.input %in% openapc$doi) {
   article <- openapc %>% filter (doi == doi.input)
   amount <-  article$euro 
   hybrid = article$is_hybrid
   in.openapc <- T
 }
 tibble(doi.input,in.openapc,hybrid,amount)
}

# Function to extract OA parameter from OADOI for an DOI
# Returns a tibble with 4 elemnts
# - DOI 
# - 

oadoioa <- function (doi.input) {
  
  if (grepl("10.",doi.input)) {
    oadoi.data <- oadoi_fetch(dois = doi.input, email = "patrick.danowski@ist.ac.at")
  } else {
    oadoi.data <- tibble(doi.input, is_oa = F)
  }
  if (length(oadoi.data) == 0) {
    oadoi.data <- tibble(doi.input, is_oa = F)
  }
  doi.oalocation <- NA
  doi.hosttype <- NA
  doi.oaurl <- NA
  doi.oaversion <- NA

  if (oadoi.data$is_oa) {
    # reads data from best OA location 
    doi.hosttype <- oadoi.data$best_oa_location[[1]]$host_type
    doi.oaurl <- oadoi.data$best_oa_location[[1]]$url
    doi.oaversion <- if (!(is.null(oadoi.data$best_oa_location[[1]]$version))) {oadoi.data$best_oa_location[[1]]$version} else {NA}
  }
  tibble(doi.input,doi.hosttype,doi.oaurl,doi.oaversion)
}

# Function to extract OA parameters from Crossref
# Returns a tibble with  elements
# * DOI
# * ISSN 1
# * ISSN 2
# * Licence
# * Year (preferred Print publication year, if not available online publication year )

Cross_ref_oa <- function(doi) { 
#  doi <- "10.1080/01690965.2013.835433"
  lic.url <- "protected"
  lic = c()
  pub.data = NA
  pub.data <- cr_works(dois = doi) %>% .$data 
  if (!(is.null(pub.data))) {
  issn <- pub.data$issn
  if (is.null(issn)) {
    issn <- "no ISSN"
    issn2 <- NA
  } else { 
    issn2 <- last(str_split(issn,",", simplify = T))
    issn <- first(str_split(issn,",", simplify = T))
    if (issn == issn2) issn2 <-  NA
  }
  
  lic <- pub.data$license
  lic <- lic[[1]]$URL
  if (!is.null(lic)) {
    for (i in 1:length(lic)) { 
      if (grepl("creativecommons",lic[i])) { lic.url <- lic[i] }
    }
  }
  if (is.null(pub.data$published.print)) {
    if (is.null(pub.data$published.print)) {
      year <- "no year"
    }
      year <- substr(pub.data$published.online, start=1,stop=4)
    } else year <- substr(pub.data$published.print, start=1,stop=4)
  } else {
    issn <- "no ISSN"
    issn2 <- NA
    lic.url <- ""
    year <- "not found"
  }
  tibble(doi,issn,issn2,lic.url,year)
}  

# init of result 
cr.result <- NA
# Reading the used Data Files in the global environment
DOAJ = read_csv("Data/DOAJ.csv")
openapc = read_csv("Data/OpenAPC.csv")
openapc.offsetting = read_csv("Data/Offsetting.csv")
colnames(DOAJ) <- make.names(colnames(DOAJ))

# Check which fields are part of the input.data
doi.list <- input.data %>% .$DOI %>% unique() 
if (is.null(input.data$Year)) input.year <- F else input.year <- T
if (is.null(input.data$Institution)) input.institution <- F else input.institution <- T
if (is.null(input.data$Country)) input.country <- F else input.country <- T


# Get OA data & year from crossref
cr.result<-  lapply(doi.list,Cross_ref_oa) %>% 
  bind_rows()

write_csv(cr.result,"Data/cr.result.csv")

doi.list <- cr.result %>% filter((!issn=="no ISSN")) %>% .$doi
doi.excluded <- cr.result %>% filter(issn=="no ISSN") %>% .$doi
# creating a ISSN List from data

cr.result.included <- cr.result %>% filter((!issn=="no ISSN"))

issn.list <- append(cr.result.included$issn,cr.result.temp$issn2)
issn.list <- unique(issn.list)

# Check four the journals DOAJ Data for OA information
doaj.result <- lapply(issn.list, DOAJoa) %>%
  bind_rows()

write_csv(doaj.result,"Data/doaj-result.csv")

# Check OpenAPC for OA information


openapc.result <- lapply(doi.list,OpenAPCOA) %>% 
  bind_rows()

write_csv(openapc.result,"Data/openapc-result.csv")

# Check unpaywall
oadoi.result <- lapply(doi.list, oadoioa) %>% bind_rows()
write_csv(oadoi.result,"Data/oadoi-result.csv")

# Work in progress including of Sherpa Romeo (Test for single journal)

sherpa.romeo <- function (ISSN) {
  ISSN <- "0036-8075"
  URL <- paste0("http://www.sherpa.ac.uk/romeo/api29.php?issn=",ISSN)
  URL
  sherpa.romeo.xml <- GET(URL)
  content <- content(sherpa.romeo.xml, as ="text")
  sherpa.romeo.xml <-read_xml(sherpa.romeo.xml)
  hits <-  xml_text(xml_find_all(sherpa.romeo.xml, "//numhits"))

  if (hits == 1) {
    embargo <- xml_find_all(sherpa.romeo.xml, "////postrestriction")
    test <- read_html(xml_text(embargo))
    embargo.time <- as.integer(xml_text(xml_find_all(test,"//num")))
    embargo.timeframe <- xml_text(xml_find_all(test,"//period"))} else {
    embargo.time <- NA
    embargo.timeframe <- NA
  }

  tibble(ISSN,embargo.time,embargo.timeframe)
}
sherpa.romeo("1095-9203")
