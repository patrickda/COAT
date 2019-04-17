# COAT joining
# TODO simply output with the fields
# - Licence URL
# - Licence Source
# - OA Version *
# - Version Source *
# - Costs
# - Cost Source
# - Embargor time (month)
# - Publication date
# - Embargo Date
# - Embargor Source
# - Identifier (as URL) * 
# - Lik to OA Version
# - Place of OA
# - Place of OA Source
# - Timestemp
# - Year
# - Insitution
# - Country
# Note: Output will be long data
require(plyr)
require(tidyverse)


input.data <- read_csv("Data/inputdata.csv")
cr.result <- read_csv("Data/cr.result.csv")
doaj.result <- read_csv("Data/doaj-result.csv")
openapc.result <- read_csv("Data/openapc-result.csv")
oadoi.result <- read_csv("Data/oadoi-result.csv")
version.from <- c("submittedVersion", "publishedVersion", "acceptedVersion")
#TODO Drive vocabulary
version.to <- c("")

licence.from <- c("CC BY","CC BY-NC","CC BY-ND","CC BY-NC-ND")
licence.to <- c("https://creativecommons.org/licenses/by/4.0/","https://creativecommons.org/licenses/by-nc/4.0","https://creativecommons.org/licenses/by-nd/4.0/","https://creativecommons.org/licenses/by-nc-nd/4.0/")
# Joining all informations is one table 
result <- left_join(cr.result,openapc.result, by = (c("doi" = "doi.input")))
result <- left_join(result, oadoi.result, by =c ("doi" = "doi.input"))
result <- left_join(result,doaj.result, by = "issn")
colnames(doaj.result) <- c("issn2", "DOAJ.result2","DOAJ.license2","oa.fee2" )
result <- left_join(result,doaj.result, by ="issn2")
result <- left_join(result, input.data, by = c("doi" = "DOI"))
result$DOAJ.licence <- mapvalues(result$DOAJ.licence,licence.from,licence.to)
str(result)
# Creating a consoludated result table,"
# result.c <- result[c("doi","issn","Year","doi.oaversion", "doi.hosttype","doi.oaurl","DOAJ.result")] %>% mutate (oa.version.source="Unpaywall")
# result.c.names <- c("DOI","ISSN","YEAR","OA:Version","OA.Version.source","Place.of.oa","place.of.oa.source")
# Write OA Data file
str(result)

result <- result %>% mutate(DOAJ.result = DOAJ.result | DOAJ.result2) %>% select(-DOAJ.result2)
licencheck <- function (licurl, doaj.licence) {
  if (licurl == "protected") {
    if (is.na(doaj.licence)) {
      return(NA)
    } else {
        return(doaj.licence) }
    } else 
          {return(licurl) } 
    
}

result <- result %>% mutate(Licence = case_when(
  lic.url == "protected" & is.na(DOAJ.licence) ~ "",
  lic.url == "protected" & !is.na(DOAJ.licence) ~ DOAJ.licence, 
  is.na(lic.url) ~ "",
  lic.url != "protected" ~ lic.url))

result <- result %>% mutate(Licence.source = case_when(
  lic.url == "protected" & is.na(DOAJ.licence) ~ "none",
  lic.url == "protected" & !is.na(DOAJ.licence) ~ "DOAJ", 
  is.na(lic.url) ~ "none",
  lic.url != "protected" ~ "CrossRef"))
result <- result %>% mutate(APC = case_when(
   !(is.na(oa.fee))  ~ oa.fee,
   !(is.na(oa.fee2)) ~ oa.fee2, 
  ammount > 0 ~ ammount))
result <- result %>% mutate(APC.source = case_when(
  !(is.na(oa.fee))  ~ "DOAJ",
  !(is.na(oa.fee2)) ~ "DOAJ", 
  ammount > 0 ~ "OpenAPC"))
result <- result %>% mutate(version = case_when(
  DOAJ.result ~ "PublisherVersion",
  !(is.na(doi.oaversion))  ~ doi.oaversion))

result <- result %>% mutate(version.source = case_when(
  DOAJ.result ~ "DOAJ",
  !(is.na(doi.oaversion))  ~ "unpaywall"))
result <- result %>% mutate(oa.place.source = case_when(
  !(is.na(doi.hosttype))  ~ "unpaywall"))
result$embargo.time <- NA
result$publication.date <- NA
result$embargo.date <- NA
result$embargo.source <- NA
result.names <- c("identifier","year","insitution","country", "licence.url","licence.source","version","version.source","APC","APC.source","embargo.time","publication.data","embargo.date","embargo.source","oaversion.link","oa.place","oa.place.source","timestemp")
result.c <- result %>% select(doi, Year, Insitution, Country,Licence,Licence.source,doi.oaversion,version.source,APC, APC.source, embargo.time,publication.date,embargo.date,embargo.source, doi.oaurl,doi.hosttype,doi.hosttype,oa.place.source) %>% mutate (timestemp = Sys.time())
colnames(result.c) <- result.names
write_csv(result.c,"Data/oa-report-normalized.csv")
result$version %>% unique()
