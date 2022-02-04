# COAT joining
# TODO simply output with the fields
# - DOI
# - Year
# - Insitution (if provided in input)
# - Country (if provided in input)
# - Type (from CrossRef)
# - Licence URL
# - Licence Source
# - OA Version *
# - Version Source *
# - Costs
# - Cost Source
# - is.hybrid
# - is.hybrid.source
# - Embargo time (month)
# - Publication date
# - Embargo Date
# - Embargo Source
# - Identifier (as URL) * 
# - Link to OA Version
# - Place of OA
# - Place of OA Source
# - Timestamp
# - Year
# - Institution
# - Country
# - TODO journal.in.doaj
# Note: Output will be long data

require(plyr)
require(tidyverse)

input.data <- read_csv("Data/inputdata.csv")
cr.result <- read_csv("Data/cr.result.csv")
doaj.result <- read_csv("Data/doaj-result.csv")
openapc.result <- read_csv("Data/openapc-result.csv")
oadoi.result <- read_csv("Data/oadoi-result.csv")
version.from <- c("draft", "submittedVersion","acceptedVersion", "publishedVersion", "updatedVersion", "PublisherVersion")
version.to <- c("info:eu-repo/semantics/draft","info:eu-repo/semantics/submittedVersion","info:eu-repo/semantics/acceptedVersion", "info:eu-repo/semantics/publishedVersion","info:eu-repo/semantics/updatedVersion", "info:eu-repo/semantics/publishedVersion")

licence.from <- c("CC BY","CC BY-NC","CC BY-ND","CC BY-NC-ND")
licence.to <- c("https://creativecommons.org/licenses/by/4.0/","https://creativecommons.org/licenses/by-nc/4.0","https://creativecommons.org/licenses/by-nd/4.0/","https://creativecommons.org/licenses/by-nc-nd/4.0/")
# Joining all informations is one table

result <- NA
result <- left_join(input.data, cr.result, by= c("DOI" = "doi"))
result <- left_join(result,openapc.result, by = (c("DOI" = "doi.input")))
result <- left_join(result, oadoi.result, by =c ("DOI" = "doi.input"))

result <- left_join(result,doaj.result, by = "issn")
colnames(doaj.result) <- c("issn2", "DOAJ.result2","DOAJ.license2","oa.fee2" )
result <- left_join(result,doaj.result, by ="issn2")

result$DOAJ.licence <- mapvalues(result$DOAJ.licence,licence.from,licence.to)

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
  amount > 0 ~ amount))
result <- result %>% mutate(APC.source = case_when(
  !(is.na(oa.fee))  ~ "DOAJ",
  !(is.na(oa.fee2)) ~ "DOAJ", 
  amount > 0 ~ "OpenAPC"))
result <- result %>% mutate(is.hybrid = case_when(
  !(is.na(oa.fee))  ~ F,
  !(is.na(oa.fee2)) ~ F, 
  amount > 0 ~ hybrid))
result <- result %>% mutate(is.hybrid.source = case_when(
  !(is.na(oa.fee))  ~ "DOAJ",
  !(is.na(oa.fee2)) ~ "DOAJ", 
  amount > 0 ~ "OpenAPC"))
result <- result %>% mutate(version = case_when(
  DOAJ.result ~ "PublisherVersion",
  !(is.na(doi.oaversion))  ~ doi.oaversion))

result <- result %>% mutate(version.source = case_when(
  DOAJ.result ~ "DOAJ",
  !(is.na(doi.oaversion))  ~ "unpaywall"))
result <- result %>% mutate(oa.place.source = case_when(
  !(is.na(doi.hosttype))  ~ "unpaywall"))
result <- result %>% mutate(embargo.time = case_when(
  lic.url != "protected" ~ 0,
  in.openapc ~ 0,
  DOAJ.result ~ 0,
  T ~ 999
)) 
result <- result %>% mutate(embargo.source = case_when(
  lic.url != "protected" ~ Licence.source,
  in.openapc ~ "OpenAPC",
  DOAJ.result ~ "DOAJ",
  T ~ ""
))

result <- result %>% mutate(DOAJ.result = case_when(
  is.na(DOAJ.result) ~ F,
  DOAJ.result ~ T,
  T ~ F
))

result <- result %>% mutate(hybrid = case_when(
  # !is.na(hybrid) ~ hybrid,
  hybrid ~ hybrid,
  (!is.na(lic.url) &  !DOAJ.result)  ~ T,
  T ~ F
))


result$publication.date <- NA
result$embargo.date <- NA
result.names <- c("identifier","year","institution","country", "licence.url","licence.source","version","version.source","APC","APC.source","embargo.time","publication.data","embargo.date","embargo.source","oaversion.link","oa.place","oa.place.source","timestamp")
result.c <- result %>% select(doi, Year, Institution, Country,Licence,Licence.source,doi.oaversion,version.source,APC, APC.source, embargo.time,publication.date,embargo.date,embargo.source, doi.oaurl,doi.hosttype,doi.hosttype,oa.place.source) %>% mutate (timestamp = Sys.time())
colnames(result.c) <- result.names
write_csv(result.c,"Data/oa-report-normalized.csv")
result$version %>% unique()
