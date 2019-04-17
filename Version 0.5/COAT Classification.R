require(tidyverse)


COAT_Callculation <- function(data) {
  licence.open <- c("CC BY","CC BY-SA","http://creativecommons.org/licenses/by/4.0/","http://creativecommons.org/licenses/by/4.0","https://creativecommons.org/licenses/by/4.0/","https://creativecommons.org/licenses/by-sa/4.0/","https://creativecommons.org/licenses/by-sa/4.0/"," http://creativecommons.org/licenses/by/3.0/")
  licence.free <- c("CC BY-NC","CC BY-NC-SA","CC BY-ND","CC BY NC-ND","http://creativecommons.org/licenses/by-nc-sa/4.0/","http://creativecommons.org/licenses/by-nc-nd/4.0/","http://creativecommons.org/licenses/by-nc-nd/3.0/")   
  coat.place <- 4
  coat.licence <- 4
  coat.version <- 4
  coat.embargo <- 4
  coat.conditions <- 4
  ##### Auswertung
  if (!(is.na(data$doi.hosttype))) {
    if (data$doi.hosttype =="publisher") {
      coat.place <- 1
      coat.version <- 1
      coat.embargo <- 1
      coat.conditions <- 3
    }
    if (data$doi.hosttype =="repository") coat.place <- 2
  }
  if (!(is.na(data$doi.oaversion))) {
    if (data$doi.oaversion == "publishedVersion") coat.version <- 1
    if (data$doi.oaversion == "acceptedVersion") coat.version <- 2
    if (data$doi.oaversion == "submittedVersion") coat.version <- 3
  }
  if (!(is.na(data$DOAJ.result))) {
    if (!is.na(data$DOAJ.result)) {
      if (data$DOAJ.result) {
        coat.place <-  1
        coat.version <-  1
        coat.embargo  <- 1 }
    }}
  if (!(is.na(data$in.openapc))) {
    if (data$in.openapc) {
      coat.place <-  1
      coat.version <-  1
      if (data$hybrid) coat.conditions <-  3 else coat.conditions <-  2
    }
  }
  if (!is.null(data$ammount)) {
  if (!is.na(data$ammount)) {
    if (data$ammount == 0) coat.conditions <- 1
  }}
  
  # identification of licence does not work correctly 
  if (data$lic.url %in% licence.open) coat.licence = 1
  if (data$lic.url %in% licence.free) coat.licence = 2
  if (data$DOAJ.licence %in% licence.open) coat.licence  <- 1
  if (data$DOAJ.licence %in% licence.free) coat.licence  <- 2
  
  tibble(doi = data$doi,year= data$year,coat.place,coat.licence,coat.version,coat.embargo,coat.conditions)
}


# Require a table with the following informations
# DOI
# 
result <- read_csv("Data/OA-Information.csv")

coat.result <- tibble(doi = 0,year =0, coat.place= 0,coat.licence= 0,coat.version= 0,coat.embargo= 0,coat.conditions= 0)
for (i in 1:nrow(result)) {
  coat.result <- rbind (coat.result,COAT_Callculation(result[i,]))
}
coat.result <- coat.result[-1,] 
write_csv(coat.result,"Data/COAT.csv")
