library(haven)

nubcs <- read_sav("./data/nubc2021.sav")
nubcc <- read.csv("R code/nubc.csv")

nubcs <- nubcs %>% select(-campusName)


class(nubc.s) 

nubc2021joined <- sqldf("
SELECT ns.*, nc.campusName
FROM nubcs ns
LEFT JOIN nubcc nc ON ns.ExternalReference = nc.`ï..ExternalReference`
")

data.ok <- nubc2021joined[which(nubc2021joined$directtransfer == "DIRECT-ENTRY" 
                                & nubc2021joined$campusName == "Okanagan"),]

write_sav(nubc2021joined, "nubc2021joined.sav")

table(nubcs$campusName)

table(nubcc$campusName)





data.ok <-
  nubc2021joined %>%
  filter(campusName == "Okanagan")







str(nubc.c)

