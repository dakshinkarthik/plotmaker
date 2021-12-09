nubc <- read_sav("./data/nubc2021.sav")
nubc$mc.reside %>% attr('labels')

install.packages('haven')
library(haven)
install.packages("devtools")
install.packages("Rtools")
devtools::install_github("martinctc/surveytoolbox")
