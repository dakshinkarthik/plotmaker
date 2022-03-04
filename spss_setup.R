nubc <- read_sav("./data/nubc2021_v3.sav")
nubc$mc.reside %>% attr('label')

install.packages('haven')
library(haven)
install.packages("devtools")
install.packages("Rtools")
devtools::install_github("martinctc/surveytoolbox")

nubcd <- data.frame(nubc)

i <- 0
for (stu in nubc$ExternalReference) {
  i <- i + 1
  
  nubc$rk.QN98complete[i] <- nubc$rk.QN98_1[i]+nubc$rk.QN98_2[i]+nubc$rk.QN98_3[i]+nubc$rk.QN98_4[i]+
     nubc$rk.QN98_5[i]+nubc$rk.QN98_6[i]+nubc$rk.QN98_7[i]
  # if(!is.na(nubc$rk.QN98_1[i]+0)){
  #   if(!is.na(nubc$rk.QN98_2[i]+0)){
  #     if(!is.na(nubc$rk.QN98_3[i]+0)){
  #       if(!is.na(nubc$rk.QN98_4[i]+0)){
  #         if(!is.na(nubc$rk.QN98_5[i]+0)){
  #           if(!is.na(nubc$rk.QN98_6[i]+0)){
  #             if(!is.na(nubc$rk.QN98_7[i]+0)){
  #               nubc$rk.QN98complete[i] <- 1
  #             }
  #             else
  #               nubc$rk.QN98complete[i] <- 0
  #           }
  #           else
  #             nubc$rk.QN98complete[i] <- 0
  #         }
  #         else
  #           nubc$rk.QN98complete[i] <- 0
  #       }
  #       else
  #         nubc$rk.QN98complete[i] <- 0
  #     }
  #     else
  #       nubc$rk.QN98complete[i] <- 0
  #   }
  #   else
  #     nubc$rk.QN98complete[i] <- 0
  # }
  # else
  #   nubc$rk.QN98complete[i] <- 0
}

nubc$rk.QN98complete

write_sav(nubc, "./data/nubc2021_v4.sav")
nubc <- read_sav("./data/nubc2021com.sav")
