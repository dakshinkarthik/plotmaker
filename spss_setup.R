nubc <- read_sav("./data/nubc2021_v4.sav")
nubc$mc.reside %>% attr('label')

nubc$rk.QN98complete


library(haven)


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

nubc$mx.QN104_3 %>% attr('label')

attr(nubc$mx.QN104_1, 'label') <- "How concerned are you with the following issues in your first year at UBC? - Accessing services for my disability/on-going medical condition"
attr(nubc$mx.QN104_2, 'label') <- "How concerned are you with the following issues in your first year at UBC? - Getting into my first-choice program"
attr(nubc$mx.QN104_3, 'label') <- "How concerned are you with the following issues in your first year at UBC? - Being able to maintain my Grade Point Average (GPA)"
attr(nubc$mx.QN104_4, 'label') <- "How concerned are you with the following issues in your first year at UBC? - Receiving the career and academic advice I need"
attr(nubc$mx.QN104_7, 'label') <- "How concerned are you with the following issues in your first year at UBC? - Discovering/confirming if UBC is the right place for me"
attr(nubc$mx.QN104_8, 'label') <- "How concerned are you with the following issues in your first year at UBC? - Travelling to Canada in time for the start of term"

write_sav(nubc, "./data/nubc2021_v5.sav")
nubc <- read_sav("./data/nubc2021_v5.sav")
nubc$rk.QN98complete
