source("plotmaker.R")
source("data_process.R")
library(knitr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(tidyverse)
library(extrafont)
library(pander)
library(haven)
library(flextable)
# **************************************************************************************
# ```````````````````````````````PARAMETERS`````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````
# **************************************************************************************

# Options: "ALL","Okanagan","Vancouver"
CAMPUS <- "Okanagan"
# Options: "ALL","Direct-Transfer","Transfer"
ENTRY_TYPE <- "Direct-Entry"
# Options: "ALL","First Nations","Metis","Inuit","Aboriginal","Arab","Black","Chinese","Filipino","Japanese","Korean","Americas","South Asian","Southeast Asian","West Asian","White","Other","No Answer"
ETHNICITY <- "ALL"
# Options: "ALL","Female","Male","Non-Binary","No Answer"
GENDER <- "ALL"
# Options: "ALL","Cis","Trans","No Answer"
GENDER_ORIENTATION <- "ALL"
# Options: "ALL","Homosexual","Heterosexual","Bisexual","Asexual","Queer","Questioning","Other","No Answer"
SEXUALITY <- "ALL"

# ***************************************************************************************
# ```````````````````````````````DATA READS AND FILTERIMG````````````````````````````````
# ```````````````````````````````````````````````````````````````````````````````````````
# ***************************************************************************************

param_list <- c(ENTRY_TYPE,GENDER_ORIENTATION,SEXUALITY,GENDER,ETHNICITY,CAMPUS)
# param_list <- c("Direct-Transfer","ALL","ALL","ALL","ALL","Okanagan")
nubc2021joined <- read_sav("data/nubc2021_v5.sav")
data.ok <- nubc2021joined[which(nubc2021joined$directtransfer == "DIRECT-ENTRY" & nubc2021joined$campusName == "Okanagan"),]
i.dat <- data.ok[which(data.ok$isi == "ISI"),]
d.dat <- data.ok[which(data.ok$isi == "Domestic"),]

#CAMPUS
if(CAMPUS == "Okanagan"){
  data.ok <- nubc2021joined[which(nubc2021joined$campusName == "Okanagan"),]
}else if(CAMPUS == "Vancouver"){
  data.ok <- nubc2021joined[which(nubc2021joined$campusName == "Vancouver"),]
}else{
  data.ok <- nubc2021joined
}
#ENTRY_TYPE
if(ENTRY_TYPE == "Direct-Entry"){
  data.ok <- data.ok[which(data.ok$directtransfer == "DIRECT-ENTRY"),]
}else if(ENTRY_TYPE == "Transfer"){
  data.ok <- data.ok[which(data.ok$directtransfer == "TRANSFER"),]
}
#ETHNICITY
if(ETHNICITY == "First Nations"){
  data.ok <- data.ok[which(data.ok$ms.QN48_1 == 1),]
}else if(ETHNICITY == "Metis"){
  data.ok <- data.ok[which(data.ok$ms.QN48_17 == 1),]
}else if(ETHNICITY == "Inuit"){
  data.ok <- data.ok[which(data.ok$ms.QN48_18 == 1),]
}else if(ETHNICITY == "Aboriginal"){
  data.ok <- data.ok[which(data.ok$ms.QN48_2 == 1),]
}else if(ETHNICITY == "Arab"){
  data.ok <- data.ok[which(data.ok$ms.QN48_3 == 1),]
}else if(ETHNICITY == "Black"){
  data.ok <- data.ok[which(data.ok$ms.QN48_4 == 1),]
}else if(ETHNICITY == "Chinese"){
  data.ok <- data.ok[which(data.ok$ms.QN48_5 == 1),]
}else if(ETHNICITY == "Filipino"){
  data.ok <- data.ok[which(data.ok$ms.QN48_6 == 1),]
}else if(ETHNICITY == "Japanese"){
  data.ok <- data.ok[which(data.ok$ms.QN48_7 == 1),]
}else if(ETHNICITY == "Korean"){
  data.ok <- data.ok[which(data.ok$ms.QN48_8 == 1),]
}else if(ETHNICITY == "Americas"){
  data.ok <- data.ok[which(data.ok$ms.QN48_9 == 1),]
}else if(ETHNICITY == "South Asian"){
  data.ok <- data.ok[which(data.ok$ms.QN48_10 == 1),]
}else if(ETHNICITY == "Southeast Asian"){
  data.ok <- data.ok[which(data.ok$ms.QN48_11 == 1),]
}else if(ETHNICITY == "West Asian"){
  data.ok <- data.ok[which(data.ok$ms.QN48_13 == 1),]
}else if(ETHNICITY == "White"){
  data.ok <- data.ok[which(data.ok$ms.QN48_14 == 1),]
}else if(ETHNICITY == "Other"){
  data.ok <- data.ok[which(data.ok$ms.QN48_15 == 1),]
}else if(ETHNICITY == "No Answer"){
  data.ok <- data.ok[which(data.ok$ms.QN48_16 == 1),]
}
#GENDER
if(GENDER == "Female"){
  data.ok <- data.ok[which(data.ok$mc.QN56 == 1),]
}else if(GENDER == "Male"){
  data.ok <- data.ok[which(data.ok$mc.QN56 == 2),]
}else if(GENDER == "Non-Binary"){
  data.ok <- data.ok[which(data.ok$mc.QN56 == 3),]
}else if(GENDER == "No Answer"){
  data.ok <- data.ok[which(data.ok$mc.QN56 == 999),]
}
#GENDER_ORIENTATION
if(GENDER_ORIENTATION == "Cis"){
  data.ok <- data.ok[which(data.ok$mc.QN57 == 1),]
}else if(GENDER_ORIENTATION == "Trans"){
  data.ok <- data.ok[which(data.ok$mc.QN57 == 2),]
}else if(GENDER_ORIENTATION == "No Answer"){
  data.ok <- data.ok[which(data.ok$mc.QN57 == 999),]
}
# SEXUALITY
# Options: "ALL","HETEROSEXUAL","HOMOSEXUAL","BISEXUAL","ASEXUAL","QUEER","QUESTIONING","OTHER","NO ANSWER"
if(SEXUALITY == "Heterosexual"){
  data.ok <- data.ok[which(data.ok$mc.QN58 == 1),]
}else if(SEXUALITY == "Homosexual"){
  data.ok <- data.ok[which(data.ok$mc.QN58 == 2),]
}else if(SEXUALITY == "Bisexual"){
  data.ok <- data.ok[which(data.ok$mc.QN58 == 3),]
}else if(SEXUALITY == "Asexual"){
  data.ok <- data.ok[which(data.ok$mc.QN58 == 4),]
}else if(SEXUALITY == "Queer"){
  data.ok <- data.ok[which(data.ok$mc.QN58 == 5),]
}else if(SEXUALITY == "Questioning"){
  data.ok <- data.ok[which(data.ok$mc.QN58 == 6),]
}else if(SEXUALITY == "Other"){
  data.ok <- data.ok[which(data.ok$mc.QN58 == 9),]
}else if(SEXUALITY == "No Answer"){
  data.ok <- data.ok[which(data.ok$mc.QN58 == 999),]
}

i.dat <- data.ok[which(data.ok$isi == "ISI"),]
d.dat <- data.ok[which(data.ok$isi == "Domestic"),]

# ***************************************************************************************************
# ````````````````````````````````DATA PROCESSING````````````````````````````````````````````````````
# ```````````````````````````````````````````````````````````````````````````````````````````````````
# ***************************************************************************************************

processed_graph_dataList <- list()
processed_table_dataList <- list()

q_list <- c("reside",
         "housing",
         "housingCountry",
         "QN28",
         "QN100",
         "QN104",
         "QN105",
         "mindset",
         "interSupport",
         "commSatisfy",
         "commFreq",
         "commAgree",
         "QN30",
         "QN94",
         "acdEssential",
         "acdEssentialcrs",
         "QN980",
         "QN98",
         "QN111",
         "commuteFreq",
         "QN44",
         "QN45",
         "commuteerrand",
         "wbFamdoc",
         "wbMntsprt",
         "wbNavigate",
         "healthResource",
         "QN65",
         "QN34",
         "QN37",
         "QN40",
         "QN41",
         "QN35",
         "QN36",
         "covidFinance",
         "covidTuition",
         "spApply",
         "spRestriction",
         "QN48",
         "QN50",
         "QN52",
         "QN53",
         "QN54",
         "QN55",
         "QN66",
         "QN67",
         "QN56",
         "QN57",
         "QN58",
         "QN59",
         "daRsrc")
 
 qType.selector <- function(qval, new.dat, d.dat, i.dat){
   
   # Column names to read data
   cnames <- colnames(new.dat) # Gets column names(question IDs) from the dataset
   rc_list <- rc_list.get(qval, new.dat) # Subset of field names based on input parameters
   
   # Response levels which is used for determining the appropriate further function call
   resp <- names(get(rc_list[1],new.dat) %>% attr('labels')) 
   
   if(length(rc_list) == 1){ # To check for mc questions (Normal bar graph)
     # The following if() tests for singular mx type question (stacked bar graph) based on responses
     if(unlist(gregexpr(pattern = 'agree', resp[1])) != -1|| 
        unlist(gregexpr(pattern = 'satisfied', resp[1])) != -1||
        unlist(gregexpr(pattern = 'concerned', resp[1])) != -1||
        unlist(gregexpr(pattern = 'impact', resp[1])) != -1){
       # print("1")
       # Each qID from the markdown file gets a single call to an appropriate graph function and a table function
       # mx_graph_proc(qval,new.dat) 
       # mx_table_proc(qval,new.dat)
       return(list(mx_graph_proc(qval,d.dat),mx_table_proc(qval,d.dat),
                   mx_graph_proc(qval,i.dat),mx_table_proc(qval,i.dat)))
     }
     else{
       chk <- 0 # Local check variable for 'Yes/No' response levels
       # mc() questions with Yes/No responses have slightly different tables than a regular mc question.
       for (j in 1:length(resp)) {
         # This if statement checks for 'Yes' in the response levels
         if(unlist(gregexpr(pattern = 'Yes', resp[j])) != -1){
           chk <- 1
           break
         }
       }
       # if chk == 1 then it has 'Yes/No' in the response levels
       if(chk == 1){
         # print("2")
         # mc.yn_graph_proc(qval, new.dat)
         # mc.yn_table_proc(qval, new.dat)
         return(list(mc.yn_graph_proc(qval, new.dat),mc.yn_table_proc(qval, new.dat)))
       }
       else{
         # print("3")
         # mc_graph_proc(qval, new.dat)
         # mc_table_proc(qval, new.dat)
         return(list(mc_graph_proc(qval, new.dat),mc_table_proc(qval, new.dat)))
       }
     }
   }
   # Other question types have more than 1 sub question
   else{
     # Check for mx question type based on question identifiers
     if(unlist(gregexpr(pattern = 'mx', rc_list[1])) != -1){
       # mx question type checked based on response levels.
       # There is an exception in one of the mx questions where the graph was grouped in levels of 3.
       # This question has response levels that do not match the other mx question response levels
       if(unlist(gregexpr(pattern = 'agree', resp[1])) == -1&& 
          unlist(gregexpr(pattern = 'satisfied', resp[1])) == -1&&
          unlist(gregexpr(pattern = 'concerned', resp[1])) == -1&&
          unlist(gregexpr(pattern = 'impact', resp[1])) == -1){
         # print("4")
         # This function call and table call is for mx questions with only 3 response levels that are different 
         # from the usual mx response levels
         # mx.tri_graph_proc(qval,new.dat)
         # mx.tri_table_proc(qval,new.dat)
         return(list(mx.tri_graph_proc(qval,d.dat),mx.tri_table_proc(qval,d.dat),
                     mx.tri_graph_proc(qval,i.dat),mx.tri_table_proc(qval,i.dat)))
       }else{
         # print("5")
         # mx_graph_proc(qval,new.dat)
         # mx_table_proc(qval,new.dat)
         return(list(mx_graph_proc(qval,d.dat),mx_table_proc(qval,d.dat),
                     mx_graph_proc(qval,i.dat),mx_table_proc(qval,i.dat)))
       }
     }
     # Checks for rk question type
     else if(unlist(gregexpr(pattern = 'rk', rc_list[1])) != -1){
       # print("6")
       # rk_graph_proc(qval,new.dat)
       # rk_table_proc(qval,new.dat)
       return(list(rk_graph_proc(qval,d.dat),rk_table_proc(qval,d.dat),
                   rk_graph_proc(qval,i.dat),rk_table_proc(qval,i.dat)))
     }
     # Checks for ms question type
     else if(unlist(gregexpr(pattern = 'ms', rc_list[1])) != -1){
       # print("7")
       # ms_graph_proc(qval,new.dat)
       # ms_table_proc(qval,new.dat)
       return(list(ms_graph_proc(qval,new.dat),ms_table_proc(qval,new.dat)))
     }
     # Checks for cs question types
     else if(unlist(gregexpr(pattern = 'cs', rc_list[1])) != -1){
       # print("8")
       # cs_graph_proc(qval,new.dat)
       # cs_table_proc(qval,new.dat)
       return(list(cs_graph_proc(qval,new.dat),cs_table_proc(qval,new.dat)))
     }
   }
 }
 
 
 
 data.process <- function(qList, new.dat){
   
   c1 <- 1
   c2 <- 1
   
   cnames <- colnames(data.ok)
   
   processed_graph_dataList <- list()
   processed_table_dataList <- list()
   
   for (i in qList) {
     current_list <- qType.selector(i, data.ok, d.dat, i.dat)

     if(length(current_list) == 2){
       processed_graph_dataList[[c1]] <- current_list[[1]]
       processed_table_dataList[[c2]] <- current_list[[2]]
       c1 <- c1 + 1
       c2 <- c2 + 1
     }
     else{
       processed_graph_dataList[[c1]] <- current_list[[1]]
       processed_table_dataList[[c2]] <- current_list[[2]]
       c1 <- c1 + 1
       c2 <- c2 + 1
       processed_graph_dataList[[c1]] <- current_list[[3]]
       processed_table_dataList[[c2]] <- current_list[[4]]
       c1 <- c1 + 1
       c2 <- c2 + 1
     }
   }
   
   return(list(processed_graph_dataList, processed_table_dataList))
   
 }
 
 processedData <- NULL
 processedData <- data.process(q_list, data.ok)
 
 processed_graph_dataList <- processedData[[1]]
 processed_table_dataList <- processedData[[2]]
 
 save(processed_graph_dataList, processed_table_dataList, file = "processedData.RData")
 
 # load("processedData.R")
 