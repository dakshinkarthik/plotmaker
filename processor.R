
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
 
 qType.selector <- function(qval, new.dat){
   
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
       mx_graph_proc(qval,new.dat) 
       mx_table_proc(qval,new.dat)
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
         mc.yn_graph_proc(qval, new.dat)
         mc.yn_table_proc(qval, new.dat)
       }
       else{
         # print("3")
         mc_graph_proc(qval, new.dat)
         mc_table_proc(qval, new.dat)
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
         mx.tri_graph_proc(qval,new.dat)
         mx.tri_table_proc(qval,new.dat)
       }else{
         # print("5")
         mx_graph_proc(qval,new.dat)
         mx_table_proc(qval,new.dat)
       }
     }
     # Checks for rk question type
     else if(unlist(gregexpr(pattern = 'rk', rc_list[1])) != -1){
       # print("6")
       rk_graph_proc(qval,new.dat)
       rk_table_proc(qval,new.dat)
     }
     # Checks for ms question type
     else if(unlist(gregexpr(pattern = 'ms', rc_list[1])) != -1){
       # print("7")
       ms_graph_proc(qval,new.dat)
       ms_table_proc(qval,new.dat)
     }
     # Checks for cs question types
     else if(unlist(gregexpr(pattern = 'cs', rc_list[1])) != -1){
       # print("8")
       cs_graph_proc(qval,new.dat)
       cs_table_proc(qval,new.dat)
     }
   }
 }
 
 data.process <- function(qList, new.dat){
   
   cnames <- colnames(data.ok)
   
   for (i in qList) {
     
   }
   
 }