library(haven)
library(dplyr)
library(tidyr)
nubc2022 <- read_sav("data/nubc2022.sav")
str(nubc2022)
colnames(nubc2022)
colnames(data.ok)
rc_list.get("dmgDismh", nubc2022)
rc_list.get("QN104", data.ok)

nubc2022$dmg

## ADDING SAMPLE ISI DATA
nubc2022 <- cbind(nubc2022,ISI_STATUS = c(rep("Domestic",579),rep("ISI",578)))


## TO CHANGE QUESTION IDENTIFIERS FOR THE UNIQUE 'mx' question type
nubc2022 <- cbind(nubc2022,
                  m2x.dmgDismh_sev = nubc2022$n22_mx.dmgDismh_sev, m2x.dmgDismh_frq = nubc2022$n22_mx.dmgDismh_frq,
                  m2x.dmgDislrn_sev = nubc2022$n22_mx.dmgDislrn_sev, m2x.dmgDislrn_frq = nubc2022$n22_mx.dmgDislrn_frq,
                  m2x.dmgDisdvp_sev = nubc2022$n22_mx.dmgDisdvp_sev, m2x.dmgDisdvp_frq = nubc2022$n22_mx.dmgDisdvp_frq,
                  m2x.dmgDismem_sev = nubc2022$n22_mx.dmgDismem_sev, m2x.dmgDismem_frq = nubc2022$n22_mx.dmgDismem_frq,
                  m2x.dmgDispn_sev = nubc2022$n22_mx.dmgDispn_sev, m2x.dmgDispn_frq = nubc2022$n22_mx.dmgDispn_frq,
                  m2x.dmgDismob_sev = nubc2022$n22_mx.dmgDismob_sev, m2x.dmgDismob_frq = nubc2022$n22_mx.dmgDismob_frq,
                  m2x.dmgDisdex_sev = nubc2022$n22_mx.dmgDisdex_sev, m2x.dmgDisdex_frq = nubc2022$n22_mx.dmgDisdex_frq,
                  m2x.dmgDisflx_sev = nubc2022$n22_mx.dmgDisflx_sev, m2x.dmgDisflx_frq = nubc2022$n22_mx.dmgDisflx_frq,
                  m2x.dmgDisvis_sev = nubc2022$n22_mx.dmgDisvis_sev, m2x.dmgDisvis_frq = nubc2022$n22_mx.dmgDisvis_frq,
                  m2x.dmgDishrn_sev = nubc2022$n22_mx.dmgDishrn_sev, m2x.dmgDishrn_frq = nubc2022$n22_mx.dmgDishrn_frq,
                  m2x.dmgDisspk_sev = nubc2022$n22_mx.dmgDisspk_sev, m2x.dmgDisspk_frq = nubc2022$n22_mx.dmgDisspk_frq)

nubc2022 <- nubc2022[,!names(nubc2022) %in% c("n22_mx.dmgDismh_sev", "n22_mx.dmgDismh_frq",
                                              "n22_mx.dmgDislrn_sev", "n22_mx.dmgDislrn_frq",
                                              "n22_mx.dmgDisdvp_sev", "n22_mx.dmgDisdvp_frq",
                                              "n22_mx.dmgDismem_sev", "n22_mx.dmgDismem_frq",
                                              "n22_mx.dmgDispn_sev", "n22_mx.dmgDispn_frq",
                                              "n22_mx.dmgDismob_sev", "n22_mx.dmgDismob_frq",
                                              "n22_mx.dmgDisdex_sev", "n22_mx.dmgDisdex_frq",
                                              "n22_mx.dmgDisflx_sev", "n22_mx.dmgDisflx_frq",
                                              "n22_mx.dmgDisvis_sev", "n22_mx.dmgDisvis_frq",
                                              "n22_mx.dmgDishrn_sev", "n22_mx.dmgDishrn_frq",
                                              "n22_mx.dmgDisspk_sev", "n22_mx.dmgDisspk_frq")]

## REMOVING ALL LANGUAGES EXCEPT ENGLISH AND CHANGING IDENTIFIER FROM 'mx' TO 'mc'
l1 <- c()
l2 <- c()
l3 <- c()

for (nm in colnames(nubc2022)){
  if(unlist(gregexpr(pattern = 'dmglngls', nm)) != -1){
    l1 <- c(l1, nm)
  }
}


for (nm in colnames(nubc2022)){
  if(unlist(gregexpr(pattern = 'dmglngread', nm)) != -1){
    l2 <- c(l2, nm)
  }
}

for (nm in colnames(nubc2022)){
  if(unlist(gregexpr(pattern = 'dmglngwrite', nm)) != -1){
    l3 <- c(l3, nm)
  }
}

l1 <- l1[-c(1)]
l2 <- l2[-c(1)]
l3 <- l3[-c(1)]

l4 <- c(l1,l2,l3)

nubc2022 <- cbind(nubc2022, mc.dmglngls = nubc2022$A1_n22_mx.dmglngls, mc.dmglngread = nubc2022$A1_n22_mx.dmglngread,
                  mc.dmglngwrite = nubc2022$A1_n22_mx.dmglngwrite)

nubc2022 <- nubc2022[,!names(nubc2022) %in% l4]
colnames(nubc2022[,!names(nubc2022) %in% l4])



nubc2022 <- nubc2022[,!names(nubc2022) %in% c("A1_n22_mx.dmglngls","A1_n22_mx.dmglngread","A1_n22_mx.dmglngwrite")]



get("n21_mx.ubcBelong_2", nubc2022)

ms("mltryType", nubc2022)

nubc2022$mc.dmglngls
data.ok$mx.QN105_2

mc("dmglngwrite",nubc2022) # RECENT CODE

# ("A1_n22_mx.dmglngls" ,       
 # "A1_n22_mx.dmglngread"      , "A1_n22_mx.dmglngwrite"   ,   "A2_n22_mx.dmglngls"  ,       "A2_n22_mx.dmglngread"     , 
 # "A2_n22_mx.dmglngwrite"  ,    "A3_n22_mx.dmglngls"     ,    "A3_n22_mx.dmglngread"   ,    "A3_n22_mx.dmglngwrite"     ,
 # "A17_n22_mx.dmglngls"    ,    "A17_n22_mx.dmglngread"  ,    "A17_n22_mx.dmglngwrite"  ,   "A18_n22_mx.dmglngls"     ,  
 # "A18_n22_mx.dmglngread"    ,  "A18_n22_mx.dmglngwrite"   ,  "A19_n22_mx.dmglngls"   ,     "A19_n22_mx.dmglngread"     ,
 # "A19_n22_mx.dmglngwrite"  ,   "A20_n22_mx.dmglngls"   ,     "A20_n22_mx.dmglngread" ,     "A20_n22_mx.dmglngwrite"  ,  
 # "A21_n22_mx.dmglngls"    ,    "A21_n22_mx.dmglngread"   ,   "A21_n22_mx.dmglngwrite"  ,   "A22_n22_mx.dmglngls"     ,  
 # "A22_n22_mx.dmglngread"   ,   "A22_n22_mx.dmglngwrite"   ,  "A23_n22_mx.dmglngls"   ,     "A23_n22_mx.dmglngread"  ,   
 # "A23_n22_mx.dmglngwrite"  ,   "A24_n22_mx.dmglngls"     ,   "A24_n22_mx.dmglngread"  ,    "A24_n22_mx.dmglngwrite"   , 
 # "A25_n22_mx.dmglngls"     ,   "A25_n22_mx.dmglngread"   ,   "A25_n22_mx.dmglngwrite" ,    "A26_n22_mx.dmglngls"   ,    
 # "A26_n22_mx.dmglngread"   ,   "A26_n22_mx.dmglngwrite"   ,  "A27_n22_mx.dmglngls"    ,    "A27_n22_mx.dmglngread"   ,  
 # "A27_n22_mx.dmglngwrite"     "A28_n22_mx.dmglngls"        "A28_n22_mx.dmglngread"      "A28_n22_mx.dmglngwrite"    
 # "A29_n22_mx.dmglngls"        "A29_n22_mx.dmglngread"      "A29_n22_mx.dmglngwrite"     "A30_n22_mx.dmglngls"       
 # "A30_n22_mx.dmglngread"      "A30_n22_mx.dmglngwrite"     "A31_n22_mx.dmglngls"        "A31_n22_mx.dmglngread"     
 # "A31_n22_mx.dmglngwrite"     "A32_n22_mx.dmglngls"        "A32_n22_mx.dmglngread"      "A32_n22_mx.dmglngwrite")    
 # "A34_n22_mx.dmglngls"        "A34_n22_mx.dmglngread"      "A34_n22_mx.dmglngwrite"





testMs <- function(qval, new.dat){ # code is similar to the tb_ms() function and shares the same reasoning
  # splitting data by domestic/international
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  
  # print(d.dat$ExternalReference)
  # print(i.dat$ExternalReference)
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  rc_list <- test_rc_eval("ms",rc_list)
  
  print(rc_list)
  
  # computing distinct count
  i.dc <- 0
  d.dc <- 0
  for (stu in i.dat$ExternalReference) { # international distinct count
    print(stu)
    for (qn in rc_list) {
      if(!is.na(get(qn, i.dat)[i.dat$ExternalReference == stu])){
        if((get(qn, i.dat)[i.dat$ExternalReference == stu] + 0) == 1){
          # print((get(qn, i.dat)[i.dat$ExternalReference == stu] + 0))
          i.dc <- i.dc + 1
          break
        }
      }
    }
  }
  
  for (stu in d.dat$ExternalReference) { # domestic distinct count
    for(qn in rc_list){
      if(!is.na(get(qn, d.dat)[d.dat$ExternalReference == stu])){
        if((get(qn, d.dat)[d.dat$ExternalReference == stu] + 0) == 1){
          # print((get(qn, d.dat)[d.dat$ExternalReference == stu] + 0))
          d.dc <- d.dc + 1
          break
        }
      }
    }
  }
  
  # print(i.dc)
  # print(d.dc)
  
  # Variable initialization
  df.list <- list()
  i.df.list <- list()
  d.df.list <- list()
  main.df <- NULL
  prop <- list()
  i.prop <- list()
  d.prop <- list()
  main.prop <- c()
  main.df<- data.frame()
  tex.col <- c()
  label_count <- length(tex.col)
  ld.title <- c()
  axis.q <- c()
  axis.c <- NULL
  
  i <- 1
  j <- 1
  for (qn in rc_list) {
    # Dataframe building ---- dfs are built exactly the same way as it was done in tb_ms()
    ## Domestic fraction
    axis.c <- names(get(qn, new.dat) %>% attr('labels'))
    axis.c <- region.get(axis.c,new.dat)
    if(nrow(table(get(qn, d.dat))) == 0){
      tcv <- matrix(0)
      rownames(tcv) <- c(i)
      tcv <- as.table(tcv)
      colnames(tcv) <- c("")
      tdf <- data.frame(tcv)
      tdf <- data.frame(Var1 = tdf$Var1, Freq = tdf$Freq, Ques = c("Domestic"))
      d.df.list[[i]] <- tdf
      d.df.list[[i]]$Var1 <- c(axis.c)
      main.df <- rbind(main.df,d.df.list[[i]])
    }
    else{
      tdf <- data.frame(table(get(qn, d.dat)), Ques = c("Domestic"))
      d.df.list[[i]] <- tdf
      if(dim(d.df.list[[i]])[1] != 1){
        d.df.list[[i]][2,]$Freq <- round((100*d.df.list[[i]][2,]$Freq/d.dc))
        d.df.list[[i]]$Var1 <- c(axis.c)
        main.df <- rbind(main.df,d.df.list[[i]][2,])
      }
      else{
        if(d.df.list[[i]]$Var1 == 0){
          d.df.list[[i]] <- data.frame(Var1 = c(axis.c), Freq = c(0), Ques = c("Domestic"))
          main.df <- rbind(main.df,d.df.list[[i]])
        }
        else{
          d.df.list[[i]]$Freq <- round((100*d.df.list[[i]]$Freq/d.dc))
          d.df.list[[i]]$Var1 <- c(axis.c)
          main.df <- rbind(main.df,d.df.list[[i]])
        }
      }
    }
    ## International fraction
    if(nrow(table(get(qn, i.dat))) == 0){
      tcv <- matrix(0)
      rownames(tcv) <- c(j)
      tcv <- as.table(tcv)
      colnames(tcv) <- c("")
      tdf <- data.frame(tcv)
      tdf <- data.frame(Var1 = tdf$Var1, Freq = tdf$Freq, Ques = c("International"))
      i.df.list[[j]] <- tdf
      i.df.list[[j]]$Var1 <- c(axis.c)
      main.df <- rbind(main.df,i.df.list[[j]])
    }
    else{
      tdf <- data.frame(table(get(qn, i.dat)), Ques = c("International"))
      i.df.list[[j]] <- tdf
      if(dim(i.df.list[[j]])[1] != 1){
        i.df.list[[j]][2,]$Freq <- round((100*i.df.list[[j]][2,]$Freq/i.dc))
        i.df.list[[j]]$Var1 <- c(axis.c)
        main.df <- rbind(main.df,i.df.list[[j]][2,])
      }
      else{
        if(i.df.list[[j]]$Var1 == 0){
          i.df.list[[j]] <- data.frame(Var1 = c(axis.c), Freq = c(0), Ques = c("International"))
          main.df <- rbind(main.df,i.df.list[[j]])
        }
        else{
          i.df.list[[j]]$Freq <- round((100*i.df.list[[j]]$Freq/i.dc))
          i.df.list[[j]]$Var1 <- c(axis.c)
          main.df <- rbind(main.df,i.df.list[[j]])
        }
      }
    }
    
    i <- i + 1
    j <- j + 1
  }
  
  # to remove response levels where both domestic and international are 0 (empty)
  k <- 1
  nnull <- c()
  for (qn in 1:dim(main.df)[1]) {
    if(k < dim(main.df)[1]){
      if(main.df$Freq[k] == 0 & main.df$Freq[k+1] == 0){ # data in the df is ordered domestic,international,domestic,... by question
        nnull <- c(nnull,k,k+1)
      }
    }
    k <- k + 2
  }
  
  if(!is.null(nnull)){
    main.df <- main.df[-nnull,]
  }
  
  # pasting '%' to the prop data
  for (frq in 1:length(main.df$Freq)){
    main.prop <-  c(main.prop,paste0(main.df$Freq[frq],"%"))
  }
  
  
  # Subtitle building
  subt <- subt_builder(rc_list, new.dat)
  
  plot.bar <- ggplot(data = main.df, aes(x=Freq, y=factor(Var1,levels = rev(unique(Var1))),
                                         fill = factor(Ques,levels = rev(unique(Ques))))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = c("#FFC279","#579C2C"),
                      guide = guide_legend(reverse = TRUE,nrow = 2)) +
    geom_text(data = main.df, label = main.prop,
              position = position_dodge(width = 0.8), size = 60, hjust = -0.1) +
    labs(title = title_builder(param_list),
         subtitle = subt) +
    ubc.theme() +
    theme(legend.position = c(0.85,0.5)) +
    scale_x_continuous(limits = c(0, 2 * max(main.df$Freq))) # setting x-axis bound limits
  
  print(plot.bar)
  # print(main.df)
  # print(main.df$Freq)
}

testMs("dmgLngacd", nubc2022)
testMs("spRestriction", data.ok)

mx("mhMgmt", nubc2022)

test_rc_eval <- function(eval.st,rc_list){ 
  bl <- c()
  for (j in 1:length(rc_list)) {
    if(unlist(gregexpr(pattern = eval.st, rc_list[j])) != -1){
      if(unlist(gregexpr(pattern = "Complete", rc_list[j])) != -1 ||
         unlist(gregexpr(pattern = "complete", rc_list[j])) != -1){
        bl <- c(bl,FALSE)
      }
      else if(unlist(gregexpr(pattern = "Sum", rc_list[j])) != -1 ||
              unlist(gregexpr(pattern = "sum", rc_list[j])) != -1){
        bl <- c(bl,FALSE)
      }
      else if(unlist(gregexpr(pattern = "TEXT", rc_list[j])) != -1 ||
              unlist(gregexpr(pattern = "text", rc_list[j])) != -1){
        bl <- c(bl,FALSE)
      }
      else{
        bl <- c(bl,TRUE)
      }
    }else{
      bl <- c(bl,FALSE)
    }
  }
  return(rc_list[bl])
}

test_rc_eval("ms",rc_list.get("dmgLngacd", nubc2022))

testRk <- function(qval, new.dat){
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval, new.dat)  
  # input dataset is subsetted based on sum/complete field value
  # 28 is a parameter here because there is only one rk question in the dataset that requires 28 as a validating factor
  # Future code can be changed to accommodate a dynamic parameter value
  new.dat <- rc_complete(rc_list, new.dat, 28) 
  rc_list <- test_rc_eval("rk",rc_list) # Checks if all the sub questions selected belong to the qID
  
  ti.tle <- title_builder(param_list)
  # The following if-statements check for domestic/international data and assigns the correct title and color scheme for the graph
  if(new.dat$isi[1] == "Domestic"){
    col <- c("#316C1A", "#4C9C2C", "#61AF41", "#76A464", "#92C180", "#ADD99C", "#BFE7B0")
    ti.tle <- paste("Domestic",ti.tle,sep = " ")
  }
  else if(new.dat$isi[1] == "ISI"){
    col <- c("#A1600A", "#C37918", "#D38622", "#FF940A", "#FFA55D", "#FFB377", "#FFD5A0")
    ti.tle <- paste("International",ti.tle,sep = " ")
  }
  else{
    col <- c("#002145", "#0055B7", "#00A7E1", "#26C7FF", "#5CD5FF", "#85E0FF", "#A2E7FF")
    ti.tle <- ti.tle
  }
  
  # 'Rank' is pasted with the numerical response levels to be on the legend 
  resp <- paste("Rank", rev(names(get(rc_list[1], new.dat) %>% attr('labels'))), sep = " ")
  
  # Variable initialization
  df.list <- list() # Each sub question and its responses are stored in a list of dataframes
  prop <- list() # Each sub question's frequency of response are stored as a list of props
  main.prop <- NULL # The list of props is added together to make one vector of props for all the sub-questions
  main.df<- data.frame() # The list of dataframes is concatenated into one main dataframe
  col <- rev(col) # Order of the color vectors reversed for aesthetic purposes in ggplot
  # Base color codes for the props(for geom_text)
  tex.col.base <- rev(c("white","white","white","black","black","black","black")) 
  # mx questions have differing number of response levels.
  tex.col <- c() # To store the correct number of color codes for prop labels
  label_count <- length(tex.col) # To keep track of number of response levels
  ld.title <- c() # Stores question labels for displaying on axis
  sel <- c()
  i <- 0
  
  # Axis question building and formatting
  for (qn in rc_list) {
    label_count_var <- 0
    i <- i + 1
    # mx question labels have ' - ' in them. This serves as a separator to extract sub-question labels.
    if(unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label'))) != -1){
      ld <- substr(get(qn, new.dat) %>% attr('label'),
                   unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label')))+3,
                   nchar(get(qn, new.dat) %>% attr('label')))
    }
    else{ # in case ' - ' is missing
      ld <- get(qn, new.dat) %>% attr('label')
    }
    
    if(nchar(ld)>63){ # adds new line to labels based on its length
      ld <- paste0(substr(ld,1,sapply(gregexpr(pattern = " ", substr(ld,1,63)),max)), "\n ",
                   substr(ld,sapply(gregexpr(pattern = " ", substr(ld,1,63)),max)+1,nchar(ld)))
    }
    
    ld.title <- c(ld.title, ld)
    
    # Dataframe building for each sub question
    temp.df <- data.frame(rev(table(get(qn, new.dat))), Ques = c(i))
    temp.df <- complete(temp.df, Var1 = factor(c(7:1),levels = c(7:1)), fill = list(Freq = 0, Ques = c(i)))
    df.list[[i]] <- temp.df
    df.list[[i]]$Ques <- ld # Question labels added into the dataframe directly
    
    # Stores the first rank of the sub questions for formatting the ggplot later
    sel <- c(sel,df.list[[i]]$Freq[length(df.list[[i]]$Freq)])
    
    # Geometry text prep
    prop[[i]] <- round(100*df.list[[i]]$Freq/sum(df.list[[i]]$Freq))
    
    # Geom text has a no character if < 5, else '%' is pasted to it 
    for(j in 1:length(prop[[i]])){
      label_count_var <- label_count_var + 1
      if(as.numeric(prop[[i]][j])<5)
        prop[[i]][j] = ''
      else
        prop[[i]][j] = paste0(prop[[i]][j], "%")
    }
    # Number of Color codes for geom text is determined based on the count of prop text
    if(label_count_var == label_count){
      tex.col <- c(tex.col, tex.col.base)
    }
    else{
      tex.col <- c(tex.col, tex.col.base[1:label_count_var])
    }
    
    # Main dataframe and geom text for plotting made by combining data from the list
    main.prop <- c(main.prop, prop[[i]])
    main.df <- rbind(main.df, df.list[[i]])
    
  }
  
  # Subtitle building
  subt <- subt_builder(rc_list, new.dat)
  
  # Subtitle positioning and geom text size
  geom_text_size <- sizer(rc_list)[2] # sizer computes and returns size for geom_text and column width
  c.width <- sizer(rc_list)[1]
  
  leveler <- c()
  # Questions displayed based on decreasing rank 1 scores
  sel <- sort(sel,decreasing = T) # From earlier, this used as a metric to set 'leveler' for ggplot
  track <- c() # to keep track of row index of df.list to avoid repetition of levels
  for (i in 1:length(sel)) {
    for (j in 1:length(df.list)) {
      # leveler is made up of question labels based on the order of first ranks in sel 
      if(sel[i] == df.list[[j]]$Freq[length(df.list[[j]]$Freq)] & !(j %in% track)){
        track <- c(track,j)
        leveler <- c(leveler,df.list[[j]]$Ques[1])
        break
      }
    }
  }
  
  # GGplot graphing
  plot.bar <- ggplot(data = main.df, aes(x=Freq, y=factor(Ques,levels = rev(leveler)),
                                         fill = Var1)) +
    # leveler is used to set levels for the factor on y-axis
    geom_bar(stat = "identity", position = "fill", width = c.width) +
    theme_economist(base_size = 14) +
    # resp is passed to legend labels
    scale_fill_manual(values = col, guide = guide_legend(reverse = TRUE, nrow = 1), labels = resp) +
    # main.prop is passed to geom_text label. Aesthetic needs to match the main aesthetic
    geom_text(data = main.df, aes(Freq, Ques, group = Var1), label = main.prop,
              position = position_fill(vjust=0.5), color = tex.col, size = geom_text_size) +
    labs(title = ti.tle,
         subtitle = subt) +
    # Initially, y-axis labels were manually set;
    # but after adding question labels to the dataframe it did not seem neccessary
    # scale_y_discrete(breaks = unique(main.df$Ques),
    #                  labels = ld.title) +
    scale_x_continuous(labels = scales::percent) + # Sets the scale to percentage
    ubc.theme() + # custom formatting function for the report
    theme(axis.text.x = element_text(size = 180)) # specific elements of the theme formatted here
  
  print(plot.bar)
  # print(main.df)
  # print(leveler)
  # print(sel)
  # print(prop)
}

testRk("eduGoal", nubc2022)
testRk("QN98",d.dat)
rk("QN98",d.dat)
mx("ubcBelong", nubc2022)

testType <- function(qval, new.dat){
  
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
      print("mx")
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
        print("mc.yn")
      }
      else{
        # print("3")
        print("mc")
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
        print("mx.tri")
      }else{
        # print("5")
        print("mx")
      }
    }
    # Checks for rk question type
    else if(unlist(gregexpr(pattern = 'rk', rc_list[1])) != -1){
      # print("6")
      print("rk")
    }
    # Checks for ms question type
    else if(unlist(gregexpr(pattern = 'ms', rc_list[1])) != -1){
      # print("7")
      print("ms")
    }
    # Checks for cs question types
    else if(unlist(gregexpr(pattern = 'cs', rc_list[1])) != -1){
      # print("8")
      print("cs")
    }
    # Checks for m2x question type
    else if(unlist(gregexpr(pattern = 'm2x', rc_list[1])) != -1){
      # print("8")
      print("cs")
    }
  }
}
testType("fncCncrnent", nubc2022)

col_get <- function(qval, new.dat){
  cnames <- colnames(new.dat)
  rc_list <- cnames[grepl(paste0(qval,"$"), cnames, fixed = F)]
  
  print(rc_list)
  
  if(length(rc_list) == 0){
    rc_list <- cnames[grepl(paste0(qval,""), cnames, fixed = T)]
  }
  
  print(rc_list)
  
  if(length(rc_list) != 1){
    rc_list <- c(cnames[grepl(paste0(qval,"_"), cnames, fixed = T)],
                 cnames[grepl(paste0(qval,"c"), cnames, fixed = T)],
                 cnames[grepl(paste0(qval,"C"), cnames, fixed = T)],
                 cnames[grepl(paste0(qval,"s"), cnames, fixed = T)],
                 cnames[grepl(paste0(qval,"S"), cnames, fixed = T)])
  }
  
  print(rc_list)
  
  return(rc_list)
}
rc_list.get("dmgDis", nubc2022)
