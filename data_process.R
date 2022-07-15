
  
mx_graph_proc <- function(qval, new.dat){
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval, new.dat)
  
  # Reading in response levels
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  # print(resp)
  resp1 <- resp # resp1 is used later to solve dataset inconsistencies
  # resp is reordered to match the order of frequency of responses in the graphs on the reports that were sent out
  resp <- c(unique(resp)[length(unique(resp))],unique(resp)[1:length(unique(resp))-1])
  # print(resp)
  
  # Domestic/international titles and colors
  ti.tle <- title_builder(param_list)
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
  
  # Variable initialization
  df.list <- list()
  prop <- list()
  main.prop <- NULL
  main.df<- data.frame()
  col <- rev(col)  
  tex.col.base <- rev(c("black","white","white","black","black","black"))
  tex.col <- c()
  label_count <- length(tex.col)
  ld.title <- c()
  ld.title.max <- 0 # used as a metric for aligning legend; length of the question with the highest character count is stored
  i <- 0
  
  
  
  # Axis question building and formatting
  for (qn in rc_list) {
    label_count_var <- 0
    i <- i + 1
    if(unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label'))) != -1){
      ld <- substr(get(qn, new.dat) %>% attr('label'),
                   unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label')))+3,
                   nchar(get(qn, new.dat) %>% attr('label')))
    }
    else{
      ld <- get(qn, new.dat) %>% attr('label')
    }
    
    # incase of region values in question labels
    ld <- region.get(ld,new.dat)
    
    if(nchar(ld)>63){
      ld <- paste0(substr(ld,1,sapply(gregexpr(pattern = " ", substr(ld,1,63)),max)), "\n ",
                   substr(ld,sapply(gregexpr(pattern = " ", substr(ld,1,63)),max)+1,nchar(ld)))
    }
    
    
    
    # Dataframe building
    # temp.df <- data.frame(table(get(qn, new.dat)), Ques = c(i))
    
    # The type of response levels of mx questions are either 6 or 7 in number
    # In case of dataset responses missing, complete() is used to fix these inconsistencies
    if(unlist(gregexpr(pattern = 'concerned', resp[length(resp)])) != -1 ||
       unlist(gregexpr(pattern = 'impact', resp[length(resp)])) != -1){
      # default df incase of empty subset of data from parameters
      if(nrow(table(get(qn, new.dat))) == 0){
        temp.df <- data.frame(Var1 = factor(c(1:5,999),levels = c(1:5,999)), Freq = c(rep(0,6)), Ques = c(i))
      }
      else{
        if(length(resp1) < 6){
          temp.df <- data.frame(rev(table(get(qn, new.dat))), Ques = c(i))
          # print(temp.df)
        }
        else{
          temp.df <- data.frame(table(get(qn, new.dat)), Ques = c(i))
        }
        # temp.df <- data.frame(table(get(qn, new.dat)), Ques = c(i))
      }
      if(length(resp1) < 6){
        temp.df <- complete(temp.df, Var1 = factor(c(5:1,999),levels = c(5:1,999)), fill = list(Freq = 0, Ques = c(i)))
      }
      else{
        temp.df <- complete(temp.df, Var1 = factor(c(1:5,999),levels = c(1:5,999)), fill = list(Freq = 0, Ques = c(i)))
      }
      # temp.df <- complete(temp.df, Var1 = factor(c(1:5,999),levels = c(1:5,999)), fill = list(Freq = 0, Ques = c(i)))
      tex.col.base <- rev(c("black","white","white","black","black","black"))
    }
    else{
      # default df incase of empty subset of data from parameters
      if(nrow(table(get(qn, new.dat))) == 0){
        temp.df <- data.frame(Var1 = factor(c(1:6,999),levels = c(1:6,999)), Freq = c(rep(0,7)), Ques = c(i))
      }
      else{
        if(length(resp1) < 6){
          temp.df <- data.frame(rev(table(get(qn, new.dat))), Ques = c(i))
          # print(temp.df)
        }
        else{
          temp.df <- data.frame(table(get(qn, new.dat)), Ques = c(i))
        }
        # temp.df <- data.frame(table(get(qn, new.dat)), Ques = c(i))
      }
      if(length(resp1) < 6){
        temp.df <- complete(temp.df, Var1 = factor(c(6:1,999),levels = c(6:1,999)), fill = list(Freq = 0, Ques = c(i)))
      }
      else{
        temp.df <- complete(temp.df, Var1 = factor(c(1:6,999),levels = c(1:6,999)), fill = list(Freq = 0, Ques = c(i)))
      }
      # temp.df <- complete(temp.df, Var1 = factor(c(1:6,999),levels = c(1:6,999)), fill = list(Freq = 0, Ques = c(i)))
      tex.col.base <- rev(c("black","white","white","white","black","black","black"))
    }
    
    
    if(dim(temp.df)[1] >= 6){ # to ensure the count of levelled responses passed the minimum threshold
      df.list[[i]] <- temp.df
      
      if(nchar(ld)>ld.title.max){ # to find the question label with highest character count
        ld.title.max <- nchar(ld)
      }
      ld.title <- c(ld.title, ld)
      df.list[[i]]$Ques <- ld
      
      # Geometry text prep
      prop[[i]] <- round(100*df.list[[i]]$Freq/sum(df.list[[i]]$Freq))
      
      
      
      # Main dataframe and geom text for plotting
      # if all levels have a frequency of 0 then that question is omitted from the graph
      if(length(unique(df.list[[i]]$Freq)) == 1 & unique(df.list[[i]]$Freq)[1] == 0){}
      else{
        for(j in 1:length(prop[[i]])){
          label_count_var <- label_count_var + 1
          if(as.numeric(prop[[i]][j])<5){
            prop[[i]][j] = ""
          }
          else
            prop[[i]][j] = paste0(prop[[i]][j], "%")
        }
        
        
        # Color for geom text
        if(label_count_var == label_count){
          tex.col <- c(tex.col, tex.col.base)
        }
        else{
          tex.col <- c(tex.col, tex.col.base[1:label_count_var])
        }
        
        main.prop <- c(main.prop, prop[[i]]) 
        main.df <- rbind(main.df, df.list[[i]])
      }
    }
    else{
      i <- i - 1
    }
  }
  
  
  # some questions did not have "No opinion/NA"; resp1 preserves the original values of resp and is used to make a complete response vector
  # "No opinion/Not applicable" is added if missing
  if(length(unique(main.df$Var1)) != length(resp)){
    resp <- c("No opinion/Not applicable", resp1)
  }
  
  if(length(resp1) < 6){ # for leveled-responses missing the 999-level
    resp <- c("No opinion/Not applicable",rev(resp1))
  }
  # print(resp)
  
  
  
  
  # Subtitle building
  subt <- subt_builder(rc_list, new.dat)
  
  # Subtitle positioning and geom text size
  geom_text_size <- sizer(rc_list)[2]
  c.width <- sizer(rc_list)[1]
  
  # defines the order of questions on the ggplot
  leveler <- c(unique(main.df$Var1)[length(unique(main.df$Var1))],
               unique(main.df$Var1)[1:(length(unique(main.df$Var1))-1)])
  
  # legend alignment based on length of question label with the largest character count and number of questions
  legend.pos.x <- 0.15
  if(ld.title.max < 35){
    legend.pos.x <- 0.15 + ((ld.title.max)/240)
  }
  else if(ld.title.max < 50){
    legend.pos.x <- 0.15 + ((ld.title.max)/460)
  }
  else if(ld.title.max < 60){
    legend.pos.x <- 0.15 + ((ld.title.max)/500)
  }
  else if(ld.title.max <= 100){
    legend.pos.x <- 0.15 + ((ld.title.max)/750)
  }
  else if(ld.title.max > 150){
    legend.pos.x <- 0.15 - ((ld.title.max)/400)
  }
  else if(ld.title.max > 120){
    legend.pos.x <- 0.15 - ((ld.title.max)/1400)
  }
  else if(ld.title.max > 100){
    legend.pos.x <- 0.15 - ((ld.title.max)/350)
  }
  
  legend.pos.y <- 0.98
  if(length(rc_list) < 2){
    legend.pos.y <- 0.95
  }
  else if(length(rc_list) < 4){
    legend.pos.y <- 0.96
  }
  
  
  
  # print(plot.bar)
  q_data_list <- list(qval, main.df, resp, leveler, c.width, col, main.prop, tex.col, geom_text_size, ti.tle, subt, legend.pos.x, legend.pos.y)
  
  return(q_data_list)
}

mx_table_proc <- function(qval, new.dat){
  
  # column names for data reads
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  # reading response levels
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  resp1 <- resp # used to recognize if it is
  if(length(resp) < 6){ # levelled responses have a mininium of 6 response levels; some levelled response questions were missing "No opinion/Not applicable"(999) response level and the numeric coding is reversed compared to other mx questions(eq QN105)
    resp <- c(rev(resp),"No opinion/Not applicable")
  }
  # print(resp)
  
  resp <- c(unique(resp)[length(unique(resp))],unique(resp)[1:length(unique(resp))-1])
  
  # matrix definition
  mattt <- matrix(rep(1,(length(resp)+0)*length(rc_list)), ncol = length(resp)+0)
  
  # variable initialization
  df.list <- list()
  main.df<- data.frame()
  ld.main <- c()
  row_tot <- c()
  c_vc <- c()
  c_vc_sc <- c()
  
  i <- 1
  for (qn in rc_list) {
    # dataframe building
    # temp.df <- data.frame(table(get(qn, new.dat)))
    
    if(unlist(gregexpr(pattern = 'concerned', resp[length(resp)])) != -1 ||
       unlist(gregexpr(pattern = 'impact', resp[length(resp)])) != -1){
      if(nrow(table(get(qn, new.dat))) == 0){
        temp.df <- data.frame(Var1 = factor(c(1:5,999),levels = c(1:5,999)), Freq = c(rep(0,6)))
      }
      else{
        if(length(resp1) < 6){
          temp.df <- data.frame(rev(table(get(qn, new.dat))))
          # print(temp.df)
        }
        else{
          temp.df <- data.frame(table(get(qn, new.dat)))
        }
      }
      if(length(resp1) < 6){
        temp.df <- complete(temp.df, Var1 = factor(c(5:1,999),levels = c(5:1,999)), fill = list(Freq = 0))
      }
      else{
        temp.df <- complete(temp.df, Var1 = factor(c(1:5,999),levels = c(1:5,999)), fill = list(Freq = 0))
      }
      
      # tex.col.base <- rev(c("black","white","white","black","black","black"))
    }
    else{
      if(nrow(table(get(qn, new.dat))) == 0){
        temp.df <- data.frame(Var1 = factor(c(1:6,999),levels = c(1:6,999)), Freq = c(rep(0,7)))
      }
      else{
        if(length(resp1) < 6){
          temp.df <- data.frame(rev(table(get(qn, new.dat))))
          # print(temp.df)
        }
        else{
          temp.df <- data.frame(table(get(qn, new.dat)))
        }
      }
      if(length(resp1) < 6){
        temp.df <- complete(temp.df, Var1 = factor(c(6:1,999),levels = c(6:1,999)), fill = list(Freq = 0))
      }
      else{
        temp.df <- complete(temp.df, Var1 = factor(c(1:6,999),levels = c(1:6,999)), fill = list(Freq = 0))
      }
      # tex.col.base <- rev(c("black","white","white","white","black","black","black"))
    }
    
    
    df.list[[i]] <- temp.df
    
    #Row labels
    if(unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label'))) != -1){
      ld <- substr(get(qn, new.dat) %>% attr('label'),
                   unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label')))+3,
                   nchar(get(qn, new.dat) %>% attr('label')))
    }
    else{
      ld <- get(qn, new.dat) %>% attr('label')
    }
    
    # to compute and store row totals
    row_tot <- c(row_tot, sum(df.list[[i]]$Freq))
    
    #To calculate the cumulative top 2 and 3 response levels
    c_vc.sum <- 0 # top 2 levels
    c_vc_sc.sum <- 0 # top 3 levels
    
    for (j in 1:length(resp)+0) {
      
      if(!is.na(df.list[[i]]$Freq[j])){
        mattt[i,j] <- paste0(round(100*df.list[[i]]$Freq[j]/row_tot[i]),"%") # data added to matrix 
        if(unlist(gregexpr(pattern = 'concerned', resp[length(resp)])) != -1 ||
           unlist(gregexpr(pattern = 'impact', resp[length(resp)])) != -1){
          if(j==3){ # top 3 levels sum total starts here
            c_vc_sc.sum <- c_vc_sc.sum + df.list[[i]]$Freq[j]
          }
          if(j>=4 && j<=5){ # top 3 calculation continues here; top 2 levels sum start here
            c_vc.sum <- c_vc.sum + df.list[[i]]$Freq[j]
            c_vc_sc.sum <- c_vc_sc.sum + df.list[[i]]$Freq[j]
          }
        } # calculations for different response levels need different start points for sum calculation
        else if(unlist(gregexpr(pattern = 'agree', resp[length(resp)])) != -1 || 
                unlist(gregexpr(pattern = 'satisfied', resp[length(resp)])) != -1){
          if(j==4){ # top 3 levels sum total starts here
            c_vc_sc.sum <- c_vc_sc.sum + df.list[[i]]$Freq[j]
          }
          if(j>=5 && j<=6){ # top 3 calculation continues here; top 2 levels sum start here
            c_vc.sum <- c_vc.sum + df.list[[i]]$Freq[j]
            c_vc_sc.sum <- c_vc_sc.sum + df.list[[i]]$Freq[j]
          }
        }
        
      }
      else
        mattt[i,j] <- "NA"
    }
    ld.main <- c(ld.main, ld)
    
    # sums are stored
    c_vc <- c(c_vc, paste0(round(100*c_vc.sum/row_tot[i]),"%"))
    c_vc_sc <- c(c_vc_sc, paste0(round(100*c_vc_sc.sum/row_tot[i]),"%"))
    main.df <- rbind(main.df, df.list[[i]])
    i <- i + 1
  }
  # print(main.df)
  # print(resp)
  main.df <- rev(data.frame(mattt)) # reording columns because the response levels are in the reverse order
  main.df <- cbind(main.df[,2:dim(main.df)[2]],main.df[,1]) # reordering response level 999 to the end of the columns
  # print(main.df)
  # print(main.df[dim(main.df)[1],] == "NaN%")
  # print(main.df[-c(9),])
  
  # to remove questions where frequncies of all response levels are 0
  track <- c()
  for (j in 1:dim(main.df)[1]) {
    counter = 0
    for (k in 1:dim(main.df)[2]) {
      if(main.df[j,k] == "NaN%"){
        counter = counter + 1
      }
    }
    if(counter == dim(main.df)[2]){
      track <- c(track,j)
    }
  }
  if(!is.null(track)){
    main.df <- main.df[-track,]
    ld.main <- ld.main[-track]
    c_vc_sc <- c_vc_sc[-track]
    c_vc <- c_vc[-track]
    row_tot <- row_tot[-track]
  }
  
  # identifier variables to add appropriate cumulative title names in the table
  is_conc <- unlist(gregexpr(pattern = 'concerned', resp[2]))
  is_agr <- unlist(gregexpr(pattern = 'agree', resp[2]))
  is_satis <- unlist(gregexpr(pattern = 'satisfied', resp[2]))
  is_impact <- unlist(gregexpr(pattern = 'impact', resp[2]))
  
  resp <- rev(resp)
  colnames(main.df) <- c(resp)
  c.width <- 1
  ft.size <- 6
  
  
  if(param_list[6]=="Okanagan"){
    if(new.dat$isi[1] == "Domestic"){
      main.df <- main.df %>% add_column(`UBCO Domestic` = ld.main, .before = resp[1])
    }
    else if(new.dat$isi[1] == "ISI"){
      main.df <- main.df %>% add_column(`UBCO International` = ld.main, .before = resp[1])
    }
  }
  else if(param_list[6]=="Vancouver"){
    if(new.dat$isi[1] == "Domestic"){
      main.df <- main.df %>% add_column(`UBCV Domestic` = ld.main, .before = resp[1])
    }
    else if(new.dat$isi[1] == "ISI"){
      main.df <- main.df %>% add_column(`UBCV International` = ld.main, .before = resp[1])
    }
  }
  else{
    if(new.dat$isi[1] == "Domestic"){
      main.df <- main.df %>% add_column(`UBC Domestic` = ld.main, .before = resp[1])
    }
    else if(new.dat$isi[1] == "ISI"){
      main.df <- main.df %>% add_column(`UBC International` = ld.main, .before = resp[1])
    }
  }
  
  
  # the identifier variables are used here to identify and set the correct cumulative column names; row total column is also added to the table
  if(is_conc != -1){
    main.df <- main.df %>% add_column(`Very concerned/Concerned` = c_vc, .before = resp[1]) %>%
      add_column(`Including somewhat concerned` = c_vc_sc, .after = "Very concerned/Concerned") %>%
      add_column(Total = row_tot)
    c.width <- 0.65
    ft.size <- 6
  }else if(is_agr != -1){
    main.df <- main.df %>% add_column(`Strongly agree/\nAgree` = c_vc, .before = resp[1]) %>%
      add_column(`Including somewhat agree` = c_vc_sc, .after = "Strongly agree/\nAgree") %>%
      add_column(Total = row_tot)
    c.width <- 0.59
    ft.size <- 5.5
  }else if(is_satis != -1){
    main.df <- main.df %>% add_column(`Very satisfied/\nSatisfied` = c_vc, .before = resp[1]) %>%
      add_column(`Including somewhat satisfied` = c_vc_sc, .after = "Very satisfied/\nSatisfied") %>%
      add_column(Total = row_tot)
    c.width <- 0.59
    ft.size <- 5.5
  }
  else if(is_impact != -1){
    main.df <- main.df %>% add_column(`Very significant impact/\nSignificant impact` = c_vc, .before = resp[1]) %>%
      add_column(`Including moderate impact` = c_vc_sc, .after = "Very significant impact/\nSignificant impact") %>%
      add_column(Total = row_tot)
    c.width <- 0.59
    ft.size <- 5.5
  }
  
  # list(qval, main.df, resp, leveler, c.width, col, main.prop, tex.col, geom_text_size, ti.tle, subt, legend.pos.x, legend.pos.y)
  
  q_data_list <- list(qval, main.df, ft.size, c.width, mattt)
  
  return(q_data_list)
  
}

mc_graph_proc <- function(qval, new.dat){
  
  # splitting data by domestic/international
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval, new.dat)
  rc_list <- rc_eval("mc",rc_list)
  
  # response levels
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  
  # Variable initialization
  df.list <- list()
  main.df <- NULL
  main.prop <- c()
  tex.col <- c()
  label_count <- length(tex.col)
  ld.title <- c()
  i <- 1
  
  # Dataframe building
  if(nrow(table(get(rc_list, new.dat)))==0){
    main.df <- data.frame(Var1 = c(1:(length(resp)-1),999), Freq = c(rep(0,length(resp))))
  }
  else{
    main.df <- data.frame((table(get(rc_list, new.dat))))
  }
  
  if(nrow(table(get(rc_list, i.dat)))==0){
    i.df <- data.frame(Var1 = c(main.df$Var1), Freq = c(rep(0,length(main.df$Var1))), Ques = c("International"))
  }
  else{
    i.df <- data.frame(table(get(rc_list, i.dat)), Ques = c("International"))
  }
  if(nrow(table(get(rc_list, d.dat)))==0){
    d.df <- data.frame(Var1 = c(main.df$Var1), Freq = c(rep(0,length(main.df$Var1))), Ques = c("Domestic"))
  }
  else{
    d.df <- data.frame(table(get(rc_list, d.dat)), Ques = c("Domestic"))
  }
  
  # print(main.df)
  # print(d.df)
  # print(i.df)
  
  # to fill in missing responses using the common df as reference
  i.df <- complete(i.df, Var1 = main.df$Var1, fill = list(Freq = 0, Ques = c("International")))
  d.df <- complete(d.df, Var1 = main.df$Var1, fill = list(Freq = 0, Ques = c("Domestic")))
  
  # i.df$Freq <- round(100*i.df$Freq/sum(i.df$Freq)) # counts replaced with its respective percentage value
  # d.df$Freq <- round(100*d.df$Freq/sum(d.df$Freq)) # counts replaced with its respective percentage value
  
  if(is.nan(round(100*i.df$Freq/sum(i.df$Freq)))){
    i.df$Freq <- 0
  }else{
    i.df$Freq <- round(100*i.df$Freq/sum(i.df$Freq))
  }
  
  if(is.nan(round(100*d.df$Freq/sum(d.df$Freq)))){
    d.df$Freq <- 0
  }else{
    d.df$Freq <- round(100*d.df$Freq/sum(d.df$Freq))
  }
  
  # prop variables set up for geom_text
  i.prop <- paste0(i.df$Freq,"%") 
  d.prop <- paste0(d.df$Freq,"%")
  main.prop <- c(d.prop,i.prop)
  
  
  # Selecting valid choices from the data subset as numerical levels
  resp_b <- c()
  for (qn in main.df$Var1) {
    resp_b <- c(resp_b, as.numeric(qn))
  }
  
  # selecting valid question labels; same reasoning from the tb_mc() function
  axis.q <- c()
  # axis.q.i <- c()
  # axis.q.d <- c()
  if(0 %in% resp_b){
    i <- 0
    for (k in resp_b) {
      i <- i + 1
      if(k == 999){
        axis.q <- c(axis.q,resp[length(resp)])
      }
      else{
        if(is.na(resp[k+1])){
          axis.q <- c(axis.q, resp[i])
        }
        else{
          axis.q <- c(axis.q, resp[k+1])
        }
      }
    }
  }
  else{
    i <- 0
    for (k in resp_b) {
      i <- i + 1
      if(k == 999){
        axis.q <- c(axis.q, resp[length(resp)])
      }
      else{
        if(is.na(resp[k])){
          axis.q <- c(axis.q, resp[i])
        }
        else{
          axis.q <- c(axis.q, resp[k])
        }
      }
    }
  }
  # print(axis.q)
  
  # adding new line to selected question labels based on their character lengths
  for (j in 1:length(axis.q)) {
    axis.q[j] <- region.get(axis.q[j],new.dat) # modifies question labels to display the correct region (removes brackets and field names and replaces it with the 'Okanagan' or 'Vancouver')
    if(nchar(axis.q[j])>40){
      axis.q[j] <- paste0(substr(axis.q[j],1,sapply(gregexpr(pattern = " ", substr(axis.q[j],1,20)),max)), "\n ",
                          substr(axis.q[j],sapply(gregexpr(pattern = " ", substr(axis.q[j],1,20)),max)+1,
                                 sapply(gregexpr(pattern = " ", substr(axis.q[j],1,40)),max)), "\n ",
                          substr(axis.q[j],sapply(gregexpr(pattern = " ", substr(axis.q[j],1,40)),max)+1,nchar(axis.q[j])))
    }else if(nchar(axis.q[j])>20){
      axis.q[j] <- paste0(substr(axis.q[j],1,sapply(gregexpr(pattern = " ", substr(axis.q[j],1,20)),max)), "\n ",
                          substr(axis.q[j],sapply(gregexpr(pattern = " ", substr(axis.q[j],1,20)),max)+1,nchar(axis.q[j])))
    }
  }
  
  # integrating the selected question labels into the dfs
  d.df$Var1 <- axis.q
  i.df$Var1 <- axis.q
  
  # domestic df and international df combined horizontally (by rows)
  main.df <- rbind(d.df,i.df)
  # print(main.df)
  # Subtitle building
  subt <- subt_builder(rc_list, new.dat)
  subt <- region.get(subt, new.dat)
  
  # sizing format
  geom_text_size <- sizer(main.df$Var1)[2]
  c.width <- sizer(main.df$Var1)[1]
  
  q_data_list <- list(qval, main.df, c.width, main.prop, geom_text_size, param_list, subt)
  
  return(q_data_list)
  
}

mc_table_proc <- function(qval, new.dat){
  
  # splitting data by domestic/international
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  rc_list <- rc_eval("mc",rc_list)
  
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels')) # reading response levels
  
  # Dataframe building
  if(nrow(table(get(rc_list, new.dat)))==0){
    main.df <- data.frame(Var1 = c(1:(length(resp)-1),999), Freq = c(rep(0,length(resp))))
  }
  else{
    main.df <- data.frame((table(get(rc_list, new.dat))))
  }
  
  if(nrow(table(get(rc_list, i.dat)))==0){
    i.df <- data.frame(Var1 = c(main.df$Var1), Freq = c(rep(0,length(main.df$Var1))), Ques = c("International"))
  }
  else{
    i.df <- data.frame(table(get(rc_list, i.dat)), Ques = c("International"))
  }
  if(nrow(table(get(rc_list, d.dat)))==0){
    d.df <- data.frame(Var1 = c(main.df$Var1), Freq = c(rep(0,length(main.df$Var1))), Ques = c("Domestic"))
  }
  else{
    d.df <- data.frame(table(get(rc_list, d.dat)), Ques = c("Domestic"))
  }
  
  # to fill in missing responses using the common df as reference
  i.df <- complete(i.df, Var1 = main.df$Var1, fill = list(Freq = 0, Ques = c("International")))
  d.df <- complete(d.df, Var1 = main.df$Var1, fill = list(Freq = 0, Ques = c("Domestic")))
  
  # print(main.df)
  # print(d.df)
  # print(i.df)
  
  # stores valid numerical response levels
  resp_b <- c() # vector to store valid response levels
  for (qn in main.df$Var1) {
    resp_b <- c(resp_b, as.numeric(qn))
  }
  
  # matrix intialization; no. of rows = no. of valid response levels, no. of columns = 4 (2 % columns, 2 count columns)
  mattt <- matrix(rep(1,4*(length(resp_b)+1)), ncol = 4)
  
  
  # matrix definition
  for (j in 1:(dim(mattt)[1])) {
    if(j == dim(mattt)[1]){ # last row
      mattt[j,1] <- "100%"
      mattt[j,2] <- sum(d.df$Freq)
      mattt[j,3] <- "100%"
      mattt[j,4] <- sum(i.df$Freq)
    }
    else{ # other rows
      if(is.nan(round(100*d.df$Freq[j]/sum(d.df$Freq)))){
        mattt[j,1] <- "0%"
      }else{
        mattt[j,1] <- paste0(round(100*d.df$Freq[j]/sum(d.df$Freq)),"%")
      }
      if(is.nan(round(100*i.df$Freq[j]/sum(i.df$Freq)))){
        mattt[j,3] <- "0%"
      }else{
        mattt[j,3] <- paste0(round(100*i.df$Freq[j]/sum(i.df$Freq)),"%")
      }
      # mattt[j,1] <- paste0(round(100*d.df$Freq[j]/sum(d.df$Freq)),"%")
      mattt[j,2] <- d.df$Freq[j] 
      # mattt[j,3] <- paste0(round(100*i.df$Freq[j]/sum(i.df$Freq)),"%")
      mattt[j,4] <- i.df$Freq[j]
    }
  }
  # print(main.df)
  
  # matrix to df conversion
  main.df <- data.frame(mattt)
  # print(main.df)
  
  axis.q <- c()
  ## to select valid question labels
  if(0 %in% resp_b){ # 0 is a numerical level only for yes(1)/no(0) mc questions
    i <- 0
    for (k in resp_b) {
      i <- i + 1
      if(k == 999){
        axis.q <- c(axis.q,resp[length(resp)])
      }
      else{
        if(is.na(resp[k+1])){
          axis.q <- c(axis.q, resp[i])
        }
        else{
          axis.q <- c(axis.q, resp[k+1])
        }
      }
    }
    
    # if(999 %in% resp_b){
    #   axis.q <- c(resp[(resp_b+1)[-c(length(resp_b))]],resp[length(resp)])
    # }
    # else{
    #   axis.q <- resp[(resp_b+1)]
    # }
  }
  else{ # for other mc questions
    i <- 0 # because some numerical levels are not always sequential, a fake counter('i') that keeps track of vector indices is used
    for (k in resp_b) { 
      i <- i + 1
      if(k == 999){ # '999' is always associated with "NA/No opinion" and is positioned towards the end of the responses for all questions
        axis.q <- c(axis.q, resp[length(resp)])
      }
      else{
        # if the numerical level is not synchronous with question the vector index, there is no data to read at that index; hence 'i' is used
        if(is.na(resp[k])){ 
          axis.q <- c(axis.q, resp[i])
        }
        else{
          axis.q <- c(axis.q, resp[k])
        }
      }
    }
  }
  # 'distinct count' added to the valid question vector
  axis.q <- c((axis.q),"Distinct count of respondents")
  
  if(param_list[6]=="Okanagan"){
    main.df <- cbind(UBCO = axis.q, main.df)
    # print(main.df)
  }
  else if(param_list[6]=="Vancouver"){
    main.df <- cbind(UBCV = axis.q, main.df)
    # print(main.df)
  }
  else{
    main.df <- cbind(UBC = axis.q, main.df)
  }
  
  q_data_list <- list(qval, main.df, param_list)
  
  return(q_data_list)
  
}

mc.yn_graph_proc <- function(qval, new.dat){
  mc_graph_proc(qval,new.dat) # function call to mc() because the type of graphs for mc and mc.yn questions are the same, and their responses are similarly structured in the dataset
}

mc.yn_table_proc <- function(qval, new.dat){
  
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  
  
  if(nrow(table(get(rc_list, new.dat)))==0){
    main.df <- data.frame(Var1 = c(0:(length(resp)-2),999), Freq = c(rep(0,length(resp))))
  }
  else{
    main.df <- data.frame((table(get(rc_list, new.dat))))
  }
  
  if(nrow(table(get(rc_list, i.dat)))==0){
    i.df <- data.frame(Var1 = c(0:(length(resp)-2),999), Freq = c(rep(0,length(resp))), Ques = c("International"))
  }
  else{
    i.df <- data.frame(table(get(rc_list, i.dat)), Ques = c("International"))
  }
  
  if(nrow(table(get(rc_list, d.dat)))==0){
    d.df <- data.frame(Var1 = c(0:(length(resp)-2),999), Freq = c(rep(0,length(resp))), Ques = c("Domestic"))
  }
  else{
    d.df <- data.frame(table(get(rc_list, d.dat)), Ques = c("Domestic"))
  }
  
  # to fill in missing responses using the common df as reference
  i.df <- complete(i.df, Var1 = main.df$Var1, fill = list(Freq = 0, Ques = c("International")))
  d.df <- complete(d.df, Var1 = main.df$Var1, fill = list(Freq = 0, Ques = c("Domestic")))
  
  # print(main.df)
  # print(d.df)
  # print(i.df)
  
  # Selecting valid choices from the data subset
  resp_b <- c()
  for (qn in main.df$Var1) {
    resp_b <- c(resp_b, as.numeric(qn))
  }
  
  mattt <- matrix(rep(1,4*(length(resp_b)+1)), ncol = 4)
  
  for (j in 1:dim(mattt)[1]) {
    if(j == dim(mattt)[1]){
      mattt[j,1] <- "100%"
      mattt[j,2] <- sum(d.df$Freq)
      mattt[j,3] <- "100%"
      mattt[j,4] <- sum(i.df$Freq)
    }
    else{ # other rows
      if(is.nan(round(100*d.df$Freq[j]/sum(d.df$Freq)))){
        mattt[j,1] <- "0%"
      }else{
        mattt[j,1] <- paste0(round(100*d.df$Freq[j]/sum(d.df$Freq)),"%")
      }
      if(is.nan(round(100*i.df$Freq[j]/sum(i.df$Freq)))){
        mattt[j,3] <- "0%"
      }else{
        mattt[j,3] <- paste0(round(100*i.df$Freq[j]/sum(i.df$Freq)),"%")
      }
      # mattt[j,1] <- paste0(round(100*d.df$Freq[j]/sum(d.df$Freq)),"%")
      mattt[j,2] <- d.df$Freq[j] 
      # mattt[j,3] <- paste0(round(100*i.df$Freq[j]/sum(i.df$Freq)),"%")
      mattt[j,4] <- i.df$Freq[j]
    }
  }
  
  main.df <- data.frame(mattt)
  # print(mattt)
  
  axis.q <- c()
  ## to select valid question labels
  if(0 %in% resp_b){ # 0 is a numerical level only for yes(1)/no(0) mc questions
    i <- 0
    for (k in resp_b) {
      i <- i + 1
      if(k == 999){
        axis.q <- c(axis.q,resp[length(resp)])
      }
      else{
        if(is.na(resp[k+1])){
          axis.q <- c(axis.q, resp[i])
        }
        else{
          axis.q <- c(axis.q, resp[k+1])
        }
      }
    }
    
    # if(999 %in% resp_b){
    #   axis.q <- c(resp[(resp_b+1)[-c(length(resp_b))]],resp[length(resp)])
    # }
    # else{
    #   axis.q <- resp[(resp_b+1)]
    # }
  }
  else{ # for other mc questions
    i <- 0 # because some numerical levels are not always sequential, a fake counter('i') that keeps track of vector indices is used
    for (k in resp_b) { 
      i <- i + 1
      if(k == 999){ # '999' is always associated with "NA/No opinion" and is positioned towards the end of the responses for all questions
        axis.q <- c(axis.q, resp[length(resp)])
      }
      else{
        # if the numerical level is not synchronous with question the vector index, there is no data to read at that index; hence 'i' is used
        if(is.na(resp[k])){ 
          axis.q <- c(axis.q, resp[i])
        }
        else{
          axis.q <- c(axis.q, resp[k])
        }
      }
    }
  }
  
  # 'distinct count' added to the valid question vector
  axis.q <- c((axis.q),"Total")
  
  if(param_list[6]=="Okanagan"){
    main.df <- cbind(UBCO = axis.q, main.df)
    # print(main.df)
  }
  else if(param_list[6]=="Vancouver"){
    main.df <- cbind(UBCV = axis.q, main.df)
    # print(main.df)
  }
  else{
    main.df <- cbind(UBC = axis.q, main.df)
    # print(main.df)
  }
  
  q_data_list <- list(qval, main.df, param_list)
  
  return(q_data_list)
  
}


processed_graph_dataList <- list()
processed_table_dataList <- list()

processed_graph_dataList[[1]] <- mx_graph_proc("QN105", d.dat)
processed_graph_dataList[[2]] <- mx_graph_proc("QN104", d.dat)
processed_graph_dataList[[3]] <- mx_graph_proc("QN100", d.dat)
processed_graph_dataList[[4]] <- mc_graph_proc("QN44", data.ok)
processed_graph_dataList[[5]] <- mc.yn_graph_proc("QN94", data.ok)

processed_table_dataList[[1]] <- mx_table_proc("QN105", d.dat)
processed_table_dataList[[2]] <- mx_table_proc("QN104", d.dat)
processed_table_dataList[[3]] <- mx_table_proc("QN100", d.dat)
processed_table_dataList[[4]] <- mc_table_proc("QN44", data.ok)
processed_table_dataList[[5]] <- mc.yn_table_proc("QN94", data.ok)

str(processed_dataList)
processed_graph_dataList[[5]]

save(processed_graph_dataList, processed_table_dataList, file = "processedData.RData")
load("processedData.RData")
