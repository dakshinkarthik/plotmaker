
  
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
  q_data_list <- list("mx", qval, main.df, resp, leveler, c.width, col, main.prop, tex.col, geom_text_size, ti.tle, subt, legend.pos.x, legend.pos.y)
  
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
  
  q_data_list <- list("mx", qval, main.df, ft.size, c.width, mattt)
  
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
  
  q_data_list <- list("mc", qval, main.df, c.width, main.prop, geom_text_size, param_list, subt)
  
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
  
  q_data_list <- list("mc", qval, main.df, param_list)
  
  return(q_data_list)
  
}

mc.yn_graph_proc <- function(qval, new.dat){
  
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
  
  q_data_list <- list("mc.yn", qval, main.df, c.width, main.prop, geom_text_size, param_list, subt)
  
  return(q_data_list)
  
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
  
  q_data_list <- list("mc.yn", qval, main.df, param_list)
  
  return(q_data_list)
  
}

rk_graph_proc <- function(qval, new.dat){
  
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval, new.dat)  
  # input dataset is subsetted based on sum/complete field value
  # 28 is a parameter here because there is only one rk question in the dataset that requires 28 as a validating factor
  # Future code can be changed to accommodate a dynamic parameter value
  new.dat <- rc_complete(rc_list, new.dat, 28) 
  rc_list <- rc_eval("rk",rc_list) # Checks if all the sub questions selected belong to the qID
  
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
  
  q_data_list <- list("rk", qval, main.df, leveler, c.width, col, resp, main.prop, tex.col, geom_text_size, ti.tle, subt)
  
  return(q_data_list)
}

rk_table_proc <- function(qval, new.dat){
  
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval, new.dat)  
  new.dat <- rc_complete(rc_list, new.dat, 28)
  rc_list <- rc_eval("rk",rc_list)
  
  # Reading response numeric levels and pasting "Rank" to it
  resp <- paste("Rank", rev(names(get(rc_list[1], new.dat) %>% attr('labels'))), sep = " ")
  # matrix initialization (no. of rows = rc_list+1, no. of columns = resp); one extra row to accommodate column totals
  mattt <- matrix(rep(1,(length(resp)+0)*(length(rc_list)+1)), ncol = length(resp)+0)
  
  # Variable initialization
  df.list <- list()
  prop <- list()
  main.prop <- NULL
  main.df<- data.frame()
  ld.main <- c()
  
  
  i <- 1 # loop counter
  for(i in 1:dim(mattt)[1]){
    if(i != dim(mattt)[1]){ # sums and question label retrieval done only for question rows
      # df building
      # df.list[[i]] <- data.frame(table(get(rc_list[i], new.dat)), Ques = c(i))
      temp.df <- data.frame(rev(table(get(rc_list[i], new.dat))), Ques = c(i))
      temp.df <- complete(temp.df, Var1 = factor(c(7:1),levels = c(7:1)), fill = list(Freq = 0, Ques = c(i)))
      df.list[[i]] <- temp.df
      
      #Row labels and fornatting
      ld <- NULL
      if(unlist(gregexpr(pattern =' - ', get(rc_list[i], new.dat) %>% attr('label'))) != -1){
        ld <- substr(get(rc_list[i], data.ok) %>% attr('label'),
                     unlist(gregexpr(pattern =' - ', get(rc_list[i], new.dat) %>% attr('label')))+3,
                     nchar(get(rc_list[i], new.dat) %>% attr('label')))
      }
      else{
        ld <- get(qn, new.dat) %>% attr('label')
      }
      
      # adding question label to df
      df.list[[i]]$Ques <- ld
      
      # row total to compute percentage
      row_tot <- sum(df.list[[i]]$Freq)
      for(j in 1:dim(mattt)[2]){
        if(!is.na(df.list[[i]]$Freq[j])){ # to catch NAs in the responses
          mattt[i,j] <- round(100*df.list[[i]]$Freq[j]/row_tot) 
        }
        else{
          mattt[i,j] <- "NA"
        }
      }
      # Question labels stored for the questions column
      ld.main <- c(ld.main, ld)
    }
    else{
      for(j in 1:dim(mattt)[2]){ # final row gets only student total
        mattt[i,j] <- nrow(new.dat)
      }
    }
  }
  # print(mattt)
  # "Total" added to question labels
  ld.main <- c(ld.main,"Total")
  # matrix to df conversion
  main.df <- rev(data.frame(mattt))
  # print(main.df)
  # response levels vector reversed to match the data
  resp <- c(rev(resp))
  colnames(main.df) <- resp
  
  # adding question columns for domestic/international students
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
  
  
  # print(main.df)
  # removing "Total" row to avoid using the total for Rank 1 to sort the dataframe
  main.df <- main.df[-c(dim(main.df)[1]),]
  # print(main.df)
  # reordering rows with decreasing Rank 1 scores
  main.df <- main.df[with(main.df, order(-`Rank 1`)),]
  # print(main.df)
  
  # adding questions column for domestic/international students
  if(param_list[6]=="Okanagan"){
    if(new.dat$isi[1] == "Domestic"){
      main.df <- main.df %>% add_row(`UBCO Domestic` = "Total", `Rank 1` = nrow(new.dat),
                                     `Rank 2` = nrow(new.dat),
                                     `Rank 3` = nrow(new.dat),
                                     `Rank 4` = nrow(new.dat),
                                     `Rank 5` = nrow(new.dat),
                                     `Rank 6` = nrow(new.dat),
                                     `Rank 7` = nrow(new.dat))
    }
    else if(new.dat$isi[1] == "ISI"){
      main.df <- main.df %>% add_row(`UBCO International` = "Total", `Rank 1` = nrow(new.dat),
                                     `Rank 2` = nrow(new.dat),
                                     `Rank 3` = nrow(new.dat),
                                     `Rank 4` = nrow(new.dat),
                                     `Rank 5` = nrow(new.dat),
                                     `Rank 6` = nrow(new.dat),
                                     `Rank 7` = nrow(new.dat))
    }
  }
  else if(param_list[6]=="Vancouver"){
    if(new.dat$isi[1] == "Domestic"){
      main.df <- main.df %>% add_row(`UBCV Domestic` = "Total", `Rank 1` = nrow(new.dat),
                                     `Rank 2` = nrow(new.dat),
                                     `Rank 3` = nrow(new.dat),
                                     `Rank 4` = nrow(new.dat),
                                     `Rank 5` = nrow(new.dat),
                                     `Rank 6` = nrow(new.dat),
                                     `Rank 7` = nrow(new.dat))
    }
    else if(new.dat$isi[1] == "ISI"){
      main.df <- main.df %>% add_row(`UBCV International` = "Total", `Rank 1` = nrow(new.dat),
                                     `Rank 2` = nrow(new.dat),
                                     `Rank 3` = nrow(new.dat),
                                     `Rank 4` = nrow(new.dat),
                                     `Rank 5` = nrow(new.dat),
                                     `Rank 6` = nrow(new.dat),
                                     `Rank 7` = nrow(new.dat))
    }
  }
  else{
    if(new.dat$isi[1] == "Domestic"){
      main.df <- main.df %>% add_row(`UBC Domestic` = "Total", `Rank 1` = nrow(new.dat),
                                     `Rank 2` = nrow(new.dat),
                                     `Rank 3` = nrow(new.dat),
                                     `Rank 4` = nrow(new.dat),
                                     `Rank 5` = nrow(new.dat),
                                     `Rank 6` = nrow(new.dat),
                                     `Rank 7` = nrow(new.dat))
    }
    else if(new.dat$isi[1] == "ISI"){
      main.df <- main.df %>% add_row(`UBC International` = "Total", `Rank 1` = nrow(new.dat),
                                     `Rank 2` = nrow(new.dat),
                                     `Rank 3` = nrow(new.dat),
                                     `Rank 4` = nrow(new.dat),
                                     `Rank 5` = nrow(new.dat),
                                     `Rank 6` = nrow(new.dat),
                                     `Rank 7` = nrow(new.dat))
    }
  }
  
  
  # pasting % to all cells except the last row
  for(k in 1:dim(main.df)[1]-1){
    for(j in 2:dim(main.df)[2]){
      main.df[k,j] <- paste0(main.df[k,j],"%")
    }
  }
  
  q_data_list <- list("rk", qval, main.df, mattt)
  
  
  return(q_data_list)
}

ms_graph_proc <- function(qval, new.dat){
  
  # splitting data by domestic/international
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  rc_list <- rc_eval("ms",rc_list)
  
  # computing distinct count
  i.dc <- 0
  d.dc <- 0
  for (stu in i.dat$ExternalReference) { # international distinct count
    for (qn in rc_list) {
      if(!is.na(get(qn, i.dat)[i.dat$ExternalReference == stu])){
        if((get(qn, i.dat)[i.dat$ExternalReference == stu] + 0) == 1){
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
          d.dc <- d.dc + 1
          break
        }
      }
    }
  }
  
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
  
  q_data_list <- list("ms", qval, main.df, main.prop, param_list, subt)
  
  return(q_data_list)
  
}

ms_table_proc <- function(qval, new.dat){
  
  # splitting data by domestic/international
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  rc_list <- rc_eval("ms",rc_list)
  
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
  
  
  # variables to store distinct count of domestic and international students
  i.dc <- 0
  d.dc <- 0
  for (stu in i.dat$ExternalReference) { # distinct count calculation for international students
    for (qn in rc_list) {
      if(!is.na(get(qn, i.dat)[i.dat$ExternalReference == stu])){
        if((get(qn, i.dat)[i.dat$ExternalReference == stu] + 0) == 1){ # 0 is added to the data read from the dataset to coerce a conversion to numerical type
          i.dc <- i.dc + 1
          break
        }
      }
    }
  }
  
  for (stu in d.dat$ExternalReference) { # distinct count calculation for domestic students
    for(qn in rc_list){
      if(!is.na(get(qn, d.dat)[d.dat$ExternalReference == stu])){
        if((get(qn, d.dat)[d.dat$ExternalReference == stu] + 0) == 1){ # 0 is added to the data read from the dataset to coerce a conversion to numerical type
          d.dc <- d.dc + 1
          break
        }
      }
    }
  }
  
  # Loop counters for domestic and international
  i <- 1 # domestic loop counter
  j <- 1 # international loop counter
  
  for (qn in rc_list) {
    # Dataframe building ****NOTE****: The code for df building here was created before I learnt the existence of the complete() function. This code should be modifiable to use complete() and reduce code.
    ## Domestic fraction
    axis.c <- names(get(qn, new.dat) %>% attr('labels')) # getting question label
    axis.q <- c(axis.q,axis.c) # storing question label
    if(nrow(table(get(qn, d.dat))) == 0){ # checks for missing responses for a singular question
      tcv <- matrix(0) # dummy matrix
      rownames(tcv) <- c(i) # dummy column name
      tcv <- as.table(tcv) # converted to a table
      colnames(tcv) <- c("") # column name removed; column name could be removed from a matrix only through converting it to a table
      tdf <- data.frame(tcv) # conversion to df
      tdf <- data.frame(Var1 = tdf$Var1, Freq = tdf$Freq, Ques = c("Domestic")) # manually adding 0 to missing response frequencies and formatting the df to meet requirements
      d.df.list[[i]] <- tdf # added to domestic df list
      d.df.list[[i]]$Var1 <- c(axis.c) # question label added to df
      main.df <- rbind(main.df,d.df.list[[i]])
    }
    else{ # normal response case
      tdf <- data.frame(table(get(qn, d.dat)), Ques = c("Domestic")) # df creation
      d.df.list[[i]] <- tdf # added to domestic df list 
      if(dim(d.df.list[[i]])[1] != 1){ # If there is > 1 response level, we take only the second level('1' - Yes)
        d.df.list[[i]]$Var1 <- c(axis.c)
        main.df <- rbind(main.df,d.df.list[[i]][2,])
      }
      else{
        if(d.df.list[[i]]$Var1 == 0){ # if there is only 1 level and if it is 0 ('No'), a new df is created and with the same question label and the frequency is set to 0, assuming it be '1'(Yes)
          d.df.list[[i]] <- data.frame(Var1 = c(axis.c), Freq = c(0), Ques = c("Domestic"))
          main.df <- rbind(main.df,d.df.list[[i]])
        }
        else{ 
          d.df.list[[i]]$Var1 <- c(axis.c)
          main.df <- rbind(main.df,d.df.list[[i]])
        }
      }
    }
    ## International fraction
    # Similar df building reasoning for international fraction
    if(nrow(table(get(qn, i.dat))) == 0){ # checks for missing responses for a singular question
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
        i.df.list[[j]]$Var1 <- c(axis.c)
        main.df <- rbind(main.df,i.df.list[[j]][2,])
      }
      else{
        if(i.df.list[[j]]$Var1 == 0){
          i.df.list[[j]] <- data.frame(Var1 = c(axis.c), Freq = c(0), Ques = c("International"))
          main.df <- rbind(main.df,i.df.list[[j]])
        }
        else{
          i.df.list[[j]]$Var1 <- c(axis.c)
          main.df <- rbind(main.df,i.df.list[[j]])
        }
      }
    }
    
    i <- i + 1
    j <- j + 1
  }
  
  l <- 1 # loop counter
  nnull <- c() # vector to store indices of rows with Freq = 0
  for (qn in 1:dim(main.df)[1]) {
    if(l < dim(main.df)[1]){
      if(main.df$Freq[l] == 0 & main.df$Freq[l+1] == 0){
        nnull <- c(nnull,l,l+1)
      }
    }
    l <- l + 2
  }
  
  if(!is.null(nnull)){
    main.df <- main.df[-nnull,]
  }
  
  # matrix with all 1s; no. of columns = 4 because the columns consist only of 2 percentage fields and 2 count fields.
  # no. of rows is divided by 2 two because the df contains both domestic and international data.
  mattt <- matrix(rep(1,((dim(main.df)[1]/2)+1)*4), ncol = 4) 
  
  k <- 1 # loop counter; 'k' is incremented by 2 to accommodate both domestic and international data from the df
  for (m in 1:dim(mattt)[1]) {
    if(k < dim(main.df)[1]){ # all rows except the last
      mattt[m,1] <- paste0(round(100*main.df$Freq[k]/d.dc),"%") # Domestic data
      mattt[m,2] <- main.df$Freq[k] # Domestic data
      mattt[m,3] <- paste0(round(100*main.df$Freq[k+1]/i.dc),"%") # international data
      mattt[m,4] <- main.df$Freq[k+1] # international data
    }
    else{ # only the for the last row
      mattt[m,1] <- "100%"
      mattt[m,2] <- d.dc
      mattt[m,3] <- "100%"
      mattt[m,4] <- i.dc
    }
    k <- k + 2 # data in the df is ordered as domestic,international,domestic...; hence k = k + 2
  }
  
  axis.q <- unique(main.df$Var1) # gets a single copy of all the questions from the df
  
  main.df <- data.frame(mattt) # matrix to df conversion
  if(param_list[6]=="Okanagan"){
    main.df <- cbind(UBCO = c(axis.q,"Distinct count of Respondents"), main.df) # adding questions column to the df along with 'Distinct count'
  }
  else if(param_list[6]=="Vancouver"){
    main.df <- cbind(UBCV = c(axis.q,"Distinct count of Respondents"), main.df) # adding questions column to the df along with 'Distinct count'
  }
  else{
    main.df <- cbind(UBC = c(axis.q,"Distinct count of Respondents"), main.df) # adding questions column to the df along with 'Distinct count'
  }
  
  q_data_list <- list("ms", qval, main.df, param_list)
  
  return(q_data_list)
  
}

cs_graph_proc <- function(qval, new.dat){
  
  # splitting data by domestic/international
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  sum.field <- get_sum(rc_list) # gets the current question's sum field
  rc_list <- rc_eval("cs",rc_list)
  
  ld.title <- c()
  
  # distinct count computed exactly like it was for ms() and tb_ms(), except that 'sum.field' is used as a basis to check if a student's scores add up to 100
  i.dc <- 0
  d.dc <- 0
  for (stu in i.dat$ExternalReference) {
    if(!is.na(get(sum.field, i.dat)[i.dat$ExternalReference == stu])){
      if(get(sum.field, i.dat)[i.dat$ExternalReference == stu] == 100){
        i.dc <- i.dc + 1
      }
    }
  }
  
  for (stu in d.dat$ExternalReference) {
    if(!is.na(get(sum.field, d.dat)[d.dat$ExternalReference == stu])){
      if(get(sum.field, d.dat)[d.dat$ExternalReference == stu] == 100){
        d.dc <- d.dc + 1
      }
    }
  }
  
  
  i.perq <- c()
  d.perq <- c()
  for (i in 1:length(rc_list)) {
    resl.d <- 0
    resl.i <- 0
    # df building
    df.d <- data.frame(table(get(rc_list[i], d.dat)))
    df.i <- data.frame(table(get(rc_list[i], i.dat)))
    # total for each response level for each sub-question/option is computed by multiplying the score value with the frequency of its responses 
    for (j in 1:length(df.d$Freq)) { # domestic sum calculation
      df_v <- as.integer(levels(df.d$Var1)[as.integer(df.d$Var1)])[j] # each score level drawn by indexing the numerical level column in the df
      df_f <- df.d$Freq[j] # corresponding frequency 
      resl.d <- resl.d + (df_v*df_f) # sum is stored
    }
    for (j in 1:length(df.i$Freq)) { # international sum calculation
      df_v <- as.integer(levels(df.i$Var1)[as.integer(df.i$Var1)])[j] # each score level drawn by indexing the numerical level column in the df
      df_f <- df.i$Freq[j] # corresponding frequency 
      resl.i <- resl.i + (df_v*df_f) # sum is stored
    }
    # sum computed in every iteration is stored in 2 separate vectors
    d.perq <- c(d.perq,resl.d)
    i.perq <- c(i.perq,resl.i)
    
    
    # question label extraction
    if(unlist(gregexpr(pattern =' - ', get(rc_list[i], new.dat) %>% attr('label'))) != -1){
      ld <- substr(get(rc_list[i], new.dat) %>% attr('label'),
                   unlist(gregexpr(pattern =' - ', get(rc_list[i], new.dat) %>% attr('label')))+3,
                   nchar(get(rc_list[i], new.dat) %>% attr('label')))
    }
    else{
      ld <- names(get(rc_list[i], new.dat) %>% attr('label'))
    }
    
    ld.title <- c(ld.title, ld)
    
  }
  # df is remade to accommodate question labels and the percent scores computed from the sum and distinct count
  df.d <- data.frame(Var1 = ld.title, Freq = round(d.perq/d.dc), Ques = c("Domestic"))
  df.i <- data.frame(Var1 = ld.title, Freq = round(i.perq/i.dc), Ques = c("International"))
  main.df <- rbind(df.d,df.i)
  
  # Subtitle building
  subt <- subt_builder(rc_list, new.dat)
  
  q_data_list <- list("cs", qval, main.df, param_list, subt)
  
  return(q_data_list)
  
}

cs_table_proc <- function(qval, new.dat){
  
  # splitting data by domestic/international
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  sum.field <- get_sum(rc_list)
  rc_list <- rc_eval("cs",rc_list)
  # variable intialization
  ld.title <- c()
  
  # distinct count computation
  i.dc <- 0
  d.dc <- 0
  for (stu in i.dat$ExternalReference) {
    if(!is.na(get(sum.field, i.dat)[i.dat$ExternalReference == stu])){
      if(get(sum.field, i.dat)[i.dat$ExternalReference == stu] == 100){
        i.dc <- i.dc + 1
      }
    }
  }
  
  for (stu in d.dat$ExternalReference) {
    if(!is.na(get(sum.field, d.dat)[d.dat$ExternalReference == stu])){
      if(get(sum.field, d.dat)[d.dat$ExternalReference == stu] == 100){
        d.dc <- d.dc + 1
      }
    }
  }
  
  #df building
  i.perq <- c()
  d.perq <- c()
  for (i in 1:length(rc_list)) {
    resl.d <- 0
    resl.i <- 0
    df.d <- data.frame(table(get(rc_list[i], d.dat)))
    df.i <- data.frame(table(get(rc_list[i], i.dat)))
    # sum computation
    for (j in 1:length(df.d$Freq)) {
      df_v <- as.integer(levels(df.d$Var1)[as.integer(df.d$Var1)])[j]
      df_f <- df.d$Freq[j]
      resl.d <- resl.d + (df_v*df_f)
    }
    for (j in 1:length(df.i$Freq)) {
      df_v <- as.integer(levels(df.i$Var1)[as.integer(df.i$Var1)])[j]
      df_f <- df.i$Freq[j]
      resl.i <- resl.i + (df_v*df_f)
    }
    
    d.perq <- c(d.perq,resl.d)
    i.perq <- c(i.perq,resl.i)
    
    
    if(unlist(gregexpr(pattern =' - ', get(rc_list[i], new.dat) %>% attr('label'))) != -1){
      ld <- substr(get(rc_list[i], new.dat) %>% attr('label'),
                   unlist(gregexpr(pattern =' - ', get(rc_list[i], new.dat) %>% attr('label')))+3,
                   nchar(get(rc_list[i], new.dat) %>% attr('label')))
    }
    else{
      ld <- names(get(rc_list[i], new.dat) %>% attr('label'))
    }
    
    ld.title <- c(ld.title, ld)
    
  }
  # df is remade to accommodate question labels and the percent scores computed from the sum and distinct count
  df.d <- data.frame(Var1 = factor(ld.title,levels = ld.title), Freq = round(d.perq/d.dc), Ques = c("Domestic"))
  df.i <- data.frame(Var1 = factor(ld.title,levels = ld.title), Freq = round(i.perq/i.dc), Ques = c("International"))
  
  if(param_list[6]=="Okanagan"){
    main.df <- data.frame(UBCO = c(ld.title,"Total number of respondents"),
                          Domestic = c(paste0(df.d$Freq,"%"),d.dc),
                          International = c(paste0(df.i$Freq,"%"),i.dc))
  }
  else if(param_list[6]=="Vancouver"){
    main.df <- data.frame(UBCV = c(ld.title,"Total number of respondents"),
                          Domestic = c(paste0(df.d$Freq,"%"),d.dc),
                          International = c(paste0(df.i$Freq,"%"),i.dc))
  }
  else{
    main.df <- data.frame(UBC = c(ld.title,"Total number of respondents"),
                          Domestic = c(paste0(df.d$Freq,"%"),d.dc),
                          International = c(paste0(df.i$Freq,"%"),i.dc))
  }
  
  q_data_list <- list("cs", qval, main.df, param_list)
  
  return(q_data_list)
  
}

mx.tri_graph_proc <- function(qval, new.dat){
  
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  rc_list <- rc_eval("mx",rc_list)
  
  # getting response levels
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  
  # Domestic/international titles and colors
  ti.tle <- title_builder(param_list)
  if(new.dat$isi[1] == "Domestic"){
    col <- c("#3C5A2A","#498325","#89C265")
    ti.tle <- ti.tle <- paste("Domestic",ti.tle,sep = " ")
  }
  else if(new.dat$isi[1] == "ISI"){
    col <- c("#894E09","#BC7521","#FAB484")
    ti.tle <- ti.tle <- paste("International",ti.tle,sep = " ")
  }
  else{
    col <- c("#3C5A2A","#498325","#89C265")
    ti.tle <- ti.tle
  }
  
  # Variable initialization
  df.list <- list()
  prop <- list()
  main.prop <- NULL
  main.df<- data.frame()
  ld.title <- c()
  i <- 0
  
  # Axis question building and formatting
  for (qn in rc_list) {
    label_count_var <- 0
    i <- i + 1
    if(unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label'))) != -1){
      ld <- substr(get(qn, data.ok) %>% attr('label'),
                   unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label')))+3,
                   nchar(get(qn, new.dat) %>% attr('label')))
    }
    else{
      ld <- names(get(qn, new.dat) %>% attr('label'))
    }
    if(nchar(ld)>40){
      ld <- paste0(substr(ld,1,sapply(gregexpr(pattern = " ", substr(ld,1,20)),max)), "\n ",
                   substr(ld,sapply(gregexpr(pattern = " ", substr(ld,1,20)),max)+1,
                          sapply(gregexpr(pattern = " ", substr(ld,1,40)),max)), "\n ",
                   substr(ld,sapply(gregexpr(pattern = " ", substr(ld,1,40)),max)+1,nchar(ld)))
    }else if(nchar(ld)>20){
      ld <- paste0(substr(ld,1,sapply(gregexpr(pattern = " ", substr(ld,1,20)),max)), "\n ",
                   substr(ld,sapply(gregexpr(pattern = " ", substr(ld,1,20)),max)+1,nchar(ld)))
    }
    
    
    ld.title <- c(ld.title, ld)
    
    # Dataframe building
    temp.df <- data.frame(table(get(qn, new.dat)), Ques = c(i))
    temp.df <- complete(temp.df, Var1 = factor(c(1:3),levels = c(1:3)), fill = list(Freq = 0, Ques = c(i)))
    df.list[[i]] <- temp.df
    df.list[[i]]$Ques <- as.factor(df.list[[i]]$Ques)
    
    # Geometry text prep
    prop[[i]] <- round(as.double(100*df.list[[i]]$Freq/sum(df.list[[i]]$Freq)))
    
    for(j in 1:length(prop[[i]])){
      label_count_var <- label_count_var + 1
      prop[[i]][j] = paste0(prop[[i]][j], "%")
    }
    
    # Main dataframe and geom text for plotting
    main.prop <- c(main.prop, prop[[i]])
    main.df <- rbind(main.df, df.list[[i]])
  }
  
  # Subtitle building
  subt <- subt_builder(rc_list, new.dat)
  
  q_data_list <- list("mx.tri", qval, main.df, col, resp, main.prop, ti.tle, subt, ld.title)
  
  return(q_data_list)
  
}

mx.tri_table_proc <- function(qval, new.dat){
  
  # column names for data reads
  cnames <- colnames(new.dat)
  rc_list <- cnames[grepl(qval, cnames, fixed = TRUE)]
  
  # Reading in response levels
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  # Matrix is used for easier structuring of data in the table
  # No. of columns = resp, No. of rows = no. of questions
  mattt <- matrix(rep(1,(length(resp)+0)*length(rc_list)), ncol = length(resp)+0)
  
  # Variable initialization
  df.list <- list()
  main.df<- data.frame()
  ld.main <- c()
  row_tot <- c()
  
  i <- 1
  for (qn in rc_list) {
    # dataframe building
    temp.df <- data.frame(table(get(qn, new.dat)), Ques = c(i))
    temp.df <- complete(temp.df, Var1 = factor(c(1:3),levels = c(1:3)), fill = list(Freq = 0, Ques = c(i)))
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
    
    # Row total to compute and store percentage
    row_tot <- c(row_tot, sum(df.list[[i]]$Freq))
    
    for (j in 1:length(resp)+0) {
      if (!is.na(df.list[[i]]$Freq[j])) {
        mattt[i,j] <- paste0(round(100*df.list[[i]]$Freq[j]/row_tot[i]),"%")
      }
      else
        mattt[i,j] <- "NA"
    }
    
    ld.main <- c(ld.main, ld)
    i <- i + 1
  }
  
  # matrix converted to dataframe
  main.df <- data.frame(mattt)
  
  resp <- (resp)
  colnames(main.df) <- c(resp) # column names assigned to columns 
  c.width <- 1.3
  ft.size <- 6
  
  # Setting table headers for domestic/international data splits; also adds the row totals to the df
  if(param_list[6]=="Okanagan"){
    if(new.dat$isi[1] == "Domestic"){
      main.df <- main.df %>% add_column(`UBCO Domestic` = ld.main, .before = resp[1]) %>%
        add_column(Total = row_tot)
    }
    else if(new.dat$isi[1] == "ISI"){
      main.df <- main.df %>% add_column(`UBCO International` = ld.main, .before = resp[1]) %>%
        add_column(Total = row_tot)
    }
  }
  else if(param_list[6]=="Vancouver"){
    if(new.dat$isi[1] == "Domestic"){
      main.df <- main.df %>% add_column(`UBCV Domestic` = ld.main, .before = resp[1]) %>%
        add_column(Total = row_tot)
    }
    else if(new.dat$isi[1] == "ISI"){
      main.df <- main.df %>% add_column(`UBCV International` = ld.main, .before = resp[1]) %>%
        add_column(Total = row_tot)
    }
  }
  else{
    if(new.dat$isi[1] == "Domestic"){
      main.df <- main.df %>% add_column(`UBC Domestic` = ld.main, .before = resp[1]) %>%
        add_column(Total = row_tot)
    }
    else if(new.dat$isi[1] == "ISI"){
      main.df <- main.df %>% add_column(`UBC International` = ld.main, .before = resp[1]) %>%
        add_column(Total = row_tot)
    }
  }
  
  q_data_list <- list("mx.tri", qval, main.df, ft.size, c.width, mattt)
  
  return(q_data_list)
  
}


# processed_graph_dataList <- list()
# processed_table_dataList <- list()
# 
# processed_graph_dataList[[1]] <- mx_graph_proc("QN105", d.dat)
# processed_graph_dataList[[2]] <- mx_graph_proc("QN104", d.dat)
# processed_graph_dataList[[3]] <- mx_graph_proc("QN100", d.dat)
# processed_graph_dataList[[4]] <- mc_graph_proc("QN44", data.ok)
# processed_graph_dataList[[5]] <- mc.yn_graph_proc("QN94", data.ok)
# processed_graph_dataList[[6]] <- rk_graph_proc("QN98", i.dat)
# processed_graph_dataList[[7]] <- ms_graph_proc("spRestriction", data.ok)
# processed_graph_dataList[[8]] <- cs_graph_proc("QN34", data.ok)
# processed_graph_dataList[[9]] <- mx.tri_graph_proc("commFreq", i.dat)
# 
# processed_table_dataList[[1]] <- mx_table_proc("QN105", d.dat)
# processed_table_dataList[[2]] <- mx_table_proc("QN104", d.dat)
# processed_table_dataList[[3]] <- mx_table_proc("QN100", d.dat)
# processed_table_dataList[[4]] <- mc_table_proc("QN44", data.ok)
# processed_table_dataList[[5]] <- mc.yn_table_proc("QN94", data.ok)
# processed_table_dataList[[6]] <- rk_table_proc("QN98", i.dat)
# processed_table_dataList[[7]] <- ms_table_proc("spRestriction", data.ok) 
# processed_table_dataList[[8]] <- cs_table_proc("QN34", data.ok)
# processed_table_dataList[[9]] <- mx.tri_table_proc("commFreq", i.dat)

# processed_table_dataList[[6]][3]
# 
# str(processed_dataList)
# processed_graph_dataList[[6]]
# rk_graph(processed_graph_dataList[[6]])
# rk_table(processed_table_dataList[[6]])
# 
# save(processed_graph_dataList, processed_table_dataList, file = "processedData.RData")
# load("processedData.RData")
# rm(processed_table_dataList)
