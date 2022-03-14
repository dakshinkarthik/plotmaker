#---------------------Graph and Table functions---------------------


# The main function that accepts calls from the markdown file
main.graph <- function(qval, new.dat){
  
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
      mx(qval,new.dat) 
      tb_mx(qval,new.dat)
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
        mc.yn(qval, new.dat)
        tb_mc.yn(qval, new.dat)
      }
      else{
        # print("3")
        mc(qval, new.dat)
        tb_mc(qval, new.dat)
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
        mx.tri(qval,new.dat)
        tb_mx.tri(qval,new.dat)
      }else{
        # print("5")
        mx(qval,new.dat)
        tb_mx(qval,new.dat)
      }
    }
    # Checks for rk question type
    else if(unlist(gregexpr(pattern = 'rk', rc_list[1])) != -1){
      # print("6")
      rk(qval,new.dat)
      tb_rk(qval,new.dat)
    }
    # Checks for ms question type
    else if(unlist(gregexpr(pattern = 'ms', rc_list[1])) != -1){
      # print("7")
      ms(qval,new.dat)
      tb_ms(qval,new.dat)
    }
    # Checks for cs question types
    else if(unlist(gregexpr(pattern = 'cs', rc_list[1])) != -1){
      # print("8")
      cs(qval,new.dat)
      tb_cs(qval,new.dat)
    }
  }
}

# Graph function for rk question type
rk <- function(qval, new.dat){
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval, new.dat)  
  # input dataset is subsetted based on sum/complete field value
  # 28 is a parameter here because there is only one rk question in the dataset that requires 28 as a validating factor
  # Future code can be changed to accommodate a dynamic parameter value
  new.dat <- rc_complete(rc_list, new.dat, 28) 
  rc_list <- rc_eval("rk",rc_list) # Checks if all the sub questions selected belong to the qID
  
  ti.tle <- NULL
  # The following if-statements check for domestic/international data and assigns the correct title and color scheme for the graph
  if(new.dat$isi[1] == "Domestic"){
    col <- c("#316C1A", "#4C9C2C", "#61AF41", "#76A464", "#92C180", "#ADD99C", "#BFE7B0")
    ti.tle <- "Domestic Direct-Entry Undergraduate Students, UBC Okanagan"
  }
  else if(new.dat$isi[1] == "ISI"){
    col <- c("#A1600A", "#C37918", "#D38622", "#FF940A", "#FFA55D", "#FFB377", "#FFD5A0")
    ti.tle <- "International Direct-Entry Undergraduate Students, UBC Okanagan"
  }
  else{
    col <- c("#002145", "#0055B7", "#00A7E1", "#26C7FF", "#5CD5FF", "#85E0FF", "#A2E7FF")
    ti.tle <- "Direct-Entry Undergraduate Students, UBC Okanagan"
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
    df.list[[i]] <- data.frame(rev(table(get(qn, new.dat))), Ques = c(i))
    df.list[[i]]$Ques <- ld # Question labels added into the dataframe directly

    # Stores the first rank of the sub questions for formatting the ggplot later
    sel <- c(sel,df.list[[i]]$Freq[length(df.list[[i]]$Freq)])
    
    # Geometry text prep
    prop[[i]] <- round(100*df.list[[i]]$Freq/sum(df.list[[i]]$Freq))

    # Geom text has a no character if < 5, else '%' is pasted to it 
    for(j in 1:length(prop[[i]])){
      label_count_var <- label_count_var + 1
      if(as.integer(prop[[i]][j])<5)
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
  for (i in 1:length(sel)) {
    for (j in 1:length(df.list)) {
      # leveler is made up of question labels based on the order of first ranks in sel 
      if(sel[i] == df.list[[j]]$Freq[length(df.list[[j]]$Freq)]){
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
  
}
# rk("QN98",d.dat)

# for mx questions
mx <- function(qval, new.dat){
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval, new.dat)

  # Reading in response levels
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  resp1 <- resp # resp1 is used later to solve dataset inconsistencies
  # resp is reordered to match the order of frequency of responses in the graphs on the reports that were sent out
  resp <- c(unique(resp)[length(unique(resp))],unique(resp)[1:length(unique(resp))-1])
  
  # Domestic/international titles and colors
  ti.tle <- NULL
  if(new.dat$isi[1] == "Domestic"){
    col <- c("#316C1A", "#4C9C2C", "#61AF41", "#76A464", "#92C180", "#ADD99C", "#BFE7B0")
    ti.tle <- "Domestic Direct-Entry Undergraduate Students, UBC Okanagan"
  }
  else if(new.dat$isi[1] == "ISI"){
    col <- c("#A1600A", "#C37918", "#D38622", "#FF940A", "#FFA55D", "#FFB377", "#FFD5A0")
    ti.tle <- "International Direct-Entry Undergraduate Students, UBC Okanagan"
  }
  else{
    col <- c("#002145", "#0055B7", "#00A7E1", "#26C7FF", "#5CD5FF", "#85E0FF", "#A2E7FF")
    ti.tle <- "Direct-Entry Undergraduate Students, UBC Okanagan"
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
    
    if(nchar(ld)>63){
      ld <- paste0(substr(ld,1,sapply(gregexpr(pattern = " ", substr(ld,1,63)),max)), "\n ",
                   substr(ld,sapply(gregexpr(pattern = " ", substr(ld,1,63)),max)+1,nchar(ld)))
    }
    

    
    # Dataframe building
    temp.df <- data.frame(table(get(qn, new.dat)), Ques = c(i))
    
    # The type of response levels of mx questions are either 6 or 7 in number
    # In case of dataset responses missing, complete() is used to fix these inconsistencies
    if(unlist(gregexpr(pattern = 'concerned', resp[length(resp)])) != -1 ||
       unlist(gregexpr(pattern = 'impact', resp[length(resp)])) != -1){
      temp.df <- complete(temp.df, Var1 = factor(c(1:5,999),levels = c(1:5,999)), fill = list(Freq = 0, Ques = c(i)))
      tex.col.base <- rev(c("black","white","white","black","black","black"))
    }
    else{
      temp.df <- complete(temp.df, Var1 = factor(c(1:6,999),levels = c(1:6,999)), fill = list(Freq = 0, Ques = c(i)))
      tex.col.base <- rev(c("black","white","white","white","black","black","black"))
    }
    
    
    if(dim(temp.df)[1] >= 5){
      df.list[[i]] <- temp.df
      ld.title <- c(ld.title, ld)
      df.list[[i]]$Ques <- ld
      
      # Geometry text prep
      prop[[i]] <- round(100*df.list[[i]]$Freq/sum(df.list[[i]]$Freq))
      
      for(j in 1:length(prop[[i]])){
        label_count_var <- label_count_var + 1
        if(prop[[i]][j]<=0){
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
      
      # Main dataframe and geom text for plotting
      main.prop <- c(main.prop, prop[[i]]) 
      main.df <- rbind(main.df, df.list[[i]])
    }
    else{
      i <- i - 1
    }
  }
  
  # some questions did not have "No opinion/NA"; resp1 preserves the order of responses
  # "No opinion/Not applicable" is added if missing
  if(length(unique(main.df$Var1)) != length(resp)){
    resp <- c("No opinion/Not applicable", resp1)
  }
  
  
  # Subtitle building
  subt <- subt_builder(rc_list, new.dat)
  
  # Subtitle positioning and geom text size
  geom_text_size <- sizer(rc_list)[2]
  c.width <- sizer(rc_list)[1]
  
  leveler <- c(unique(main.df$Var1)[length(unique(main.df$Var1))],
               unique(main.df$Var1)[1:(length(unique(main.df$Var1))-1)])

  
  # GGplot graphing
  plot.bar <- ggplot(data = main.df, aes(x=factor(Ques, levels = rev(unique(Ques))), y=Freq,
                                         fill = factor(Var1, levels = leveler))) +
    geom_bar(stat = "identity", position = "fill", width = c.width) + 
    theme_economist(base_size = 14) +
    scale_fill_manual(values = col, guide = guide_legend(reverse = TRUE, nrow = 1), labels = resp) +
    geom_text(data = main.df, aes(Ques, Freq, group = factor(Var1, levels = leveler)),
              label = main.prop,
              position = position_fill(vjust=0.5), color = tex.col, size = geom_text_size) + 
    labs(title = ti.tle,
         subtitle = subt) +
    # scale_x_discrete(breaks = unique(main.df$Ques),
    #                  labels = ld.title) +
    ubc.theme() + 
    coord_flip() # this function was originally built with questions on the x-axis; hence coord_flip() is used
  
  print(plot.bar)
}
# mx("QN65",d.dat)

# for mx tri questions with only 3 response levels
mx.tri <- function(qval, new.dat){
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  rc_list <- rc_eval("mx",rc_list)
  
  # getting response levels
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  
  # Domestic/international titles and colors
  ti.tle <- NULL
  if(new.dat$isi[1] == "Domestic"){
    col <- c("#3C5A2A","#498325","#89C265")
    ti.tle <- "Domestic Direct-Entry Undergraduate Students, UBC Okanagan"
  }
  else if(new.dat$isi[1] == "ISI"){
    col <- c("#894E09","#BC7521","#FAB484")
    ti.tle <- "International Direct-Entry Undergraduate Students, UBC Okanagan"
  }
  else{
    col <- c("#3C5A2A","#498325","#89C265")
    ti.tle <- "Direct-Entry Undergraduate Students, UBC Okanagan"
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
    df.list[[i]] <- data.frame(table(get(qn, new.dat)), Ques = c(i))
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
  
  # GGplot graphing
  plot.bar <- ggplot(data = main.df, aes(x=Ques, y=Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.9) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = col, guide = guide_legend(reverse = FALSE, nrow = 3), labels = resp) +
    geom_text(data = main.df, label = main.prop,
              position = position_dodge(width = 0.9), size = 60, vjust = -1) +
    labs(title = ti.tle,
         subtitle = subt) +
    scale_x_discrete(breaks = unique(main.df$Ques),
                     labels = ld.title) +
    ubc.theme() +
    # This function needs special formatting because it is different from the normal mx graph
    theme(axis.text.x = element_text(family = "serif", size = 180),
          axis.text.y = element_blank(),
          legend.position = c(0.5,0.75),
          legend.spacing.y = unit(3, "in")
          ) +
    scale_y_continuous(limits = c(0, 2 * max(main.df$Freq)))
  
  # Printing plot
  print(plot.bar)
}
# table function for mx.tri question type
tb_mx.tri <- function(qval, new.dat){
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
    df.list[[i]] <- data.frame(table(get(qn, new.dat)))
    
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
  if(new.dat$isi[1] == "Domestic"){
    main.df <- main.df %>% add_column(`UBCO Domestic` = ld.main, .before = resp[1]) %>%
      add_column(Total = row_tot)
  }
  else if(new.dat$isi[1] == "ISI"){
    main.df <- main.df %>% add_column(`UBCO International` = ld.main, .before = resp[1]) %>%
      add_column(Total = row_tot)
  }
  
  # flextable object created from df
  ft <- flextable(main.df) %>% theme_box() # theme_box() adds a box with borders to the table
  ft <- fontsize(ft, size = ft.size, part = "all") # setting font size
  set_table_properties(ft, layout = "autofit") # automatically sets proportion of spacing based on contents
  ft <- align_text_col(ft, align = "center", header = TRUE) %>% # text alignment for characters
    align_nottext_col(align = "center", header = TRUE) # text alingment for non-characters(numbers)
  ft %>% colformat_int(big.mark = "") %>% # Removing separators in numbers
    valign(valign = "center", part = "all") %>% # vertical alignment of contents
    # bg(bg = "grey", part = "all") %>%
    # Adding border and color to the border
    border(border = fp_border_default(color = "#A7A19D", width = 0.8), part = "all") %>%
    width(width = c.width, unit = "in") %>% # cell width
    color(color = "#A7A19D", part = "header") %>% # text color for the header
    color(j = 0:dim(mattt)[2]+2, color = "#A7A19D", part = "body") # text color for the body
  
}

tb_mx <- function(qval, new.dat){
  # column names for data reads
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval,new.dat)
  # reading response levels
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  resp1 <- resp
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
    temp.df <- data.frame(table(get(qn, new.dat)))
    
    if(unlist(gregexpr(pattern = 'concerned', resp[length(resp)])) != -1 ||
       unlist(gregexpr(pattern = 'impact', resp[length(resp)])) != -1){
      temp.df <- complete(temp.df, Var1 = factor(c(1:5,999),levels = c(1:5,999)), fill = list(Freq = 0))
      # tex.col.base <- rev(c("black","white","white","black","black","black"))
    }
    else{
      temp.df <- complete(temp.df, Var1 = factor(c(1:6,999),levels = c(1:6,999)), fill = list(Freq = 0))
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
    i <- i + 1
  }

  main.df <- rev(data.frame(mattt)) # reording columns because the response levels are in the reverse order
  main.df <- cbind(main.df[,2:dim(main.df)[2]],main.df[,1]) # reordering response level 999 to the end of the columns

  # identifier variables to add appropriate cumulative title names in the table
  is_conc <- unlist(gregexpr(pattern = 'concerned', resp[2]))
  is_agr <- unlist(gregexpr(pattern = 'agree', resp[2]))
  is_satis <- unlist(gregexpr(pattern = 'satisfied', resp[2]))
  is_impact <- unlist(gregexpr(pattern = 'impact', resp[2]))

  resp <- rev(resp)
  colnames(main.df) <- c(resp)
  c.width <- 1
  ft.size <- 6
  


  if(new.dat$isi[1] == "Domestic"){
    main.df <- main.df %>% add_column(`UBCO Domestic` = ld.main, .before = resp[1])
  }
  else if(new.dat$isi[1] == "ISI"){
    main.df <- main.df %>% add_column(`UBCO International` = ld.main, .before = resp[1])
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
  
  # flextable object creation and format
  ft <- flextable(main.df) %>% theme_box()
  ft <- fontsize(ft, size = ft.size, part = "all")
  set_table_properties(ft, layout = "autofit")
  ft <- align_text_col(ft, align = "center", header = TRUE) %>%
    align_nottext_col(align = "center", header = TRUE)
  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
    width(width = c.width, unit = "in") %>%
    color(color = "#A7A19D", part = "header") %>%
    color(j = -2:dim(mattt)[2]+4, color = "#A7A19D", part = "body")

}
# tb_mx("QN105",i.dat)

tb_rk <- function(qval, new.dat){
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval, new.dat)  
  new.dat <- rc_complete(rc_list, new.dat, 28)
  rc_list <- rc_eval("rk",rc_list)
  
  # Reading response numeric levels and pasting "Rank" to it
  resp <- paste("Rank", rev(names(get(rc_list[1], new.dat) %>% attr('labels'))), sep = " ")
  # matrix initialization (no. of rows = rc_list, no. of columns = resp)
  mattt <- matrix(rep(1,(length(resp)+0)*(length(rc_list)+1)), ncol = length(resp)+0)
  
  # Variable initialization
  df.list <- list()
  prop <- list()
  main.prop <- NULL
  main.df<- data.frame()
  ld.main <- c()

  
  i <- 1 # loop counter
  for(i in 1:dim(mattt)[1]){
    if(i != dim(mattt)[1]){
      # df building
      df.list[[i]] <- data.frame(table(get(rc_list[i], new.dat)), Ques = c(i))
      
      
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
        if(!is.na(df.list[[i]]$Freq[j])){
          mattt[i,j] <- round(100*df.list[[i]]$Freq[j]/row_tot) 
        }
        else{
          mattt[i,j] <- "NA"
        }
      }
      ld.main <- c(ld.main, ld)
    }
    else{
      for(j in 1:dim(mattt)[2]){
        mattt[i,j] <- nrow(new.dat)
      }
    }
  }
  ld.main <- c(ld.main,"Total")
  main.df <- data.frame(mattt)
  resp <- c(rev(resp))
  colnames(main.df) <- resp
  
  
  if(new.dat$isi[1] == "Domestic"){
    main.df <- main.df %>% add_column(`UBCO Domestic` = ld.main, .before = resp[1])
  }
  else if(new.dat$isi[1] == "ISI"){
    main.df <- main.df %>% add_column(`UBCO International` = ld.main, .before = resp[1])
  }
  main.df <- main.df[-c(dim(main.df)[1]),]
  main.df <- main.df[with(main.df, order(-`Rank 1`)),]

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
  
  for(k in 1:dim(main.df)[1]-1){
    for(j in 2:dim(main.df)[2]){
      main.df[k,j] <- paste0(main.df[k,j],"%")
    }
  }
  
  ft <- flextable(main.df) %>% theme_box()
  ft <- fontsize(ft, size = 5.5, part = "all")
  set_table_properties(ft, layout = "autofit")
  ft <- align_text_col(ft, align = "center", header = TRUE) %>%
    align_nottext_col(align = "center", header = TRUE)
  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
    # width(width = 0.81, unit = "in") %>%
    width(width = 0.5, unit = "in") %>%
    width(j = 1, width = 2.98, unit = "in") %>%
    color(j = 2:dim(mattt)[2]+1, color = "#A7A19D", part = "header") %>%
    color(j = 2:dim(mattt)[2]+1, color = "#A7A19D", part = "all")
  # print(paste0(main.df[1:dim(main.df)[1],2:dim(main.df)[2]]),"%")
  # print(main.df)
}
# tb_rk("QN98",d.dat)

tb_ms <- function(qval, new.dat){
  # Column names to read data
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  cnames <- colnames(new.dat)
  rc_list <- (cnames[grepl(qval, cnames, fixed = TRUE)])
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
  

  
  i.dc <- 0
  d.dc <- 0
  for (stu in i.dat$ExternalReference) {
    for (qn in rc_list) {
      if(!is.na(get(qn, i.dat)[i.dat$ExternalReference == stu])){
        if((get(qn, i.dat)[i.dat$ExternalReference == stu] + 0) == 1){
          i.dc <- i.dc + 1
          break
        }
      }
    }
  }
  
  for (stu in d.dat$ExternalReference) {
    for(qn in rc_list){
      if(!is.na(get(qn, d.dat)[d.dat$ExternalReference == stu])){
        if((get(qn, d.dat)[d.dat$ExternalReference == stu] + 0) == 1){
          d.dc <- d.dc + 1
          break
        }
      }
    }
  }
  
  i <- 1
  j <- 1
  
  for (qn in rc_list) {
    # Dataframe building
    ## Domestic fraction
    axis.c <- names(get(qn, new.dat) %>% attr('labels'))
    axis.q <- c(axis.q,axis.c)
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
        # d.df.list[[i]][2,]$Freq <- round((100*d.df.list[[i]][2,]$Freq/d.dc))
        d.df.list[[i]]$Var1 <- c(axis.c)
        main.df <- rbind(main.df,d.df.list[[i]][2,])
      }
      else{
        if(d.df.list[[i]]$Var1 == 0){
          # i <- i - 1
          d.df.list[[i]] <- data.frame(Var1 = c(axis.c), Freq = c(0), Ques = c("Domestic"))
          main.df <- rbind(main.df,d.df.list[[i]])
        }
        else{
          # d.df.list[[i]]$Freq <- round((100*d.df.list[[i]]$Freq/d.dc))
          d.df.list[[i]]$Var1 <- c(axis.c)
          main.df <- rbind(main.df,d.df.list[[i]])
        }
      }
    }
    ## International fracrtion
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
        # i.df.list[[j]][2,]$Freq <- round((100*i.df.list[[j]][2,]$Freq/i.dc))
        i.df.list[[j]]$Var1 <- c(axis.c)
        main.df <- rbind(main.df,i.df.list[[j]][2,])
      }
      else{
        if(i.df.list[[j]]$Var1 == 0){
          # j <- j - 1
          i.df.list[[j]] <- data.frame(Var1 = c(axis.c), Freq = c(0), Ques = c("International"))
          main.df <- rbind(main.df,i.df.list[[j]])
        }
        else{
          # i.df.list[[j]]$Freq <- round((100*i.df.list[[j]]$Freq/i.dc))
          i.df.list[[j]]$Var1 <- c(axis.c)
          main.df <- rbind(main.df,i.df.list[[j]])
        }
      }
    }
    
    i <- i + 1
    j <- j + 1
  }
  
  l <- 1
  nnull <- c()
  for (qn in 1:dim(main.df)[1]) {
    # print(k)
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


  mattt <- matrix(rep(1,((dim(main.df)[1]/2)+1)*4), ncol = 4)
  
  k <- 1
  for (m in 1:dim(mattt)[1]) {
    if(k < dim(main.df)[1]){
      mattt[m,1] <- paste0(round(100*main.df$Freq[k]/d.dc),"%")
      mattt[m,2] <- main.df$Freq[k]
      mattt[m,3] <- paste0(round(100*main.df$Freq[k+1]/i.dc),"%")
      mattt[m,4] <- main.df$Freq[k+1]
    }
    else{
      mattt[m,1] <- "100%"
      mattt[m,2] <- d.dc
      mattt[m,3] <- "100%"
      mattt[m,4] <- i.dc
    }
    k <- k + 2
  }

  axis.q <- unique(main.df$Var1)

  main.df <- data.frame(mattt)
  main.df <- cbind(UBCO = c(axis.q,"Distinct count of Respondents"), main.df)

  ft <- flextable(main.df) %>% theme_box() %>%
    set_header_labels(X1="%",X2="n",X3="%",X4="n") %>%
    add_header(UBCO = "UBCO", X1 = "Domestic", X2 = "Domestic", X3 = "International", X4 = "International") %>%
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    color(j = c("X1","X2","X3","X4"), color = "#A7A19D", part = "all") %>%
    color(j = "UBCO", color = "#A7A19D", part = "header") %>%
    # color(j = "Domestic", part = "header", color = "#54504C") %>%
    fontsize(size = 6, part = "all") %>%
    align_text_col(align = "center", header = TRUE) %>%
    align_nottext_col(align = "center", header = TRUE)

  set_table_properties(ft, layout = "autofit")

  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
    width(width = 3.5, unit = "in",j = "UBCO")
  
  # print(main.df)
  # print(is.null(nnull))
}
# tb_ms("QN59",data.ok)

tb_mc <- function(qval, new.dat){
  # Column names to read data
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  cnames <- colnames(new.dat)
  rc_list <- (cnames[grepl(paste0(qval,"$"), cnames, fixed = F)])
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  rc_list <- rc_eval("mc",rc_list)
  resp_b <- c()
  
  main.df <- data.frame((table(get(rc_list, new.dat))))
  i.df <- data.frame(table(get(rc_list, i.dat)), Ques = c("International"))
  d.df <- data.frame(table(get(rc_list, d.dat)), Ques = c("Domestic"))
  
  i.df <- complete(i.df, Var1 = main.df$Var1, fill = list(Freq = 0, Ques = c("International")))
  d.df <- complete(d.df, Var1 = main.df$Var1, fill = list(Freq = 0, Ques = c("Domestic")))
  
  resp_b <- c()
  for (qn in main.df$Var1) {
    resp_b <- c(resp_b, as.numeric(qn))
  }
  
  mattt <- matrix(rep(1,4*(length(resp_b)+1)), ncol = 4)
  
  for (j in 1:(dim(mattt)[1])) {
    if(j == dim(mattt)[1]){
      mattt[j,1] <- "100%"
      mattt[j,2] <- sum(d.df$Freq)
      mattt[j,3] <- "100%"
      mattt[j,4] <- sum(i.df$Freq)
    }
    else{
      mattt[j,1] <- paste0(round(100*d.df$Freq[j]/sum(d.df$Freq)),"%")
      mattt[j,2] <- d.df$Freq[j] 
      mattt[j,3] <- paste0(round(100*i.df$Freq[j]/sum(i.df$Freq)),"%")
      mattt[j,4] <- i.df$Freq[j]
    }
  }

  main.df <- data.frame(mattt)
  
  axis.q <- c()
  if(0 %in% resp_b){
    axis.q <- resp
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
  axis.q <- c((axis.q),"Distinct count of respondents")
  
  main.df <- cbind(UBCO = axis.q, main.df)
  
  ft <- flextable(main.df) %>% theme_box() %>%
    set_header_labels(X1="%",X2="n",X3="%",X4="n") %>%
    add_header(UBCO = "UBCO", X1 = "Domestic", X2 = "Domestic", X3 = "International", X4 = "International") %>%
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    color(j = c("X1","X2","X3","X4"), color = "#A7A19D", part = "all") %>%
    color(j = "UBCO", color = "#A7A19D", part = "header") %>%
    # color(j = "Domestic", part = "header", color = "#54504C") %>%
    fontsize(size = 6, part = "all") %>%
    align_text_col(align = "center", header = TRUE) %>%
    align_nottext_col(align = "center", header = TRUE)

  set_table_properties(ft, layout = "autofit")

  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
    width(width = 3.5, unit = "in",j = "UBCO")
  # print(main.df)
}


tb_mc.yn <- function(qval, new.dat){
  # Column names to read data
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  cnames <- colnames(new.dat)
  rc_list <- (cnames[grepl(paste0(qval,"$"), cnames, fixed = F)])
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  resp_b <- c()
  
  main.df <- data.frame(rev(table(get(rc_list, new.dat))))
  i.df <- data.frame((table(get(rc_list, i.dat))), Ques = c("International"))
  d.df <- data.frame((table(get(rc_list, d.dat))), Ques = c("Domestic"))
  
  # Selecting valid choices from the data subset
  resp_b <- c()
  for (qn in main.df$Var1) {
    resp_b <- c(resp_b, as.numeric(qn))
  }
  
  axis.q <- c()
  if(0 %in% resp_b){
    axis.q <- c((resp),"Total")
  }else{
    axis.q <- c((resp[resp_b]),"Total")
  }
  
  mattt <- matrix(rep(1,4*(length(resp_b)+1)), ncol = 4)
  
  for (j in 1:dim(mattt)[1]) {
    if(j == dim(mattt)[1]){
      mattt[j,1] <- "100%"
      mattt[j,2] <- sum(d.df$Freq)
      mattt[j,3] <- "100%"
      mattt[j,4] <- sum(i.df$Freq)
    }
    else{
      mattt[j,1] <- paste0(round(100*d.df$Freq[j]/sum(d.df$Freq)),"%")
      mattt[j,2] <- d.df$Freq[j]
      mattt[j,3] <- paste0(round(100*i.df$Freq[j]/sum(i.df$Freq)),"%")
      mattt[j,4] <- i.df$Freq[j]
    }
  }
  
  main.df <- data.frame(mattt)
  main.df <- cbind(UBCO = axis.q, main.df)
  
  ft <- flextable(main.df) %>% theme_box() %>%
    set_header_labels(X1="%",X2="n",X3="%",X4="n") %>%
    color(j = c("X1","X2","X3","X4"), color = "#A7A19D", part = "all") %>%
    add_header(UBCO = "UBCO", X1 = "Domestic", X2 = "Domestic", X3 = "International", X4 = "International") %>%
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    color(j = "UBCO", color = "#A7A19D", part = "header") %>%
    fontsize(size = 6, part = "all") %>%
    align_text_col(align = "center", header = TRUE) %>%
    align_nottext_col(align = "center", header = TRUE)

  set_table_properties(ft, layout = "autofit")
   
  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
    width(width = 3.5, unit = "in",j = "UBCO")
  # print(main.df)
}
# tb_mc.yn("QN40",data.ok)

# for mc questions
mc <- function(qval, new.dat){
  # Column names to read data
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  cnames <- colnames(new.dat)
  rc_list <- rc_list.get(qval, new.dat)
  rc_list <- rc_eval("mc",rc_list)
  
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  resp.i <- names(get(rc_list[1], i.dat) %>% attr('labels'))
  
  # Variable initialization
  df.list <- list()
  main.df <- NULL
  main.prop <- c()
  tex.col <- c()
  label_count <- length(tex.col)
  ld.title <- c()
  i <- 1
  
  # Dataframe building
  main.df <- data.frame((table(get(rc_list, new.dat))))
  
  i.df <- data.frame(table(get(rc_list, i.dat)), Ques = c("International"))
  i.df$Freq <- round(100*i.df$Freq/sum(i.df$Freq))
  d.df <- data.frame(table(get(rc_list, d.dat)), Ques = c("Domestic"))
  d.df$Freq <- round(100*d.df$Freq/sum(d.df$Freq))
  

  i.df <- complete(i.df, Var1 = main.df$Var1, fill = list(Freq = 0, Ques = c("International")))
  d.df <- complete(d.df, Var1 = main.df$Var1, fill = list(Freq = 0, Ques = c("Domestic")))
  
  
  i.prop <- paste0(i.df$Freq,"%") 
  d.prop <- paste0(d.df$Freq,"%")
  main.prop <- c(d.prop,i.prop)

  
  # Selecting valid choices from the data subset
  resp_b <- c()
  for (qn in main.df$Var1) {
    resp_b <- c(resp_b, as.numeric(qn))
  }

  
  axis.q <- c()
  # axis.q.i <- c()
  # axis.q.d <- c()
  if(0 %in% resp_b){
    axis.q <- resp
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
  
  
  for (j in 1:length(axis.q)) {
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

  d.df$Var1 <- axis.q
  i.df$Var1 <- axis.q

  main.df <- rbind(d.df,i.df)

  tot <- sum(main.df$Freq) #row total

  # Subtitle building
  subt <- subt_builder(rc_list, new.dat)

  # sizing format
  geom_text_size <- sizer(main.df$Var1)[2]
  c.width <- sizer(main.df$Var1)[1]

  plot.bar <- ggplot(data = main.df, aes(x=Freq, y=factor(Var1, levels = rev(unique(Var1))),
                                         fill = factor(Ques, levels = rev(unique(Ques))))) +
    geom_bar(stat = "identity", position = "dodge", width = c.width) +
    theme_economist(base_size = 14) +
    # scale_y_discrete(breaks = levels(main.df$Var1), labels = axis.q) +
    scale_fill_manual(values = c("#FFC279","#579C2C"),
                      guide = guide_legend(reverse = TRUE,nrow = 2)) +
    geom_text(data = main.df, label = main.prop,
              position = position_dodge(width = c.width), size = geom_text_size, hjust = -0.1) +
    labs(title = "Direct-Entry Undergraduate Students, UBC Okanagan",
         subtitle = subt) +
    ubc.theme() +
    theme(legend.position = c(0.85,0.5)) +
    scale_x_continuous(limits = c(0, 2 * max(main.df$Freq)))
    # coord_flip()

  print(plot.bar)
}
# mc("QN58",data.ok)

# for mc.yn questions
mc.yn <- function(qval, new.dat){
  mc(qval,new.dat)
}

# for ms questions
ms <- function(qval, new.dat){
  # Column names to read data
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  cnames <- colnames(new.dat)
  rc_list <- (cnames[grepl(qval, cnames, fixed = TRUE)])
  rc_list <- rc_eval("ms",rc_list)

  i.dc <- 0
  d.dc <- 0
  for (stu in i.dat$ExternalReference) {
    for (qn in rc_list) {
      if(!is.na(get(qn, i.dat)[i.dat$ExternalReference == stu])){
        if((get(qn, i.dat)[i.dat$ExternalReference == stu] + 0) == 1){
          i.dc <- i.dc + 1
          break
        }
      }
    }
  }
  
  for (stu in d.dat$ExternalReference) {
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
    # Dataframe building
    ## Domestic fraction
    axis.c <- names(get(qn, new.dat) %>% attr('labels'))
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
          # i <- i - 1
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
          # j <- j - 1
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
  
  
  # levels(main.df$Ques) <- c("Domestic","International")
  
  k <- 1
  nnull <- c()
  for (qn in 1:dim(main.df)[1]) {
    # print(k)
    if(k < dim(main.df)[1]){
      if(main.df$Freq[k] == 0 & main.df$Freq[k+1] == 0){
        nnull <- c(nnull,k,k+1)
        # print(main.df$Freq[k])
        # print(main.df$Freq[k+1])
      }
    }
    k <- k + 2
  }
  
  if(!is.null(nnull)){
    main.df <- main.df[-nnull,]
  }
  
  for (frq in 1:length(main.df$Freq)){
    if(main.df$Freq < 1){
      main.prop <- c(main.prop,"")
    }
    else{
      main.prop <-  c(main.prop,paste0(main.df$Freq[frq],"%"))
    }
  }

  
  # Subtitle building
  subt <- subt_builder(rc_list, new.dat)

  plot.bar <- ggplot(data = main.df, aes(x=Freq, y=factor(Var1,levels = rev(unique(Var1))),
                                         fill = factor(Ques,levels = rev(unique(Ques))))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
    # geom_col(width=2.5, position=position_dodge(5)) +
    theme_economist(base_size = 14) +
    # scale_y_discrete(breaks = levels(main.df$Var1), labels = axis.q) +
    scale_fill_manual(values = c("#FFC279","#579C2C"),
                      guide = guide_legend(reverse = TRUE,nrow = 2)) +
    geom_text(data = main.df, label = main.prop,
              position = position_dodge(width = 0.8), size = 60, hjust = -0.1) +
    labs(title = "Direct-Entry Undergraduate Students, UBC Okanagan",
         subtitle = subt) +
    ubc.theme() +
    theme(legend.position = c(0.85,0.5)) +
    scale_x_continuous(limits = c(0, 2 * max(main.df$Freq)))
  
  
  # print(new.df)
  # print(str(main.df$Var1))
  print(plot.bar)
}
# ms("spRestriction",data.ok)

cs <- function(qval, new.dat){
  # Column names to read data
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  cnames <- colnames(new.dat)
  rc_list <- (cnames[grepl(qval, cnames, fixed = TRUE)])
  sum.field <- get_sum(rc_list)
  rc_list <- rc_eval("cs",rc_list)
  
  ld.title <- c()
  
  
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
    df.d <- data.frame(table(get(rc_list[i], d.dat)))
    df.i <- data.frame(table(get(rc_list[i], i.dat)))
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
    # resl.vc <- c(resl.vc,resl)
    d.perq <- c(d.perq,resl.d)
    i.perq <- c(i.perq,resl.i)
    

    # print(names(get(rc_list[i], data.ok) %>% attr('labels')))
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
  df.d <- data.frame(Var1 = ld.title, Freq = round(d.perq/d.dc), Ques = c("Domestic"))
  df.i <- data.frame(Var1 = ld.title, Freq = round(i.perq/i.dc), Ques = c("International"))
  main.df <- rbind(df.d,df.i)
  
  # Subtitle building
  subt <- subt_builder(rc_list, new.dat)
  
  # factor(Var1,levels = rev(unique(Var1))) reorder(Var1, Freq)
  
  plot.bar <- ggplot(data = main.df, aes(x=Freq, y=factor(Var1,levels = rev(unique(Var1))),
                                         fill = factor(Ques,levels = rev(unique(Ques))))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
    # geom_col(width=2.5, position=position_dodge(5)) +
    theme_economist(base_size = 14) +
    # scale_y_discrete(breaks = levels(main.df$Var1), labels = axis.q) +
    scale_fill_manual(values = c("#FFC279","#579C2C"),
                      guide = guide_legend(reverse = TRUE,nrow = 2)) +
    geom_text(data = main.df, label = paste0(main.df$Freq,"%"),
              position = position_dodge(width = 0.8), size = 60, hjust = -0.1) +
    labs(title = "Direct-Entry Undergraduate Students, UBC Okanagan",
         subtitle = subt) +
    ubc.theme() +
    theme(legend.position = c(0.85,0.5)) +
    scale_x_continuous(limits = c(0, 2 * max(main.df$Freq)))
  
  # print(df.d)
  # print(main.df)
  print(plot.bar)
  
}
# cs("QN34",data.ok)

tb_cs <- function(qval, new.dat){
  # Column names to read data
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  cnames <- colnames(new.dat)
  rc_list <- (cnames[grepl(qval, cnames, fixed = TRUE)])
  sum.field <- get_sum(rc_list)
  rc_list <- rc_eval("cs",rc_list)
  
  ld.title <- c()
  
  
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
    df.d <- data.frame(table(get(rc_list[i], d.dat)))
    df.i <- data.frame(table(get(rc_list[i], i.dat)))
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
    # resl.vc <- c(resl.vc,resl)
    d.perq <- c(d.perq,resl.d)
    i.perq <- c(i.perq,resl.i)
    
    
    # print(names(get(rc_list[i], data.ok) %>% attr('labels')))
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
  df.d <- data.frame(Var1 = factor(ld.title,levels = ld.title), Freq = round(d.perq/d.dc), Ques = c("Domestic"))
  df.i <- data.frame(Var1 = factor(ld.title,levels = ld.title), Freq = round(i.perq/i.dc), Ques = c("International"))

  
  # mattt <- matrix(rep(1,length(rc_list)*2), ncol = 2)
  # 
  # for (i in 1:dim(mattt)[1]) {
  #   mattt[i,1] <- paste0(df.d$Freq[i],"%")
  #   mattt[i,2] <- paste0(df.i$Freq[i],"%")
  # }
  # main.df <- data.frame(mattt)
  main.df <- data.frame(UBCO = c(ld.title,"Total number of respondents"),
                        Domestic = c(paste0(df.d$Freq,"%"),d.dc),
                        International = c(paste0(df.i$Freq,"%"),i.dc))
  
  ft <- flextable(main.df) %>% theme_box() %>%
    color(j = c("Domestic","International"), color = "#A7A19D", part = "all") %>%
    color(j = "UBCO", color = "#A7A19D", part = "header") %>%
    # color(j = "Domestic", part = "header", color = "#54504C") %>%
    fontsize(size = 6, part = "all") %>%
    align_text_col(align = "center", header = TRUE) %>%
    align_nottext_col(align = "center", header = TRUE)
  
  set_table_properties(ft, layout = "autofit")
  
  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
    width(width = 5, unit = "in",j = "UBCO")
  
  # print(main.df)
}
# tb_cs("QN34",data.ok)
#-----------------------------------HELPER FUNCTIONS----------------------------------------

subt_builder <- function(rc_list, new.dat){
  subt <- get(rc_list[1], new.dat) %>% attr('label')
  subt_how <- unlist(gregexpr(pattern ='How', get(rc_list[1], new.dat) %>% attr('label')))
  subt_to <- unlist(gregexpr(pattern ='To', get(rc_list[1], new.dat) %>% attr('label')))
  subt_where <- unlist(gregexpr(pattern ='Where', get(rc_list[1], new.dat) %>% attr('label')))
  subt_hyp <- unlist(gregexpr(pattern =' - ', get(rc_list[1], new.dat) %>% attr('label')))
  subt_are <- unlist(gregexpr(pattern ='Are', get(rc_list[1], new.dat) %>% attr('label')))
  end_apply <- unlist(gregexpr(pattern ='apply', get(rc_list[1], new.dat) %>% attr('label')))
  
  if(end_apply == -1){
    if(subt_how != -1){
      subt <- substr(get(rc_list[1], new.dat) %>% attr('label'),subt_how,
                     unlist(gregexpr(pattern ='\\?', get(rc_list[1], new.dat) %>% attr('label'))))
      # print(1)
    }else if(subt_to != -1){
      subt <- substr(get(rc_list[1], new.dat) %>% attr('label'),subt_to,
                     unlist(gregexpr(pattern ='\\?', get(rc_list[1], new.dat) %>% attr('label'))))
      # print(2)
    }else if(subt_where != -1){
      subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_where,
                     unlist(gregexpr(pattern ='\\?', get(rc_list[1], new.dat) %>% attr('label'))))
      # print(3)
    }else if(subt_are != -1){
      subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_are,
                     unlist(gregexpr(pattern ='\\?', get(rc_list[1], new.dat) %>% attr('label'))))
      # print(4)
    }else if(subt_hyp != -1){
      subt <-  substr(get(rc_list[1], new.dat) %>% attr('label'),1,
                      unlist(gregexpr(pattern =' - ', get(rc_list[1], new.dat) %>% attr('label')))-1)
      # print(5)
    }
    else if(subt_how == -1 || subt_to == -1 || subt_where == -1 || subt_hyp == -1 || subt_are == -1){
      subt <- get(rc_list[1], new.dat) %>% attr('label')
      # print(6)
    }else{
      subt <- "Unidentified subtitle format"
    }
  }
  else{
    if(subt_how != -1){
      subt <- substr(get(rc_list[1], new.dat) %>% attr('label'),subt_how,
                     end_apply+5)
      # print(7)
    }else if(subt_to != -1){
      subt <- substr(get(rc_list[1], new.dat) %>% attr('label'),subt_to,
                     end_apply+5)
      # print(8)
    }else if(subt_where != -1){
      subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_where,
                     end_apply+5)
      # print(9)
    }else if(subt_are != -1){
      subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_are,
                     end_apply+5)
      # print(10)
    }else if(subt_hyp != -1){
      subt <-  substr(get(rc_list[1], new.dat) %>% attr('label'),1,
                      unlist(gregexpr(pattern =' - ', get(rc_list[1], new.dat) %>% attr('label')))-1)
      # print(11)
    }
    # else if(subt_how == -1 || subt_to == -1 || subt_where == -1 || subt_hyp == -1 || subt_are == -1){
    #   subt <- get(rc_list[1], new.dat) %>% attr('label')
    # }
    else if(end_apply != -1){
      subt <-  substr(get(rc_list[1], new.dat) %>% attr('label'),1,
                      end_apply)
      # print(12)
    }else{
      subt <- "Unidentified subtitle format"
    }
  }
  
  if(nchar(subt)>58){
    subt <- paste0(substr(subt,1,sapply(gregexpr(pattern = " ", substr(subt,1,58)),max)), "\n ",
                 substr(subt,sapply(gregexpr(pattern = " ", substr(subt,1,58)),max)+1,nchar(subt)))
  }
  
  return(subt)
}

addline_format <- function(x,...){
  gsub('\\s',' ',x)
}

flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}

ubc.theme <- function(){
  return(theme(text = element_text(family = "Calibri"),
               legend.position = c(0.15,0.98),
               legend.direction = "horizontal",
               legend.title = element_blank(),
               legend.key.height = unit(2, 'cm'),
               legend.key.width = unit(4, 'cm'),
               legend.text = element_text(size = 160),
               legend.spacing.x = unit(2, "cm"),
               legend.spacing.y = unit(1, "cm"),
               legend.box.spacing = unit(2, "cm"),
               plot.background = element_rect(colour = "grey", fill = NA, size = 6),
               plot.subtitle = element_text(colour = "#54504C", size = 170, hjust = 0.5),
               plot.title.position = "plot",
               plot.title = element_text(color = "#2B73C2", size = 175, hjust = 0.5),
               axis.line = element_blank(),
               axis.text.x = element_blank(),
               axis.text.y = element_text(family = "serif", size = 200, hjust = 0.5),
               axis.ticks = element_blank(),
               axis.title.x = element_blank(),
               axis.title.y = element_blank()))
}

rc_eval <- function(eval.st,rc_list){
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
      else{
        bl <- c(bl,TRUE)
      }
    }else{
      bl <- c(bl,FALSE)
    }
  }
  return(rc_list[bl])
}

get_complete <- function(rc_list){
  bl <- c()
  for(j in 1:length(rc_list)){
    if(unlist(gregexpr(pattern = "Complete", rc_list[j])) != -1 ||
       unlist(gregexpr(pattern = "complete", rc_list[j])) != -1){
      bl <- c(bl,TRUE)
    }
    else{
      bl <- c(bl,FALSE)
    }
  }
  return(rc_list[bl])
}

get_sum <-  function(rc_list){
  bl <- c()
  for(j in 1:length(rc_list)){
    if(unlist(gregexpr(pattern = "Sum", rc_list[j])) != -1 ||
       unlist(gregexpr(pattern = "sum", rc_list[j])) != -1){
      bl <- c(bl,TRUE)
    }
    else{
      bl <- c(bl,FALSE)
    }
  }
  return(rc_list[bl])
}

rc_complete <- function(rc_list, new.dat, comp_val = 1){
  chk <- 0
  for (j in 1:length(rc_list)) {
    if(unlist(gregexpr(pattern = "complete", rc_list[j])) != -1){
      chk <- 1
      return(new.dat[which(get(rc_list[j], new.dat) == comp_val),])
    }
  }
  if(chk == 0)
    return(new.dat)
}

sizer <- function(rc_list){
  geom_text_size <- NULL
  c.width <- NULL
  
  if(length(rc_list) <= 2){
    geom_text_size <- 75
    c.width <- 0.15
  }else if(length(rc_list) <= 3){
    geom_text_size <- 90
    c.width <- 0.2
  }else if(length(rc_list) <= 6){ # 
    geom_text_size <- 75
    c.width <- 0.7
  }
  else{
    geom_text_size <- 50
    c.width <- 0.5
  }
  
  return(c(c.width,geom_text_size))
}

rc_list.get <- function(qval, new.dat){
  cnames <- colnames(new.dat)
  rc_list <- cnames[grepl(paste0(qval,"$"), cnames, fixed = F)]
  
  if(length(rc_list) == 0){
    rc_list <- cnames[grepl(paste0(qval,""), cnames, fixed = T)]
  }
  
  if(length(rc_list) != 1){
    rc_list <- c(cnames[grepl(paste0(qval,"_"), cnames, fixed = T)],
                 cnames[grepl(paste0(qval,"c"), cnames, fixed = T)],
                 cnames[grepl(paste0(qval,"C"), cnames, fixed = T)],
                 cnames[grepl(paste0(qval,"s"), cnames, fixed = T)],
                 cnames[grepl(paste0(qval,"S"), cnames, fixed = T)])
  }
  return(rc_list)
}


