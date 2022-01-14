addline_format <- function(x,...){
  gsub('\\s',' ',x)
}

flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}

# Plot theme formatting
ubc.theme <-  theme(text = element_text(family = "calibri"),
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
                       plot.subtitle = element_text(size = 160),
                       plot.title.position = "plot",
                       plot.title = element_text(color = "#2B73C2", size = 175, hjust = 0.5),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_text(family = "serif", face = "bold", size = 200, hjust = 1),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank())


mx <- function(qval, new.dat){
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rev(cnames[grepl(qval, cnames, fixed = TRUE)])
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  
  # Variable initialization
  df.list <- list()
  prop <- list()
  main.prop <- NULL
  main.df<- data.frame()
  col <- rev(c("#002145", "#0055B7", "#00A7E1", "#26C7FF", "#5CD5FF", "#85E0FF", "#A2E7FF"))
  tex.col.base <- rev(c("white","white","black","black","black","black"))
  tex.col <- c()
  label_count <- length(tex.col)
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
      ld <- get(qn, new.dat) %>% attr('label')
    }
    
    if(nchar(ld)>63){
      ld <- paste0(substr(ld,1,sapply(gregexpr(pattern = " ", substr(ld,1,63)),max)), "\n ",
                   substr(ld,sapply(gregexpr(pattern = " ", substr(ld,1,63)),max)+1,nchar(ld)))
    }
    
    ld.title <- c(ld.title, ld)
    
    # Dataframe building
    df.list[[i]] <- data.frame(table(get(qn, data.ok)), Ques = c(i))
    levels(df.list[[i]]) <- factor(resp)
    df.list[[i]]$Ques <- as.factor(df.list[[i]]$Ques)
    
    # Geometry text prep
    prop[[i]] <- floor(as.double(100*df.list[[i]]$Freq/sum(df.list[[i]]$Freq)))
    
    for(j in 1:length(prop[[i]])){
      label_count_var <- label_count_var + 1
      if(as.integer(prop[[i]][j])<5)
        prop[[i]][j] = ''
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
  
  # Subtitle building
  subt <- "Subtitle"
  subt_how <- unlist(gregexpr(pattern ='How', get(rc_list[1], data.ok) %>% attr('label')))
  subt_to <- unlist(gregexpr(pattern ='To', get(rc_list[1], data.ok) %>% attr('label')))
  
  if(subt_how != -1){
    subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_how,
                   unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))
  }else if(subt_to != -1){
    subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_to,
                   unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))
  }else if(subt_how == -1 || subt_to == -1){
    subt <-  substr(get(rc_list[1], data.ok) %>% attr('label'),1,
                    unlist(gregexpr(pattern =' - ', get(rc_list[1], data.ok) %>% attr('label')))-1)
  }
  else{
    subt <- "Unidentified subtitle format"
  }
  
  # Subtitle positioning and geom text size
  geom_text_size <- NULL
  c.width <- NULL
  
  if(length(rc_list) == 1){
    geom_text_size <- 75
    c.width <- 0.15
  }else if(length(rc_list) <= 3){
    geom_text_size <- 90
    c.width <- 0.5
  }else if(length(rc_list) <= 6){
    geom_text_size <- 75
    c.width <- 0.7
  }else{
    geom_text_size <- 50
    c.width <- 0.5
  }
  
  # GGplot graphing
  plot.bar <- ggplot(data = main.df, aes(x=Ques, y=Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "fill", width = c.width) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = col, guide = guide_legend(reverse = TRUE, nrow = 1), labels = resp) +
    geom_text(data = main.df, aes(Ques, Freq, group = Var1), label = main.prop,
              position = position_fill(vjust=0.5), color = tex.col, size = geom_text_size) +
    labs(title = "Direct-Entry Undergraduate Students, UBC Okanagan",
         subtitle = subt) +
    scale_x_discrete(breaks = unique(main.df$Ques),
                     labels = ld.title) +
    ubc.theme + 
    # theme(plot.subtitle = element_text(hjust = sidestep)) +
    coord_flip()
  
  # Printing plot
  print(plot.bar)
}

mx.tri <- function(qval, new.dat){
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- (cnames[grepl(qval, cnames, fixed = TRUE)])
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  
  # Variable initialization
  df.list <- list()
  prop <- list()
  main.prop <- NULL
  main.df<- data.frame()
  col <- c("#3C5A2A","#498325","#89C265")
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
    df.list[[i]] <- data.frame(table(get(qn, data.ok)), Ques = c(i))
    levels(df.list[[i]]) <- factor(resp)
    df.list[[i]]$Ques <- as.factor(df.list[[i]]$Ques)
    
    # Geometry text prep
    prop[[i]] <- floor(as.double(100*df.list[[i]]$Freq/sum(df.list[[i]]$Freq)))
    
    for(j in 1:length(prop[[i]])){
      label_count_var <- label_count_var + 1
      prop[[i]][j] = paste0(prop[[i]][j], "%")
    }
    
    # Main dataframe and geom text for plotting
    main.prop <- c(main.prop, prop[[i]])
    main.df <- rbind(main.df, df.list[[i]])
  }
  
  # Subtitle building
  subt <- "Subtitle"
  subt_how <- unlist(gregexpr(pattern ='How', get(rc_list[1], data.ok) %>% attr('label')))
  subt_to <- unlist(gregexpr(pattern ='To', get(rc_list[1], data.ok) %>% attr('label')))
  
  if(subt_how != -1){
    subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_how,
                   unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))
  }else if(subt_to != -1){
    subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_to,
                   unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))
  }else if(subt_how == -1 || subt_to == -1){
    subt <-  substr(get(rc_list[1], data.ok) %>% attr('label'),1,
                    unlist(gregexpr(pattern =' - ', get(rc_list[1], data.ok) %>% attr('label')))-1)
  }
  else{
    subt <- "Unidentified subtitle format"
  }
  
  # GGplot graphing
  plot.bar <- ggplot(data = main.df, aes(x=Ques, y=Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.9) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = col, guide = guide_legend(reverse = FALSE, nrow = 3), labels = resp) +
    geom_text(data = main.df, label = main.prop,
              position = position_dodge(width = 0.9), size = 60, vjust = -1) +
    labs(title = "Direct-Entry Undergraduate Students, UBC Okanagan",
         subtitle = subt) +
    scale_x_discrete(breaks = unique(main.df$Ques),
                     labels = ld.title) +
    ubc.theme +
    theme(axis.text.x = element_text(family = "serif", face = "bold", size = 180),
          axis.text.y = element_blank(),
          legend.position = c(0.5,0.75),
          legend.spacing.y = unit(3, "in")
          ) +
    scale_y_continuous(limits = c(0, 2 * max(main.df$Freq)))
  
  # Printing plot
  print(plot.bar)
}

tb_mx.tri <- function(qval, new.dat){
  cnames <- colnames(new.dat)
  rc_list <- cnames[grepl(qval, cnames, fixed = TRUE)]
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  mattt <- matrix(rep(1,(length(resp)+0)*length(rc_list)), ncol = length(resp)+0)
  
  df.list <- list()
  main.df<- data.frame()
  ld.main <- c()
  row_tot <- c()
  
  i <- 1
  for (qn in rc_list) {
    df.list[[i]] <- data.frame(table(get(qn, data.ok)))
    
    #Row labels
    if(unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label'))) != -1){
      ld <- substr(get(qn, data.ok) %>% attr('label'),
                   unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label')))+3,
                   nchar(get(qn, new.dat) %>% attr('label')))
    }
    else{
      ld <- get(qn, new.dat) %>% attr('label')
    }
    
    row_tot <- c(row_tot, sum(df.list[[i]]$Freq))
    
    for (j in 1:length(resp)+0) {
      if (!is.na(df.list[[i]]$Freq[j])) {
        mattt[i,j] <- paste0(floor(100*df.list[[i]]$Freq[j]/row_tot[i]),"%")
      }
      else
        mattt[i,j] <- "NA"
    }
    
    ld.main <- c(ld.main, ld)
    i <- i + 1
  }
  
  main.df <- data.frame(mattt)
  
  resp <- (resp)
  colnames(main.df) <- c(resp)
  c.width <- 1.3
  ft.size <- 6
  
  main.df <- main.df %>% add_column(UBCO = ld.main, .before = resp[1]) %>%
    add_column(Total = row_tot)
  
  ft <- flextable(main.df) %>% theme_box()
  ft <- fontsize(ft, size = ft.size, part = "all")
  set_table_properties(ft, layout = "autofit")
  ft <- align_text_col(ft, align = "center", header = TRUE) %>%
    align_nottext_col(align = "center", header = TRUE)
  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    # bg(bg = "grey", part = "all") %>%
    border(border = fp_border_default(color = "grey", width = 1), part = "all") %>%
    width(width = c.width, unit = "in")
  
  
}


tb_mx <- function(qval, new.dat){
  cnames <- colnames(new.dat)
  rc_list <- cnames[grepl(qval, cnames, fixed = TRUE)]
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  mattt <- matrix(rep(1,(length(resp)+0)*length(rc_list)), ncol = length(resp)+0)
  
  df.list <- list()
  main.df<- data.frame()
  ld.main <- c()
  row_tot <- c()
  c_vc <- c()
  c_vc_sc <- c()
  
  i <- 1
  for (qn in rc_list) {
    df.list[[i]] <- data.frame(table(get(qn, data.ok)))
    
    #Row labels
    if(unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label'))) != -1){
      ld <- substr(get(qn, data.ok) %>% attr('label'),
                   unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label')))+3,
                   nchar(get(qn, new.dat) %>% attr('label')))
    }
    else{
      ld <- get(qn, new.dat) %>% attr('label')
    }

    
    row_tot <- c(row_tot, sum(df.list[[i]]$Freq))
    #To calculate the cumulative top 2 and 3 response levels
    c_vc.sum <- 0
    c_vc_sc.sum <- 0
    for (j in 1:length(resp)+0) {

      if(!is.na(df.list[[i]]$Freq[j])){
        mattt[i,j] <- paste0(floor(100*df.list[[i]]$Freq[j]/row_tot[i]),"%")
        if(unlist(gregexpr(pattern = 'concerned', resp[1])) != -1){
          if(j==3){
            c_vc_sc.sum <- c_vc_sc.sum + df.list[[i]]$Freq[j]
          }
          if(j>=4 && j<=5){
            c_vc.sum <- c_vc.sum + df.list[[i]]$Freq[j]
            c_vc_sc.sum <- c_vc_sc.sum + df.list[[i]]$Freq[j]
          }
        }
        else if(unlist(gregexpr(pattern = 'agree', resp[1])) != -1 || 
                unlist(gregexpr(pattern = 'satisfied', resp[1])) != -1){
          if(j==4){
            c_vc_sc.sum <- c_vc_sc.sum + df.list[[i]]$Freq[j]
          }
          if(j>=5 && j<=6){
            c_vc.sum <- c_vc.sum + df.list[[i]]$Freq[j]
            c_vc_sc.sum <- c_vc_sc.sum + df.list[[i]]$Freq[j]
          }
        }
      }
      else
        mattt[i,j] <- "NA"
    }
    ld.main <- c(ld.main, ld)
    c_vc <- c(c_vc, paste0(floor(100*c_vc.sum/row_tot[i]),"%"))
    c_vc_sc <- c(c_vc_sc, paste0(floor(100*c_vc_sc.sum/row_tot[i]),"%"))
    i <- i + 1
  }
  
  main.df <- rev(data.frame(mattt))
  # main.df <- data.frame(mattt)
  is_conc <- unlist(gregexpr(pattern = 'concerned', resp[1]))
  is_agr <- unlist(gregexpr(pattern = 'agree', resp[1]))
  is_satis <- unlist(gregexpr(pattern = 'satisfied', resp[1]))
  
  resp <- rev(resp)
  colnames(main.df) <- c(resp)
  c.width <- 1
  ft.size <- 6
  
  main.df <- main.df %>% add_column(UBCO = ld.main, .before = resp[1])
  
  if(is_conc != -1){
    main.df <- main.df %>% add_column(`Very concerned/Concerned` = c_vc, .after = resp[1]) %>%
      add_column(`Including somewhat concerned` = c_vc_sc, .after = "Very concerned/Concerned") %>%
      add_column(Total = row_tot)
    c.width <- 0.65
    ft.size <- 6
  }else if(is_agr != -1){
    main.df <- main.df %>% add_column(`Strongly agree/\nAgree` = c_vc, .after = resp[1]) %>%
      add_column(`Including somewhat agree` = c_vc_sc, .after = "Strongly agree/\nAgree") %>%
      add_column(Total = row_tot)
    c.width <- 0.59
    ft.size <- 5.5
  }else if(is_satis != -1){
    main.df <- main.df %>% add_column(`Very satisfied/\nSatisfied` = c_vc, .after = resp[1]) %>%
      add_column(`Including somewhat satisfied` = c_vc_sc, .after = "Very satisfied/\nSatisfied") %>%
      add_column(Total = row_tot)
    c.width <- 0.59
    ft.size <- 5.5
  }

  ft <- flextable(main.df) %>% theme_box()
  ft <- fontsize(ft, size = ft.size, part = "all")
  set_table_properties(ft, layout = "autofit")
  ft <- align_text_col(ft, align = "center", header = TRUE) %>%
    align_nottext_col(align = "center", header = TRUE)
  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    # bg(bg = "grey", part = "all") %>%
    border(border = fp_border_default(color = "grey"), part = "all") %>%
    width(width = c.width, unit = "in")
}


mc <- function(qval, new.dat){
  # Column names to read data
  i.dat <- new.dat[which(new.dat$isi == "ISI"),]
  d.dat <- new.dat[which(new.dat$isi == "Domestic"),]
  cnames <- colnames(new.dat)
  rc_list <- (cnames[grepl(qval, cnames, fixed = TRUE)])
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  resp_b <- c()
  
  # Variable initialization
  df.list <- list()
  main.df <- NULL
  prop <- list()
  main.prop <- c()
  main.df<- data.frame()
  col <- rev(c("#002145", "#0055B7", "#00A7E1", "#26C7FF", "#5CD5FF", "#85E0FF", "#A2E7FF"))
  tex.col.base <- rev(c("white","white","black","black","black","black"))
  tex.col <- c()
  label_count <- length(tex.col)
  ld.title <- c()
  i <- 1
  
  # Dataframe building
  main.df <- data.frame(table(get(rc_list, new.dat)))
  i.df <- data.frame(table(get(rc_list, i.dat)), Ques = c("International"))
  d.df <- data.frame(table(get(rc_list, d.dat)), Ques = c("Domestic"))
  
  i.prop <- paste0(floor(100*i.df$Freq/sum(i.df$Freq)),"%") 
  d.prop <- paste0(floor(100*d.df$Freq/sum(d.df$Freq)),"%")
  main.prop <- c(d.prop,i.prop)

  
  # Selecting valid choices from the data subset
  resp_b <- c()
  for (qn in main.df$Var1) {
    resp_b <- c(resp_b, as.numeric(qn))
  }
  
  axis.q <- c()
  axis.q <- resp[resp_b]
  
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
  
  main.df <- rbind(d.df,i.df)
  levels(main.df) <- factor(axis.q)
  main.df$Ques <- as.factor(main.df$Ques)
  # main.df <- main.df[nrow(main.df):1,]
  
  tot <- sum(main.df$Freq) #row total
  
  # Subtitle building
  subt <- "Subtitle"
  subt_how <- unlist(gregexpr(pattern ='How', get(rc_list[1], data.ok) %>% attr('label')))
  subt_to <- unlist(gregexpr(pattern ='To', get(rc_list[1], data.ok) %>% attr('label')))
  subt_where <- unlist(gregexpr(pattern ='Where', get(rc_list[1], data.ok) %>% attr('label')))
  
  if(subt_how != -1){
    subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_how,
                   unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))
  }else if(subt_to != -1){
    subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_to,
                   unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))
  }else if(subt_where != -1){
    subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),subt_to,
                   unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))
  }else if(subt_how == -1 || subt_to == -1 || subt_where == -1){
    subt <-  substr(get(rc_list[1], data.ok) %>% attr('label'),1,
                    unlist(gregexpr(pattern =' - ', get(rc_list[1], data.ok) %>% attr('label')))-1)
  }
  else{
    subt <- "Unidentified subtitle format"
  }
  
  plot.bar <- ggplot(data = main.df, aes(x=Var1, y=Freq, fill = Ques)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5) +
    theme_economist(base_size = 14) +
    scale_x_discrete(breaks = levels(main.df$Var1), labels = axis.q) +
    scale_fill_manual(values = c("#579C2C","#FFC279"),
                      guide = guide_legend(reverse = FALSE)) +
    geom_text(data = main.df, label = main.prop,
              position = position_dodge(width = 0.5), size = 60, vjust = -1) +
    labs(title = "Direct-Entry Undergraduate Students, UBC Okanagan",
         subtitle = subt) +
    ubc.theme +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(family = "serif", face = "bold", size = 200),
          legend.position = c(0.5,0.75)) +
    scale_y_continuous(limits = c(0, 2 * max(main.df$Freq)))
  
  print(plot.bar)
}
# mc("reside",data.ok)


