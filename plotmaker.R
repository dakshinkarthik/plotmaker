addline_format <- function(x,...){
  gsub('\\s',' ',x)
}

flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}

# Plot theme formatting
ubc.theme.ld <-  theme(legend.position = c(0.07,0.02),
                       legend.direction = "horizontal",
                       legend.title = element_blank(),
                       legend.key.height = unit(2, 'cm'),
                       legend.key.width = unit(4, 'cm'),
                       legend.text = element_text(size = 175),
                       legend.spacing.x = unit(2, "cm"),
                       legend.spacing.y = unit(1, "cm"),
                       legend.box.spacing = unit(2, "cm"),
                       plot.background = element_rect(colour = "grey", fill = NA, size = 2),
                       plot.subtitle = element_text(size = 150),
                       plot.title.position = "plot",
                       plot.title = element_text(color = "#2B73C2", size = 175, hjust = 0.5),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_text(family = "serif", size = 200, hjust = 1),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank())






mx <- function(qval, new.dat){
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- cnames[grepl(qval, cnames, fixed = TRUE)]
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
    ld <- substr(get(qn, data.ok) %>% attr('label'),
                 unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label')))+3,
                 nchar(get(qn, new.dat) %>% attr('label')))
    
    if (nchar(ld)>63) {
      ld <- paste0(substr(ld,1,sapply(gregexpr(pattern = " ", substr(ld,1,63)),max)), "\n ",
                   substr(ld,sapply(gregexpr(pattern = " ", substr(ld,1,63)),max)+1,nchar(ld)))
    }
    
    ld.title <- c(ld.title, ld)
    
    # Datafram building
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
    if (label_count_var == label_count) {
      tex.col <- c(tex.col, tex.col.base)
    }
    else {
      tex.col <- c(tex.col, tex.col.base[1:label_count_var])
    }
    
    # Main dataframe and geom text for plotting
    main.prop <- c(main.prop, prop[[i]])
    main.df <- rbind(main.df, df.list[[i]])
  }
  
  # Subtitle building
  subt <- "Subtitle"
  subt_index <- unlist(gregexpr(pattern ='How', get(rc_list[1], data.ok) %>% attr('label')))
  
  if(unlist(gregexpr(pattern ='How', get(rc_list[1], data.ok) %>% attr('label'))) == -1 ||
     unlist(gregexpr(pattern ='To', get(rc_list[1], data.ok) %>% attr('label')))){
    subt <-  substr(get(rc_list[1], data.ok) %>% attr('label'),1,
                    unlist(gregexpr(pattern =' - ', get(rc_list[1], data.ok) %>% attr('label')))-1)
  }
  
  else if (unlist(gregexpr(pattern ='How', get(rc_list[1], data.ok) %>% attr('label')))){
    subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),
           unlist(gregexpr(pattern ='How', get(rc_list[1], data.ok) %>% attr('label'))),
           unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))
  }
  else if (unlist(gregexpr(pattern ='To', get(rc_list[1], data.ok) %>% attr('label')))){
    subt <- substr(get(rc_list[1], data.ok) %>% attr('label'),
                   unlist(gregexpr(pattern ='To', get(rc_list[1], data.ok) %>% attr('label'))),
                   unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))
  }
  else {
    subt <- "Unidentified subtitle format"
  }
  
  # Subtitle positioning and geom text size
  sidestep <- NULL
  geom_text_size <- NULL
  if(length(rc_list) <= 3) {
    geom_text_size <- 90
    sidestep <- -1
  } else if (length(rc_list) <= 6) {
    geom_text_size <- 75
    sidestep <- -1.2
  } else {
    geom_text_size <- 50
    sidestep <- -1.5
  }
  

  

  
  # GGplot graphing
  plot.bar <- ggplot(data = main.df, aes(x=Ques, y=Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "fill", width = 0.5) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = col, guide = guide_legend(reverse = TRUE, nrow = 1), labels = resp) +
    geom_text(data = main.df, aes(Ques, Freq, group = Var1), label = main.prop,
              position = position_fill(vjust=0.5), color = tex.col, size = geom_text_size) +
    labs(title = "Direct-Entry Undergraduate Students, UBC Okanagan",
         subtitle = subt) +
    scale_x_discrete(breaks = unique(main.df$Ques),
                     labels = ld.title) +
    ubc.theme.ld + 
    # theme(plot.subtitle = element_text(hjust = sidestep)) +
    coord_flip()
  
  # Printing plot
  print(plot.bar)
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
    ld <- substr(get(qn, data.ok) %>% attr('label'),
                 unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label')))+3,
                 nchar(get(qn, new.dat) %>% attr('label')))
    
    row_tot <- c(row_tot, sum(df.list[[i]]$Freq))
    c_vc.sum <- 0
    c_vc_sc.sum <- 0
    for (j in 1:length(resp)+0) {

      if(!is.na(df.list[[i]]$Freq[j])){
        mattt[i,j] <- paste0(floor(100*df.list[[i]]$Freq[j]/row_tot[i]),"%")
        if(j==3){
          c_vc_sc.sum <- c_vc_sc.sum + df.list[[i]]$Freq[j]
        }
        if(j>=4){
          c_vc.sum <- c_vc.sum + df.list[[i]]$Freq[j]
          c_vc_sc.sum <- c_vc_sc.sum + df.list[[i]]$Freq[j]
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
  
  main.df <- data.frame(mattt)
  colnames(main.df) <- c(resp)
  main.df <- main.df %>% add_column(UBCO = ld.main, .before = resp[1]) %>% 
    add_column(`Very concerned/Concerned` = c_vc) %>%
    add_column(`Including somewhat concerned` = c_vc_sc) %>%
    add_column(Total = row_tot)
  
  
  ft <- flextable(main.df) %>% theme_box()
  ft <- fontsize(ft, size = 6, part = "all")
  set_table_properties(ft, layout = "autofit")
  ft <- align_text_col(ft, align = "center", header = TRUE) %>% 
    align_nottext_col(align = "center", header = TRUE)
  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    bg(bg = "grey", part = "all") %>%
    border(border = fp_border_default(color = "white"), part = "all") %>%
    width(width = 0.65, unit = "in")
}




