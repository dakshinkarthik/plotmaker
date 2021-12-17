addline_format <- function(x,...){
  gsub('\\s',' ',x)
}

flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}

ubc.theme.ld <-  theme(legend.position = c(0.1,0.02),
                       legend.direction = "horizontal",
                       legend.title = element_blank(),
                       legend.key.height = unit(2, 'cm'),
                       legend.key.width = unit(4, 'cm'),
                       legend.text = element_text(size = 175),
                       legend.spacing.x = unit(2, "cm"),
                       plot.background = element_rect(colour = "grey", fill = NA, size = 2),
                       plot.title = element_text(hjust = 0.1, color = "#2B73C2", size = 85),
                       plot.subtitle = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_text(family = "serif", size = 200, hjust = 1),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank())




#For options
# nubc2021 <- read.csv('R code/nubc2021joined.csv')
# new.dat <- nubc2021[which(nubc2021$directtransfer ==
#                                   "DIRECT-ENTRY" & nubc2021$campusName == "Okanagan"),]
# qval <- "QN105"

mc <- function(qval, new.dat){
  # qval <- "QN105"
  # new.dat <- data.ok
  cnames <- colnames(new.dat)
  rc_list <- cnames[grepl(qval, cnames, fixed = TRUE)]
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
  resp <- names(get(rc_list[1], data.ok) %>% attr('labels'))
  
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
    df.list[[i]] <- data.frame(table(get(qn, data.ok)), Ques = c(i))
    levels(df.list[[i]]) <- factor(resp)
    df.list[[i]]$Ques <- as.factor(df.list[[i]]$Ques)
    prop[[i]] <- ceiling(as.double(100*df.list[[i]]$Freq/sum(df.list[[i]]$Freq)))
    
    for(j in 1:length(prop[[i]])){
      label_count_var <- label_count_var + 1
      if(as.integer(prop[[i]][j])<5)
        prop[[i]][j] = ''
      else
        prop[[i]][j] = paste0(prop[[i]][j], "%")
    }
    
    if (label_count_var == label_count) {
      tex.col <- c(tex.col, tex.col.base)
    }
    else {
      tex.col <- c(tex.col, tex.col.base[1:label_count_var])
    }
    
    main.prop <- c(main.prop, prop[[i]])
    main.df <- rbind(main.df, df.list[[i]])
  }
  # print(ld.title)
  
  
  plot.bar <- ggplot(data = main.df, aes(x=Ques, y=Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "fill", width = 0.5) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = col, guide = guide_legend(reverse = TRUE, nrow = 1), labels = resp) +
    geom_text(data = main.df, aes(Ques, Freq, group = Var1), label = main.prop,
              position = position_fill(vjust=0.5), color = tex.col, size = 75) +
    # labs(title = qval) +
    scale_x_discrete(breaks = unique(main.df$Ques),
                     labels = ld.title) +
    ubc.theme.ld +
    coord_flip()
  
  print(plot.bar)
}



# mc <- function(qval, new.dat){
#   x <- na.omit(unique(new.dat$qSubid[new.dat$qID == qval]))
#   resp <- na.omit(unique(new.dat$responseTxt[new.dat$qID == qval]))
#   i <- 0
#   df.list <- list()
#   prop <- list()
#   main.prop <- NULL
#   main.df<- data.frame()
#   col <- rev(c("#002145", "#0055B7", "#00A7E1", "#26C7FF", "#5CD5FF", "#85E0FF", "#A2E7FF"))
#   tex.col.base <- rev(c("white","white","black","black","black","black"))
#   tex.col <- c()
#   label_count <- length(tex.col)
#   ld.title <- na.omit(unique(nubc2021$qSublabel[nubc2021$qID == qval]))
# 
#   for (qn in x) {
#     label_count_var <- 0
#     i <- i+1
#     df.list[[i]] <- data.frame(table(new.dat$responseTxt[new.dat$qSubid == qn]), Ques = c(i))
#     levels(df.list[[i]]) <- resp
#     df.list[[i]]$Ques <- as.factor(df.list[[i]]$Ques)
#     prop[[i]] <- ceiling(as.double(100*df.list[[i]]$Freq/sum(df.list[[i]]$Freq)))
# 
#     for(j in 1:length(prop[[i]])){
#       label_count_var <- label_count_var + 1
#       if(as.integer(prop[[i]][j])<5)
#         prop[[i]][j] = ''
#       else
#         prop[[i]][j] = paste0(prop[[i]][j], "%")
#     }
# 
#     if (label_count_var == label_count) {
#       tex.col <- c(tex.col, tex.col.base)
#     }
#     else {
#       tex.col <- c(tex.col, tex.col.base[1:label_count_var])
#     }
# 
#     main.prop <- c(main.prop, prop[[i]])
#     main.df <- rbind(main.df, df.list[[i]])
#   }
# 
#   print(resp)
# 
#   plot.bar <- ggplot(data = main.df, aes(x=Ques, y=Freq, fill = Var1)) +
#     geom_bar(stat = "identity", position = "fill", width = 0.4) +
#     theme_economist(base_size = 14) +
#     scale_fill_manual(values = col, guide = guide_legend(reverse = TRUE)) +
#     geom_text(data = main.df, aes(Ques, Freq, group = Var1), label = main.prop,
#               position = position_fill(vjust=0.5), color = tex.col, size = 30) +
#     # labs(title = qval) +
#     scale_x_discrete(breaks = unique(main.df$Ques),
#                      labels = addline_format(ld.title)) +
#     # ubc.theme.ld +
#     coord_flip()
# 
#   print(plot.bar)
# }
# 
# mc("QN105", new.dat)




