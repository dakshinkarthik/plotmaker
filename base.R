# source("plotmaker.R")

addline_format <- function(x,...){
  gsub('\\s',' ',x)
}

flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}

nubc2021joined <- read.csv("nubc2021joined.csv")
data.ok <- nubc2021joined[which(nubc2021joined$directtransfer.x == "DIRECT-ENTRY" & nubc2021joined$campusName.x == "Okanagan"),]


ubc.theme.ld <-  theme(legend.position = "bottom",
                       legend.direction = "horizontal",
                       legend.title = element_blank(),
                       legend.key.height = unit(2, 'mm'),
                       legend.key.width = unit(6, 'mm'),
                       plot.background = element_rect(colour = "grey", fill = NA, size = 2),
                       plot.title = element_text(hjust = 0.1, color = "#2B73C2", size = 85),
                       plot.subtitle = element_text(hjust = 0.1, color = "grey", size = 60),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_text(family = "serif", size = 70),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank())




#For options


mc <- function(qval, new.dat){
  qval <- "QN105"
  x <- na.omit(unique(new.dat$qSubid[new.dat$qID == qval]))
  resp <- na.omit(unique(new.dat$responseTxt[new.dat$qID == qval]))
  i <- 0
  df.list <- list()
  prop <- list()
  main.prop <- NULL
  main.df<- data.frame()
  col <- rev(c("#002145", "#0055B7", "#00A7E1", "#26C7FF", "#5CD5FF", "#85E0FF", "#A2E7FF"))
  tex.col.base <- rev(c("white","white","black","black","black","black"))
  tex.col <- c()
  label_count <- length(tex.col)
  ld.title <- na.omit(unique(nubc2021joined$qSublabel[nubc2021joined$qID == qval]))
  samp.text <- str_wrap(ld.title[[7]],width = 80)
  
  
  for (qn in x) {
    label_count_var <- 0
    i <- i+1
    print(qn)
    df.list[[i]] <- data.frame(table(new.dat$responseTxt[new.dat$qSubid == qn]), Ques = c(i))
    levels(df.list[[i]]) <- resp
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
  
  # print(main.df)
  
  plot.bar <- ggplot(data = main.df, aes(x=Ques, y=Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "fill", width = 0.4) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = col, guide = guide_legend(reverse = TRUE)) +
    geom_text(data = main.df, aes(Ques, Freq, group = Var1), label = main.prop,
              position = position_fill(vjust=0.5), color = tex.col, size = 5) +
    labs(title = qval) +
    scale_x_discrete(breaks = unique(main.df$Ques),
                     labels = addline_format(ld.title)) +
    # ubc.theme.ld 
    coord_flip()
  
  print(plot.bar)
  
  return(df.list)
}


for(j in 1:length(prop[[3]])){
  if(as.integer(prop[[3]][j])<5)
    prop[[3]][j] = ''
  else
    prop[[3]][j] = paste0(prop[[3]][j], "%")
}



new.list <- mc("QN104", data.ok)
new.list[[3]]

df.list[[9]]




install.packages('excelR')

excelTable(head(iris))
