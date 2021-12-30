

library(haven)
library(devtools)
library(glue)
library(surveytoolbox)

nubc2021joined <- read_sav("./data/nubc2021_new.sav")
data.ok <- nubc2021joined

data.ok$mc.housing %>% attr('label')
data.ok$campusName 

data.ok <- nubc2021joined[which(nubc2021joined$directtransfer == "DIRECT-ENTRY"
                     & nubc2021joined$campusName == "Okanagan"),]

str(data.ok)

qval <- "QN28"

cnames <- colnames(data.ok)
rc_list <- cnames[grepl(qval, cnames, fixed = TRUE)]


get(rc_list[1], data.ok) %>% attr('label')






mc <- function(qval, new.dat){
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
    
    if (nchar(ld)>=77) {
      ld <- paste0(substr(ld,1,sapply(gregexpr(pattern = " ", substr(ld,1,77)),max)), "\n ",
             substr(ld,sapply(gregexpr(pattern = " ", substr(ld,1,77)),max)+1,nchar(ld)))
    }
    
    ld.title <- c(ld.title, ld)
    df.list[[i]] <- data.frame(table(get(qn, data.ok)), Ques = c(i))
    levels(df.list[[i]]) <- as.factor(resp)
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
  print(ld.title)
  
  
  plot.bar <- ggplot(data = main.df, aes(x=Ques, y=Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "fill", width = 0.4) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = col, guide = guide_legend(reverse = TRUE), labels = resp) +
    geom_text(data = main.df, aes(Ques, Freq, group = Var1), label = main.prop,
              position = position_fill(vjust=0.5), color = tex.col, size = 5) +
    # labs(title = qval) +
    scale_x_discrete(breaks = unique(main.df$Ques),
                     labels = ld.title) +
    # ubc.theme.ld +
    coord_flip()
  
  print(plot.bar)
}


mc("QN105", data.ok)


mc("QN105", new.dat)

names(get(rc_list[1], data.ok) %>% attr('labels'))
get(rc_list[4], data.ok) %>% attr('label')


sampst <- substr(get(rc_list[1], data.ok) %>% attr('label'),
                 unlist(gregexpr(pattern =' - ', get(rc_list[1], data.ok) %>% attr('label')))+3,
                 nchar(get(rc_list[1], data.ok) %>% attr('label')))

substr(sampst,1,nchar(sampst)/2)
nchar(sampst)

if (nchar(sampst)>=77) {
  paste0(substr(sampst,1,sapply(gregexpr(pattern = " ", substr(sampst,1,77)),max)), "\n ",
         substr(sampst,sapply(gregexpr(pattern = " ", substr(sampst,1,77)),max)+1,nchar(sampst)))
}


get(rc_list[1], data.ok) %>% attr('label')
get("mx.QN105_2", data.ok) %>% attr('label')
get(rc_list[1], data.ok) %>% attr('label')
unlist(gregexpr(pattern ='How', get(rc_list[1], data.ok) %>% attr('label')))

substr(get(rc_list[1], data.ok) %>% attr('label'),
       unlist(gregexpr(pattern ='How', get(rc_list[1], data.ok) %>% attr('label'))),
       unlist(gregexpr(pattern =' - ', get(rc_list[1], data.ok) %>% attr('label')))-1)

substr(get(rc_list[1], data.ok) %>% attr('label'),1,
       unlist(gregexpr(pattern =' - ', get(rc_list[1], data.ok) %>% attr('label')))-1)

substr(get(rc_list[1], data.ok) %>% attr('label'),
       unlist(gregexpr(pattern ='\\.', get(rc_list[1], data.ok) %>% attr('label')))+2,
       unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))

substr(get(rc_list[1], data.ok) %>% attr('label'),
       unlist(gregexpr(pattern ='To', get(rc_list[1], data.ok) %>% attr('label'))),
       unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label'))))

unlist(gregexpr(pattern ='To', get(rc_list[1], data.ok) %>% attr('label')))
unlist(gregexpr(pattern = 'satisfied', "Very dissatisfied"))
unlist(gregexpr(pattern = 'How', 
                "How satisfied are you with your overall experience with UBC so far?"))

sidestep <- NULL
if(length(rc_list) <= 3){
  sidestep <- -1
} else if (length(rc_list) <= 6){
  sidestep <- -1.2
} else {
  sidestep <- -1.5
}



nchar(substr(get(rc_list[1], data.ok) %>% attr('label'),
             unlist(gregexpr(pattern ='To', get(rc_list[1], data.ok) %>% attr('label'))),
             unlist(gregexpr(pattern ='\\?', get(rc_list[1], data.ok) %>% attr('label')))))
nchar("To what extent do you agree or disagree with the following statements?")


