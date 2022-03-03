

library(haven)
library(devtools)
library(glue)
library(surveytoolbox)

nubc2021joined <- read_sav("./data/nubc2021com.sav")
data.ok <- nubc2021joined

data.ok$mc.QN30 %>% attr('labels')
get("mx.QN65_1",data.ok) %>% attr('labels')
data.ok$campusName 

data.ok <- nubc2021joined[which(nubc2021joined$directtransfer == "DIRECT-ENTRY"
                     & nubc2021joined$campusName == "Okanagan"
                     & get("isi", nubc2021joined) == "Domestic"),]

str(data.ok)


qval <- "healthResource"

cnames <- colnames(data.ok)
rc_list <- cnames[grepl(paste0(qval,""), cnames, fixed = T)]
rc_list <- c(cnames[grepl(paste0(qval,""), cnames, fixed = T)],
             cnames[grepl(paste0(qval,"_"), cnames, fixed = T)],
             cnames[grepl(paste0(qval,"c"), cnames, fixed = T)],
             cnames[grepl(paste0(qval,"C"), cnames, fixed = T)],
             cnames[grepl(paste0(qval,"s"), cnames, fixed = T)],
             cnames[grepl(paste0(qval,"S"), cnames, fixed = T)])
rc_list <- rc_eval("rk",rc_list)

substr(rc_list[1],1,2)


ckr <- substr(rc_list[1],1,2)
for (i in 1:length(rc_list)) {
  if(substr(rc_list[i],1,2) == ckr){
    print("YES")
  }
  else
    print("No")
}
get(rc_list[1], data.ok) %>% attr('label')


as.integer()
as.integer(levels(data.frame(table(get(rc_list[1], d.dat)))$Var1)[as.integer(data.frame(table(get(rc_list[1], d.dat)))$Var1)])
data.frame(table(get(rc_list[8], data.ok)))
str(data.frame(table(get(rc_list[1], data.ok)))$Freq)
data.frame(table(get(rc_list[1], d.dat)))$Var1
table(get(rc_list[1], data.ok))
nrow(table(get(rc_list[2], i.dat)))

get_sum(rc_list)

get(rc_list[6], d.dat)[d.dat$ExternalReference == "NUBC2021_ 10028"]

tcv <- matrix(0)
rownames(tcv) <- c(1)
# tcv[1,1] <- 0
tcv <- as.table(tcv)
colnames(tcv) <- c("")
tdf <- data.frame(tcv)
tdf <- data.frame(Var1 = tdf$Var1, Freq = tdf$Freq)
tdf

unlist(gregexpr(pattern ='complete',rc_eval("rk",rc_list)[8]))
table(get(rc_eval("rk",rc_list)[10], data.ok))
data.frame(get(rc_list[11], data.ok))
table(data.ok$rk.QN98complete)

data.ok$isi[1000]

get("rk.QN98complete", data.ok) %>% attr('label')

df1 <- data.frame(table(get(rc_list[1], data.ok)), Ques = c(1))
df2 <- data.frame(table(get(rc_list[2], data.ok)), Ques = c(2))
df3 <- rbind(df1,df2)
ggplot(df3, aes(fill=Var1, y=Freq, x=Ques)) + 
  geom_bar(position="dodge", stat="identity")

unlist(gregexpr(pattern =' - ', get(rc_list[1], data.ok) %>% attr('labels')))

substr(get(rc_list[1], data.ok) %>% attr('labels'),
       unlist(gregexpr(pattern =' - ', get(rc_list[1], data.ok) %>% attr('labels')))+3,
       nchar(get(rc_list[1], data.ok) %>% attr('labels')))

names(get(rc_list[2], data.ok) %>% attr('label'))


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

qval <- "reside"

cnames <- colnames(data.ok)
rc_list <- cnames[grepl(qval, cnames, fixed = TRUE)]

data.frame(table(get(rc_list[1],data.ok)))

mc <- function(qval, new.dat){
  # Column names to read data
  cnames <- colnames(new.dat)
  rc_list <- rev(cnames[grepl(qval, cnames, fixed = TRUE)])
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  resp_b <- c()
  
  # Variable initialization
  df.list <- list()
  main.df <- NULL
  prop <- list()
  main.prop <- NULL
  main.df<- data.frame()
  col <- rev(c("#002145", "#0055B7", "#00A7E1", "#26C7FF", "#5CD5FF", "#85E0FF", "#A2E7FF"))
  tex.col.base <- rev(c("white","white","black","black","black","black"))
  tex.col <- c()
  label_count <- length(tex.col)
  ld.title <- c()
  i <- 1
  

  main.df <- data.frame(table(get(rc_list, data.ok)))
  for (qn in main.df$Var1) {
    resp_b <- c(resp_b, as.numeric(qn))
  }

  plot.bar <- ggplot(data = main.df, aes(x=Var1, y=Freq)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(breaks = main.df$Var1, labels = resp[resp_b]) +
    scale_fill_manual(values = rep("#0055B7",length(resp_b)), 
                      guide = guide_legend(reverse = TRUE),
                      labels = resp[resp_b]) +
    ubc.theme +
    coord_flip()
  print(plot.bar)
  # print(resp[resp_b])
}
mc("reside",data.ok)

font_import()
loadfonts(device = "win")

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
  ggtitle("Fuel Efficiency of 32 Cars") +
  xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
  theme(text=element_text(size=16,  family="calibri"))

rev(table(get("mc.reside", data.ok)))


ft <- flextable( head( iris ))
ft <- set_header_labels(ft, Sepal.Length = "Sepal length",
                        Sepal.Width = "Sepal width", Petal.Length = "Petal length",
                        Petal.Width = "Petal width"
)

ft <- flextable( head( iris ))
ft <- set_header_labels(ft,
                        values = list(Sepal.Length = "Sepal length",
                                      Sepal.Width = "Sepal width",
                                      Petal.Length = "Petal length",
                                      Petal.Width = "Petal width" ) )
print(ft)



data.ok$ms.spRestriction_1[data.ok$ExternalReference == "NUBC2021_ 983"] + 0
data.ok$ms.[7]

get(rc_list[8], d.dat)[d.dat$ExternalReference == "NUBC2021_ 9919"]
d.dat$ExternalReference
ceiling(100*8/832)




dat <- read.table(text=
"SC_LTSL_BM    16.8275
SC_STSL_BM    17.3914
proB_FrBC_FL   122.1580
preB_FrD_FL    18.5051
B_Fo_Sp    14.4693
B_GC_Sp    15.4986", header = FALSE, stringsAsFactors = FALSE)

str(dat)

dat$V1 <- factor(dat$V1, levels = dat$V1)
ggplot(dat,aes(x=V1,y=V2))+geom_bar(stat="identity")

sort(c("Domestic","International"))
