ubc.theme.ld <-  theme(legend.position = "bottom",
                       legend.title = element_blank(),
                       legend.text = element_text(size = 8),
                       legend.key.height = unit(1, 'mm'),
                       legend.key.width = unit(3, 'mm'),
                       plot.background = element_rect(colour = "grey", fill = NA, size = 2),
                       plot.title = element_text(hjust = 0.5, color = "#2B73C2"),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank())

col1 <- rev(c("#002145", "#0055B7", "#00A7E1", "#EBC796", "#EB9C30", "#965A07"))
tex.col1 <- rev(c("white","white","black","black","black","black"))

nubc21 <- read.csv(file = 'nubc2021.csv')
nubc21.q <- read.csv(file = 'question.csv')
nubc21.o <- read.csv(file = 'rspOptions.csv')
nubc21.coh <- read.csv(file = 'nubc2021Tableaucohort.csv')
nubc21 <- merge(x = nubc21.coh, y = nubc21, by.x = "externalDataReference", by.y = "ExternalReference")

head(nubc21.o)

nubc21.o <- nubc21.o[order(nubc21.o$responseNum),]


nubc21.o$responseTxt[nubc21.o$qID.x=='reside']


nubc21.o$responseNum[nubc21.o$qID.x=='QN100']            #TO GET (NUMBER OF) OPTIONS
nubc21.o$responseTxt[nubc21.o$qID.x=='QN104']            #TO GET VALUE OF EACH OPTION FOR GIVEN QUESTION TYPE FOR LEGEND LABLES
nubc21.q$qSublabel[nubc21.q$qSubid=='mx.QN104_7']

plotmaker <- function(qID, qSubid){
  pass = TRUE
  plot_list <- list()
  q_list <- list()                                       #FOR PRINTING QUESTIONS NEXT TO EACH BAR
  n_list <- list()                                       #FOR STORING NUMBERED OPTIONS
  o_list <- list()                                       #FOR STORING NAMED OPTIONS; USED FOR LABELS
  
  
  for (i in 1:length(qID)) {                              #TO GET OPTIONS AND LABELS
    temp.n = nubc21.o$responseNum[nubc21.o$qID.x==qID[i]] #TO GET NUMBERED OPTIONS
    temp.o = nubc21.o$responseTxt[nubc21.o$qID.x==qID[i]] #TO GET WORDED OPTIONS
    n_list[[i]] = (temp.n)
    o_list[[i]] = (temp.o)
    if(length(n_list) > 1){
      for (i in i:length(n_list)) {
        val = n_list[[i-1]]
        if(sort(temp.n) == sort(val))
          pass = TRUE
        else{
          pass = FALSE
          break
        }
      }
    }
  }
  
  if(pass){
    print(n_list)
    print(o_list)
  }
  
  else
    print("Please choose questions with the same number of options")
  
  # while(i < length(x)){
  #   
  # }
}
plotmaker(c('QN100','QN28'))

