data.ok

install.packages('flextable')
library(flextable)



length(names(get("mx.QN105_2", data.ok) %>% attr('labels')))
data.frame(table(get(qn, data.ok)), Ques = c(i))
#table
coln <- names(get(rc_list[1], data.ok) %>% attr('labels'))
coln

df1 <- data.frame(table(get(rc_list[1], data.ok)))
colnames()

tb_mx <- function(qval, new.dat){
  cnames <- colnames(new.dat)
  rc_list <- cnames[grepl(qval, cnames, fixed = TRUE)]
  resp <- names(get(rc_list[1], new.dat) %>% attr('labels'))
  mattt <- matrix(rep(1,length(resp)*length(rc_list)), ncol = length(resp))
  
  df.list <- list()
  main.df<- data.frame()
  ld.main <- c()
  row_tot <- c()
  
  i <- 1
  for (qn in rc_list) {
    df.list[[i]] <- data.frame(table(get(qn, data.ok)))
    ld <- substr(get(qn, data.ok) %>% attr('label'),
                 unlist(gregexpr(pattern =' - ', get(qn, new.dat) %>% attr('label')))+3,
                 nchar(get(qn, new.dat) %>% attr('label')))
    
    row_tot <- c(row_tot, sum(df.list[[i]]$Freq))
    for (j in 1:length(resp)) {
      if(!is.na(df.list[[i]]$Freq[j]))
        mattt[i,j] <- paste0(floor(100*df.list[[i]]$Freq[j]/row_tot[i]),"%")
      else
        mattt[i,j] <- "NA"
    }
    
    ld.main <- c(ld.main, ld)
    i <- i + 1
  }

  main.df <- data.frame(ld.main,mattt,row_tot)
  colnames(main.df) <- c("UBCO",resp,"Total")
  ft <- flextable(main.df) %>% theme_box()
  print(ft)
}
tb_mx("QN105", data.ok)

mattt <- matrix(rep(1,12), ncol = 4)
mattt[1,1] <- NA 
flextable(as.data.frame(mattt))


