
  
mx_proc <- function(qval, new.dat){
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
  q_param_list <- list(qval, main.df, resp, leveler, c.width, col, main.prop, tex.col, geom_text_size, ti.tle, subt, legend.pos.x, legend.pos.y)
  
  return(q_param_list)
}

processed_dataList <- list()
processed_dataList[[1]] <- mx_proc("QN105", d.dat)
processed_dataList[[2]] <- mx_proc("QN104", d.dat)
processed_dataList[[3]] <- mx_proc("QN100", d.dat)

str(processed_dataList)
processed_dataList[[3]][[2]]
