source("plotmaker.R")

mx_graph <- function(processed_dataList){
  
  # for (i in 1:length(processed_dataList)) {
  #   print(processed_dataList[[i]])
  # }
  
  #       1     2     3       4       5       6     7       8           9           10          11    12        13            14
  # list(mx, qval, main.df, resp, leveler, c.width, col, main.prop, tex.col, geom_text_size, ti.tle, subt, legend.pos.x, legend.pos.y)
  
  plot.bar <- ggplot(data = processed_dataList[[3]], aes(x=factor(Ques, levels = rev(unique(Ques))), y=Freq,
                                         fill = factor(Var1, levels = processed_dataList[[5]]))) +
    geom_bar(stat = "identity", position = "fill", width = processed_dataList[[6]]) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = processed_dataList[[7]], guide = guide_legend(reverse = TRUE, nrow = 1), labels = processed_dataList[[4]]) +
    geom_text(data = processed_dataList[[3]], aes(Ques, Freq, group = factor(Var1, levels = processed_dataList[[5]])),
              label = processed_dataList[[8]],
              position = position_fill(vjust=0.5), color = processed_dataList[[9]], size = processed_dataList[[10]]) +
    labs(title = processed_dataList[[11]],
         subtitle = processed_dataList[[12]]) +
    # scale_x_discrete(breaks = unique(main.df$Ques),
    #                  labels = ld.title) +
    ubc.theme() +
    theme(legend.position = c(processed_dataList[[13]],processed_dataList[[14]])) +
    coord_flip() # this function was originally built with questions on the x-axis; hence coord_flip() is used
  
  print(plot.bar)
  
}

mx_table <- function(processed_dataList){

  #       1   2       3         4       5       6
  # list(mx, qval, main.df, ft.size, c.width, mattt)
  
  ft <- flextable(processed_dataList[[3]]) %>% theme_box()
  ft <- fontsize(ft, size = processed_dataList[[4]], part = "all")
  set_table_properties(ft, layout = "autofit")
  ft <- align_text_col(ft, align = "center", header = TRUE) %>%
    align_nottext_col(align = "center", header = TRUE)
  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
    width(width = processed_dataList[[5]], unit = "in") %>%
    width(width = 0.42, unit = "in", j = dim(processed_dataList[[6]])[2]+4) %>%
    width(width = 0.75, unit = "in", j = 1) %>%
    color(color = "#A7A19D", part = "header") %>%
    color(j = -2:dim(processed_dataList[[6]])[2]+4, color = "#A7A19D", part = "body")
  
}

mc_graph <- function(processed_dataList){
  
  #       1   2       3         4           5             6         7         8
  # list(mc, qval, main.df, c.width, main.prop, geom_text_size, param_list, subt)
  
  plot.bar <- ggplot(data = processed_dataList[[3]], aes(x=Freq, y=factor(Var1, levels = rev(unique(Var1))),
                                         fill = factor(Ques, levels = rev(unique(Ques))))) +
    geom_bar(stat = "identity", position = "dodge", width = processed_dataList[[4]]) +
    theme_economist(base_size = 14) +
    # scale_y_discrete(breaks = levels(main.df$Var1), labels = axis.q) + ## This was commented out because question labels were integrated directly into the df
    scale_fill_manual(values = c("#FFC279","#579C2C"),
                      guide = guide_legend(reverse = TRUE,nrow = 2)) +
    geom_text(data = processed_dataList[[3]], label = processed_dataList[[5]],
              position = position_dodge(width = processed_dataList[[4]]), size = processed_dataList[[6]], hjust = -0.1) +
    labs(title = title_builder(processed_dataList[[7]]),
         subtitle = processed_dataList[[8]]) +
    ubc.theme() +
    theme(legend.position = c(0.85,0.5)) +
    scale_x_continuous(limits = c(0, 2 * max(processed_dataList[[3]]$Freq))) # setting x-axis boundaries
  # coord_flip()
  
  print(plot.bar)
  
}

mc_table <- function(processed_dataList){
  
  #       1     2       3       4
  # list(mc,  qval, main.df, param_list)
  
  if(processed_dataList[[4]][6]=="Okanagan"){
    
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
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
  }
  else if(processed_dataList[[4]][6]=="Vancouver"){
    
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
      set_header_labels(X1="%",X2="n",X3="%",X4="n") %>%
      add_header(UBCV = "UBCV", X1 = "Domestic", X2 = "Domestic", X3 = "International", X4 = "International") %>%
      merge_h(part = "header") %>%
      merge_v(part = "header") %>%
      color(j = c("X1","X2","X3","X4"), color = "#A7A19D", part = "all") %>%
      color(j = "UBCV", color = "#A7A19D", part = "header") %>%
      # color(j = "Domestic", part = "header", color = "#54504C") %>%
      fontsize(size = 6, part = "all") %>%
      align_text_col(align = "center", header = TRUE) %>%
      align_nottext_col(align = "center", header = TRUE)
    
    set_table_properties(ft, layout = "autofit")
    
    ft %>% colformat_int(big.mark = "") %>%
      valign(valign = "center", part = "all") %>%
      border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
      width(width = 3.5, unit = "in",j = "UBCV")
  }
  else{
    
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
      set_header_labels(X1="%",X2="n",X3="%",X4="n") %>%
      add_header(UBC = "UBC", X1 = "Domestic", X2 = "Domestic", X3 = "International", X4 = "International") %>%
      merge_h(part = "header") %>%
      merge_v(part = "header") %>%
      color(j = c("X1","X2","X3","X4"), color = "#A7A19D", part = "all") %>%
      color(j = "UBC", color = "#A7A19D", part = "header") %>%
      # color(j = "Domestic", part = "header", color = "#54504C") %>%
      fontsize(size = 6, part = "all") %>%
      align_text_col(align = "center", header = TRUE) %>%
      align_nottext_col(align = "center", header = TRUE)
    
    set_table_properties(ft, layout = "autofit")
    
    ft %>% colformat_int(big.mark = "") %>%
      valign(valign = "center", part = "all") %>%
      border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
      width(width = 3.5, unit = "in",j = "UBC")
  }
  
}

mc.yn_graph <- function(processed_dataList){
  
  mc_graph(processed_dataList)
  
}

mc.yn_table <- function(processed_dataList){
  
  #         1     2       3       4
  # list(mc.yn, qval, main.df, param_list)
  
  if(processed_dataList[[4]][6]=="Okanagan"){
    
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
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
    
  }
  else if(processed_dataList[[4]][6]=="Vancouver"){
    
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
      set_header_labels(X1="%",X2="n",X3="%",X4="n") %>%
      color(j = c("X1","X2","X3","X4"), color = "#A7A19D", part = "all") %>%
      add_header(UBCV = "UBCV", X1 = "Domestic", X2 = "Domestic", X3 = "International", X4 = "International") %>%
      merge_h(part = "header") %>%
      merge_v(part = "header") %>%
      color(j = "UBCV", color = "#A7A19D", part = "header") %>%
      fontsize(size = 6, part = "all") %>%
      align_text_col(align = "center", header = TRUE) %>%
      align_nottext_col(align = "center", header = TRUE)
    
    set_table_properties(ft, layout = "autofit")
    
    ft %>% colformat_int(big.mark = "") %>%
      valign(valign = "center", part = "all") %>%
      border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
      width(width = 3.5, unit = "in",j = "UBCV")
    
  }
  else{
    
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
      set_header_labels(X1="%",X2="n",X3="%",X4="n") %>%
      color(j = c("X1","X2","X3","X4"), color = "#A7A19D", part = "all") %>%
      add_header(UBC = "UBC", X1 = "Domestic", X2 = "Domestic", X3 = "International", X4 = "International") %>%
      merge_h(part = "header") %>%
      merge_v(part = "header") %>%
      color(j = "UBC", color = "#A7A19D", part = "header") %>%
      fontsize(size = 6, part = "all") %>%
      align_text_col(align = "center", header = TRUE) %>%
      align_nottext_col(align = "center", header = TRUE)
    
    set_table_properties(ft, layout = "autofit")
    
    ft %>% colformat_int(big.mark = "") %>%
      valign(valign = "center", part = "all") %>%
      border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
      width(width = 3.5, unit = "in",j = "UBC")
    
  }
  
}

rk_graph <- function(processed_dataList){
  
  #       1   2         3       4       5      6    7         8         9       10              11    12
  # list(rk, qval, main.df, leveler, c.width, col, resp, main.prop, tex.col, geom_text_size, ti.tle, subt)
  
  plot.bar <- ggplot(data = processed_dataList[[3]], aes(x=Freq, y=factor(Ques,levels = rev(processed_dataList[[4]])),
                                         fill = Var1)) +
    # leveler is used to set levels for the factor on y-axis
    geom_bar(stat = "identity", position = "fill", width = processed_dataList[[5]]) +
    theme_economist(base_size = 14) +
    # resp is passed to legend labels
    scale_fill_manual(values = processed_dataList[[6]], guide = guide_legend(reverse = TRUE, nrow = 1), labels = processed_dataList[[7]]) +
    # main.prop is passed to geom_text label. Aesthetic needs to match the main aesthetic
    geom_text(data = processed_dataList[[3]], aes(Freq, Ques, group = Var1), label = processed_dataList[[8]],
              position = position_fill(vjust=0.5), color = processed_dataList[[9]], size = processed_dataList[[10]]) +
    labs(title = processed_dataList[[11]],
         subtitle = processed_dataList[[12]]) +
    # Initially, y-axis labels were manually set;
    # but after adding question labels to the dataframe it did not seem neccessary
    # scale_y_discrete(breaks = unique(main.df$Ques),
    #                  labels = ld.title) +
    scale_x_continuous(labels = scales::percent) + # Sets the scale to percentage
    ubc.theme() + # custom formatting function for the report
    theme(axis.text.x = element_text(size = 180)) # specific elements of the theme formatted here
  
  print(plot.bar)
  
}

rk_table <- function(processed_dataList){
  
  
  #       1   2       3       4
  # list(rk, qval, main.df, mattt)
  
  # creating a flextable object and formatting it
  ft <- flextable(processed_dataList[[3]]) %>% theme_box()
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
    color(j = 2:dim(processed_dataList[[4]])[2]+1, color = "#A7A19D", part = "header") %>%
    color(j = 2:dim(processed_dataList[[4]])[2]+1, color = "#A7A19D", part = "all")
  
}

ms_graph <- function(processed_dataList){
  
  #       1   2       3         4         5         6
  # list(ms, qval, main.df, main.prop, param_list, subt)
  
  plot.bar <- ggplot(data = processed_dataList[[3]], aes(x=Freq, y=factor(Var1,levels = rev(unique(Var1))),
                                         fill = factor(Ques,levels = rev(unique(Ques))))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = c("#FFC279","#579C2C"),
                      guide = guide_legend(reverse = TRUE,nrow = 2)) +
    geom_text(data = processed_dataList[[3]], label = processed_dataList[[4]],
              position = position_dodge(width = 0.8), size = 60, hjust = -0.1) +
    labs(title = title_builder(processed_dataList[[5]]),
         subtitle = processed_dataList[[6]]) +
    ubc.theme() +
    theme(legend.position = c(0.85,0.5)) +
    scale_x_continuous(limits = c(0, 2 * max(processed_dataList[[3]]$Freq))) # setting x-axis bound limits
  
  print(plot.bar)
  
}

ms_table <- function(processed_dataList){
  
  #       1    2      3         4
  # list(ms, qval, main.df, param_list)
  if(processed_dataList[[4]][6]=="Okanagan"){
    
    # flextable object creation
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
      set_header_labels(X1="%",X2="n",X3="%",X4="n") %>% # setting header labels to their appropriate columns
      add_header(UBCO = "UBCO", X1 = "Domestic", X2 = "Domestic", X3 = "International", X4 = "International") %>% # adding a super heading
      merge_h(part = "header") %>% # merge horizontal column names (merges 'UBCO')
      merge_v(part = "header") %>% # merges vertical column names (merges Domestic together and merges international together)
      color(j = c("X1","X2","X3","X4"), color = "#A7A19D", part = "all") %>% # column header colors
      color(j = "UBCO", color = "#A7A19D", part = "header") %>% # question column header colors
      # color(j = "Domestic", part = "header", color = "#54504C") %>%
      fontsize(size = 6, part = "all") %>% # font size
      align_text_col(align = "center", header = TRUE) %>%
      align_nottext_col(align = "center", header = TRUE)
    
    set_table_properties(ft, layout = "autofit")
    
    ft %>% colformat_int(big.mark = "") %>%
      valign(valign = "center", part = "all") %>%
      border(border = fp_border_default(color = "#A7A19D"), part = "all") %>% # border color
      width(width = 3.5, unit = "in",j = "UBCO") # cell width
  }
  else if(processed_dataList[[4]][6]=="Vancouver"){
    
    # flextable object creation
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
      set_header_labels(X1="%",X2="n",X3="%",X4="n") %>% # setting header labels to their appropriate columns
      add_header(UBCV = "UBCV", X1 = "Domestic", X2 = "Domestic", X3 = "International", X4 = "International") %>% # adding a super heading
      merge_h(part = "header") %>% # merge horizontal column names (merges 'UBCO')
      merge_v(part = "header") %>% # merges vertical column names (merges Domestic together and merges international together)
      color(j = c("X1","X2","X3","X4"), color = "#A7A19D", part = "all") %>% # column header colors
      color(j = "UBCV", color = "#A7A19D", part = "header") %>% # question column header colors
      # color(j = "Domestic", part = "header", color = "#54504C") %>%
      fontsize(size = 6, part = "all") %>% # font size
      align_text_col(align = "center", header = TRUE) %>%
      align_nottext_col(align = "center", header = TRUE)
    
    set_table_properties(ft, layout = "autofit")
    
    ft %>% colformat_int(big.mark = "") %>%
      valign(valign = "center", part = "all") %>%
      border(border = fp_border_default(color = "#A7A19D"), part = "all") %>% # border color
      width(width = 3.5, unit = "in",j = "UBCV") # cell width
  }
  else{
    
    # flextable object creation
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
      set_header_labels(X1="%",X2="n",X3="%",X4="n") %>% # setting header labels to their appropriate columns
      add_header(UBC = "UBC", X1 = "Domestic", X2 = "Domestic", X3 = "International", X4 = "International") %>% # adding a super heading
      merge_h(part = "header") %>% # merge horizontal column names (merges 'UBCO')
      merge_v(part = "header") %>% # merges vertical column names (merges Domestic together and merges international together)
      color(j = c("X1","X2","X3","X4"), color = "#A7A19D", part = "all") %>% # column header colors
      color(j = "UBC", color = "#A7A19D", part = "header") %>% # question column header colors
      # color(j = "Domestic", part = "header", color = "#54504C") %>%
      fontsize(size = 6, part = "all") %>% # font size
      align_text_col(align = "center", header = TRUE) %>%
      align_nottext_col(align = "center", header = TRUE)
    
    set_table_properties(ft, layout = "autofit")
    
    ft %>% colformat_int(big.mark = "") %>%
      valign(valign = "center", part = "all") %>%
      border(border = fp_border_default(color = "#A7A19D"), part = "all") %>% # border color
      width(width = 3.5, unit = "in",j = "UBC") # cell width
  }
}

cs_graph <- function(processed_dataList){
  
  #       1    2      3         4         5
  # list(cs, qval, main.df, param_list, subt)
  
  
  plot.bar <- ggplot(data = processed_dataList[[3]], aes(x=Freq, y=factor(Var1,levels = rev(unique(Var1))),
                                         fill = factor(Ques,levels = rev(unique(Ques))))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = c("#FFC279","#579C2C"),
                      guide = guide_legend(reverse = TRUE,nrow = 2)) +
    geom_text(data = processed_dataList[[3]], label = paste0(processed_dataList[[3]]$Freq,"%"),
              position = position_dodge(width = 0.8), size = 60, hjust = -0.1) +
    labs(title = title_builder(processed_dataList[[4]]),
         subtitle = processed_dataList[[5]]) +
    ubc.theme() +
    theme(legend.position = c(0.85,0.5)) +
    scale_x_continuous(limits = c(0, 2 * max(processed_dataList[[3]]$Freq)))
  
  print(plot.bar)
  
}

cs_table <- function(processed_dataList){
  
  #       1   2       3         4
  # list(cs, qval, main.df, param_list)
  if(processed_dataList[[4]][6]=="Okanagan"){
    
    # flextable object creation
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
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
  }
  else if(processed_dataList[[4]][6]=="Vancouver"){
    
    # flextable object creation
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
      color(j = c("Domestic","International"), color = "#A7A19D", part = "all") %>%
      color(j = "UBCV", color = "#A7A19D", part = "header") %>%
      # color(j = "Domestic", part = "header", color = "#54504C") %>%
      fontsize(size = 6, part = "all") %>%
      align_text_col(align = "center", header = TRUE) %>%
      align_nottext_col(align = "center", header = TRUE)
    
    set_table_properties(ft, layout = "autofit")
    
    ft %>% colformat_int(big.mark = "") %>%
      valign(valign = "center", part = "all") %>%
      border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
      width(width = 5, unit = "in",j = "UBCV")
  }
  else{
    
    # flextable object creation
    ft <- flextable(processed_dataList[[3]]) %>% theme_box() %>%
      color(j = c("Domestic","International"), color = "#A7A19D", part = "all") %>%
      color(j = "UBC", color = "#A7A19D", part = "header") %>%
      # color(j = "Domestic", part = "header", color = "#54504C") %>%
      fontsize(size = 6, part = "all") %>%
      align_text_col(align = "center", header = TRUE) %>%
      align_nottext_col(align = "center", header = TRUE)
    
    set_table_properties(ft, layout = "autofit")
    
    ft %>% colformat_int(big.mark = "") %>%
      valign(valign = "center", part = "all") %>%
      border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
      width(width = 5, unit = "in",j = "UBC")
  }
  
}

mx.tri_graph <- function(processed_dataList){
  
  #       1       2       3       4   5         6       7       8     9
  # list(mx.tri, qval, main.df, col, resp, main.prop, ti.tle, subt, ld.title)
  plot.bar <- ggplot(data = processed_dataList[[3]], aes(x=Ques, y=Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.9) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = processed_dataList[[4]], guide = guide_legend(reverse = FALSE, nrow = 3), labels = processed_dataList[[5]]) +
    geom_text(data = processed_dataList[[3]], label = processed_dataList[[6]],
              position = position_dodge(width = 0.9), size = 60, vjust = -1) +
    labs(title = processed_dataList[[7]],
         subtitle = processed_dataList[[8]]) +
    scale_x_discrete(breaks = unique(processed_dataList[[3]]$Ques),
                     labels = processed_dataList[[9]]) +
    ubc.theme() +
    # This function needs special formatting because it is different from the normal mx graph
    theme(axis.text.x = element_text(family = "serif", size = 180),
          axis.text.y = element_blank(),
          legend.position = c(0.5,0.75),
          legend.spacing.y = unit(3, "in")
    ) +
    scale_y_continuous(limits = c(0, 2 * max(processed_dataList[[3]]$Freq)))
  
  # Printing plot
  print(plot.bar)
  
}

mx.tri_table <- function(processed_dataList){
  
  #       1       2         3       4       5       6
  # list(mx.tri, qval, main.df, ft.size, c.width, mattt)
  
  # flextable object created from df
  ft <- flextable(processed_dataList[[3]]) %>% theme_box() # theme_box() adds a box with borders to the table
  ft <- fontsize(ft, size = processed_dataList[[4]], part = "all") # setting font size
  set_table_properties(ft, layout = "autofit") # automatically sets proportion of spacing based on contents
  ft <- align_text_col(ft, align = "center", header = TRUE) %>% # text alignment for characters
    align_nottext_col(align = "center", header = TRUE) # text alingment for non-characters(numbers)
  ft %>% colformat_int(big.mark = "") %>% # Removing separators in numbers
    valign(valign = "center", part = "all") %>% # vertical alignment of contents
    # bg(bg = "grey", part = "all") %>%
    # Adding border and color to the border
    border(border = fp_border_default(color = "#A7A19D", width = 0.8), part = "all") %>%
    width(width = processed_dataList[[5]], unit = "in") %>% # cell width
    color(color = "#A7A19D", part = "header") %>% # text color for the header
    color(j = 0:dim(processed_dataList[[6]])[2]+2, color = "#A7A19D", part = "body") # text color for the body
  
}

