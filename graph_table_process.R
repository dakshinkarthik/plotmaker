mx_graph <- function(processed_dataList){
  
  # for (i in 1:length(processed_dataList)) {
  #   print(processed_dataList[[i]])
  # }
  
  #       1     2         3       4       5       6     7           8           9           10    11        12            13
  # list(qval, main.df, resp, leveler, c.width, col, main.prop, tex.col, geom_text_size, ti.tle, subt, legend.pos.x, legend.pos.y)
  
  plot.bar <- ggplot(data = processed_dataList[[2]], aes(x=factor(Ques, levels = rev(unique(Ques))), y=Freq,
                                         fill = factor(Var1, levels = processed_dataList[[4]]))) +
    geom_bar(stat = "identity", position = "fill", width = processed_dataList[[5]]) +
    theme_economist(base_size = 14) +
    scale_fill_manual(values = processed_dataList[[6]], guide = guide_legend(reverse = TRUE, nrow = 1), labels = processed_dataList[[3]]) +
    geom_text(data = processed_dataList[[2]], aes(Ques, Freq, group = factor(Var1, levels = processed_dataList[[4]])),
              label = processed_dataList[[7]],
              position = position_fill(vjust=0.5), color = processed_dataList[[8]], size = processed_dataList[[9]]) +
    labs(title = processed_dataList[[10]],
         subtitle = processed_dataList[[11]]) +
    # scale_x_discrete(breaks = unique(main.df$Ques),
    #                  labels = ld.title) +
    ubc.theme() +
    theme(legend.position = c(processed_dataList[[12]],processed_dataList[[13]])) +
    coord_flip() # this function was originally built with questions on the x-axis; hence coord_flip() is used
  
  print(plot.bar)
  
}

mx_table <- function(processed_dataList){

  #       1       2       3         4       5
  # list(qval, main.df, ft.size, c.width, mattt)
  
  ft <- flextable(processed_dataList[[2]]) %>% theme_box()
  ft <- fontsize(ft, size = processed_dataList[[3]], part = "all")
  set_table_properties(ft, layout = "autofit")
  ft <- align_text_col(ft, align = "center", header = TRUE) %>%
    align_nottext_col(align = "center", header = TRUE)
  ft %>% colformat_int(big.mark = "") %>%
    valign(valign = "center", part = "all") %>%
    border(border = fp_border_default(color = "#A7A19D"), part = "all") %>%
    width(width = processed_dataList[[4]], unit = "in") %>%
    width(width = 0.42, unit = "in", j = dim(processed_dataList[[5]])[2]+4) %>%
    width(width = 0.75, unit = "in", j = 1) %>%
    color(color = "#A7A19D", part = "header") %>%
    color(j = -2:dim(processed_dataList[[5]])[2]+4, color = "#A7A19D", part = "body")
  
}

mc_graph <- function(processed_dataList){
  
  #       1       2       3         4             5             6         7
  # list(qval, main.df, c.width, main.prop, geom_text_size, param_list, subt)
  
  plot.bar <- ggplot(data = processed_dataList[[2]], aes(x=Freq, y=factor(Var1, levels = rev(unique(Var1))),
                                         fill = factor(Ques, levels = rev(unique(Ques))))) +
    geom_bar(stat = "identity", position = "dodge", width = processed_dataList[[3]]) +
    theme_economist(base_size = 14) +
    # scale_y_discrete(breaks = levels(main.df$Var1), labels = axis.q) + ## This was commented out because question labels were integrated directly into the df
    scale_fill_manual(values = c("#FFC279","#579C2C"),
                      guide = guide_legend(reverse = TRUE,nrow = 2)) +
    geom_text(data = processed_dataList[[2]], label = processed_dataList[[4]],
              position = position_dodge(width = processed_dataList[[3]]), size = processed_dataList[[5]], hjust = -0.1) +
    labs(title = title_builder(processed_dataList[[6]]),
         subtitle = processed_dataList[[7]]) +
    ubc.theme() +
    theme(legend.position = c(0.85,0.5)) +
    scale_x_continuous(limits = c(0, 2 * max(processed_dataList[[2]]$Freq))) # setting x-axis boundaries
  # coord_flip()
  
  print(plot.bar)
  
}

mc_table <- function(processed_dataList){
  
  #       1       2       3
  # list(qval, main.df, param_list)
  
  if(processed_dataList[[3]][6]=="Okanagan"){
    
    ft <- flextable(processed_dataList[[2]]) %>% theme_box() %>%
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
  else if(processed_dataList[[3]][6]=="Vancouver"){
    
    ft <- flextable(processed_dataList[[2]]) %>% theme_box() %>%
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
    
    ft <- flextable(processed_dataList[[2]]) %>% theme_box() %>%
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
  
  #       1       2         3
  # list(qval, main.df, param_list)
  
  if(processed_dataList[[3]][6]=="Okanagan"){
    
    ft <- flextable(processed_dataList[[2]]) %>% theme_box() %>%
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
  else if(processed_dataList[[3]][6]=="Vancouver"){
    
    ft <- flextable(processed_dataList[[2]]) %>% theme_box() %>%
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
    
    ft <- flextable(processed_dataList[[2]]) %>% theme_box() %>%
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
