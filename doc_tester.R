fullPlot.timer <- function(processed_graph_dataList, processed_table_dataList) {
  
  for (qData in 1:length(processed_graph_dataList)) {
    if(processed_graph_dataList[[qData]][[1]] == "mx"){
      print(qData)
      mx_graph(processed_graph_dataList[[qData]])
      mx_table(processed_table_dataList[[qData]])
    }
    else if(processed_graph_dataList[[qData]][[1]] == "mc"){
      print(qData)
      mc_graph(processed_graph_dataList[[qData]])
      mc_table(processed_table_dataList[[qData]])
    }
    else if(processed_graph_dataList[[qData]][[1]] == "ms"){
      print(qData)
      ms_graph(processed_graph_dataList[[qData]])
      ms_table(processed_table_dataList[[qData]])
    }
    else if(processed_graph_dataList[[qData]][[1]] == "mx.tri"){
      print(qData)
      mx.tri_graph(processed_graph_dataList[[qData]])
      mx.tri_table(processed_table_dataList[[qData]])
    }
    else if(processed_graph_dataList[[qData]][[1]] == "mc.yn"){
      print(qData)
      mc.yn_graph(processed_graph_dataList[[qData]])
      mc.yn_table(processed_table_dataList[[qData]])
    }
    else if(processed_graph_dataList[[qData]][[1]] == "rk"){
      print(qData)
      rk_graph(processed_graph_dataList[[qData]])
      rk_table(processed_table_dataList[[qData]])
    }
    else if(processed_graph_dataList[[qData]][[1]] == "cs"){
      print(qData)
      cs_graph(processed_graph_dataList[[qData]])
      cs_table(processed_table_dataList[[qData]])
    }
    else{
      print(paste0("One or more question ID(s) at ",qData," is invalid."))
      break
    }
  }
}

# system.time(result <- fullPlot.timer(processed_graph_dataList, processed_table_dataList))

start.time <- Sys.time()
fullPlot.timer(processed_graph_dataList, processed_table_dataList)
print(Sys.time() - start.time)

