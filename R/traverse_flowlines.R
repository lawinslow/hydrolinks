traverse_flowlines = function(g, distance, start, direction = "out"){
  nodes = c()
  to_check = c()
  to_check = sapply(neighbors(g, start, direction), function(x){
    to_check[x] = to_check[x] + x$LENGTHKM
  })
  if(distance == 0){
    return(to_check)
  }
  #for(i in 1:(distance - 1)){
  while(1){
    next_check = c()
    to_check = to_check[to_check != 0]
    nodes = c(nodes, to_check)
    for(j in 1:length(to_check)){
      next_check = c(next_check,sapply(neighbors(g, names(to_check)[j], direction), function(x){
        to_check[x] = to_check[x] + x$LENGTHKM
      }))
    }
    for(j in names(next_check)){
      if(next_check[j] > distance){
        break
      }
    }
    to_check = next_check
  }
  nodes = c(nodes, to_check)
  return(nodes)
}

neighbors = function(db, node, direction){
  graph = db %>%
    tbl("flowtable")
  if(direction == "out"){
    graph = filter(graph, From_Permanent_Identifier %in% node)
  }
  else if(direction == "in"){
    graph = filter(graph, To_Permanent_Identifier %in% node)
  }
  graph = collect(graph)
  ret = NULL
  if(direction == "out"){
    ret = data.frame(graph$To_Permanent_Identifier, graph$LENGTHKM)
  }
  else if(direction == "in"){
    ret = data.frame(graph$From_Permanent_Identifier, graph$LENGTHKM)
  }
  colnames(ret) = c("ID", "LENGTHKM")
  return(ret)
}
