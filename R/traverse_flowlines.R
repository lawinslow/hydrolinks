#' @title traverse_flowlines
#' 
#' @description traverse hydrological network
#'
#' @param g network graph - load using \code{\link[dplyr]{src_sqlite}}
#' @param distance maximum distance to traverse in km
#' @param start character node to start
#' @param direction character; either "out" or "in"
#' 
#' @import dplyr
#'
#' @return list of nodes traversed
#' 
#' @export
#'
#' @examples \dontrun{traverse_flowlines(src_sqlite("flowtable.sqlite3"), 1000, "141329377", "out")}
traverse_flowlines = function(g, distance, start, direction = c("out", "in")){
  direction = match.arg(direction)
  nodes = c()
  n = neighbors(g, start, direction)
  to_check = n$LENGTHKM
  names(to_check) = n$ID
  # to_check = apply(neighbors(g, start, direction), 1, function(x){
  #   to_check = c(to_check, x["LENGTHKM"])
  #   names(to_check) = c(names(to_check), x["ID"])
  #   # if(!is.null(to_check[x["ID"]])){
  #   #   to_check[x["ID"]] = to_check[x["ID"]] + x["LENGTHKM"]
  #   # }
  #   # else{
  #   #   to_check[x["ID"]] = x["LENGTHKM"]
  #   # }
  # })
  if(distance == 0){
    return(to_check)
  }
  #for(i in 1:(distance - 1)){
  while(1){
    next_check = c()
    to_check = to_check[to_check != 0]
    nodes = c(nodes, names(to_check))
    for(j in 1:length(to_check)){
        n = neighbors(g, names(to_check)[j], direction)
        next_check_tmp = n$LENGTHKM + to_check[j]
        names(next_check_tmp) = n$ID
        next_check = c(next_check, next_check_tmp)
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
  ret$ID = as.character(ret$ID)
  return(ret)
}
