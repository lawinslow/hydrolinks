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
#' @examples
#' \dontrun{
#' traverse_flowlines(src_sqlite("flowtable.sqlite3"), 1000, "141329377", "out")
#' }
traverse_flowlines = function(g, distance, start, direction = c("out", "in")){
  direction = match.arg(direction)
  nodes = data.frame()
  n = neighbors(g, start, direction)
  n$LENGTHKM[is.na(n$LENGTHKM)] = 0
  to_check = n$LENGTHKM
  names(to_check) = n$ID
  if(distance == 0){
    nodes = cbind(names(to_check), to_check)
    rownames(nodes) = c(1:nrow(nodes))
    colnames(nodes) = c("PERMANENT_", "LENGTHKM")
    return(nodes)
  }
  while(1){
    next_check = c()
    to_check = to_check[names(to_check) != "0"]
    nodes = rbind(nodes, cbind(names(to_check), to_check))
    for(j in 1:length(to_check)){
        n = neighbors(g, names(to_check)[j], direction)
        n$LENGTHKM[is.na(n$LENGTHKM)] = 0
        next_check_tmp = n$LENGTHKM + to_check[j]
        names(next_check_tmp) = n$ID
        next_check = c(next_check, next_check_tmp)
    }

    #We need a stop condition where all further neighbors go nowhere
    if(all(names(next_check) == '0')){
      nodes = rbind(nodes, cbind(names(to_check), to_check))
      rownames(nodes) = c(1:nrow(nodes))
      colnames(nodes) = c("PERMANENT_", "LENGTHKM")
      return(nodes)
    }

    for(j in names(next_check)){
      if(next_check[j] > distance){
        nodes = rbind(nodes, cbind(names(to_check), to_check))
        rownames(nodes) = c(1:nrow(nodes))
        colnames(nodes) = c("PERMANENT_", "LENGTHKM")
        return(nodes)
      }
    }
    to_check = next_check
  }
}

neighbors = function(db, node, direction){
  From_Permanent_Identifier = NULL
  To_Permanent_Identifier = NULL
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
