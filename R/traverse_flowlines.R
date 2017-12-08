#' @title traverse_flowlines
#'
#' @description traverse hydrological network
#'
#' @param g_path path of flowtable database
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
#' traverse_flowlines("flowtable.sqlite3", 1000, "141329377", "out")
#' }
traverse_flowlines = function(g_path, distance, start, direction = c("out", "in"), max_depth = 10000){
  # check_dl_file("traversal_graph.csv")
  # g = src_sqlite(file.path(cache_get_dir(), "unzip", "flowtable", "flowtable.sqlite3"))
  # for testing: load from specified path
  g = src_sqlite(g_path)
  direction = match.arg(direction)
  nodes = data.frame(rep(NA, max_depth), rep(NA, max_depth), rep(NA, max_depth), stringsAsFactors = FALSE)
  colnames(nodes) = c("PERMANENT_", "LENGTHKM", "CHILDREN")
  iterations = 1
  n = neighbors(g, start, direction)
  if(nrow(n) == 0){
    nodes = nodes[1, ]
    return(nodes)
  }
  n$LENGTHKM[is.na(n$LENGTHKM)] = 0
  to_check = n$LENGTHKM
  names(to_check) = n$ID
  if(distance == 0){
    nodes = cbind(names(to_check), to_check, NA, NA)
    rownames(nodes) = c(1:nrow(nodes))
    colnames(nodes) = c("PERMANENT_", "LENGTHKM", "CHILDREN")
    return(nodes)
  }
  while(1){
    next_check = c()
    if(all(names(to_check) == "0")){
      nodes = nodes[!is.na(nodes$PERMANENT_),]
      return(nodes)
    }

    to_check = to_check[names(to_check) != "0"]
    to_check = to_check[which(!(names(to_check) %in% nodes[,1]))]

    if(length(to_check) == 0){
      nodes = nodes[!is.na(nodes$PERMANENT_),]
      return(nodes)
    }

    if(length(names(to_check)) > 1){
      nodes[c(iterations:(iterations+length(names(to_check)) - 1)), ] = cbind(names(to_check), to_check, NA)
    }
    else{
      nodes[iterations, ] = cbind(names(to_check), to_check, NA)
    }

    iterations = iterations + length(to_check)
    for(j in 1:length(to_check)){
      n = neighbors(g, names(to_check)[j], direction)
      #n = n[]
      nodes[which(nodes[,1] == names(to_check)[j]), 3] = paste(n$ID, sep = ",", collapse = ",")
      n$LENGTHKM[is.na(n$LENGTHKM)] = 0
      next_check_tmp = n$LENGTHKM + to_check[j]
      names(next_check_tmp) = n$ID
      next_check = c(next_check, next_check_tmp)
    }
    next_check = next_check[unique(names(next_check))]


    if(iterations > max_depth){
      next_nodes = data.frame(names(next_check), next_check, "STUCK")
      colnames(next_nodes) = c("PERMANENT_", "LENGTHKM", "CHILDREN")
      nodes = rbind(nodes, next_nodes)
      nodes = nodes[!is.na(nodes$PERMANENT_),]
      return(nodes)
    }

    # if distance is less than zero, continue traversing until an end is reached
    if(distance < 0 && any(names(next_check) == '0')){
      nodes = nodes[!is.na(nodes$PERMANENT_),]
      return(nodes)
    }

    #We need a stop condition where all further neighbors go nowhere
    if(all(names(next_check) == '0')){
      nodes = nodes[!is.na(nodes$PERMANENT_),]
      return(nodes)
    }


    for(j in names(next_check)){
      if(distance > 0 && next_check[j] > distance){
        nodes = nodes[!is.na(nodes$PERMANENT_),]
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
