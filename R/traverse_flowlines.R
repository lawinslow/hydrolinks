#' @title traverse_flowlines
#'
#' @description traverse hydrological network
#' 
#' @param max_distance maximum distance to traverse in km. If negative, traverse until the ocean (node 0) or max_steps is reached.
#' @param start character node to start
#' @param direction character; either "out" or "in"
#' @param max_steps maximum traversal steps before terminating
#'
#' @import dplyr
#'
#' @return dataframe of nodes traversed, distance from the start node to each node, and the children of each node.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' traverse_flowlines(1000, "141329377", "out")
#' # this example traverses until a cycle is found and the end of the network is reached.
#' }
traverse_flowlines = function(max_distance, start, direction = c("out", "in"), max_steps = 10000){
  check_dl_file(system.file('extdata/flowtable.csv', package='hydrolinks'))
  check_dl_file(system.file('extdata/shape_id_cache.csv', package='hydrolinks'), fname = "nhdh_flowline_ids.zip")
  g = src_sqlite(file.path(cache_get_dir(), "unzip", "flowtable.zip", "flowtable.sqlite3"))
  direction = match.arg(direction)
  if(start == 0){
    stop("Cannot traverse from node 0!")
  }
  nodes = data.frame(rep(NA, max_steps), rep(NA, max_steps), rep(NA, max_steps), stringsAsFactors = FALSE)
  colnames(nodes) = c("PERMANENT_", "LENGTHKM", "CHILDREN")
  iterations = 1
  n = neighbors(g, start, direction)
  if(nrow(n) == 0){
    flowline = get_shape_by_id(start, feature_type = "flowline", dataset = "nhdh", match_column = "PERMANENT_")
    if(!is.na(flowline$WBAREA_PER)){
      warning(paste0("Start ID provided is a virtual flowline inside a waterbody. Continuing from ", flowline$WBAREA_PER))
      n = neighbors(g, flowline$WBAREA_PER, direction)
    }
    else{
      nodes = nodes[1, ]
      return(nodes)
    }
  }
  n$LENGTHKM[is.na(n$LENGTHKM)] = 0
  to_check = n$LENGTHKM
  names(to_check) = n$ID
  if(max_distance == 0){
    nodes = cbind(names(to_check), to_check, NA, NA)
    rownames(nodes) = c(1:nrow(nodes))
    return(nodes)
  }
  while(1){
    next_check = c()
    
    if(all(names(to_check) == "0")){
      browser()
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
    
    
    if(iterations > max_steps){
      next_nodes = data.frame(names(next_check), next_check, "STUCK")
      colnames(next_nodes) = c("PERMANENT_", "LENGTHKM", "CHILDREN")
      nodes = rbind(nodes, next_nodes)
      nodes = nodes[!is.na(nodes$PERMANENT_),]
      return(nodes)
    }
    
    # if max_distance is less than zero, continue traversing until an end is reached
    if(max_distance < 0 && any(names(next_check) == '0')){
      nodes = nodes[!is.na(nodes$PERMANENT_),]
      return(nodes)
    }
    
    #We need a stop condition where all further neighbors go nowhere
    if(all(names(next_check) == '0')){
      nodes = nodes[!is.na(nodes$PERMANENT_),]
      return(nodes)
    }
    
    
    for(j in names(next_check)){
      if(max_distance > 0 && next_check[j] > max_distance){
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
