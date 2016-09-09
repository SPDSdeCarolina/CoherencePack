#' Function applies the DataObs function to a subset of data as specidied by the
#'   data.link.types argument
#' 
#' @param data.link.types character vector that corresponds to the subset of 
#'   links in the dataset -links to a special observation unit- you are trying 
#'   to model 
#' @param coh.matrix the matrix holding the link weights
#' @param data the dataset specifying relationship types between elements
#' @param data.node.id the index in coh.matrix corresponding to the special 
#'   observation unit
#' @return A matrix with weights refecting the explanatory relationships 
#'   specified in the function
#' @seealso \code{\link{DataObs}} which this function wraps
#' @export
#' @examples
#' AutoDataObs()
AutoDataObs <- function(data.link.types, coh.matrix, data, data.node.id){
  # It applies the DataObs function to a subset of data as specified by the 
  #   data.link.types argument
  # 
  # Args: 
  #   DataLinkType: Character string that corresponds to the type of link in 
  #     data that represents connections tot he special observation unit
  #   coh.matrix = The matrix holding the link weights
  #   data = the data frame with the arguments we are trying to model
  #   data.node.id = The index in coh.matrix  corresponding to special 
  #     observation unit 
  #
  # Returns:
  #   A matrix with weights reflecting the explanatory relationships 
  #     specified in the function
  target.nodes <- NULL
  for(i in data.link.types){
    temp <- unique(data$Target.id[which(data$Link.Type == i)])
    target.nodes <- c(target.nodes, temp)  
  }
  target.nodes <- unique(target.nodes)
  coh.matrix <- DataObs(args=target.nodes, coh.matrix=coh.matrix, 
                        data.id=data.node.id)
  return(coh.matrix)
}
