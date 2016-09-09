#' Creates link weights between propositions and a special observation node
#' 
#' @param args a vector of indices corresponding to all the elements in the 
#'   coherence network that have a link to the special observation node
#' @param data.id the value of the indec for the special data unit
#' @param coh.matrix a matrix containing the link weights between all the nodes
#'   in the network
#' @return A matrix with new link weights reflecting the explanatory 
#'   relationships between the elements in args and te special observation unit
#' @export
#' @examples
#' DataObs()
DataObs <- function(args, data.id, coh.matrix=c1){
  # Creates link weights between propositions and a special observation node
  #
  # Args: 
  #   args: a vector of indices corresponding to all the arguments that have a 
  #     link to the special data unit.
  #   coh.matrix: a matrix representing the links between all the nodes in the 
  #     network
  #   data.id: the value of the index for the special data unit. 
  #
  # Returns: It creates a special link between the nodes represented in args and
  #   the special data unit. 
  for(i in args){
    coh.matrix[i, data.id] <- .1
    coh.matrix[data.id, i] <- .1
  }
  return(coh.matrix)
}
