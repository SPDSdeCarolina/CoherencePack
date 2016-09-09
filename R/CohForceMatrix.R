#' A function that applies the CohDiff function to all nodes in a coherence 
#'   network
#' @param coh.matrix A matrix holding the weights of all the links in the 
#'   coherence network
#' @param data.node.id A numeric index that corresponds to the special 
#'   observation unit in the network
#' @param fixed.act.lvl The activation level, usually -1 or 1, at which you want
#'   each  of the nodes to be held at one at a time
#' @return A matrix where the columns are the differences between the activation
#'   level of each node when the element corresponding to that colum is fixed to 
#'   the activation value specified in fixed.act.lvl
#' @seealso \code{\link{CohDiff}} which this function wraps
#' @export
#' @examples
#'   CohForceMatrix()
CohForceMatrix <- function(coh.matrix, data.node.id, fixed.act.lvl){
  # A function that applies the CohDiff function to all nodes in a Coherence 
  #   network matrix
  # 
  # Arguments: 
  #   coh.matrix: A matrix hodling the weight of the links in the system of 
  #     arguments whose coherence you are trying to maximize
  #   data.node.id: the id given to the data proposition 
  #   fixed.act.lvl: The activation level, usually -1 or 1,  you are fixing the 
  #     node selected under fixed.node.id
  #
  # Returns:
  #   It creates a matrix where the columns are the difference in the activation
  #     of each node when the proposition corresponding to that column is fixed 
  #     to the value in fixed.act.lvl
  tmp <- sapply(1:nrow(coh.matrix), CohDiff, coh.matrix=coh.matrix, 
                data.node.id=data.node.id, fixed.act.lvl=fixed.act.lvl)
  return(tmp)
}
