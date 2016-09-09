#' Finds the difference in activation values between nodes in a normal coherence
#'   network and one where the value of one of the nodes has be held at a fixed 
#'   activation value
#' 
#' @param coh.matrix a amatrix holding the weights of the links in the system of 
#'   arguments whose coherence you are trying to maximize
#' @param data.node.id the index of the the special observation unit
#' @param fixed.node.id a numeric value representing the index of the node whose
#'   activation you are holding at a fixed value
#' @param fixed.act.val the value, usually -1 or 1, at which you are holding the
#'   activation level of the node represented by fixed.node.id
#' @return A vector of the difference between the activation values of the 
#'    propositions in coh.matrix and the activation values one gets when the 
#'    fix the activation value of a node in the network.
#' @seealso \code{\link{Coherence}} and \code{\link{CoherenceFixed}} which this
#'   function wraps
#' @export
#' @examples
#' CohDiff()
CohDiff <- function(coh.matrix, data.node.id, fixed.node.id, fixed.act.lvl){
  # Finds the difference in Coherence values when a selected node is held at a 
  #   fixed activation level.
  #
  # Arguments: 
  #   coh.matrix: A matrix hodling the weight of the links in the system of 
  #     arguments whose Coherence you are trying to maximize
  #   data.node.id: the id given to the data proposition
  #   fixed.node.id: the id of the proposition whose value you are seeking to 
  #     fix
  #   fixed.act.lvl: The activation level usually -1 or 1 you are fixing the 
  #     node selected under fixed.node.id
  # 
  # Return:
  #   A vector of the difference between the activation values of the 
  #     propositions in coh.matrix and the activation values one gets when the 
  #     fix the activation value of a node in the network
  tmp <- Coherence(coh.matrix, data.node.id) - CoherenceFixed(coh.matrix, 
                                                              data.node.id, 
                                                              fixed.node.id, 
                                                              yfixed.act.lvl)
  tmp[fixed.node.id] <- NA
  return(tmp)
}
