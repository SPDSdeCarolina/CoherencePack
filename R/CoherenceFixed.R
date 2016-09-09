#' A function that calculates the coherence of an explanatory system when the 
#'   value of a specified node is fixed
#' 
#' @param coh.matrix the relationship matrix whose coherence we are trying to 
#'   maximize 
#' @param data.node.id the index in coh.matrix corresponding to the special 
#'   observation node
#' @param fixed.node.id the index of the element whose value you are holding at 
#'   fixed value
#' @param fixed.act.lvl the value you are fixing a node at
#' @return A vector of activation values corresponding to the network 
#'   represented by coh.matrix
#' @seealso \code{\link{Coherence}} which this function is a variation of.
#' @export
#' @examples 
#' CoherenceFixed()
CoherenceFixed <- function(coh.matrix, data.node.id, fixed.node.id, 
                           fixed.act.lvl){
  # A function that calculates the Coherence of propositions in an explanatory 
  #   system when the value of a specificed node is fixed
  #
  # Args: 
  #   coh.matrix: A matrix holding the weight of the links in the system of 
  #     arguments whose Coherence you are trying to maximize
  #   data.node.id: the id given to the data proposition
  #   fixed.node.id: the id of the proposition whose value you are seeking to 
  #     fix
  #   fixed.act.lvl: The activation level (ususally -1 or 1) you are fixing the
  #     node selected under fixed.node.id
  # 
  # Returns:
  #   A vector of activation values corresponding to the network represented by 
  #     coh.matrix
  
  data.index <- data.node.id 
  # Set initial activation for all nodes to .01
  act.vector <- rep(.01, nrow(coh.matrix))
  # Set activation for special observation unit to 1
  act.vector[data.index] <- 1 
  # Set selected node to fixed activation level
  act.vector[fixed.node.id] <- fixed.act.lvl 
  # Decay parameter presented in original Thagard Paper
  kDecayPar <- .05 
  # holding vector for future activation values
  act.vector.next <- rep(0, nrow(coh.matrix)) 
  t <- 0
  repeat{
    # a for loop to make sure all nodes get updated
    for(j in 1:nrow(coh.matrix)){ 
      # Calculate net input to node j
      net.input.j <- sum(coh.matrix[,j]*act.vector) 
      # check wheter to use first or second updating function
      if (net.input.j > 0){ 
        act.vector.next[j] <- act.vector[j] * (1 - kDecayPar) + net.input.j * 
          (1-act.vector[j])
      } else {
        act.vector.next[j] <- act.vector[j] * (1 - kDecayPar) + net.input.j * 
          (act.vector[j] - (-1))
      }
    }
    # fix data observation to 1 
    act.vector.next[data.index] <- 1 
    # Set selected node to fixed activation level
    act.vector.next[fixed.node.id] <- fixed.act.lvl 
    total.change <- abs(act.vector - act.vector.next)
    # update the activation vector with newly calculated values
    act.vector <- act.vector.next 
    # keeps track of number of iterations
    t <- t + 1 
    # stop updating once no node changes its activation value by more than .001
    #   or once the network has been updated 200 times
    if (max(total.change) < .001 | t == 200){
      break 
    }
  }
  return(act.vector)
}
