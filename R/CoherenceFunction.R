#' This function updates the activation levels of nodes in neural network to 
#'   solve coherence problems in accordance to the ECHO algorithm presented in 
#'   Thagard (1989)
#'
#' @param coh.matrix the relationship matrix whose coherence we are trying to 
#'   maximize 
#' @param data.node.id the index in coh.matrix corresponding to the special 
#'   observation node
#' @return a vector of activation values that maximizes the coherence of the 
#'   network
#' @export
#' @examples
#' Coherence()
Coherence <- function(coh.matrix, data.node.id){
  # This function updates activation levels of nodes based on the ECHO algorithm
  #   in Thagard (1989).
  # 
  # Args: 
  #   coh.matrix: the connection matrix whose coherence we are trying to 
  #     maximize. 
  #   data.node.id: the index in coh.matrix that corresponds to the data node, 
  #     it is assumed this index is the same as the id of the data node in the
  #     dataset with all the links.
  #
  # Returns:
  #   A vector of activation values
  data.index <- data.node.id 
  act.vector <- rep(.01, nrow(coh.matrix))
  act.vector[data.index] <- 1
  # Decay parameter presented in original Thagard Paper
  kDecayPar <- .05 
  # holding vector for future activation values
  act.vector.next <- rep(0, nrow(coh.matrix)) 
  t <- 0
  repeat{
    #a for loop to make sure all nodes get updated
    for (j in 1:nrow(coh.matrix)){ 
      # Calculate net input to node j
      net.input.j <- sum(coh.matrix[,j]*act.vector) 
      if (net.input.j > 0){ 
        act.vector.next[j] <- act.vector[j] * (1 - kDecayPar) + net.input.j * 
          (1 - act.vector[j])
      } else {
        act.vector.next[j] <- act.vector[j] * (1 - kDecayPar) + net.input.j * 
          (act.vector[j] - (-1))
      }
    }
    # fix data observation to 1  
    act.vector.next[data.index] <- 1
    total.change <- abs(act.vector - act.vector.next)
    # update the activatin vector with newly calculated values
    act.vector <- act.vector.next 
    # stop updating once no node changes its activation value by more than .001
    #   or once the network has been updated 200 times
    t <- t+1 #keeps track of number of iterations
    if (max(total.change) < .001 | t == 200){
      break 
    }
  }
  return(act.vector)
}
