
#' Creates explanatory links between elements in a matrix
#' 
#' @param arg1 a vector of indices corresponding to propositions that co-explain
#'   the argument represented by arg 2
#' @param arg2 a single value corresponding to the proposition being explained
#' @param coh.matrix The matrix that will contain the value of the links in the 
#'   network whose coherence we are maximizing
#' @return: A matrix with new link weights representing the explanatory 
#'   relationships between the elements in arg1 and arg2.
#' @export
#' @examples
#' Explain()   
Explain <- function(arg1, arg2, coh.matrix=c1){
  # Args:   
  #   arg1: a vector of indices corresponding to propositions that co-explain 
  #     the argument whose vector is represented in arg2
  #   arg2: a single value corresponding to the proposition being explained
  #   coh.matrix: A relationship matrix
  # Returns: It calculates a weight that gets smaller the more elements in arg1.
  #   It add the value of that weight to links between all the pairs in arg1 and
  #   between all the elements of arg1 and arg2.
  w <- .05/length(arg1)
  for(i in arg1){
    coh.matrix[i, arg2] <- coh.matrix[i, arg2] + w
    coh.matrix[arg2, i] <- coh.matrix[arg2, i] + w
  }
  n <- length(arg1)
  for(i in arg1){
    for(j in arg1[which(arg1 == i):n]){
      if (i==j){
        coh.matrix[i, j] <- coh.matrix[i, j]
        coh.matrix[j, i] <- coh.matrix[j, i]
      } else {
        coh.matrix[i, j] <- coh.matrix[i, j] + w
        coh.matrix[j, i] <- coh.matrix[j, i] + w
      }
    }
  }
  return(coh.matrix)
}
