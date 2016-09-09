#' Creates or modifies weight of link between two elements in a matrix 
#'   representing a Coherence network.
#' 
#' @param arg1 The index of the first contradictory element
#' @param arg2 The index of the second contradictory element, must be different 
#'   from arg1
#' @param coh.matrix The matrix that will contain the value of the links in the 
#'   Network whose Coherence we are maximizing
#' @return A matrix with new weight representing the contradictory relationship
#'   between the elements represented by arg1 and arg2.
#' @export
#' @examples
#' Contradict()

Contradict <- function(arg1, arg2, coh.matrix=c1){
  # Creates or modifies weight of link between two elements in a matrix 
  #   representing a Coherence network
  #
  # Args:
  #   arg1: The index of the first contradictory element
  #   arg2: The index of the second contradictory element, must be different 
  #     from arg1
  #   coh.matrix: The matrix that will contain the value of the links in the 
  #     Network whose Coherence we are maximizing
  #
  #  Returns: 
  #    A matrix with new weight representing the contradictory relationship
  #      between the elements represented by arg1 and arg2
  coh.matrix[arg1, arg2] <- coh.matrix[arg1, arg2] - .2
  coh.matrix[arg2, arg1] <- coh.matrix[arg2, arg1] - .2
  return(coh.matrix)
}