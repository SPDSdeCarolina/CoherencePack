#' Function applies the explain function to a subset of data as specified by the
#'   explain.link.type argument
#'   
#' @param explain.link.type a character vector that contains the variable names
#'   of the links in the dataset you are making a network out of.
#' @param coh.matrix a relationship matrix that will hold the link weights 
#'   representing the relationships between the different elements of the 
#'   explanatory nettwork
#' @param data data frame witht he arguments we are trying to model
#' @return a relationship matrix with the weights reflecting the explanatory 
#'   relationships specified in the function
#' @seealso \code{\link{Explain}} which the function wraps
#' @export
#' @examples
#' AutoExplain()
AutoExplain <- function(explain.link.types, coh.matrix, data){
  # It applies the explain function to a subset of data as specified by the 
  #   explain.link.type argument 
  #
  # Args: 
  #   explain.link.type: A vector of character string that contains the 
  #     variable names of the explanatory links in your dataset you are seeking 
  #     to make an explanatory network of. The assumption is that explanatory 
  #     links will divided into document years.   
  #   coh.matrix: The matrix holding the link weights
  #   data: The data frame with the arguments we are trying to model
  #
  # Returns:
  #   A Coherence matrix with weight reflecting the explanatory relationships 
  #     specified in the function
  target.nodes <- NULL
  
  for(i in explain.link.types){
    temp <- unique(data$Target.id[which(data$Link.Type == i)])
    target.nodes <- c(target.nodes, temp)
  }
  
  target.nodes <- unique(target.nodes)
  
  for(i in target.nodes){
    source.nodes.i <- NULL
    for (j in explain.link.types){
      temp <- unique(
        data$Source.id[which(data$Target.id == i & data$Link.Type == j)]
      ) 
      source.nodes.i <- c(temp)
      source.nodes.i <- unique(source.nodes.i)
      coh.matrix <- Explain(source.nodes.i, i, coh.matrix)
    }
  }
  return(coh.matrix)
}
