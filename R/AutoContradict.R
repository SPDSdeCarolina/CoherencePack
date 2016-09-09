#' Function applies the Contradict function to a set of element pairs that share
#'   the contradictory relationships specified in contradict.link.types
#' 
#' @param contradict.link.types the subset of contradictory links in the data 
#'   set we are seeking to model
#' @param data data frame with element pairs and link types between them
#' @param coh.matrix a relationship matrix whose weights will be modified by 
#'   this function 
#' @return A relationship matrix with weights reflecting the contradictory 
#'   relationships specified in this function
#' @seealso \code{\link{Contradict}} which this function wraps
#' @export
#' @examples
#' AutoContradict()
AutoContradict <- function(contradict.link.types, data, coh.matrix){
  # What it does: It applies the contradict function to a set  of proposition 
  #   pairs that share the links in contradict.link.types
  # Args: 
  #   contradict.link.types: a vector of character strings that correspond 
  #     to the contradict relationships of interest,
  #   data: the data frame with the relationships
  #   coh.matrix == the holding matrix with the weights of the explanatory
  #     relationships
  #
  # Returns: 
  #   A matrix with weights reflecting the contradictory relationships 
  #     specified in the function
  target.pairs <- NULL
  for(i in contradict.link.types){
    temp <- cbind(data$Source.id[which(data$Link.Type == i)], 
                  data$Target.id[which(data$Link.Type == i)])
    target.pairs <- rbind(target.pairs, temp)
  }
  target.pairs <- unique(target.pairs)
  # Begin trick to eliminate indentical pairs whose elements are in reverse 
  #   order
  mn <- pmin(target.pairs[,1], target.pairs[,2])
  mx <- pmax(target.pairs[,1], target.pairs[,2])
  int <- as.numeric(interaction(mn, mx))
  target.pairs <- target.pairs[match(unique(int), int), ]
  # End trick
  tmp <- nrow(target.pairs)
  for(i in 1:tmp){
    coh.matrix <- Contradict(arg1=target.pairs[i, 1], arg2=target.pairs[i, 2], 
                             coh.matrix=coh.matrix)
  }
  return(coh.matrix)
}
