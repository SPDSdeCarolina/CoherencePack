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
