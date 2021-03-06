% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoherenceFunctions.R
\name{DataObs}
\alias{DataObs}
\title{Creates link weights between propositions and a special observation node}
\usage{
DataObs(args, data.id, coh.matrix = c1)
}
\arguments{
\item{args}{a vector of indices corresponding to all the elements in the 
coherence network that have a link to the special observation node}

\item{data.id}{the value of the indec for the special data unit}

\item{coh.matrix}{a matrix containing the link weights between all the nodes
in the network}
}
\value{
A matrix with new link weights reflecting the explanatory 
  relationships between the elements in args and te special observation unit
}
\description{
Creates link weights between propositions and a special observation node
}

