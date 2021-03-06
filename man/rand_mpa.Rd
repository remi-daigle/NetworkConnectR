% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rand_mpa.R
\name{rand_mpa}
\alias{rand_mpa}
\title{rand_mpa}
\usage{
rand_mpa(grid, areas, adj_edgelist, buff_edgelist = adj_edgelist, max_edge = 4)
}
\arguments{
\item{grid}{is a grid covering your planning area}

\item{areas}{is a vector of mpa area coverage in m^2}

\item{adj_edgelist}{is the self-intersecting neighbours edgelist of grid, could be done 'in function' but that would slow it down. Used to create contiguous multi-grid cell MPA's.}

\item{buff_edgelist}{is the intersecting edgelist of grid with a buffered grid, could be done 'in function' but that would slow it down. Used to guarantee minimum distances (buffer distance) between MPAs.}

\item{max_edge}{is a numeric constraint that represents the largest number of protected neighbouring cells that an unprotected cell can have before it becomes protected itself if the MPA still needs to 'grow'.}
}
\value{

}
\description{
Randomly generates a network of MPAs
}
\examples{
#insert example here
}
