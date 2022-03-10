#' rand_mpa
#'
#' Randomly generates a network of MPAs
#'
#' @param grid is a grid covering your planning area
#' @param areas is a vector of mpa area coverage in m^2
#' @param adj_edgelist is the self-intersecting neighbours edgelist of grid, could be done 'in function' but that would slow it down. Used to create contiguous multi-grid cell MPA's.
#' @param buff_edgelist is the intersecting edgelist of grid with a buffered grid, could be done 'in function' but that would slow it down. Used to guarantee minimum distances (buffer distance) between MPAs.
#' @param max_edge is a numeric constraint that represents the largest number of protected neighbouring cells that an unprotected cell can have before it becomes protected itself if the MPA still needs to 'grow'.
#'
#' @return
#' @export
#'
#' @examples
#' #insert example here
rand_mpa <- function(grid,areas,adj_edgelist,buff_edgelist=adj_edgelist,max_edge=4){
  # find number of grid cells for each area
  cells <- floor(areas/as.numeric(sf::st_area(grid[1,])))
  network <- NULL
  for(finalsize in cells){

    # create single grid cell mpa "kernel"
    size <- 1
    index_network <- buff_edgelist$row.id %in% network$grid_cell |
      buff_edgelist$row.id %in% buff_edgelist$row.id[buff_edgelist$col.id %in% network$grid_cell]
    id_network <- unique(buff_edgelist$row.id[index_network])
    outnetwork_adj <- adj_edgelist[!adj_edgelist$row.id %in% id_network,]

    # create MPA kernel (i.e. a random cell to start)
    mpa <- as.numeric(sample(buff_edgelist$row.id[!index_network],1))

    # grow the kernel if necessary
    while(size<finalsize){
      index_mpa <- outnetwork_adj$row.id %in% mpa & !outnetwork_adj$col.id %in% mpa

      # random growth or override?
      tab <- table(outnetwork_adj$col.id[index_mpa])
      if(max(tab)<max_edge){
        mpa <- c(mpa,
                 sample(outnetwork_adj$col.id[index_mpa],1))
      } else {
        # override
        mpa <- c(mpa,
                 as.numeric(sample(x=names(tab[tab==max(tab)])),1))
      }


      size <- length(mpa)
    }

    # warnings
    if(any(mpa %in% network$grid_cell)){
      warning("mpa is within network, this function is broken")
    }
    if(any(mpa %in% buff_edgelist$row.id[buff_edgelist$row.id %in% network$grid_cell])){
      warning("mpa is within buffer, this function is broken")
    }

    # create df with `mpa` and bind it to the network
    if(is.null(network)){
      mpadf <- data.frame(mpa_id=1,
                          grid_cell=mpa)
    } else {
      mpadf <- data.frame(mpa_id=max(network$mpa_id)+1,
                          grid_cell=mpa)
    }
    network <- dplyr::bind_rows(network,mpadf)
  }

  # join network with grid
  return(network %>%
           dplyr::left_join(grid %>%
                       dplyr::mutate(grid_cell=as.numeric(row.names(.))),by = "grid_cell") %>%
           sf::st_as_sf()
  )
}
