#' rand_stratified_mpa
#'
#' Randomly generates a stratified network of MPAs
#'
#' @param grid
#' @param areas
#' @param grid_strata
#' @param area_strata
#' @param adj_edgelist
#' @param buff_edgelist
#' @param max_edge
#'
#' @return
#' @export
#'
#' @examples
rand_stratified_mpa <- function(grid,areas,grid_strata,area_strata,adj_edgelist,buff_edgelist=adj_edgelist,max_edge=4){
  purrr::map(unique(grid_strata),function(s){
    rand_mpa(grid,
             areas[area_strata==s],
             adj_edgelist[adj_edgelist$row.id %in% which(grid_strata==s) & adj_edgelist$col.id %in% which(grid_strata==s),],
             buff_edgelist[buff_edgelist$row.id %in% which(grid_strata==s) & buff_edgelist$col.id %in% which(grid_strata==s),],
             max_edge) %>%
      mutate(mpa_id=paste(s,mpa_id)) }) %>%
    bind_rows()
}
