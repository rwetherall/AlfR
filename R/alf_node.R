require(httr)
require(magrittr)

##
#' @title
#' TODO
#' @description
#' TODO
#' @param ticket TODO
#' @param path TODO
#' @return TODO
#' @export
##
alf_node <- function (ticket, path) {

  response <- alf_endpoints$node(ticket$uri) %>% alf_GET(ticket, list(relativePath=path))

  content <- function() {
    ## TODO
  }

  list (
    id = response$entry$id
  )
}


