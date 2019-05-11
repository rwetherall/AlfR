require(httr)
require(magrittr)

##
#' @title
#' TODO
#' @description
#' TODO
#' @param session TODO
#' @param path TODO
#' @return TODO
#' @export
##
alf_node <- function (session, path) {

  response <- alf_GET(session$node_endpoint(), session$ticket, list(relativePath=path))

  content <- function() {
    ## TODO
  }

  list (
    id = response$entry$id
  )
}


