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

  response <- alf_endpoints$node(session$server) %>% alf_GET(session, list(relativePath=path))

  content <- function() {
    ## TODO
  }

  list (
    id = response$entry$id
  )
}


