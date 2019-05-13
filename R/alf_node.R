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

  id <- response$entry$id

  read_content <- function() {

    alf_GET(session$node_content_endpoint(id), session$ticket, format="file")
  }

  list (
    id = id,
    read_content = read_content
  )
}


