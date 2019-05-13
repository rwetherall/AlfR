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

  print(response)

  id <- response$entry$id

  file <- function() alf_GET(session$node_content_endpoint(id), session$ticket, format="file")

  content <- list (
    mime_type = response$entry$content$mimeType,
    mime_type_name = response$entry$content$mimeTypeName,
    size = response$entry$content$sizeInBytes,
    encoding = response$entry$content$encoding,
    file = file
  )

  list (
    id = id,
    content = content
  )
}


