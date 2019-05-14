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

  # print(response)

  id <- response$entry$id

  # TODO determine whether this is a folder or not so the following can be excluded (or not)

  file <- function(destination_file = NULL) {
    if (is.null(destination_file)) destination_file <- tempfile()
    bfile <- base::file(destination_file, "wb")
    alf_GET(session$node_content_endpoint(id), session$ticket, format="raw")  %>% writeBin(bfile)
    close(bfile)
    destination_file
  }

  `file<-` <- function(file) {

  }

  content <- list (
    mime_type = response$entry$content$mimeType,
    mime_type_name = response$entry$content$mimeTypeName,
    size = response$entry$content$sizeInBytes,
    encoding = response$entry$content$encoding,
    file = file
  )

  list (
    id = id,
    name = response$entry$name,
    content = content
  )
}


