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

  # GET node details
  response <- alf_GET(session$node_endpoint(), session$ticket, list(relativePath=path))

  # TODO determine whether this is a folder or not so the following can be excluded (or not

  # node details
  list (
    id = response$entry$id,
    name = response$entry$name,
    content = alf_content(session, response)
  )
}

##
#' @title
#' TODO
#' @description
#' TODO
#' @param session TODO
#' @param response TODO
#' @return TODO
##
alf_content <- function (session, response) {

  id <- response$entry$id

  ##
  #' @title Get content as file
  ##
  as.file <- function(destination_file = NULL) {

    # create temp file if none provided
    if (is.null(destination_file)) destination_file <- tempfile()

    # open file for binary write
    bfile <- base::file(destination_file, "wb")

    # GET content and write to file
    alf_GET(session$node_content_endpoint(id), session$ticket, format="raw")  %>% writeBin(bfile)

    # close and return
    close(bfile)
    destination_file
  }

  # as.text
  # as.table

 # upload_file

  list (
    mime_type = response$entry$content$mimeType,
    mime_type_name = response$entry$content$mimeTypeName,
    size = response$entry$content$sizeInBytes,
    encoding = response$entry$content$encoding,
    as.file = as.file
  )

}


