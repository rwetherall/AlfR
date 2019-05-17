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

  ##
  # Helper to represent response as node information
  ##
  as.node <- function (response) {

    # TODO determine whether this is a folder or not

    # node details
    list (
      id = response$entry$id,
      name = response$entry$name,
      content = as.content(response))
  }

  ##
  # Helper to represent response as content information
  ##
  as.content <- function (response) {

    endpoint <- session$node_content_endpoint(response$entry$id)
    ticket <- session$ticket

    ##
    # Get content as file
    ##
    as.file <- function(destination_file = NULL) {

      # create temp file if none provided
      if (is.null(destination_file)) destination_file <- tempfile()

      # open file for binary write
      bfile <- base::file(destination_file, "wb")

      # GET content and write to file
      alf_GET(endpoint, ticket, as="raw")  %>% writeBin(bfile)

      # close and return
      close(bfile)
      destination_file
    }

    ##
    # Update content
    ##
    update <- function(path) {

      # check that the path provided references a file that exists
      if (!file.exists(path)) stop(paste("Can not upload content from", path, "as it does not exist."))

      # PUT the content
      response <- alf_PUT(endpoint, ticket, as="json", body=upload_file(path))

      # return updated node
      as.node(response)
    }

    list (
      mime_type = response$entry$content$mimeType,
      mime_type_name = response$entry$content$mimeTypeName,
      size = response$entry$content$sizeInBytes,
      encoding = response$entry$content$encoding,
      as.file = as.file,
      update = update
    )
  }

  # TODO deal with invalid path

  # GET node details found at the path provided
  alf_GET(session$node_endpoint(), session$ticket, list(relativePath=path)) %>% as.node()

}


