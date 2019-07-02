require(httr)
require(magrittr)

##
#' @title Get Alfresco node
#' @description
#' Gets the details of an Alfresco repository node matching \code{node_id} or, if provided, the node at \code{relative_path}
#' relative to \code{node_id}.
#' @param session valid Alfresco repository session
#' @param node_id node id, defaults to \code{-root-}
#' @param relative_path relative path from \code{node_id} to required node, defaults to \code{NULL}
#' @return Node details
#' @examples
#' \donttest{
#' # establish a connection to the alfresco content repository
#' my_session <- alf_session("alfresco.my-org.com", "admin", "admin")
#'
#' # get details of document node
#' my_document <- alf_node(my_session, relative_path = "testFolder/testdoc.txt")
#'
#' # output the name of the document
#' print(my_document$name)
#' #[1] "testdoc.txt"
#'
#' # output the details of documents content
#' print(my_document$content$mime_type)
#' #[1] "text/plain"
#' print(my_document$content$mime_type_name)
#' #[1] "Plain Text"
#' print(my_document$content$size)
#' #[1] 19
#' print(my_document$content$encoding)
#' #[1] "ISO-8859-1"
#'
#' # read document content
#' my_content_file <- file(my_document$content$as.file(), "r")
#' my_content <- readLines(my_content_file)
#' close(my_content_file)
#'
#' # upload new content
#' my_document <- my_document$content$update("resources/testuploaddoc.txt")
#' }
#' @export
##
alf_node <- function (session, node_id = "-root-", relative_path = NULL) {

  # TODO deal with invalid session

  # TODO deal with invalid path

  # build parameter list
  params <- list()
  if (!is.null(relative_path)) params$relativePath = relative_path

  # GET node details found at the path provided
  alf_GET(session$node_endpoint(node_id), session$ticket, params) %>%
    as.node(session)
}

##
#' @title Create a new Alfresco node
#' @description
#' Creates a new Alfresco repository node as a child of \code{node_id}.
#' @param session valid Alfresco repository session
#' @param node_id node id
#' @param node_details details of new node
#' @return node details
#' @export
##
alf_node.new <- function(session, node_id, node_details) {

  # TODO deal with invaild session

  # POST node update
  alf_POST(session$node_children_endpoint(node_id), session$ticket, body = toJSON(node_details, auto_unbox = TRUE)) %>%
    as.node(session)

}

##
# Helper to represent response as node information
##
as.node <- function (response, session) {

  # TODO determine whether this is a folder or not

  # node details
  list (
    id = response$entry$id,
    name = response$entry$name,
    content = as.content(response, session))
}

##
# Helper to represent response as content information
##
as.content <- function (response, session) {

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
  update <- function(file_path) {

    # check that the path provided references a file that exists
    if (!file.exists(file_path)) stop(paste("Can not upload content from", file_path, "as it does not exist."))

    # PUT the content
    response <- alf_PUT(endpoint, ticket, as="json", body=upload_file(file_path))

    # return updated node
    as.node(response, session)
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


