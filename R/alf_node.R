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
#' @example R/examples/example_alf_node.R
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
#' @example R/examples/example_alf_node.R
#' @export
##
alf_node.new <- function(session, node_id, node_details) {

  # TODO deal with invaild session

  # POST node update
  alf_POST(session$node_children_endpoint(node_id), session$ticket, body = toJSON(node_details, auto_unbox = TRUE)) %>%
    as.node(session)
}

##
#' @title Deletes an Alfresco node
#' @description
#' Deletes an Alfresco node identified by \code{node_id}. If the node is a folder then all the
#' delete recurses through the primary children.
#' @param session valid Alfresco repository session
#' @param node_id node id to delete
#' @param permanent indicates whether the node is permanently deleted or places in the trashcan where
#' where it can be recovered from.  \code{FALSE} by default.
#' @example R/examples/example_alf_node.R
#' @export
##
alf_node.delete <- function(session, node_id, permanent = FALSE) {

  # TODO deal with invaild session

  # DELETE node
  alf_DELETE(session$node_endpoint(node_id), session$ticket, params=list(permanent=permanent))
}

## TODO alf_node.exists

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


