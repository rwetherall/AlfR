require(httr)
require(magrittr)
require(stringr)

# base endpoint helper
base_endpoint <- function(server, endpoint) paste(server, "/alfresco/api/-default-/public/", endpoint, sep="")

# tickets endpoint function
tickets_endpoint <- function(server) base_endpoint(server, "authentication/versions/1/tickets")
ticket_endpoint <- function(server) paste(tickets_endpoint(server), "-me-", sep="/")

##
#' @title
#' Get connection session to Alfresco content repository
#' @description
#' Validates authentication details with Alfresco content repository, returning ticket, server details and endpoints if
#' successful.
#' @param server Alfresco server URL
#' @param username user name
#' @param password password
#' @return Connection session to Alfresco repository
#' @example R/examples/example_alf_session.R
#' @export
##
alf_session <- function (server, username, password) {

  # try to get the authentication ticket for the repository
  response <-
    tickets_endpoint(server) %>%
    POST(body=list(userId = username, password = password), encode = "json")

  # check for error
  if (http_error(response))

    # indicate authentication failed
    if (response$status_code == 403) stop("Authentication failed, please check your username and password are correct.")

    # otherwise stop with error message
    else stop(http_status(response)$message)

  # get the ticket from the response
  else {

    # node end points
    node_endpoint <- function(node_id) base_endpoint(server, "alfresco/versions/1/nodes/") %>% paste(node_id, sep="")
    node_content_endpoint <- function(node_id) node_endpoint(node_id) %>% paste("/content", sep="")
    node_children_endpoint <- function(node_id) node_endpoint(node_id) %>% paste("/children", sep="")

    # session details
    list(
      server = server,
      ticket = fromJSON(content(response, "text"), flatten = TRUE)$entry$id,
      node_endpoint = node_endpoint,
      node_content_endpoint = node_content_endpoint,
      node_children_endpoint = node_children_endpoint
    )
  }
}

##
#' @title
#' Determine whether a session is valid.
#' @description
#' Determines whether a given session is still valid or not.
#' @param session session
#' @return \code{TRUE} if the session is valid, \code{FALSE} otherwise
#' @example R/examples/example_alf_session.R
#' @export
##
alf_session.is_valid <- function (session) {

  tryCatch(

    # TRUE if session is valid
    if (!is.null(alf_GET(ticket_endpoint(session$server), ticket=session$ticket, params=list(ticket=session$ticket)))) TRUE,

    # FALSE if session is invalid, otherwise rethrow error
    error = function(e) if (str_detect(string=e$message, pattern = "401")) FALSE else stop(e)
  )
}

##
#' @title
#' Invalidates a session.
#' @description
#' Invalidates a valid session so it can no longer be used to connect to an Alfresco repository.
#' @param session session
#' @example R/examples/example_alf_session.R
#' @export
##
alf_session.invalidate <- function (session) {

  tryCatch (

    # attempt to invalidate the ticket
    alf_DELETE(ticket_endpoint(session$server), ticket=session$ticket, params=list(ticket=session$ticket)),

    # ignore 401, but rethrow everything else in case it's a genuine error
    error = function (e) if (!str_detect(string=e$message, pattern = "401")) stop(e)
  )
}
