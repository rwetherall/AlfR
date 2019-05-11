require(httr)
require(magrittr)

# base endpoint helper
base_endpoint <- function(server, endpoint) paste(server, "/alfresco/api/-default-/public/", endpoint, sep="")

# tickets endpoint function
tickets_endpoint <- function(server) base_endpoint(server, "authentication/versions/1/tickets")

##
#' @title
#' Get connection session to Alfresco repository
#' @description
#' Get connection session to Alfresco repository including:
#' TODO list things available
#' @param server Alfresco server URL
#' @param username user name
#' @param password password
#' @return connection session to Alfresco repository
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

    # define endpoints for session
    node_endpoint <- function(node_id = "-root-") base_endpoint(server, "alfresco/versions/1/nodes/") %>% paste(node_id, sep="")
    node_content_endpoint <- function(node_id) node_endpoint(node_id) %>% paste("/content", sep="")

    # session details
    list(
      server = server,
      ticket = fromJSON(content(response, "text"), flatten = TRUE)$entry$id,
      node_endpoint = node_endpoint,
      node_content_endpoint = node_content_endpoint
    )
  }
}
