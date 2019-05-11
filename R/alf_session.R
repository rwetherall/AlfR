require(httr)
require(magrittr)

# endpoint prefix
endpoint_prefix <- "/alfresco/api/-default-/public/"
endpoint_ticket  <- "authentication/versions/1/tickets"

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
    paste(server, endpoint_prefix, endpoint_ticket, sep="") %>%
    POST(body=list(userId = username, password = password), encode = "json")

  # check for error
  if (http_error(response))

    # indicate authentication failed
    if (response$status_code == 403) stop("Authentication failed, please check your username and password are correct.")

    # otherwise stop with error message
    else stop(http_status(response)$message)

  # get the ticket from the response
  else {



    list(
      server = server,
      ticket = fromJSON(content(response, "text"), flatten = TRUE)$entry$id
    )
  }
}
