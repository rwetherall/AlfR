require(httr)
require(magrittr)

##
#' @title
#' Get Alfresco authentication ticket
#' @description
#' Get Alfresco authentication ticket for specified respository with provided credentials.
#' @param uri base uri to Alfresco repository
#' @param username user name
#' @param password password
#' @return Alfresco ticket that can be used to authenticate subsequent calls to the repository
#' @export
##
alf_ticket <- function (uri, username, password) {

  # try to get the authentication ticket for the repository
  response <-
    alf_endpoints$tickets(uri) %>%
    POST(body=list(userId = username, password = password), encode = "json")

  # check for error
  if (http_error(response))

    # indicate authentication failed
    if (response$status_code == 403) stop("Authentication failed, please check your username and password are correct.")

    # otherwise stop with error message
    else stop(http_status(response)$message)

  # get the ticket from the response
  else list(
    uri = uri,
    ticket = fromJSON(content(response, "text"), flatten = TRUE)$entry$id
  )
}
