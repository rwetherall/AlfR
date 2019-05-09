require(httr)
require(magrittr)

endpoint.tickets <- "/alfresco/api/-default-/public/authentication/versions/1/tickets"

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
alfticket <- function (uri, username, password) {

  # try to get the authentication ticket for the repository
  response <-
    paste(uri, endpoint.tickets, sep = "") %>%
    POST(body=list(userId = username, password = password), encode = "json")

  if (http_error(response)) {

    # TODO do something!
    NULL

  } else {

    # get the ticket from the response
    fromJSON(content(response, "text"), flatten = TRUE)$entry$id
  }

}
