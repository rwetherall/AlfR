require(httr)
require(magrittr)

##
#' @title
#' TODO
#' @description
#' TODO
#' @param uri TODO
#' @param userid TODO
#' @param password TODO
#' @return TODO
#' @export
##
alfticket <- function (uri, userid, password) {

  # try to get the authentication ticket for the repository
  response <-
    paste(uri, "/alfresco/api/-default-/public/authentication/versions/1/tickets", sep = "") %>%
    POST(body=list(userId = userid, password = password), encode = "json")

  if (http_error(response)) {

    # TODO do something!
    NULL

  } else {

    # get the ticket from the response
    fromJSON(content(response, "text"), flatten = TRUE)$entry$id
  }

}
