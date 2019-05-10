require(jsonlite)

##
#' @title
#' TODO
#' @description
#' TODO
#' @param endpoint TODO
#' @param ticket TODO
#' @param params TODO
#' @return TODO
#' @export
##
alf_GET <- function (endpoint, ticket, params=list())
  add_params(endpoint, params) %>%
  GET(add_headers(Authorization = paste("Basic", base64_enc(ticket$ticket)))) %>%
  content("text") %>%
  fromJSON(flatten = TRUE)


add_params <- function (endpoint, params, sep="?")
  if (length(params) > 0) {
    paste(endpoint, sep, names(params[1]), "=", params[[1]], sep="") %>% add_params(params[-1], sep="&")
  } else
    endpoint

