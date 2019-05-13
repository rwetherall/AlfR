require(jsonlite)
require(magrittr)

##
#' @title
#' TODO
#' @description
#' TODO
#' @param endpoint TODO
#' @param ticket TODO
#' @param params TODO
#' @param format TODO
#' @return TODO
#' @export
##
alf_GET <- function (endpoint, ticket, params=list(), format=c("json", "file")) {

  # check we have a valid format
  format <- match.arg(format)

  # construct GET call
  get_call <- quote(
    GET(
      # resolve parameters
      add_params(endpoint, params),
      # add authentication
      add_headers(Authorization = paste("Basic", base64_enc(ticket)))))

  # return content as json
  if (format == "json") eval(get_call) %>% content("text") %>% fromJSON(flatten = TRUE)

  # return content as a file
  else if (format == "file") {
    tmp <- tempfile()
    get_call[[length(get_call)+1]] <- quote(write_disk(tmp))
    eval(get_call)
    tmp
  }
}


add_params <- function (endpoint, params, sep="?") {

  if (length(params) > 0)
    add_params(
      paste(endpoint, sep, names(params[1]), "=", params[[1]], sep=""),
      params[-1], sep="&")
  else
    endpoint
}

