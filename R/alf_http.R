require(jsonlite)
require(magrittr)

##
#' @title Alfresco GET method
#' @description Helper to make a GET call to the Alfresco REST API
#' @param endpoint base endpoint URI
#' @param ticket authentication ticket
#' @param params optional list of parameters
#' @param format return format from \code{json} (default), \code{raw}
#' @param body request body
#' @return result based on format provided
#' @export
##
alf_GET <- function (endpoint, ticket, params=list(), format=c("json", "raw"), body=NULL)
  alf_method("GET", endpoint, ticket, params, format, body)

##
#' @title Alfresco POST method
#' @description Helper to make a POST call to the Alfresco REST API
#' @param endpoint base endpoint URI
#' @param ticket authentication ticket
#' @param params optional list of parameters
#' @param format return format from \code{json} (default), \code{raw}
#' @param body request body
#' @return result based on format provided
#' @export
##
alf_POST <- function (endpoint, ticket, params=list(), format=c("json", "raw"), body=NULL)
  alf_method("POST", endpoint, ticket, params, format, body)

##
#' @title Alfresco PUT method
#' @description Helper to make a PUT call to the Alfresco REST API
#' @param endpoint base endpoint URI
#' @param ticket authentication ticket
#' @param params optional list of parameters
#' @param format return format from \code{json} (default), \code{raw}
#' @param body request body
#' @return result based on format provided
#' @export
##
alf_PUT <- function (endpoint, ticket, params=list(), format=c("json", "raw"), body=NULL)
  alf_method("PUT", endpoint, ticket, params, format, body)

##
#' @title Alfresco HTTP method
#' @description Helper to make a http method call to the Alfresco REST API
#' @param method HTTP method to call
#' @param endpoint base endpoint URI
#' @param ticket authentication ticket
#' @param params optional list of parameters
#' @param format return format from \code{json} (default), \code{raw}
#' @param body request body
#' @return result based on format provided
##
alf_method <- function (method=c("GET", "POST", "PUT"), endpoint, ticket, params=list(), format=c("json", "raw"), body=NULL) {

  # check we have a valid method
  method <- match.arg(method)

  # check we have a valid format
  format <- match.arg(format)

  # construct method call
  method_call <- bquote(.(as.symbol(method))(

      # resolve parameters
      add_params(endpoint, params),

        # add authentication
      add_headers(Authorization = paste("Basic", base64_enc(ticket)))))

  # add body if provided
  if (!is.null(body)) method_call[[length(method_call)+1]] <- quote(content(body))

  # get response
  response <- eval(method_call)

  # process response format
  switch (
    format,
    json = content(response, "text") %>% fromJSON(flatten = TRUE),
    raw =  content(response, "raw"))
}

##
#' @title Add parameters
#' @description Add parameters to endpoint
#' @param endpoint endpoint URI to add parameters to
#' @param params list of name/value parameters
#' @param sep separator used to delimit the next parameter to the endpoint
#' @return endpoint with added parameters
#
add_params <- function (endpoint, params, sep="?") {

  if (length(params) > 0)
    add_params(
      paste(endpoint, sep, names(params[1]), "=", params[[1]], sep=""),
      params[-1], sep="&")
  else
    endpoint
}

