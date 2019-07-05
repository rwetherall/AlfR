require(jsonlite)
require(magrittr)

##
# Helper to make a GET call to the Alfresco REST API
##
alf_GET <- function (endpoint, ticket, params=list(), as=c("json", "raw", "none"), body=NULL)
  alf_method("GET", endpoint, ticket, params, as, body)

##
# Helper to make a POST call to the Alfresco REST API
##
alf_POST <- function (endpoint, ticket, params=list(), as=c("json", "raw", "none"), body=NULL)
  alf_method("POST", endpoint, ticket, params, as, body)

##
# Helper to make a PUT call to the Alfresco REST API
##
alf_PUT <- function (endpoint, ticket, params=list(), as=c("json", "raw", "none"), body=NULL)
  alf_method("PUT", endpoint, ticket, params, as, body)

##
# Helper to make a DELETE call to the Alfresco REST API
##
alf_DELETE <- function (endpoint, ticket, params=list())
  alf_method("DELETE", endpoint, ticket, params, "none")

##
# Helper to make a http method call to the Alfresco REST API
##
alf_method <- function (method=c("GET", "POST", "PUT", "DELETE"), endpoint, ticket, params=list(), as=c("json", "raw", "none"), body=NULL) {

  # check we have a valid method
  method <- match.arg(method)

  # check we have a valid as
  as <- match.arg(as)

  # construct method call
  method_call <- bquote(.(as.symbol(method))(

      # resolve parameters
      add_params(endpoint, params),

      # add authentication
      add_headers(Authorization = paste("Basic", base64_enc(ticket))),

      # add body
      body=body))

  # get response
  response <- eval(method_call)

  # check for error
  if (http_error(response))
    if(response$status_code == 401)
      stop("Authentication details are not longer valid or have timed out.  Please renew your session. (401)")
    else if (response$status_code == 409)
      stop("A duplicate node was found.  Please ensure name is unique. (409)")
    else
      stop(http_status(response)$message)

  # process response as
  switch (
    as,
    json = content(response, "text") %>% fromJSON(flatten = TRUE),
    raw =  content(response, "raw"))
}

##
# Helper method to add parameters onto a URL endpoint
##
add_params <- function (endpoint, params, sep="?") {

  if (length(params) > 0)
    add_params(
      paste(endpoint, sep, names(params[1]), "=", params[[1]], sep=""),
      params[-1], sep="&")
  else
    endpoint
}

