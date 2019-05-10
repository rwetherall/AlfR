require(magrittr)

# endpoint prefix
endpoint_prefix   <- "/alfresco/api/-default-/public/"

# generates generic enpoint function
endpoint_fn <- function(endpoint) fn <- function(base_uri) paste(base_uri, endpoint_prefix, endpoint, sep="")

# tickets endpoint function
tickets_endpoint_fn <- endpoint_fn("authentication/versions/1/tickets")

# node endpoint function
node_endpoint_fn <- function(base_uri, node_id = "-root-") endpoint_fn("alfresco/versions/1/nodes/")(base_uri) %>% paste(node_id, sep="")

# content endpoint function
node_content_endpoint_fn <- function(base_uri, node_id) node_endpoint_fn(base_uri, node_id) %>% paste("/content", sep="")

##
#' @title
#' TODO
#' @description
#' TODO
#' @export
##
alf_endpoints <- list (
  tickets = tickets_endpoint_fn,
  node = node_endpoint_fn,
  node_content = node_content_endpoint_fn
)
