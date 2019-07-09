#' @title alfr: A package for connecting with Alfresco
#' @author Roy Wetherall \email{rwetherall@gmail.com}
#' @description
#' The \code{alfr} package provides a way to connect to Alfresco and interact with the contents of the repository.
#'
#' Session
#' \itemize{
#'   \item \code{\link{alf_session}} - connection session to an Alfresco repository
#'   \item \code{\link{alf_session.is_valid}} - determine whether the session connection to an Alfresco repository is still valid
#'   \item \code{\link{alf_session.invalidate}} - invalidates a session so it can no longer use used to connect to an Alfresco repository
#' }
#'
#' Nodes
#' \itemize{
#'   \item \code{\link{alf_node}} - get the details of a folder or content node
#'   \item \code{\link{alf_node.new}} - creates a new folder or content node
#'   \item \code{\link{alf_node.delete}} - deletes a folder or content node
#' }
#' @docType package
#' @name alfr
#'
#' @import magrittr
#' @import httr
#' @import jsonlite
#' @import stringr
NULL
