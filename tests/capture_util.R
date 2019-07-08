require(magrittr)
require(fs)

update_api_capture <- function () {

  # folder locations
  destination <- "tests/api"
  source_root <- "tests/localhost-8080"
  source <- paste(source_root, "alfresco/api/-default-/public", sep="/")

  # store origional execution mode
  og_mode <- Sys.getenv("TEST_EXECUTION_MODE", unset="mock")

  # set execution mode to capture
  Sys.setenv(TEST_EXECUTION_MODE="capture")

  # run tests and restore origional execution mode
  tryCatch(devtools::test(), finally = Sys.setenv(TEST_EXECUTION_MODE=og_mode))

  # delete current api capture
  list.files(destination) %>%
    sapply(function(folder) { paste(destination, folder, sep="/")}, simplify=TRUE, USE.NAMES = FALSE) %>%
    unlink(recursive = TRUE)

  # move new api capture
  list.files(source) %>%
    sapply(function(folder) {
      dir_copy(paste(source, folder, sep="/"), paste(destination, folder, sep="/"), overwrite = TRUE)
    })

  # clean up
  unlink(source_root, recursive = TRUE)
}
