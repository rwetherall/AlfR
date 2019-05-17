context("alf-node")

require(magrittr)
require(httptest)

test_doc_path <- "Sites/test/documentLibrary/testFolder/testdoc.txt"
test_doc_id <- "b569f89f-e5a0-4ef4-8c4e-d1314bac67a4"

# TODO Given an invalid path to a node, When I try to retieve the document by path, Then I am unsucessful

# TODO get folder

mocked_test_that(

  "Given a valid path to a document,
   When I try to retieve the documeny by path,
   Then I am successful and can access information about that node", {

  node <- alf_session(test_server, admin_username, admin_password) %>% alf_node(test_doc_path)

  expect_false(is.null(node$id))
  expect_equal(node$name, "testdoc.txt")

})

mocked_test_that(

  "Given a document with plain text content set,
   When ask for the content as a file,
   Then the file is created and I can read its content", {

  node <- alf_session(test_server, admin_username, admin_password) %>% alf_node(test_doc_path)
  content <- node$content

  expect_false(is.null(content))

  expect_equal(content$mime_type, "text/plain")
  expect_equal(content$mime_type_name, "Plain Text")
  expect_equal(content$size, 19)
  expect_equal(content$encoding, "ISO-8859-1")

  if (test_execution_mode == "live") {
    content_file <- content$as.file() %>% file("r")
    expect_false(is.null(content_file))
    readLines(content_file) %>% startsWith("Test txt document.") %>% expect_true()
    close(content_file)
  }
})

# TODO get cbinary content (eg word, pdf)
# TODO get image
# TODO get binary content that can be converted to data (eg CSV, excel)

mocked_test_that(

  "Given a document with plain text content set,
   When I try to upload content from a local file,
   Then the content is updated and the node node details are available", {

  node <- alf_session(test_server, admin_username, admin_password) %>%
          alf_node(test_doc_path)
  updated_node <- node$content$update("resources/testuploaddoc.txt")

  expect_false(is.null(updated_node))

  expect_equal(updated_node$content$mime_type, "text/plain")
  expect_equal(updated_node$content$mime_type_name, "Plain Text")
  expect_equal(updated_node$content$size, 41)
  expect_equal(updated_node$content$encoding, "ISO-8859-1")

  if (test_execution_mode == "live") {
    content_file <- updated_node$content$as.file() %>% file("r")
    expect_false(is.null(content_file))
    readLines(content_file) %>% startsWith("Somthing") %>% expect_true()
    close(content_file)
  }
})

