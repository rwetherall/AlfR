context("alf-node")

require(magrittr)
require(httptest)

test_doc_path <- "Sites/test/documentLibrary/testFolder/testdoc.txt"
test_doc_id <- "75ac9b37-853f-4b8c-b52f-3cf7ead28314"

mocked_test_that(

  "Given a document, When I get the document, Then I successfully retrieve it's information", {

  node <- alf_session(test_server, admin_username, admin_password) %>% alf_node(test_doc_path)

  expect_equal(node$id, test_doc_id)
  expect_equal(node$name, "testdoc.txt")

})

mocked_test_that(

  "Given a document, When I get plain content, Then it is downloaded into a local file ", {

  node <- alf_session(test_server, admin_username, admin_password) %>% alf_node(test_doc_path)
  content <- node$content

  expect_false(is.null(content))

  expect_equal(content$mime_type, "text/plain")
  expect_equal(content$mime_type_name, "Plain Text")
  expect_equal(content$size, 20)
  expect_equal(content$encoding, "ISO-8859-1")

  content_file <- content$as.file()
  expect_false(is.null(content_file))
  expect_equal(readLines(content_file), "Test txt document.")
})
