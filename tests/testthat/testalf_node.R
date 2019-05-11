context("alf-node")

require(magrittr)
require(httptest)

test_doc_path <- "Sites/test/documentLibrary/testFolder/testdoc.txt"
test_doc_id <- "75ac9b37-853f-4b8c-b52f-3cf7ead28314"

with_mock_api({
  test_that("Given a document, When I get the document, Then I successfully retrieve it's information", {

    test_doc <- alf_session(test_server, "admin", "admin") %>% alf_node(test_doc_path)

    expect_equal(test_doc$id, test_doc_id)

  })
})
