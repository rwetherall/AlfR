context("alf-node")

require(magrittr)
require(httptest)

mocked_test_that(

  "Given a valid session,
   When I try to create a folder,
   Then I am successful,
   And the folder is created", {

 node <-
  alf_session(test_server, admin_username, admin_password) %>%
  alf_node.new(node_id = "-root-", list(
    name = "test-alf-node",
    nodeType = "cm:folder"
  ))

 expect_false(is.null(node))

 # TODO check properties

})

mocked_test_that(

  "Given a valid session,
   When I try to create a document,
   Then I am successful,
   And the document is created", {

  node <-
    alf_session(test_server, admin_username, admin_password) %>%
    alf_node.new(node_id = "-root-", list(
      name = "test-alf-node.txt",
      nodeType = "cm:content",
      relativePath = "test-alf-node"
    ))

  expect_false(is.null(node))

  # TODO check properties
})

mocked_test_that(

  "Given a folder,
   When I try to delete the folder,
   Then I am successful,
   And the folder is deleted", {

  session <- alf_session(test_server, admin_username, admin_password)
  node <- alf_node(session, relative_path = "test-alf-node")

  expect_false(is.null(node))

  alf_node.delete(session, node$id)

  # TODO check it's actually deleted
})




