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
  expect_false(is.null(node$id))
  expect_equal(node$name, "test-alf-node")
})

mocked_test_that(

  "Given a valid session,
   And a folder path relative to the root
   When I try to create a document,
   Then the document is created", {

  node <-
    alf_session(test_server, admin_username, admin_password) %>%
    alf_node.new(node_id = "-root-", list(
      name = "test-alf-node.txt",
      nodeType = "cm:content",
      relativePath = "test-alf-node"
    ))

  expect_false(is.null(node))
  expect_false(is.null(node$id))
  expect_equal(node$name, "test-alf-node.txt")
})

mocked_test_that(

  "Given a valid sesion,
   And a folder id
   When I try to create a document
   Then the document is created", {

  session <- alf_session(test_server, admin_username, admin_password)

  folder <- alf_node(session, relative_path="test-alf-node")
  expect_false(is.null(folder))

  node <- alf_node.new(session, folder$id, list(
    name = "test-alf-node2.txt",
    nodeType = "cm:content"
  ))

  expect_false(is.null(node))
  expect_false(is.null(node$id))
  expect_equal(node$name, "test-alf-node2.txt")
})

# TODO negative test: parent doesn't exist

mocked_test_that(
  "Given a valid session,
   And a content node,
   When I try to get the node,
   Then I am successful,
   And I can inspect the properties of the content node", {

  node <-
    alf_session(test_server, admin_username, admin_password) %>%
    alf_node(relative_path = "test-alf-node/test-alf-node.txt")

  expect_false(is.null(node))
  expect_false(is.null(node$id))
  expect_equal(node$name, "test-alf-node.txt")
})

## TODO negative test: invalid relative path

## TODO test: specify node id rather than relative path

## TODO negative test: invalid node id

## TODO test upload content

## TODO test read content

## TODO test update content

mocked_test_that(

  "Given a document,
   When I try to delete the document,
   Then I am successful,
   And the document is deleted", {

  session <- alf_session(test_server, admin_username, admin_password)
  node <- alf_node(session, relative_path = "test-alf-node/test-alf-node.txt")

  expect_false(is.null(node))

  alf_node.delete(session, node$id)

  # TODO check it's actually deleted
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




