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
   When I try to get the node using a relative path,
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

mocked_test_that(

  "Given a valid session,
   And a content node,
   When I try to get the node using a node id,
   Then I am successful,
   And I can inspect the properties of the content node", {

    session <- alf_session(test_server, admin_username, admin_password)
    node <- alf_node(session, relative_path = "test-alf-node/test-alf-node.txt")
    expect_false(is.null(node))

    node_by_id <- alf_node(session, node$id)
    expect_false(is.null(node_by_id))
    expect_equal(node_by_id$id, node$id)
    expect_equal(node_by_id$name, "test-alf-node.txt")
})

## TODO negative test: invalid node id

mocked_test_that(

  "Given a vaild content node with no content,
   And a text file,
   When I try to upload the text file,
   Then I am successful,
   And I can access information about the file uploaded to the content node", {

     node <-
       alf_session(test_server, admin_username, admin_password) %>%
       alf_node.new(node_id = "-root-", list(
         name = "test-alf-node-content-upload.txt",
         nodeType = "cm:content",
         relativePath = "test-alf-node"
       ))

     expect_equal(node$content$mime_type, "text/plain")
     expect_equal(node$content$mime_type_name, "Plain Text")
     expect_equal(node$content$size, 0)
     expect_equal(node$content$encoding, "UTF-8")

     node <- system.file("extdata", "sample.txt", package="alfr") %>%
             node$content$update()

     expect_equal(node$content$size, 45)
     expect_equal(node$content$encoding, "ISO-8859-1")
   }
)

## TODO test read content
mocked_test_that(

  "Given a valid content node with uploaded content,
   When I try to read the content,
   Then I am successful", {

     node <- alf_session(test_server, admin_username, admin_password) %>%
       alf_node(relative_path = "test-alf-node/test-alf-node-content-upload.txt")

     file <- file(node$content$as.file(), "r")
     content <- readLines(file)
     close(file)

     expect_false(is.null(content))

     # TODO this fails on Travis so only run in live mode
     if (test_execution_mode == "live") {
       expect_equal(length(content), 2)
       expect_equal(nchar(content[[1]]) + nchar(content[[2]]), 45-4)
     }
   }
)

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




