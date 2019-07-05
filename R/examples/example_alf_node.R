\donttest{
# establish a connection to the alfresco content repository
my_session <- alf_session("http://localhost:8080", "admin", "admin")

# document name
my_document_name <- paste(round(proc.time()[[3]]), ".txt", sep="")
relative_path <- paste("testFolder/", my_document_name, sep="")

# create document
my_new_document <- alf_node.new(my_session, node_id="-root-",
  list(
    name = my_document_name,
    nodeType = "cm:content",
    relativePath = "testFolder"
  ))

# upload content
my_new_document$content$update("R/examples/sample_content/sample.txt")

# get details of document node
my_document <- alf_node(my_session, relative_path = relative_path)

# output the name of the document
print(my_document$name)

# output the details of documents content
print(my_document$content$mime_type)
print(my_document$content$mime_type_name)
print(my_document$content$size)
print(my_document$content$encoding)

# read document content
my_content_file <- file(my_document$content$as.file(), "r")
my_content <- readLines(my_content_file)
close(my_content_file)
print(my_content)

# upload new content
my_updated_document <- my_document$content$update("R/examples/sample_content/modified_sample.txt")

# print updated content size
print(my_updated_document$content$size)

}
