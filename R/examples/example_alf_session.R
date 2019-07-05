# try to establish a connection to the alfresco content repository
my_session <- tryCatch(
                alf_session("http://localhost:8080", "admin", "admin"),
                error = function(e) NULL)

if (!is.null(my_session)) {

  # output the server URL
  print(my_session$server)

  # output the connection ticket
  print(my_session$ticket)
}
