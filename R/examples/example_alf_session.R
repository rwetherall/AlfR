\donttest{

# establish a connection to the alfresco content repository
my_session <- alf_session("http://localhost:8080", "admin", "admin")

# output the server URL
print(my_session$server)

# output the connection ticket
print(my_session$ticket)

}
