context("alfticket")

require(magrittr)

test_that("Given a valid respository, And valid user credentials, When I request a ticket, Then I recieve a valid ticket", {

  validTicket <- function (ticket) ticket %>% is.null() %>% not() && ticket %>% startsWith("TICKET_")
  alfticket("https://api-explorer.alfresco.com", "admin", "admin") %>% validTicket() %>% expect_true()
})
