context("alf-ticket")

require(magrittr)
require(httptest)

with_mock_api({
  test_that("Given a valid respository, And valid user credentials, When I request a ticket, Then I recieve a valid ticket", {

    validTicket <- function (ticket) ticket %>% is.null() %>% not() && ticket %>% startsWith("TICKET_")
    alf_ticket("http://localhost:8080", "admin", "admin") %>% validTicket() %>% expect_true()
  })
})

with_mock_api ({
  test_that("Given a valid respository, And invalid user credentials, When I request a ticket, Then I recieve an error", {
    expect_error(alf_ticket("http://localhost:8080", "wrong", "wrong"), "Authentication")
  })
})
