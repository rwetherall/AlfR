context("alfticket")

require(magrittr)
require(httptest)

with_mock_api({
  test_that("Given a valid respository, And valid user credentials, When I request a ticket, Then I recieve a valid ticket", {

    validTicket <- function (ticket) ticket %>% is.null() %>% not() && ticket %>% startsWith("TICKET_")
    alfticket("http://localhost:8080", "admin", "admin") %>% validTicket() %>% expect_true()
  })
})

with_mock_api ({
  test_that("Given a valid respository, And invalid user credentials, When I request a ticket, Then I recieve an error", {
    expect_error(alfticket("http://localhost:8080", "wrong", "wrong"), "Authentication")
  })
})
