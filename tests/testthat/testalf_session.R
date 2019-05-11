context("alf-session")

require(magrittr)
require(httptest)

with_mock_api({
  test_that("Given a valid respository, And valid user credentials, When I request a session, Then I recieve a valid session", {

    validSession <- function (session)
      session %>% is.null() %>% not() &&
      session$ticket %>% startsWith("TICKET_") &&
      session$server == test_server

    alf_session(test_server, "admin", "admin") %>% validSession() %>% expect_true()
  })
})

with_mock_api ({
  test_that("Given a valid respository, And invalid user credentials, When I request a session, Then I recieve an error", {
    expect_error(alf_session(test_server, "wrong", "wrong"), "Authentication")
  })
})