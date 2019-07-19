context("alf-session")

require(magrittr)
require(httptest)

invalid_ticket <- "TICKET_b1433bd56ba762daeec869f6e6d8d2578c463689"

mocked_test_that(

  "Given a valid respository,
   And valid user credentials,
   When I request a session,
   Then I recieve a valid session", {

  validSession <- function (session)
    session %>% is.null() %>% not() &&
    session$ticket %>% startsWith("TICKET_") &&
    session$server == test_server

  alf_session(test_server, admin_username, admin_password) %>% validSession() %>% expect_true()
})

mocked_test_that(

  "Given a valid respository,
   And invalid user credentials,
   When I request a session,
   Then I recieve an error", {

  expect_error(alf_session(test_server, "wrong", "wrong"), "Authentication")
})

mocked_test_that(

  "Given a valid session,
   When I check it is valid,
   Then I am told that it is", {

  session <- alf_session(test_server, admin_username, admin_password)
  expect_true(alf_session.is_valid(session))
})

mocked_test_that(

  "Given an invalid session,
   When I check it is valid,
   Then I am told that it is", {

  session <- alf_session(test_server, admin_username, admin_password)
  session$ticket <- invalid_ticket
  expect_false(alf_session.is_valid(session))
})

mocked_test_that(

  "Given a valid session,
   When I invalidate the session,
   Then it is no longer valid", {

  session <- alf_session(test_server, admin_username, admin_password)
  result <- alf_session.invalidate(session)
  expect_true(result)
})

mocked_test_that(

  "Given an invalid session,
   When I invalidate the session,
   Then nothing happens", {

  session <- alf_session(test_server, admin_username, admin_password)
  session$ticket <- invalid_ticket
  result <- alf_session.invalidate(session)
  expect_false(result)
})
