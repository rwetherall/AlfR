library(httptest)

.mockPaths("../")

set_requester(function (request) {
  gsub_request(request, "http://localhost:8080/alfresco/api/-default-/public/", "api/")
})

test_execution_mode <- "mock"

test_server <- "http://localhost:8080"

admin_username <- "admin"
admin_password <- "admin"

mocked_test_that <- function (desc, code) do_mocking(test_that(desc, code), test_execution_mode)

do_mocking <- function (expr, mode=c("mock", "live", "capture"))
  match.arg(mode) %>%
  switch(
    mock = with_mock_api(expr),
    live = eval(expr),
    capture = capture_requests(expr))
