## Auth guard paths (no network / live IdP required).

test_that("lynker_spatial_auth rejects a malformed token", {
  expect_error(lynker_spatial_auth(token = "not-a-token"), "malformed")
})

test_that("lynker_spatial_refresh rejects a malformed token", {
  expect_error(lynker_spatial_refresh("not-a-token"), "malformed")
})

test_that("lynker_spatial_auth validates the libs argument", {
  expect_error(
    lynker_spatial_auth(token = structure(list(), class = "httr2_token"),
      libs = "nope"),
    "should be one of|arg"
  )
})
