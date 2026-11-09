test_that(
  "Ready4 can be instantiated",
  {

    obj <- new("Ready4")

    expect_true(
      is(obj, "Ready4")
    )

  }
)
