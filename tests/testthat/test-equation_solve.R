test_that("equation_solve multiplies a simple LHS by Im", {
  Im <- data.frame(
    key = c("r1", "r2"),
    A = c(1, 2),
    B = c(3, 4)
  )
  LHS <- data.frame(
    key = "lhs",
    A = 0.5,
    B = 1.0
  )
  
  sol <- equation_solve(LHS = LHS, Im = Im)
  # numeric 1x2 matrix: [0.5*1 + 1*3, 0.5*2 + 1*4] = [3.5, 5]
  expect_true(is.matrix(sol))
  expect_equal(dim(sol), c(1, 2))
})

test_that("equation_solve errors when inputs are NULL", {
  expect_error(equation_solve(NULL, NULL), "inputs are not given")
})

test_that("equation_solve drops allowed missing codes from LHS", {
  Im <- data.frame(
    key = c("r1", "r2"),
    A = c(1, 0),
    B = c(0, 1)
  )
  LHS <- data.frame(
    key = "lhs",
    A = 2,
    B = 3,
    TOTAL = 999 # allowed to be dropped per code path
  )
  
  expect_warning(
    sol <- equation_solve(LHS = LHS, Im = Im),
    "from the input vector is removed"
  )
  # With Im = identity on A/B, result should be [2, 3]
  expect_equal(as.numeric(sol), c(2, 3))
})
