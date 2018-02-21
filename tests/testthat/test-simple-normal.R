context("Simulations for simple expt w/ normally distributed outcome variable")

test_that("Single mean sim outputs parameter estimates properly", {
  expect_length(one.sim.mean.normal(mean = 3,
                                    sd = 2,
                                    n.trials = 30), 2)
})

test_that("Single diff sim outputs parameter estimates properly", {
  expect_length(one.sim.diff.normal(mean1 = 3,
                                    sd1 = 2,
                                    mean2 = 5,
                                    n.trials1 = 30), 2)
})
