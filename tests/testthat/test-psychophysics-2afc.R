context("Single subject simulation for 2AFC psychophysics-style expt")

test_that("Outputs parameter estimates properly", {
  expect_length(one.sim.psyphys2afc(xmin = 0, xmax = 0.8,
                                    n.xlevels = 5, mean = 0.5, sd = 0.2,
                                    trials.per.bin = 20), 2)
})
