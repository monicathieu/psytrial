context("Simulations for 2AFC psychophysics-style expt")

test_that("Single sim outputs parameter estimates properly", {
  expect_length(one.sim.psyphys2afc(xmin = 0, xmax = 0.8,
                                    n.xlevels = 5, mean = 0.5, sd = 0.2,
                                    trials.per.bin = 20), 2)
})

test_that("Repeated sim outputs parameter estimates properly", {
  expect_equal(nrow(replicate.sim(type = "psychophysics.2afc",
                                  n.reps = 5,
                                  xmin = 0, xmax = 0.8,
                                  n.xlevels = 5, mean = 0.5, sd = 0.2,
                                  trials.per.bin = 20)), 5)
  expect_equal(ncol(replicate.sim(type = "psychophysics.2afc",
                                  n.reps = 5,
                                  xmin = 0, xmax = 0.8,
                                  n.xlevels = 5, mean = 0.5, sd = 0.2,
                                  trials.per.bin = 20)), 2)
})

test_that("Repeated sim by different expt parameter levels outputs parameter estimates properly", {
  expect_equal(nrow(replicate.sim.by.param(type = "psychophysics.2afc",
                                           n.reps = 5,
                                           xmin = 0, xmax = 0.8,
                                           n.xlevels = c(3,5), mean = 0.5, sd = 0.2,
                                           trials.per.bin = 20)), 10)
  expect_equal(ncol(replicate.sim.by.param(type = "psychophysics.2afc",
                                           n.reps = 5,
                                           xmin = 0, xmax = 0.8,
                                           n.xlevels = c(3,5), mean = 0.5, sd = 0.2,
                                           trials.per.bin = 20)), 3)
  expect_equal(nrow(replicate.sim.by.param(type = "psychophysics.2afc",
                                           n.reps = 5,
                                           xmin = 0, xmax = 0.8,
                                           n.xlevels = c(3,5), mean = 0.5, sd = 0.2,
                                           trials.per.bin = c(10, 20))), 20)
  expect_equal(ncol(replicate.sim.by.param(type = "psychophysics.2afc",
                                           n.reps = 5,
                                           xmin = 0, xmax = 0.8,
                                           n.xlevels = c(3,5), mean = 0.5, sd = 0.2,
                                           trials.per.bin = c(10, 20))), 4)
})
