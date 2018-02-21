context("Simulations for SDT-style d' expt")

test_that("Single sim outputs parameter estimates properly", {
  expect_length(one.sim.sdt.dprime(dprime = 3,
                                   n.trials.signal = 50,
                                   n.trials.noise = 30), 2)
})
