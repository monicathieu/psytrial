replicate.sim <- function (n.reps = 100, type = "psychophysics.2afc", ...) {
  if (type == "psychophysics.2afc") {
    params <- as.data.frame(t(replicate(n.reps,
                                        one.psychometric.sim(...))))
    names(params) <- c("mean", "sd")
  }
  return (params)
}

replicate.sim.by.param <- function(type = "psychophysics.2afc",
                                   param = "trials.per.bin",
                                   values,
                                   ...) {

  params = lapply(as.list(values),
                  function(x) replicate.sim(n.reps = n.reps,
                                             bins = seq(0, 0.8, 0.8 / (n.bins - 1)),
                                             mean = mean,
                                             sd = sd,
                                             trials.per.bin = x,
                                            ...))
  names(params) = binsizes
  return (params)
}
