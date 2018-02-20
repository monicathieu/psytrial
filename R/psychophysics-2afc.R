#' Generate fake data for one subject, 2AFC psychophysics-style task
#'
#' @param bins A vector of numeric values, indicating the levels
#' of intensity at which stimuli will be presented.
#' @param mean A numeric value specifying the "true mean" from which
#' simulated data will be generated.
#' @param sd A numeric value specifying the "true standard deviation"
#' from which simulated data will be generated.
#' @param trials.per.bin A numeric value specifying the number of trials
#' presented PER stimulus intensity bin.
#' @return The estimated mean and standard deviation of the probit model
#' fit to one round of simulated data.

one.psychometric.sim <- function (bins,
                                  mean,
                                  sd,
                                  trials.per.bin) {
  bin.probs = pnorm(bins, mean = mean, sd = sd)
  sim.data = as.data.frame(t(replicate(1, rbinom(n = length(bins),
                                                 size = trials.per.bin,
                                                 prob = bin.probs)))) %>%
    mutate(fake.subj.num = 1:n()) %>%
    gather(key = "bin", value = "n.yes.resps", -fake.subj.num) %>%
    mutate(bin = as.numeric(plyr::mapvalues(bin, from = unique(bin), to = bins)),
           n.no.resps = trials.per.bin - n.yes.resps,
           prop.resps = n.yes.resps / trials.per.bin)

  sim.model = summary(glm(cbind(n.yes.resps, n.no.resps) ~ bin,
                          family = binomial(link = "probit"),
                          data = sim.data))

  # calculating the mean and standard deviation of cumulative normal from probit params
  sim.model.params = unname(c(-(sim.model$coefficients[1, "Estimate"] / sim.model$coefficients[2, "Estimate"]),
                              1/sim.model$coefficients[2, "Estimate"]))

return (sim.model.params)
}

multi.binsize.params <- function (binsizes = c(4, 5, 10, 20, 40),
                                  n.bins = 5,
                                  n.reps = 100,
                                  mean = .561,
                                  sd = .212) {
  params = lapply(as.list(binsizes),
                  function(x) get.rep.params(n.reps = n.reps,
                                             bins = seq(0, 0.8, 0.8 / (n.bins - 1)),
                                             mean = mean,
                                             sd = sd,
                                             trials.per.bin = x))
  names(params) = binsizes
  return (params)
}
