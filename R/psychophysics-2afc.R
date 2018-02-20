#' Generate fake data for one subject, 2AFC psychophysics-style task
#' 
#' @importFrom magrittr %>%
#'
#' @param trash A throwaway first argument to allow piping from apply functions like replicate.
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

one.sim.psyphys2afc <- function (trash = NULL,
                                  xmin = NULL,
                                  xmax = NULL,
                                  xstep = NULL,
                                  n.xlevels = NULL,
                                  mean,
                                  sd,
                                  trials.per.bin) {
  
  if (!is.null(xstep) & !is.null(n.xlevels)) warning("Both step size and # levels specified! Defaulting to use # levels.")
  if (!is.null(n.xlevels)) {
    xlevels = seq(xmin, xmax, length.out = n.xlevels)
  } else if (!is.null(xstep)) {
    xlevels = seq(xmin, xmax, xstep)
  }
  
  
  bin.probs = pnorm(xlevels, mean = mean, sd = sd)
  sim.data = as.data.frame(t(replicate(1, rbinom(n = length(xlevels),
                                                 size = trials.per.bin,
                                                 prob = bin.probs)))) %>%
    mutate(fake.subj.num = 1:n()) %>%
    gather(key = "xlevel", value = "n.yes.resps", -fake.subj.num) %>%
    mutate(xlevel = as.numeric(plyr::mapvalues(xlevel, from = unique(xlevel), to = xlevels)),
           n.no.resps = trials.per.bin - n.yes.resps,
           prop.resps = n.yes.resps / trials.per.bin)
  
  sim.model = summary(glm(cbind(n.yes.resps, n.no.resps) ~ xlevel,
                          family = binomial(link = "probit"),
                          data = sim.data))
  
  # calculating the mean and standard deviation of cumulative normal from probit params
  sim.model.params = unname(c(-(sim.model$coefficients[1, "Estimate"] / sim.model$coefficients[2, "Estimate"]),
                              1/sim.model$coefficients[2, "Estimate"]))
  
  return (sim.model.params)
}
