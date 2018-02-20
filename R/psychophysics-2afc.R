#' Generate fake data for one subject, 2AFC psychophysics-style task
#' 
#' Fits a probit/cumulative normal model to fake single-subject data.
#' 
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom plyr mapvalues
#' @importFrom stats pnorm rbinom glm binomial
#' @importFrom tidyr gather
#'
#' @param trash A throwaway first argument to allow piping from apply functions like replicate.
#' @param xmin A numeric value specifying the minimum stimulus intensity
#' to be presented.
#' @param xmax A numeric value specifying the maximum stimulus intensity
#' to be presented.
#' @param n.xlevels A numeric value specifying the number of intensity levels
#' at which stimuli will be presented. Assumes that \code{xmin} and \code{xmax}
#' will be the first and last intensity levels respectively.
#' @param xstep Optional numeric value specifying the step size from one intensity
#' level to the next. If specifying this, do not specify \code{n.xlevels}. If both
#' are specified, the function defaults to calculating intensity levels from \code{n.xlevels}.
#' @param mean A numeric value specifying the "true mean" of the probit
#' model from which simulated data will be generated.
#' @param sd A numeric value specifying the "true standard deviation"
#' of the probit model from which simulated data will be generated.
#' @param trials.per.bin A numeric value specifying the number of trials
#' presented PER stimulus intensity bin.
#' @return The estimated mean and standard deviation of the probit model
#' fit to one round of simulated data.

one.sim.psyphys2afc <- function (trash = NULL,
                                 xmin = NULL,
                                 xmax = NULL,
                                 n.xlevels = NULL,
                                 xstep = NULL,
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
    dplyr::mutate(fake.subj.num = 1:n()) %>%
    tidyr::gather(key = "xlevel", value = "n.yes.resps", -fake.subj.num) %>%
    dplyr::mutate(xlevel = as.numeric(plyr::mapvalues(xlevel, from = unique(xlevel), to = xlevels)),
                  n.no.resps = trials.per.bin - n.yes.resps,
                  prop.resps = n.yes.resps / trials.per.bin)
  
  sim.model = summary(stats::glm(cbind(n.yes.resps, n.no.resps) ~ xlevel,
                                 family = binomial(link = "probit"),
                                 data = sim.data))
  
  # calculating the mean and standard deviation of cumulative normal from probit params
  sim.model.params = unname(c(-(sim.model$coefficients[1, "Estimate"] / sim.model$coefficients[2, "Estimate"]),
                              1/sim.model$coefficients[2, "Estimate"]))
  
  return (sim.model.params)
}
