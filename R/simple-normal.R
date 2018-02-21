#' Generate fake data for one subject, one normally distributed outcome variable
#'
#' Calculates the mean of one normally distributed outcome variable for a single subject.
#' @importFrom stats rnorm sd
#'
#' @param trash A throwaway first argument to allow piping from apply functions like replicate.
#' @param mean A numeric value specifying the "true mean" from which
#' simulated data will be generated.
#' @param sd A numeric value specifying the "true standard deviation"
#' from which simulated data will be generated.
#' @param n.trials A numeric value specifying the number of trials to
#' be presented.
#' @return The estimated mean and standard error of the mean estimate
#' from one round of simulated data.

one.sim.mean.normal <- function(trash = NULL,
                            mean = 0,
                            sd = 1,
                            n.trials) {
  sim <- stats::rnorm(n.trials, mean = mean, sd = sd)
  return(c(mean(sim), stats::sd(sim) / sqrt(n.trials)))
}

#' Generate fake data for one subject, difference by condition of normally distributed outcome variable
#'
#' Calculates the difference of a normally distributed outcome variable by condition for a single subject.
#' @importFrom stats rnorm var
#'
#' @param trash A throwaway first argument to allow piping from apply functions like replicate.
#' @param mean1 A numeric value specifying the "true mean" of the first
#' task condition from which simulated data will be generated.
#' @param mean2 A numeric value specifying the "true mean" of the second
#' task condition from which simulated data will be generated.
#' @param sd1 A numeric value specifying the "true standard deviation"
#' of the first task condition from which simulated data will be generated.
#' @param sd2 A numeric value specifying the "true standard deviation"
#' of the second task condition from which simulated data will be generated.
#' Defaults to \code{sd1}, aka assumes equal variance between conditions.
#' @param n.trials1 A numeric value specifying the number of trials to
#' be presented in the first task condition.
#' @param n.trials2 A numeric value specifying the number of trials to
#' be presented in the second task condition. Defaults to \code{n.trials1},
#' aka assumes equal number of trials in both conditions.
#' @return The estimated difference and standard error of the difference
#' estimate from one round of simulated data.

one.sim.diff.normal <- function(trash = NULL,
                           mean1,
                           mean2,
                           sd1,
                           sd2 = sd1,
                           n.trials1,
                           n.trials2 = n.trials1) {
  sim1 <- stats::rnorm(n.trials1, mean = mean1, sd = sd1)
  sim2 <- stats::rnorm(n.trials2, mean = mean2, sd = sd2)

  return(c(mean(sim1) - mean(sim2),
           sqrt(stats::var(sim1) / n.trials1 + stats::var(sim2) / n.trials2)))
}
