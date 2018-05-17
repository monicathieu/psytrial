#' Generate fake data for one subject, SDT task, calculating d'
#'
#' Calculates d' from a fake SDT task for a single subject.
#' @importFrom stats pnorm rbinom
#'
#' @param trash A throwaway first argument to allow piping from apply functions like replicate.
#' @param dprime A numeric value specifying the "true d'" from which
#' simulated data will be generated.
#' @param sdt.c A numeric value specifying the "true c" (criterion parameter)
#' from which simulated data will be generated. Defaults to 0 (no bias).
#' @param n.trials.signal A numeric value specifying the number of trials
#' to be presented that contain signal (correct response is "yes").
#' @param n.trials.noise A numeric value specifying the number of trials
#' to be presented that contain noise (correct response is "no").
#' @return The estimated d' and c from one round of simulated data. Calculated
#' on Snodgrass-corrected simulated hit and FA rates (Snodgrass & Corwin, 1988).

one.sim.sdt.dprime <- function(trash = NULL,
                               dprime,
                               sdt.c = 0,
                               n.trials.signal,
                               n.trials.noise) {

  hit.rate <- stats::pnorm(.5 * dprime - sdt.c)
  fa.rate <- stats::pnorm(-.5 * dprime - sdt.c)

  sim.hits <- stats::rbinom(1, n.trials.signal, prob = hit.rate)
  sim.fas <- stats::rbinom(1, n.trials.noise, prob = fa.rate)

  sim.hit.rate <- snodgrass(sim.hits, n.trials.signal)
  sim.fa.rate <- snodgrass(sim.fas, n.trials.noise)

  result <- c(sdt_dprime(sim.hit.rate, sim.fa.rate),
             sdt_c(sim.hit.rate, sim.fa.rate))

  return (result)
}
