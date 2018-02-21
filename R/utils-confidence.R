#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! syms quo
#' @importFrom stats sd qnorm

confidence.by.param <- function(data, params, outcomes, conf.level = 0.95) {

  crit <- stats::qnorm((1 - conf.level) / 2, lower.tail = F)
  outcomes.sym <- rlang::syms(outcomes)

  args.mean <- lapply(outcomes.sym, function(x) rlang::quo(mean(!!x)))
  names(args.mean) <- c(paste0("mean.", outcomes))
  args.sd <- lapply(outcomes.sym, function(x) rlang::quo(stats::sd(!!x)))
  names(args.sd) <- paste0("sd.", outcomes)
  args.ci <- lapply(outcomes.sym, function(x) rlang::quo(mean(!!x) - crit * stats::sd(!!x)))
  args.ci <- c(args.ci, lapply(outcomes.sym, function(x) rlang::quo(mean(!!x) + crit * stats::sd(!!x))))
  names(args.ci) <- c(paste0("ci.lower.", outcomes),
                      paste0("ci.upper.", outcomes))
  args.colorder <- lapply(outcomes, function(x) rlang::quo(dplyr::ends_with(!!x)))

  data2 <- data %>%
    group_by(!!! rlang::syms(params)) %>%
    summarize(!!! args.mean, !!! args.sd, !!! args.ci) %>%
    select(!!!  rlang::syms(params), !!! args.colorder)

  return (data)
}

quote.transform.args <- function (x, ...) {
  fs <- quos(...)
  if (length(fs) > 1) {stop()}

  a <- x
  return (lapply(a, function(x) {
    return (rlang::quo(!!fs))
          }))
}
