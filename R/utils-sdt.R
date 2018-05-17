
#' internal
#' 
#' Calculates rate correction against ceiling/floor rates
#' per Snodgrass & Corwin, 1988.
#' 
#' @param num numerator
#' @param denom denominator

snodgrass <- function (num, denom) {result = (num+.5)/(denom+1); return(result)}

#' internal
#' 
#' @importFrom stats qnorm
#' @param hit hit rate
#' @param fa false alarm rate

sdt_dprime <- function (hit, fa) {result = (stats::qnorm(hit) - stats::qnorm(fa)); return(result)}

#' internal
#' 
#' @importFrom stats qnorm
#' @param hit hit rate
#' @param fa false alarm rate

sdt_c <- function (hit,fa) {result = (-.5 * (stats::qnorm(hit) + stats::qnorm(fa))); return(result)}

#' internal
#' 
#' @param hit hit rate
#' @param fa false alarm rate
sdt_pr <- function (hit, fa) {result = hit - fa; return(result)}

#' internal
#' 
#' @param hit hit rate
#' @param fa false alarm rate
sdt_br <- function(hit, fa) {result = fa / (1 - (hit - fa)); return(result)}

#' internal
#' 
#' @param hit hit rate
#' @param fa false alarm rate
sdt_aprime <- function (hit, fa) {result = (.5 + (sign(hit-fa)*((hit-fa)^2 + abs(hit-fa))/(4*pmax(hit,fa)-(4*hit*fa)))); return(result)}

#' internal
#' 
#' @param hit hit rate
#' @param fa false alarm rate
sdt_b2prime <- function (hit, fa) {result = (sign(hit-fa)*(hit*(1-hit) - fa*(1-fa))/(hit*(1-hit) + fa*(1-fa))); return(result)}

#' internal
#' 
#' @param vec vector from which to calculate probability
#' @param value1 first value to count in vector
#' @param value2 optional second value to count in vector
#' @param value3 optional third value to count in vector
#' @param snodgrass calculate Snodgrass-corrected fraction? defaults to \code{FALSE}.
prob <- function (vec, value1, value2 = NULL, value3 = NULL, snodgrass = FALSE) {
  # assumes union of conditions
  if(snodgrass) {result = (length(vec[vec %in% c(value1, value2, value3)]) + .5) / (length(vec) + 1)
  } else {
    result = length(vec[vec %in% c(value1, value2, value3)])/length(vec)
  }
  return(result)}

#' internal
#' 
#' take the median of the values in a column if the paired identifiers
#' in another column are members of condition of interest
#' @param vec vector to median
#' @param vec2 vector for logical indexing
#' @param if1 first value to check equality
#' @param if2 optional second value to check equality
#' @param if3 optional second value to check equality
median.if <- function (vec, vec2, if1, if2 = NULL, if3 = NULL) {
  result = median(vec[vec2 %in% c(if1,if2,if3)]) # assumes union of conditions
  return(result)}

#' internal
#' 
#' take the count of the # of values in a column that meet a certain criterion
#' @param vec vector to median
#' @param vec2 vector for logical indexing
#' @param if1 first value to check equality
#' @param if2 optional second value to check equality
#' @param if3 optional second value to check equality
length.if <- function (vec, if1, if2 = NULL, if3 = NULL) {
  result = length(vec[vec %in% c(if1,if2,if3)]) # assumes union of conditions
  return(result)}