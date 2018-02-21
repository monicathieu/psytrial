#' Repeat data simulation for multiple levels of a parameter
#'
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tidyr separate
#'
#' @param type A string indicating the label for the type of
#' task to be replicated. Currently supports \code{"psychophysics.2afc"}.
#' @param ... arguments ultimately passed to the workhorse single simulation
#' function specified by \code{type}.
#' For \code{"psychophysics.2afc"}, please see the help for \code{one.sim.psyphys2afc}.
#' @return a long dataframe containing the performance parameter estimates
#' from each simulated task run, labeled by the varying parameter of interest.

replicate.sim.by.param <- function(type = "psychophysics.2afc",
                                   ...) {
  these.dots = list(...)
  param.names.to.rep = names(these.dots)[lapply(these.dots, length) > 1]
  # nb that expand.grid() outputs a dataframe
  these.dots.expanded = as.list(do.call("expand.grid", these.dots))
  # cleaning, idk if this affects anything but why not
  attr(these.dots.expanded, "out.attrs") <- NULL

  if (type == "psychophysics.2afc") {
    these.dots.expanded$FUN = quote(replicate.sim)
    out.array = do.call("mapply", these.dots.expanded)

    out = lapply(1:dim(out.array)[2],
                 flatten.array.byrow, a = out.array, colnames = c("mean", "sd"))
    names(out) = do.call("paste", c(these.dots.expanded[lapply(these.dots, length) > 1], sep = "_"))
  }

  out = dplyr::bind_rows(out, .id = "param")
  out = tidyr::separate(out, param, into = param.names.to.rep)
  return (out)
}

#' internal
#' workhorse function to repeatedly call a \code{one.sim} function
#' and store the results of every call
#' @param n.reps number of simulations to run
#' @param type A string indicating the label for the type of
#' task to be replicated. Currently supports \code{"psychophysics.2afc"}.
#' @param ... arguments ultimately passed to the workhorse single simulation
#' function specified by \code{type}.
#' For \code{"psychophysics.2afc"}, please see the help for \code{one.sim.psyphys2afc}.
#' @return a dataframe where each row is a simulation and each column
#' is an estimated parameter.

replicate.sim <- function (n.reps, type = "psychophysics.2afc", ...) {

  if (type == "psychophysics.2afc") {
    params <- as.data.frame(t(sapply(1:n.reps,
                                     one.sim.psyphys2afc, ...)))
    names(params) <- c("mean", "sd")
  }
  return (params)
}

#' internal
#' flattens an array of vectors row-wise
#' designed for use inside an \code{lapply} call vectorized
#' along columns of an array
#' @param x index of column to be flattened
#' @param a array being flattened
#' @param colnames names of columns of each output dataframe
#' @return a list of dataframes, where each list element was a
#' column of the original array, and each column in each dataframe
#' was a vector in that column

flatten.array.byrow <- function (x, a, colnames) {
  df <- as.data.frame(matrix(unlist(a[, x]), ncol = dim(a)[1], byrow = F))
  names(df) <- colnames
  return (df)
}
