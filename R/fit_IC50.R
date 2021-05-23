#' Fitting IC50 curves
#'
#' Fits an IC50 curve using \code{\link[drc:drm]{drc::drm}}.
#'
#' @param x A data frame to be evaluated.
#' @param formula A formula using variables of \code{x}.
#' @param limits_lower,limits_upper,limits_hill,limits_IC50 Upper and lower bounds
#' for the paramters to be fitted.
#'
#' @details
#' Fitted parameters are \code{"hill"}, \code{"lower"}, \code{"upper"}, and \code{"IC50"}.
#' The defaults for upper and lower limits assume percentage
#'
#' @export
fit_IC50 <- function(x, formula,
                       limits_lower = c(0, 5), limits_upper = c(50, 100),
                       limits_hill = c(-Inf, +Inf), limits_IC50 = c(0, 1e3)) {

  ll <- c(limits_hill[[1]], limits_lower[[1]], limits_upper[[1]], limits_IC50[[1]])
  ul <- c(limits_hill[[2]], limits_lower[[2]], limits_upper[[2]], limits_IC50[[2]])

  drc::drm(data = x, formula = formula, lowerl = ll, upperl = ul,
           fct = drc::LL.4(names = c("hill", "lower", "upper", "IC50")))

}
