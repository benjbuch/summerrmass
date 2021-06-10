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
                       limits_lower = c(-10, 10), limits_upper = c(50, 150),
                       limits_hill = c(-Inf, +Inf), limits_IC50 = c(0, 1e3)) {

  params <- list(hill = limits_hill, lower = limits_lower, upper = limits_upper,
                 IC50 = limits_IC50)

  ll <- sapply(params, min, na.rm = TRUE, USE.NAMES = TRUE)
  ul <- sapply(params, max, na.rm = TRUE, USE.NAMES = TRUE)

  drc::drm(data = x, formula = formula, lowerl = ll, upperl = ul,
           fct = drc::LL.4(names = c("hill", "lower", "upper", "IC50")))

}
