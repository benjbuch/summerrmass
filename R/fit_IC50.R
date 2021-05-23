#' Build formula for IC50 fitting
#'
#' Builds a formula using \code{\link[drc:drm]{drc::drm}} for IC50 curve fitting.
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
#' @return
#' A quoted formula to be evaluated with \code{\link[base:eval]{eval}}.
#'
build_IC50 <- function(x, formula,
                       limits_lower = c(0, 5), limits_upper = c(50, 100),
                       limits_hill = c(-Inf, +Inf), limits_IC50 = c(0, 1e3)) {

  ll <- c(limits_hill[[1]], limits_lower[[1]], limits_upper[[1]], limits_IC50[[1]])
  ul <- c(limits_hill[[2]], limits_lower[[2]], limits_upper[[2]], limits_IC50[[2]])

  substitute(drc::drm(data = d, formula = f, lowerl = ll, upperl = ul,
                      fct = drc::LL.4(names = c("hill", "lower", "upper", "IC50"))),
             list(d = x, f = formula, ll = ll, ul = ul))

}

#' Fitting IC50 curves
#'
#' Convenience wrapper calling \code{\link[broom:tidy]{broom::tidy}} and
#' \code{\link[broom:augment]{broom::augment}} upon the model specified in
#' \link{build_IC50}.
#'
#' @inheritParams build_IC50
#' @param newdata Data used for extrapolating of the generated model.
#' @param ... Other arguments passed to \link{build_IC50}.
#'
#' @details
#' The IC50 curves are fitted for each group if \code{x} is a
#' \link[dplyr:grouped_df]{grouped data frame}.
#'
#' Error and warning messages are mostly suppressed.
#'
#' @return
#' A named list containing the tidied and augmented data frames.
#'
#' @importFrom magrittr %>%
#'
#' @export
fit_IC50 <- function(x, formula, newdata = NULL, ...) {

  # handle error messages from drc::drm
  old_ep <- getOption("show.error.messages")
  on.exit(options(show.error.messages = old_ep))

  options(show.error.messages = FALSE)

  log_process("group variables", paste(sQuote(dplyr::group_vars(x)), collapse = ", "))

  suppressWarnings({

    log_process("fitting")

    FUN <- build_IC50(x = x, formula = formula, ...)

    x.tidy <- dplyr::group_modify(x, ~ wrap_tidy(FUN))

    log_done()

    log_process("intra- and extrapolating")

    # for augment.drc, both data and newdata must be given; further, the returned
    # object may not contain the original group variables

    olddata <- x %>%
      dplyr::ungroup() %>%
      dplyr::select(!dplyr::group_vars(x))

    if (!is.null(newdata)) {

      newdata <- newdata %>%
        dplyr::ungroup() %>%
        dplyr::select(!dplyr::group_vars(x))

    }

    x.fito <- dplyr::group_modify(x, ~ wrap_augment(FUN, data = olddata))

    x.fitn <- dplyr::group_modify(x, ~ wrap_augment(FUN, data = olddata,
                                                    newdata = newdata))

    log_done()

  })

  list(tidy = x.tidy, fit_old = x.fito, fit_new = x.fitn)

}

