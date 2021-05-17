#' Startup parameters
#'
#' @noRd
.onAttach <- function(...) {

  # initial setup for formatting output

  options(useFancyQuotes = .Platform$OS.type == "unix")
  options(tibble.width = Inf)

  # startup messages

  if (is.null(getOption("summerr.log"))) options("summerr.log" = TRUE)

  if (getOption("summerr.log", default = FALSE)) {

    packageStartupMessage("... Logging to the console for 'summerr' is enabled. ",
                          "You can disable it with `options(summerr.log = FALSE)`.")

  } else {

    packageStartupMessage("... Llogging to the console for 'summerr' is disabled. ",
                          "You can enable it with `options(summerr.log = TRUE)`.")

  }

}

#' Make first letter of a string uppercase
#'
#' @param x an string
#' @noRd
str_first_up <- function(x) {

  # Note: stringr::str_to_sentence messes up upper case letters in paths.

  substr(x, 1, 1) <- toupper(substr(x, 1, 1))

  x

}

#' Logging a separation line
#'
#' @param char the character to repeat
#' @param line the length of the line
#' @noRd
log_line <- function(char = "=", line = 68) {

  if (getOption("summerr.log", default = TRUE)) {

    rlang::inform(message = paste0(rep(char, line), collapse = ""))

  }

}

#' Logging of tasks
#'
#' @param ... message
#' @noRd
log_task <- function(...) {

  if (getOption("summerr.log", default = TRUE)) {

    rlang::inform(message = rlang::format_error_bullets(
      c(i = paste0(str_first_up(paste(...)), "."))
    ))

  }

}

#' Logging of processes
#'
#' @description
#' Sub-routines performed under the headline of the current task.
#'
#' @param ... message
#' @noRd
log_process <- function(...) {

  if (getOption("summerr.log", default = TRUE)) {

    rlang::inform(message = rlang::format_error_bullets(
      c(i = paste0("... ", paste(...), "."))
    ))

  }

}

#' Logging of process completion
#'
#' @param ... message
#' @noRd
log_done <- function() {

  log_process("done")

}

#' Logging of messages
#'
#' @description
#' Anything else.
#'
#' @param ... message
#' @noRd
log_message <- function(...) {

  if (getOption("summerr.log", default = TRUE)) {

    rlang::inform(message = rlang::format_error_bullets(
      c(i = paste("...", ...))
    ))

  }

}

#' Logging an object
#'
#' @description
#' Typically a table or else.
#'
#' @param object object
#' @noRd
log_object <- function(object) {

  if (getOption("summerr.log", default = TRUE)) {

    print(object)

  }

}

#' Handling of errors
#'
#' @param header A concise description of the error.
#' @param body A named character that indicates, e.g. which elements caused an
#' error.
#' @param ... Anything else passed to \code{\link[rlang:abort]{abort(...)}} as metadata.
#' @noRd
log_error <- function(header, body, ...) {

  rlang::abort(message = paste0(
    str_first_up(header), "\n",
    rlang::format_error_bullets(body)), ...)

}

#' Logging for debugging
#'
#' @description
#' Will print the function name it was invoked from.
#'
#' @param ... message
#' @noRd
log_debugging <- function(..., object = NULL) {

  if (getOption("summerr.debug", default = FALSE)) {

    fn_call <- match.call(call = sys.call(-1))[1L]

    if (is.null(object)) {

      rlang::inform(message = rlang::format_error_bullets(
        paste0(
          "", fn_call, ": ", str_first_up(paste(...)), ".")
      ))

    } else {

      rlang::inform(message = rlang::format_error_bullets(
        paste0(
          "", fn_call, ": ", str_first_up(paste(...)))
      ))

      print(object)

    }

  }

}
