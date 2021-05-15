str_first_up <- function(x) {

  # Note: stringr::str_to_sentence messes up upper case letters in paths.

  substr(x, 1, 1) <- toupper(substr(x, 1, 1))

  x

}

log_task <- function(...) {

  if (getOption("log", default = TRUE)) {

    rlang::inform(message = rlang::format_error_bullets(
      c(i = paste0(str_first_up(paste(...)), "."))
    ))

  }

}

log_process <- function(...) {

  if (getOption("log", default = TRUE)) {

    rlang::inform(message = rlang::format_error_bullets(
      c(i = paste0("... ", paste(...), "."))
    ))

  }

}

log_done <- function() {

  log_process("done")

}

log_message <- function(...) {

  if (getOption("log", default = TRUE)) {

    rlang::inform(message = rlang::format_error_bullets(
      c(i = paste("...", ...))
    ))

  }

}

log_error <- function(header, body) {

  rlang::abort(message = paste0(
    str_first_up(header), "\n",
    rlang::format_error_bullets(body)))

}

log_debugging <- function(...) {

  if (getOption("debug", default = TRUE)) {

    fn_call <- match.call(call = sys.call(-1))[1L]

    rlang::inform(message = rlang::format_error_bullets(
      paste0(
        "", fn_call, ": ", str_first_up(paste(...)), ".")
      ))

  }

}
