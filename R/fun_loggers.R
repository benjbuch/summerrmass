log_task <- function(...) {

  if (getOption("verbose", default = TRUE)) {

    rlang::inform(message = rlang::format_error_bullets(
      c(i = paste0(stringr::str_to_sentence(paste(...)), "."))
    ))

  }

}

log_process <- function(...) {

  if (getOption("verbose", default = TRUE)) {

    rlang::inform(message = rlang::format_error_bullets(
      c(i = paste0("... ", paste(...), "."))
    ))

  }

}

log_done <- function() {

  log_process("done")

}

log_message <- function(...) {

  if (getOption("verbose", default = TRUE)) {

    rlang::inform(message = rlang::format_error_bullets(
      c(i = paste("...", ...))
    ))

  }

}

log_error <- function(header, body) {

  rlang::abort(message = paste0(
    stringr::str_to_sentence(header), "\n",
    rlang::format_error_bullets(body)))

}

log_debugging <- function(...) {

  if (getOption("debug", default = TRUE)) {

    fn_call <- match.call(call = sys.call(-1))[1L]

    rlang::inform(message = rlang::format_error_bullets(
      paste0(
        "", fn_call, ": ", stringr::str_to_sentence(paste(...)), ".")
      ))

  }

}
