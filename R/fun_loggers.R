log_message <- function(...) {

  if (getOption("verbose", default = TRUE)) {

    rlang::inform(message = rlang::format_error_bullets(
      c(i = paste(...))
    ))

  }

}

log_error <- function(...) {


}

log_debugging <- function(...) {

  if (getOption("debug", default = TRUE)) {

    fn_call <- match.call(call = sys.call(-1))[1L]

    rlang::inform(message = rlang::format_error_bullets(
      paste0(
        "", fn_call, ": ", paste(...))
      ))

  }

}
