#' Express file paths in Canonical Form. Truly.
#'
#' Under Windows and UNIX, \code{\link[base:.Platform]{.Platform$file.sep}} is "/". Wraps \code{\link[base:normalizePath]{normalizePath}} with \code{.Platform$file.sep} and removes trailing slashes.
#'
#' @param path A character vector of file paths.
#'
#' @details
#' Tilde-expansion is first done on \code{path} (see \code{\link[base:path.expand]{path.expand}}.
#'
#' @return
#' A character vector.
#'
#' @export
renormalize_path <- function(path) {

  sub(x = normalizePath(path, winslash = .Platform$file.sep),
      # remove trailing path separator which is not accepted on Windows
      pattern = paste0(.Platform$file.sep, "$"), replacement = "")

}

#' Interactively choose a single file.
#'
#' This is a convenient wrapper to select one file in the directory \code{path} from multiple matching \code{pattern}.
#'
#' @param path A character vector of file paths; the default corresponds to the working directory, \code{\link[base]{getwd}()}. Tilde expansion (see \code{\link[base]{path.expand}}) is performed.
#' @param pattern A \link[base:regex]{regular expression}. Only file names which match the regular expression will be returned.
#'
#' @details
#' In case multiple files are found, the user is prompted to choose one option or none.
#'
#' @return
#' A character vector with a single element in case a single file matches \code{pattern} or a single choice was made, else an empty character vector.
#'
#' In a non-interactive session, returns an empty character vector and a warning if multiple files were found.
#'
#' @export
get_single_file <- function(path = ".", pattern) {

  dir_files <- list.files(path = path, pattern = pattern, full.names = TRUE)

  if (length(dir_files) > 1) {

    if (interactive()) {

      ANS <- as.numeric(readline(
        paste0("Which file to use?\n",
               paste(c("[0] None of these (skip)",
                       lapply(seq_along(dir_files), function(i) paste0("[", i, "] ", list.files(path = path, pattern = pattern)[i]))),
                     collapse =  "\n"), "  ")))

      if (ANS %in% seq_along(dir_files))
        dir_files <- dir_files[ANS]
      else
        dir_files <- character()

    } else {

      message("Warning: '", pattern, "' matches multiple files in '", path, "'. Skipping.")

      dir_files <- character()

    }

  }

  if (length(dir_files) != 1) message("Warning: No files matching '", pattern, "'.")

  return(dir_files)

}
