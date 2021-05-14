# ==== DIRECTORY NAVIAGATION ====

#' Express file paths in their canonical form. Truly.
#'
#' Under Windows and UNIX, \code{\link[base:.Platform]{.Platform$file.sep}} both
#' may expand to "\code{/}", whereas some upstream applications may still use
#' "\code{\\}" to separate folders and files.
#'
#' @param path A character vector of file paths.
#'
#' @details
#' Calls \code{\link[base:normalizePath]{normalizePath}} with \code{.Platform$file.sep}.
#' In addition, trailing slashes are removed.
#'
#' @return
#' A character vector.
#'
normalizePath <- function(path, verbose = getOption("verbose")) {

  if (verbose) message("... ", getNamespaceName(environment(normalizePath)),
                       ":normalizePath received '", path, "'")

  norm_path <- stringr::str_replace(
    base::normalizePath(path, winslash = .Platform$file.sep, mustWork = FALSE),
    # remove trailing path separator which is not accepted on Windows
    pattern = paste0(.Platform$file.sep, "$"), replacement = ""
  )

  if (verbose) message("... ", getNamespaceName(environment(normalizePath)),
                       ":normalizePath returned '", norm_path, "'")

  return(norm_path)

}

#' Prompt to select from a list of options.
#'
#' Helper asking the user to select an item from a list.
#'
#' @param items A character vector of options/items.
#' @param caption The prompt.
#'
#' @return
#' A single character element.
#'
select_from_list <- function(items, caption = "Select an item") {

  stopifnot(is.character(items))

  it (length(items) == 1) return(items)

  choice <- readline(paste0(
    caption, "\n",
    paste(sprintf(paste0("%", as.integer(log10(length(items)) + 3), "s %s"),
                  sprintf("[%i]", c(0, seq_along(items))),
                  c("None of these (skip)", items)),
          collapse = "\n"), "  "))

  choice <- as.numeric(stringr::str_extract(choice, "[0-9]+"))

  if (choice %in% seq_along(items)) {

    item <- items[choice]

  } else {

    item <- character()

  }

  return(item)

}

#' Interactively choose a directory.
#'
#' This is a wrapper to select a directory \code{path} to process. Depending on
#' the mode R is invoked from, choosing is interactive and may use the RStudio IDE.
#'
#' @param path A character vector of length 1 to start browsing; defaults to the
#' current working directory.
#'
#' @return
#' A character with the path of the chosen directory.
#'
#' @export
select_directory <- function(path = getwd(), caption = "Select a directory",
                             label = "Select") {

  if (interactive()) {

    if (rstudioapi::isAvailable()) {

      path <- rstudioapi::selectDirectory(caption = caption, label = label, path = path)

    } else {

      dirs <- list.dirs(path = ".", full.names = FALSE, recursive = TRUE)

      dirs <- dirs[which(!grepl(pattern = "^\\..+", x = dirs))]
      dirs <- dirs[which(nchar(dirs) > 0)]

      path <- file.path(path, select_from_list(
        caption = caption, items = dirs))

    }

  }

  path <- normalizePath(path)

  return(path)

}

#' Interactively choose a single file.
#'
#' This is a convenient wrapper to select one file in the directory \code{path}
#' from multiple files matching \code{pattern}.
#'
#' @param path A character vector of file paths; the default corresponds to the
#' working directory, \code{\link[base]{getwd}()}. Tilde expansion (see \code{
#' \link[base]{path.expand}}) is performed.
#' @param pattern A \link[base:regex]{regular expression}. Only file names which
#' match the regular expression will be returned.
#' @param prefix A regular expression to match the filename (without extension).
#' @param suffix A character to match the file extension, e. g. "csv".
#'
#'
#' @details
#' In case multiple files are found, the user is prompted to choose one option
#' or none.
#'
#' If \code{pattern} is not \code{NULL}, \code{prefix} and \code{suffix} will be
#' ignored.
#'
#' \code{prefix} is ignored when the RStudio API is used.
#'
#' @return
#' A character vector with a single element in case a single file matches \code{pattern}
#' or a single choice was made, else an empty character vector.
#'
#' In a non-interactive session, returns an empty character vector and a warning
#' if multiple files were found.
#'
#' @export
select_single_file <- function(path = getwd(), prefix = "*.+", suffix = "*",
                            pattern = NULL, filetype = "file",
                            caption = "Select a", label = "Select") {

  if (is.null(pattern)) {

    pattern <- paste0(prefix, "\\.", suffix, "$")

  }

  dir_files <- list.files(path = path, pattern = pattern, full.names = TRUE,
                          no.. = TRUE, recursive = FALSE)
  dirs <- list.dirs(path = path, full.names = TRUE, recursive = FALSE)

  dir_files <- setdiff(dir_files, dirs)

  if (length(dir_files) > 1) {

    if (interactive()) {

      if (rstudioapi::isAvailable(version_needed = "1.1.287")) {

        dir_files <- rstudioapi::selectFile(
          caption = paste(caption, filetype), label = label,
          path = path, filter = paste0(filetype, "(*.", suffix, ")"))

      } else {

        dir_files <- file.path(path, select_from_list(
          caption = caption, items = basename(dir_files)))

      }

    } else {

      message("Warning: '", pattern, "' matched multiple files in '", path, "'. Skipping.")

      dir_files <- character()

    }

  }

  if (is.null(dir_files)) dir_files <- character()

  if (length(dir_files) != 1) message("Warning: No files matching '", pattern, "'.")

  dir_files <- normalizePath(dir_files)

  return(dir_files)

}

# ==== GENERAL FILE IMPORT AND EXPORT ====


