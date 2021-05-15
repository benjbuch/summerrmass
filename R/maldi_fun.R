# ==== FILE IMPORT AND EXPORT ====

#' Establish the experiment layout from nested directories for MALDI analyses.
#'
#' This wrapper conveniently establishes the experiment layout from nested file
#' groups as exported by Bruker FlexAnalysis. Here, measurements for each plate
#' position (well) are saved in a folder such as "0_A1", "0_A2", etc. Replicate
#' measurements of the same well are grouped in subfolders such as "0_A1/1",
#' "0_A1/2" etc.
#'
#' @inheritParams import_layout_from_paths
#'
#' @details
#' \code{pivot} must contain a regular expression to identify the well, i.e.,
#' contain [A-Z]+[0-9]+".
#'
#' @seealso \code{\link{import_layout_from_paths}}
#'
#'
import_layout_from_paths.maldi <- function(paths, pivot = "[0-9]_[A-Z]+[0-9]+",
                                           relative_to = getwd()) {

  require(dplyr, quietly = TRUE)

  well_regex <- "[A-Z]+[0-9]+"

  stopifnot(grepl(well_regex, x = pivot, fixed = TRUE))

  datad <- import_layout_from_paths(paths = paths, pivot = pivot,
                                    relative_to = relative_to)

  datam <- dplyr::mutate(datad, as_well(stringr::str_extract(pivot, well_regex),
                                        as.tibble = TRUE)) %>%
    dplyr::arrange(well, sub_1, .by_group = TRUE)

  attr(datam, "dir") <- attr(datad, "dir")

  return(datam)

}

#' Import all MALDI spectra from a directory.
#'
#' Calls \link[MALDIquantForeign:MALDIquantForeign]{MALDIquantForeign} to scan
#' a directory for MALDI spectra, import them, and perform intial qualtiy control.
#'
#' @details
#' In case a version of Bruker's CompassXport is installed, the user will be asked
#' whether the original FID files should be (re-)exported to the corresponding mzXML.
#'
#' The spectra are checked for emptiness and regularity.
#'
#' @inheritParams MALDIquantForeign::importMzXml
#' @inheritDotParams MALDIquantForeign::importMzXml
#'
#' @return
#' A \code{list} of \code{\link[MALDIquant:MassSpectrum-class]{MassSpectrum}}
#' objects. The scanned path is preserved as \code{attr(., "dir")}.
#'
#' @export
import_maldi_spectra <- function(path = getwd(), ...) {

  # make sure to re-enter the current working directory if anything goes wrong

  old_wd <- getwd()
  on.exit(setwd(old_wd))

  path <- normalizePath(path)

  setwd(path)

  # processing of fid files (optional)

  if (.Platform$OS.type == "windows" && nchar(Sys.which("compassxport")) > 0 &&
      interactive() && rstudioapi::isAvailable()) {

    # check if a version of compassxport is installed and prompt to process
    # the fdi files

    answer <- rstudioapi::showQuestion(
      "Run CompassXport?",
      "A command line version of 'compassxport' has been detected on your system. Do you want to (re-)process the FID data in this directory?\n\nChoosing 'yes' will overwrite the existing 'Analysis.mzXML' files.
      ", ok = "No", cancel = "Yes")

    if (!answer) {

      log_task("scanning", sQuote(path), "for FID files")

      fids <- list.files(path = path, pattern = "fid",
                         recursive = TRUE, full.names = TRUE)

      log_process("processing", length(fids), "FDI files")

      for (i in fids) shell(shQuote(paste("compassxport -a",
                                          shQuote(normalizePath(i), type = "cmd"),
                                          "-raw 1"), type = "cmd2"))

      log_done()

    }

  }

  # processing of mzXML spectra

  log_task("scanning ", sQuote(path), " for mzXML mass spectra")
  log_process("this might take a while")

  suppressMessages({

    data_mzxml <- MALDIquantForeign::importMzXml(path, ...)

  })

  log_process("imported", length(data_mzxml), "spectra")

  # quality control

  empties <- sapply(data_mzxml, MALDIquant::isEmpty)
  centies <- sapply(data_mzxml, MALDIquant::isRegular)

  if (any(empties)) {

    empties <- sapply(data_mzxml, function(x) MALDIquant::metaData(x)$file)[which(empties)]
    names(empties) <- rep("x", length(empties))

    log_error(header = "empty spectra detected",
              body = c(i = "The following mass spectra are empty:", empties))

  }

  if (any(centies)) {

    centies <- sapply(data_mzxml, function(x) MALDIquant::metaData(x)$file)[which(centies)]
    names(centies) <- rep("x", length(centies))

    log_error(header = "centroid spectra detected",
              body = c(i = "The following mass spectra are centroid:", centies))

  }

  attr(data_mzxml, "dir") <- path

  data_mzxml

}
