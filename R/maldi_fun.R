# FILE IMPORT AND EXPORT -------------------------------------------------------

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
#' The resulting \link[dplyr:grouped_df]{grouped data frame} is grouped by \code{well}.
#'
#' @seealso \code{\link{import_layout_from_paths}}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
import_layout_from_paths.maldi <- function(paths, pivot = "[0-9]_[A-Z]+[0-9]+",
                                           relative_to = getwd()) {

  well_regex <- "[A-Z]+[0-9]+"

  stopifnot(grepl(well_regex, x = pivot, fixed = TRUE))

  datad <- import_layout_from_paths(paths = paths, pivot = pivot,
                                    relative_to = relative_to)

  datam <- dplyr::mutate(datad, as_well(stringr::str_extract(pivot, well_regex),
                                        as.tibble = TRUE)) %>%
    dplyr::arrange(.data$well, .data$sub_1, .by_group = TRUE) %>%
    dplyr::group_by(.data$well)

  attr(datam, "dir") <- attr(datad, "dir")

  return(datam)

}

#' Import all MALDI spectra from a directory.
#'
#' Calls \link[MALDIquantForeign:MALDIquantForeign-package]{MALDIquantForeign} to scan
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
#' @importFrom rlang .data
#'
#' @export
maldi_import_spectra <- function(path = getwd(), ...) {

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

  log_task("scanning", sQuote(path), "for mzXML mass spectra. This might take a while")

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

#' Get the file paths of a MALDI spectrum as vector.
#'
#' @param object A (list of) \code{\link[MALDIquant:MassSpectrum-class]{MassSpectrum}}.
#'
maldi_get_paths <- function(object) {

  sapply(object, function(x) MALDIquant::metaData(x)$file, USE.NAMES = FALSE)

}

#  PROCESSING MASS SPECTRA -----------------------------------------------------

#' Align and average MALDI spectra
#'
#' Default method to align and average MALDI spectra.
#'
#' @inheritParams maldi_get_paths
#' @param method_baseline Method for background subtraction.
#' See \link[MALDIquant:removeBaseline]{MALDIquant::removeBaseline}.
#' @param method_average Method to average replicate measurements.
#' See \link[MALDIquant:averageMassSpectra]{MALDIquant::averageMassSpectra}.
#' @param final_trim_range A numeric vector from which the maximum and the minimum
#' value are taken to trim the spectrum to a relevant range.
#' @param ... Arguments passed to \code{\link[MALDIquant:alignSpectra]{alignSpectra}}.
#'
#' @return A single \code{\link[MALDIquant:MassSpectrum-class]{MassSpectrum}}.
#'
#' @details
#' The baseline of the spectra is removed using \code{method_baseline} first,
#' then the spectra aligned using the default parameters of
#' \code{\link[MALDIquant:alignSpectra]{alignSpectra}} unless other arguments
#' are passed via \code{...}. The aligned spectra are averaged and trimmed to
#' \code{final_trim_range}.
#'
#' This method can be substituted/overwritten to adapt specific needs in a pipeline.
#'
#' @importFrom magrittr %>%
#'
#' @export
maldi_average_spectra <- function(object,
                                  method_baseline = "SNIP",
                                  method_average  = "mean",
                                  ...,
                                  final_trim_range = c(0, Inf)) {

  object  %>%
    # remove baseline
    MALDIquant::removeBaseline(
      method = method_baseline) %>%
    # align spectra in case of repeated measurements ...
    MALDIquant::alignSpectra(...) %>%
    # ... and average them
    MALDIquant::averageMassSpectra(
      method = method_average) %>%
    # trim spectra
    MALDIquant::trim(range = final_trim_range)

}

#' Apply a function to a group of MALDI spectra
#'
#' @inheritParams maldi_get_paths
#' @inheritParams base::tapply
#' @param MoreArgs A list of parameters passed to \code{FUN}.
#'
#' @details
#' \code{FUN} is called with \code{MoreArgs} along the mass spectrum list \code{object}.
#' Thereby, \code{FUN} is called upon groups of \code{object}, if a grouping can
#' be established; else, \code{FUN} is applied for each element of \code{object}
#' separately.
#'
#' To establish the grouping, \code{INDEX} is used.
#'
#' \itemize{
#' \item{If \code{INDEX} is a \code{\link[base:data.frame]{data.frame}} or a similar
#' object such as a \code{\link[tibble:tibble]{tibble}}, the number and order of
#' the rows must correspond to the number and order of the elements in \code{object}.
#' }
#' \item{If \code{INDEX} qualifies as list of \code{\link[base:factor]{factor}}s,
#' each of the same length as \code{object}, this is used as grouping.
#' }
#' \item{If \code{INDEX} is \code{NULL}, an attempt is made to extract the
#' experiment layout from the file paths stored in the spectra (see
#' \link{import_layout_from_paths.maldi}) and the function is applied by well,
#' i.e., by a portion of the non-unique file path that matches \code{"[A-Z]+[0-9]+"}}.
#' }
#'
#' @return Depends on \code{FUN}.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom rlang !!!
#'
#' @export
maldi_tapply <- function(object, FUN,
                         INDEX = NULL, MoreArgs = list()) {

  if (is.null(INDEX)) {

    INDEX <- import_layout_from_paths.maldi(maldi_get_paths(object),
                                            relative_to = attr(object, "dir")) %>%
      # it is important to pass the index along the order of the MassSpectrum list
      dplyr::arrange(.data$findex) %>%
      dplyr::group_indices()

  }

  log_debugging("INDEX is", object = INDEX)

  eval(rlang::call2(tapply, X = object, INDEX = INDEX, FUN = FUN, !!!MoreArgs))

}

#' Align and average MALDI spectra by well
#'
#' Applies \link{maldi_average_spectra} to each group of wells using \link{maldi_tapply}.
#'
#' @inheritParams maldi_tapply
#' @inheritParams maldi_average_spectra
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
maldi_average_by_well <- function(object,
                                  INDEX = NULL,
                                  method_baseline = "SNIP",
                                  method_average  = "mean",
                                  ...,
                                  final_trim_range = c(0, Inf)) {

  log_process("averaging and aligning spectra")

  data_mzxml <- maldi_tapply(object = object, FUN = maldi_average_spectra,
                             INDEX = INDEX,
                             MoreArgs = list(method_baseline = method_baseline,
                                             method_average  = method_average,
                                             ...,
                                             final_trim_range = final_trim_range))

  log_done()

  attr(data_mzxml, "dir") <- attr(object, "dir")

  data_mzxml

}

