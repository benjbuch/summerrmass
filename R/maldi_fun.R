# FILE IMPORT AND EXPORT -------------------------------------------------------

#' Establish the experiment layout from nested directories for MALDI analyses
#'
#' This wrapper conveniently establishes the experiment layout from nested file
#' groups as exported by Bruker FlexAnalysis. Here, measurements for each plate
#' position (well) are saved in a folder such as "0_A1", "0_A2", etc. Replicate
#' measurements of the same well are grouped in subfolders such as "0_A1/1",
#' "0_A1/2" etc.
#'
#' @inheritParams summerr::import_layout_from_paths
#'
#' @details
#' \code{pivot} must contain a regular expression to identify the well, i.e.,
#' contain [A-Z]+[0-9]+".
#'
#' The resulting \link[dplyr:grouped_df]{grouped data frame} is grouped by \code{well}.
#'
#' @seealso \code{\link[summerr:import_layout_from_paths]{import_layout_from_paths}}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
import_layout_from_paths.maldi <- function(paths, pivot = "[0-9]_[A-Z]+[0-9]+",
                                           relative_to = getwd()) {

  well_regex <- "[A-Z]+[0-9]+"

  stopifnot("Regular expression does not match [A-Z]+[0-9]+." = grepl(
    well_regex, x = pivot, fixed = TRUE))

  datad <- summerr::import_layout_from_paths(paths = paths, pivot = pivot,
                                             relative_to = relative_to)

  datam <- dplyr::mutate(datad, summerr::as_well(stringr::str_extract(pivot, well_regex),
                                        as.tibble = TRUE)) %>%
    dplyr::arrange(dplyr::across(tidyselect::any_of(c("well", "sub_1"))), 
                   .by_group = TRUE) %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(c("well"))))

  attr(datam, "dir") <- attr(datad, "dir")

  return(datam)

}

#' Import all MALDI spectra from a directory
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

  path <- summerr::normalizePath(path)

  # this is important to call the terminal exection

  setwd(path)

  # number of existing mzXML and fid files in path

  summerr::log_task("scanning", sQuote(path), "for mass spectra")

  files_mzxml <- list.files(
    path = path, pattern = "\\.mzxml", ignore.case = TRUE,
    recursive = TRUE, full.names = TRUE)

  files_fid   <- list.files(
    path = path, pattern = "fid", ignore.case = TRUE,
    recursive = TRUE, full.names = TRUE)

  summerr::log_process("found", length(files_fid), "Bruker FID files")

  summerr::log_process("found", length(files_mzxml), "mass spectrometry data (mzXML) files")

  # processing of fid files (optional)

  if (.Platform$OS.type == "windows" && nchar(Sys.which("compassxport")) > 0 &&
      interactive() && rstudioapi::isAvailable()) {

    # check if a version of compassxport is installed and prompt to process
    # the fdi files

    answer <- rstudioapi::showQuestion(
      "Run CompassXport?",
      "A command line version of 'compassxport' has been detected on your system. Do you want to (re-)process the FID files in this path?\n\nChoosing 'yes' will overwrite the existing 'Analysis.mzXML' files.
      ", ok = "No", cancel = "Yes")

    if (!answer) {

      summerr::log_process("processing", length(files_fid), "Bruker FID files")

      for (i in files_fid) shell(shQuote(paste("compassxport -a",
                                               shQuote(summerr::normalizePath(i), type = "cmd"),
                                               "-raw 1"), type = "cmd2"))

      summerr::log_done()

    }

  }

  # importing of mzXML spectra

  files_mzxml <- list.files(
    path = path, pattern = "\\.mzxml", ignore.case = TRUE,
    recursive = TRUE, full.names = TRUE)

  summerr::log_process("importing", length(files_mzxml), "mass spectrometry data (mzXML) files")

  suppressMessages({

    data_mzxml <- MALDIquantForeign::importMzXml(path, ...)

  })

  # quality control

  empties <- sapply(data_mzxml, MALDIquant::isEmpty)
  centies <- sapply(data_mzxml, MALDIquant::isRegular)

  if (any(empties)) {

    empties <- sapply(data_mzxml, function(x) MALDIquant::metaData(x)$file)[which(empties)]
    names(empties) <- rep("x", length(empties))

    summerr::log_error(header = "empty spectra detected",
              body = c(i = "The following mass spectra are empty:", empties))

  }

  if (any(centies)) {

    centies <- sapply(data_mzxml, function(x) MALDIquant::metaData(x)$file)[which(centies)]
    names(centies) <- rep("x", length(centies))

    summerr::log_error(header = "centroid spectra detected",
              body = c(i = "The following mass spectra are centroid:", centies))

  }

  attr(data_mzxml, "dir") <- path

  data_mzxml

}

#' Get the file paths of a MALDI spectrum as vector
#'
#' @param object A (list of) \code{\link[MALDIquant:MassSpectrum-class]{MassSpectrum}}.
#'
#' @export
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

  summerr::log_debugging("INDEX is", object = INDEX)

  eval(rlang::call2(tapply, X = object, INDEX = INDEX, FUN = FUN, !!!MoreArgs))

}

#' Align and average MALDI spectra by well
#'
#' Applies \link{maldi_average_spectra} to each group of wells using \link{maldi_tapply}.
#'
#' @inheritParams maldi_tapply
#' @inheritParams maldi_average_spectra
#' @inheritParams import_layout_from_paths.maldi
#'
#' @details
#' The wells are identified using \code{pivot} from the associated paths in the
#' object. See \link{import_layout_from_paths.maldi}.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
maldi_average_by_well <- function(object,
                                  method_baseline = "SNIP",
                                  method_average  = "mean",
                                  ...,
                                  final_trim_range = c(0, Inf),
                                  pivot = "[0-9]_[A-Z]+[0-9]+") {

  summerr::log_process("averaging and aligning spectra")

  INDEX <- maldi_get_paths(object) %>%
    import_layout_from_paths.maldi(pivot = pivot,
                                   relative_to = attr(object, "dir")) %>%
    # it is important to pass the index along the order of the MassSpectrum list
    dplyr::arrange(.data$findex) %>%
    dplyr::group_indices()

  data_mzxml <- maldi_tapply(object = object, FUN = maldi_average_spectra,
                             INDEX = INDEX,
                             MoreArgs = list(method_baseline = method_baseline,
                                             method_average  = method_average,
                                             ...,
                                             final_trim_range = final_trim_range))

  summerr::log_done()

  attr(data_mzxml, "dir") <- attr(object, "dir")

  data_mzxml

}

#' Extract intensities for given m/z values
#'
#' @inheritParams maldi_get_paths
#' @inheritParams import_layout_from_paths.maldi
#' @param mass_list Named list specifying the m/z at which to try extracting the
#' intensity.
#' @param tolerance_assignment Maximum difference between the calculated m/z and
#' the mass at the detected peak m/z.
#' @inheritParams MALDIquant::detectPeaks
#' @param ... Further arguments passed to \code{\link[MALDIquant:detectPeaks]{MALDIquant::detectPeaks}}.
#' @param manual Logical whether all peaks must be picked manually (\code{TRUE})
#' or only in ambiguous cases (\code{FALSE}).
#'
#' @details
#' Peaks are pre-selected from a smoothed spectrum within the m/z range given by
#' \code{mass_list} and \code{tolerance_assignment}. Peaks are measured in the
#' original, unsmoothed spectrum using \code{\link[MALDIquant:detectPeaks]{MALDIquant::detectPeaks}}.
#'
#' @return
#' A \code{\link[tibble:tibble]{tibble}} with columns "ion", "mass", "intensity",
#' "percent" and well identifiers "well" etc. that relate the measured intensies
#' to the \code{object}, e.g. using "findex" (position in list) and/or "gindex"
#' (group number in list).
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' data("BobCAT")
#'
#' my_masses <- c(ion_species_A = 2425, ion_species_B = 2441)
#'
#' # trim the spectra during averaging to speed-up the peak detection; alternatively
#' # use MALDIquant::trim(...).
#' my_spectrum <- maldi_average_by_well(BobCAT, final_trim_range = c(2420, 2445))
#'
#' maldi_find_peaks_by_well(my_spectrum, my_masses)  # automatically
#' maldi_find_peaks_by_well(my_spectrum, my_masses, manual = TRUE)  # all by hand
#' maldi_find_peaks_by_well(my_spectrum, mass_list = c(ion_species_C = 2439))  # tricky ones by hand
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
maldi_find_peaks_by_well <- function(object,
                                     mass_list,
                                     tolerance_assignment = 0.5,
                                     method = "MAD",
                                     SNR = 3,
                                     ...,
                                     manual = FALSE,
                                     pivot = "[0-9]_[A-Z]+[0-9]+") {

  stopifnot(length(mass_list) > 0)

  pick_peaks_by_hand <- function(s0, s1, p1, cx, mx, tol = tolerance_assignment) {

    # s0: spectrum as is, for plotting only
    # s1: spectrum processed for peak detection, e.g. smoothed
    # p1: list with peaks on s1
    # mx: named list with masses to be assigned
    # cx: named list with missing (NA) candidate peaks to be picked from

    for (ci in which(is.na(cx))) {

      MALDIquant::plot(s0, col = "lightgray")

      graphics::points(MALDIquant::mass(s1), MALDIquant::intensity(s1),
                         type = "l", col = "blue")
      graphics::points(MALDIquant::mass(p1), MALDIquant::intensity(p1), col = "blue")

      graphics::title(paste("Assign", names(cx)[ci], "peak"))
      graphics::mtext("Hit 'Esc' when done or to skip.", line = 0.25, col = "lightgray")

      graphics::abline(v = c(tol, -tol) + as.numeric(mx[ci]), lty = 2)

      # graphics::axis(side = 1, at = c(tol, -tol) + as.numeric(mx[ci]),
      #      labels = FALSE, col = "magenta", lwd = 0, lwd.ticks = 2, line = -.5)

      graphics::identify(MALDIquant::mass(p1), MALDIquant::intensity(p1),
                         plot = FALSE) -> new_point

      if (length(new_point) > 0) {

        graphics::points(MALDIquant::mass(p1)[new_point],
                         MALDIquant::intensity(p1)[new_point],
                         pch = 16, col = "blue")

        graphics::mtext(paste0(
          round(MALDIquant::mass(p1)[new_point], 2), " (",
          round(as.numeric(mx[ci]), 2), ")"), side = 3, line = -1.5)

        Sys.sleep(0.9)

        cx[[ci]] <- MALDIquant::mass(p1)[new_point]

      } else {

        graphics::mtext("none assigned", side = 3, line = -1.5)

      }

    }

    return(cx)

  }

  data_peaks.tmp <- list()

  # proceed well by well now for peak detection and assignment

  for (i in seq_along(object)) {

    smoo_spectrum <- mass_spectrum <- object[[i]]
    reso_spectrum <- mean(diff(MALDIquant::mass(mass_spectrum)))

    summerr::log_debugging("spectrum resolution is", reso_spectrum)

    tryCatch({

      suppressWarnings({

        # warning is: 'Negative intensity values are replaced by zeros.'

        smoo_spectrum <- MALDIquant::smoothIntensity(
          mass_spectrum, method = "SavitzkyGolay",
          halfWindowSize = floor(tolerance_assignment / reso_spectrum))

      })

      # browser()

      smoo_peaks <- MALDIquant::detectPeaks(object = smoo_spectrum,
                                            method = method, SNR = SNR, ...)

      # preselect automatically or manually?

      if (manual) {

        smoo_list <- pick_peaks_by_hand(
          s0 = mass_spectrum,
          s1 = smoo_spectrum,
          p1 = smoo_peaks,
          cx = rep(NA, length(mass_list)) %>% as.list() %>%
            magrittr::set_names(names(mass_list)),
          mx = mass_list
        )

      } else {

        smoo_list <- sapply(mass_list, function(x) MALDIquant::mass(
          smoo_peaks)[MALDIquant::match.closest(x, table = MALDIquant::mass(
            smoo_peaks), tolerance = tolerance_assignment, nomatch = NA)])

        if (any(is.na(smoo_list))) {

          smoo_list <- pick_peaks_by_hand(
            s0 = mass_spectrum,
            s1 = smoo_spectrum,
            p1 = smoo_peaks,
            cx = smoo_list,
            mx = mass_list
          )

        }

      }

      peaks <- MALDIquant::detectPeaks(object = mass_spectrum,
                                       method = "MAD", SNR = SNR, ...)

      data_peaks.tmp[[i]] <- sapply(
        smoo_list, MALDIquant::match.closest,
        table = MALDIquant::mass(peaks),
        tolerance = tolerance_assignment,
        nomatch = NA) %>%
        tibble::as_tibble(rownames = "ion") %>%
        dplyr::mutate(mass = MALDIquant::mass(peaks)[.data$value],
                      intensity = MALDIquant::intensity(peaks)[.data$value]) %>%
        dplyr::select(!.data$value)

    }, error = function(e) {

      summerr::log_message("failed peak detection for", sQuote(MALDIquant::metaData(object[[i]])$file))

    })

  }

  data_peaks <- maldi_get_paths(object) %>%
    import_layout_from_paths.maldi(pivot = pivot, relative_to = attr(object, "dir")) %>%
    dplyr::left_join(
      data_peaks.tmp %>% dplyr::bind_rows(.id = "findex") %>%
        dplyr::mutate(findex = as.integer(.data$findex)),
      by = "findex") %>%
    dplyr::mutate(ion = factor(.data$ion, levels = names(mass_list))) %>%
    dplyr::mutate(percent = .data$intensity / sum(.data$intensity,
                                                  na.rm = TRUE) * 100) %>%
    dplyr::select(dplyr::group_vars(.),
                  tidyselect::any_of(c("ion", "mass", "intensity", "percent",
                                       "n_replicates", "findex", "gindex")),
                  tidyselect::everything())

  attr(data_peaks, "SNR") <- SNR

  data_peaks

}

#' Plot a mass spectrum
#'
#' Draws a mass spectrum using the default method and adds additional points and
#' lines if required.
#'
#' @param object A \code{\link[MALDIquant:MassSpectrum-class]{MassSpectrum}}.
#' @param xaxt A character which specifies the x axis type. See \link[graphics:par]{par()}.
#' @param x,y The coordinates of additional points to draw.
#' @param SNR Level at which the signal-to-noise-ratio should be indicated.
#' @param col Color for points and lines.
#' @param overlay_color Color for overlay at 50\% transparency.
#'
#' @details
#' If \code{object} is not a \code{\link[MALDIquant:MassSpectrum-class]{MassSpectrum}},
#' an empty plot is drawn.
#'
#' @export
maldi_draw_spectrum <- function(object = NULL, xaxt = "s", x = NULL, y = NULL,
                                overlay_color = NA, SNR = NaN, col = "red") {

  if (MALDIquant::isMassSpectrum(object)) {

    MALDIquant::plot(object, main = NULL, xaxt = xaxt)

    # draw colorful overlay (NA will not fill the rectangle)

    graphics::rect(
      graphics::par("usr")[1], graphics::par("usr")[3],
      graphics::par("usr")[2], graphics::par("usr")[4],
      col = grDevices::adjustcolor(overlay_color, alpha.f = 0.5)
    )

    # add points

    if (!is.null(x) && !is.null(y)) graphics::points(x, y, col = col)

    # indicate SNR

    if (!is.null(SNR) && !is.na(SNR)) {

      noise <- MALDIquant::estimateNoise(object)

      graphics::lines(noise[, 1], noise[, 2] * SNR, col = col)

    }

  } else {

    graphics::plot.new()

  }

}


#' Draw m/z peaks in spectra
#'
#' Draws spectra and places them on a pdf page for printing.
#'
#' @inheritParams maldi_get_paths
#' @param data_peaks A peak data table as returned by \link{maldi_find_peaks_by_well};
#' must be a subset of \code{object}.
#' @param ncol Number of plot columns to arrange per page.
#' @param nrow Number of plot rows to arrange per page.
#' @param title Optional title of plot.
#' @param highlight_missing_peaks Whether or not spectra in which not all peaks
#' are detected should be highlighted.
#' @param SNR Optional signal-to-noise level used to detect peaks.
#'
#' @details
#' No checking is done whether \code{data_peaks} was indeed derived from \code{object}.
#' However, \code{data_peaks} must contain a column \code{findex} which points to
#' the index of corresponding spectrum in \code{object}.
#'
#' @examples
#' data("BobCAT")
#'
#' my_masses <- c(ion_species_A = 2425, ion_species_B = 2441)
#'
#' # trim the spectra during averaging to speed-up the peak detection; alternatively
#' # use MALDIquant::trim(...).
#' my_spectrum <- maldi_average_by_well(BobCAT, final_trim_range = c(2420, 2445))
#' # detect peaks at the masses
#' my_peaks <- maldi_find_peaks_by_well(my_spectrum, my_masses)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
maldi_draw_peaks_by_well <- function(object, data_peaks, ncol = 2, nrow = 6,
                                     highlight_missing_peaks = TRUE, title = NULL,
                                     SNR = NULL) {

  summerr::log_debugging("entered graphics device to plot peaks", object = data_peaks)

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))  # reconstitute par settings

  dat_p <- data_peaks %>%
    dplyr::group_by(.data$findex, .data$well) %>%
    dplyr::summarize(highlight = c(highlight_missing_peaks & any(is.na(.data$mass))),
                     mass = list(.data$mass), intensity = list(.data$intensity),
                     n_replicates = max(.data$n_replicates), .groups = "keep") %>%
    dplyr::distinct()

  graphics::par(mfcol = c(nrow, ncol), mar = c(0, 2, 0, 1), oma = c(2, 2, 4, 2))

  for (i in summerr::arrange_on_page(dat_p$findex, byrow = TRUE)) {

    if (is.na(i)) {

      graphics::plot.new()
      next

    }

    fi <- dat_p$findex[[i]]

    maldi_draw_spectrum(object[[fi]], x = dat_p$mass[[i]], y = dat_p$intensity[[i]],
                        overlay_color = c(NA, "lightgray")[dat_p$highlight[[i]] + 1],
                        xaxt = c("n", "s")[(i %in% summerr::get_border_indices(
                          dat_p$findex, border = "b", byrow = TRUE) + 1)],
                        SNR = SNR)

    graphics::title(paste0(dat_p$well[[i]], c("", "*")[(dat_p$n_replicates[[i]] > 1) + 1]),
                    line = -1.5)

    if (!is.null(title) && i == 1) graphics::mtext(title, line = 1, side = 3,
                                                   outer = TRUE, adj = 0)

  }

  summerr::log_debugging("exited graphics device to plot peaks")

}
