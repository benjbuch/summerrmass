#' Batch MALDI peak analysis of a directory
#'
#' This function is to control the flow of actions when batch processing a
#' directory. It establishes the save and recover policies of previously
#' analysed data so that the same data is not reanalyzed twice if new data
#' is added to a directory.
#'
#' @param path Directory with mass spectra to analyze, store and import results;
#' may contain the plate layout(s).
#' @inheritParams import_layout_from_paths.maldi
#' @param manual Do you want to assign peaks manually in the first place or automatically?
#' (If )
#' @param review Do you want to review again all automatically assigned peaks?
#' (Requires RStudio.)
#' @param layout_file,confirm_layout_file,FUN_import_layout,MoreArgs_layout Path to a file that
#' contains metadata associated with each well/group. \code{layout_FUN} is applied
#' to \code{layout_file} before it is joined with the measurement result. Must
#' share column names with \link{import_layout_from_paths.maldi}. See Details.
#' @param FUN_spect,MoreArgs_spect The function to be applied for pre-processing
#' the mass spectra using \code{MoreArgs_spect}.
#' @param FUN_peaks,MoreArgs_peaks The function to be applied for peak detection
#' using \code{MoreArgs_peak}.
#' @param FUN_draw,MoreArgs_draw Arguments passed to \code{\link{maldi_draw_peaks_by_well}}.
#' @param MoreArgs_device Arguments passed to \code{\link[grDevices:pdf]{pdf()}}.
#' @param stored_peaks.generic Default name for peak backup file.
#' @param stored_spect.generic Default name for spectra backup file.
#' @inheritParams summerr::backup_file
#'
#' @details
#' The batch process is split into different stages starting from the selected
#' \code{path} as main directory:
#'
#' \enumerate{
#' \item{import and pre-processing of the spectra,}
#' \item{automated (or manual) peak picking for each spectrum,
#' depending if \code{manual = FALSE} (or \code{TRUE}),}
#' \item{review of automated peak picking if \code{review = TRUE}.}
#' }
#'
#' These tasks are performed by group and the results are stored in the folder
#' enclosing all data of the same group. This is the parent to the folders that
#' match \code{pivot} (the well folders). See \link{import_layout_from_paths.maldi}.
#'
#' If such a folder contains already analyses saved as \code{stored_peaks.generic}
#' or \code{stored_spect.generic}, these analyses can be imported for the next stage;
#' if not, the original files are renamed with a timestamp and new files added.
#'
#' \subsection{Adapting the pipeline}{
#'
#' ...
#'
#' \code{FUN_spect}, \code{FUN_peaks} and \code{layout_import_FUN} is can be of
#' type character or closure.
#'
#' In case \code{FUN_spect} or \code{FUN_peaks} use a function that must determine,
#' e.g. which spectra are repeated measurements from a single well, which is
#' typically accomplished in the default functions using \link{import_layout_from_paths.maldi},
#' the \code{pivot} regex may need to be updated in the corresponding \code{MoreArgs_...}
#' arguments as well. A warning is emitted if it is not.
#'
#' }
#'
#' \subsection{Joining with metadata from external plate layout}{
#'
#' If \code{layout_file} is \code{NULL}, the results will not be joined with an
#' external file that holds associated metadata. (Skip this section.)
#'
#' External plate layouts can be provided via \code{layout_file} either (1) as an
#' absolute path to a single file somwhere in the file system or (2) a file in
#' expressed relative to the selected \code{path}, e.g. "global_layout.xls".
#' The query is made via \code{\link[base:files]{file.exists}}.
#' In such cases, the file is used for all groups. In case (3) when such a file
#' does not exist, the file is looked for locally in each group directory, where
#' it must be present each time under the same name, e.g., as "layout.xls".
#' If (4) this is neither the case and \code{confirm_layout_file = TRUE}, you
#' will finally be prompted to select a layout file. You can force this behavior
#' by setting \code{layout_file = ""}.
#'
#' If \code{confirm_layout_file = TRUE}, you will also be asked if the file
#' identified according to one of the rules (1-3) above should be taken or not.
#'
#' To successfully merge your layout with the data, common column names must
#' exist between your layout file and the result of \link{import_layout_from_paths.maldi},
#' which extends the generic \link{import_layout_from_paths} by explicitly
#' extracting a well number as column "well".
#'
#' If you choose a graphical approach to define the plate layout, consider using
#' \link[summerr:import_layout_from_excel]{import_layout_from_excel}, which will
#' convert the plate into a (long) table with a column "well" and "content".
#'
#' If you provide such a (long) table directly, provide
#' \code{\link[readxl:read_excel]{readxl::read_excel}}
#' or another appropriate function as argument \code{layout_import_FUN}.
#'
#' }
#'
#' @return A list of two lists, named \code{$spectra} and \code{$peaks}, which
#' themselves contain one element (of spectra lists or data.frames) for each group.
#'
#' @examples
#' \dontrun{
#' maldi_batch(
#'   MoreArgs_spect = list(pivot = "[0-9]_[A-Z]+[0-9]+",
#'                         final_trim_range = c(2420, 2445)),
#'   MoreArgs_peaks = list(pivot = "[0-9]_[A-Z]+[0-9]+",
#'                         mass_list = c(mC = 2425, hmC = 2441, fC = 2439),
#'                         tolerance_assignment = 0.5,
#'                         SNR = 3),
#'   MoreArgs_draw = list(ncol = 2, nrow = 6),
#'   MoreArgs_layout = list(meta_row  = c(concentration = "1"),
#'                          meta_col  = character())
#'   )
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
maldi_batch <- function(path = NULL,
                        pivot = "[0-9]_[A-Z]+[0-9]+",
                        layout_file = NULL,
                        confirm_layout_file = TRUE,
                        manual = FALSE,
                        review = TRUE,
                        FUN_spect = maldi_average_by_well,
                        MoreArgs_spect = list(pivot = "[0-9]_[A-Z]+[0-9]+"),
                        FUN_peaks = maldi_find_peaks_by_well,
                        MoreArgs_peaks = list(pivot = "[0-9]_[A-Z]+[0-9]+"),
                        FUN_draw  = maldi_draw_peaks_by_well,
                        MoreArgs_draw  = list(),
                        FUN_import_layout = summerr::import_layout_from_excel,
                        MoreArgs_layout = list(),
                        MoreArgs_device = list(width = 21.5 / 2.54,
                                               height = 30.5 / 2.54,
                                               paper = "a4"),
                        stored_peaks.generic = "maldi_peaks.rds",
                        stored_spect.generic = "maldi_specs.rds",
                        stamp = "%y%m%d"
                        ) {

  ##### Determine directory and handling pre-existing files ####################

  old_wd <- getwd()
  on.exit(setwd(old_wd))

  if (is.null(path)) path <- summerr::select_directory()

  # make sure to re-enter the current working directory if anything goes wrong

  path <- summerr::normalizePath(path)

  setwd(path)

  # check if arguments are properly set

  if (isTRUE(MoreArgs_spect$pivot != pivot)) summerr::log_warn(
    "the `pivot` in `MoreArgs_spect` is different from the global `pivot`")

  if (isTRUE(MoreArgs_peaks$pivot != pivot)) summerr::log_warn(
    "the `pivot` in `MoreArgs_spect` is different from the global `pivot`")

  # establish groups

  list_rawdata <- function(pattern, path) {

    import_layout_from_paths.maldi(paths = list.files(
      path = path, pattern = pattern, ignore.case = TRUE,
      recursive = TRUE, full.names = TRUE),
      pivot = pivot, relative_to = path) %>%
      dplyr::ungroup() %>%
      dplyr::pull("path_to_group")

  }

  list_expdata <- function(pattern, path) {

    list.files(path = path, pattern = paste0("^", pattern, "$"), ignore.case = TRUE,
               recursive = TRUE, full.names = TRUE) %>%
      dirname()

  }

  summerr::log_task("assessing directory", sQuote(path))

  # applying cbind and rbind is strictly necessary to suppress unwanted recycling

  data_in_path <- tibble::as_tibble(cbind(
    rbind(sapply(c(raw_xml_files = "\\.mzxml",
                   raw_fid_files = "fid"), list_rawdata,
                 path = path, USE.NAMES = TRUE, simplify = FALSE)),
    rbind(sapply(c(processed_spectra_rds = stored_spect.generic,
                   processed_peaks_rds = stored_peaks.generic), list_expdata,
                 path = path, USE.NAMES = TRUE, simplify = FALSE))
  )) %>%
    tidyr::pivot_longer(dplyr::everything(), values_to = "path_to_group") %>%
    tidyr::unnest("path_to_group", keep_empty = TRUE) %>%
    # make sure to get trimmed dirnames
    dplyr::mutate(path_to_group = stringr::str_remove(.data$path_to_group, stringr::fixed(path))) %>%
    dplyr::group_by(.data$name, .data$path_to_group) %>%
    dplyr::summarize(status = dplyr::n()) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "status") %>%
    dplyr::select(tidyselect::any_of(c("path_to_group", "raw_fid_files", "raw_xml_files",
                                       "processed_spectra_rds", "processed_peaks_rds"))) %>%
    dplyr::filter(!is.na(.data$path_to_group))

  summerr::log_process("found", nrow(data_in_path), ifelse(nrow(data_in_path) > 1,
                                                  "groups", "group"))

  summerr::log_object(data_in_path)

  # browser()

  # look for plate layout file in main path

  global_layout <- NULL

  if (!is.null(layout_file)) {

    # file.exists will expand to the current working directory if no full path
    # is given.

    if (file.exists(layout_file)) {

      if (confirm_layout_file) {

        use_layout_file <- utils::askYesNo(paste("Do you want to apply", sQuote(layout_file),
                                                 "as layout to all groups in the directory?"))

        if (use_layout_file) global_layout <- summerr::normalizePath(layout_file)

      } else {

        # just take it

        global_layout <- summerr::normalizePath(layout_file)

      }

    }

  }

  all_s <- list()
  all_p <- list()

  for (curr_group_idx in seq_along(data_in_path$path_to_group)) {

    summerr::log_line("-")
    summerr::log_task("advancing to sample group", data_in_path$path_to_group[[curr_group_idx]])

    # as stringr::str_remove was used to create the shorthand, we don't need to
    # call file.path(...) here

    curr_group <- data_in_path$path_to_group[[curr_group_idx]]
    curr_group_path <- paste0(path, curr_group)

    curr_layout <- global_layout

    # check if spectra and/or peak backup file exists; ask whether to import or
    # not

    has_s <- !is.na(data_in_path[curr_group_idx, "processed_spectra_rds"])
    has_p <- !is.na(data_in_path[curr_group_idx, "processed_peaks_rds"])

    # in non-interactive mode, use the backups by default

    use_s <- !interactive() & has_s
    use_p <- !interactive() & has_p

    if (interactive() && has_s) {

      Sys.sleep(1)

      if (has_p) {

        use_p <- rstudioapi::showQuestion(
          paste0("Import All Backup Files for ", sQuote(curr_group), "?"),
          paste("Processed mass spectra and peak assignment has been backed up for this sample group. Do you want to proceed with both backups and skip importing of the raw spectra and peak detection?
            "), ok = "Yes", cancel = "No")

        if (!use_p) {

          use_s <- rstudioapi::showQuestion(
            paste("Import Mass Spectra for", sQuote(curr_group), "from Backup?"),
            paste("Do you want to proceed with the backup of the processed mass spectra and skip importing the raw spectra?
              "), ok = "Yes", cancel = "No")

        } else {

          use_s <- TRUE  # if peaks are imported, always use the spectra

        }

      } else {

        use_s <- rstudioapi::showQuestion(
          paste("Import Mass Spectra for", sQuote(curr_group), "from Backup?"),
          paste("Do you want to proceed with the backup of the processed mass spectra and skip importing the raw spectra?
              "), ok = "Yes", cancel = "No")

      }

    }

    ##### Import or (Re-)Process Files #########################################

    # processing spectra

    pth_s <- summerr::normalizePath(file.path(curr_group_path, stored_spect.generic))

    if (use_s) {

      dat_s <- readRDS(file = pth_s); attr(dat_s, "dir") <- curr_group_path

      summerr::log_process("imported", length(dat_s), "spectra from backup")

    } else {

      # keep a backup of existing spectra or peak files
      if (has_s) summerr::backup_file(file.path(curr_group_path, stored_spect.generic))

      summerr::log_process("importing mass spectra")

      err_s <- tryCatch({

        dat_s <- maldi_import_spectra(path = curr_group_path)

        dat_s <- eval(rlang::call2(FUN_spect, object = dat_s, !!!MoreArgs_spect))

        attr(dat_s, "dir") <- curr_group_path

        saveRDS(object = dat_s, file = pth_s)

        use_p <- FALSE

        FALSE

      }, error = function(e) {

        summerr::log_message("Something went wrong in this group. Let's move on.")

        TRUE

      })

      if (err_s) {next}

      summerr::log_process("saving")

      attr(dat_s, "dir") <- curr_group_path
      saveRDS(dat_s, file = pth_s)

      summerr::log_done()

    }

    # processing peaks

    pth_p <- summerr::normalizePath(file.path(curr_group_path, stored_peaks.generic))

    if (use_p) {

      dat_p <- readRDS(file = pth_p); attr(dat_p, "dir") <- curr_group_path

      summerr::log_process("imported", scales::comma(sum(lengths(dat_p))),
                  "peak assignments from backup")

    } else {

      # keep a backup of existing spectra or peak files
      if (has_p) summerr::backup_file(file.path(curr_group_path, stored_peaks.generic))

      # PEAK DETECTION 1: detect peaks automatically

      summerr::log_process("detecting peaks by well")

      dat_p <- eval(rlang::call2(FUN_peaks, object = dat_s, !!!MoreArgs_peaks))

      # draw overview spectra

      tmp_pdf <- paste0(stored_peaks.generic, "_tmp.pdf")

      eval(rlang::call2(grDevices::pdf, file = tmp_pdf, !!!MoreArgs_device))

      eval(rlang::call2(FUN_draw, object = dat_s, data_peaks = dat_p,
                        !!!MoreArgs_draw))

      grDevices::dev.off()

      # PEAK DETECTION 2: detect peaks manually

      if (rstudioapi::isAvailable() && review) {

        rstudioapi::viewer(tmp_pdf)

        curr_check <- readline(paste("For which wells of", sQuote(curr_group),
                                     "do you want manually assign peaks? "))

        curr_check <- curr_check %>%
          strsplit(split = "[^[:alnum:]]+") %>% unlist()

        curr_check <- dat_p %>%
          dplyr::filter(.data$well %in% curr_check) %>%
          dplyr::pull(.data$findex) %>%
          unique()

        if (length(curr_check) > 0) {

          dat_p.use <- dat_p %>%
            dplyr::filter(!(.data$findex %in% curr_check))

          dat_s.new <- dat_s[curr_check]
          attr(dat_s.new, "dir") <- curr_group_path

          dat_p.new <- eval(rlang::call2(
            FUN_peaks, object = dat_s.new,
            manual = TRUE,
            !!!MoreArgs_peaks[which(names(MoreArgs_peaks) != "manual")]
          ))

          # update findex

          dat_p.new$findex <- curr_check[dat_p.new$findex]

          dat_p <- dplyr::bind_rows(dat_p.use, dat_p.new) %>%
            dplyr::arrange(.data$findex)

        }

      }

      file.remove(tmp_pdf)

      summerr::log_process("saving")

      attr(dat_p, "dir") <- curr_group_path
      saveRDS(dat_p, file = pth_p)

      summerr::log_done()

    }

    ### dat_s and dat_p are now complete

    summerr::log_task("combining with plate metadata")

    # look for plate layout file in main path

    if (!is.null(curr_layout)) {

      summerr::log_process("looking for plate metadata")

      if (!is.null(layout_file)) {

        # file.exists will expand to the current working directory if no full path
        # is given.

        if (file.exists(layout_file)) {

          if (confirm_layout_file) {

            use_layout_file <- utils::askYesNo(paste("Do you want to apply", sQuote(layout_file),
                                                     "as layout to all groups in the directory?"))

            if (use_layout_file) curr_layout <- summerr::normalizePath(layout_file)

          } else {

            # just take it

            curr_layout <- summerr::normalizePath(layout_file)

          }

        }

      }

    }

    # if none of the above worked out, prompt to select a file; cancelling the
    # dialogue will keep length(curr_layout) == 0.

    if (is.null(curr_layout)) curr_layout <- summerr::select_single_file(path = curr_group_path)

    if (length(curr_layout) == 0) {

      summerr::log_process("no plate layout selected")

    } else {

      dat_l <- eval(rlang::call2(FUN_import_layout, file = summerr::normalizePath(curr_layout),
                                 !!!MoreArgs_layout))

      dat_p <- dplyr::left_join(dat_p, dat_l)

      attr(dat_p, "dir") <- curr_group_path

    }

    summerr::log_process("exporting files")

    summerr::backup_file(sub(x = pth_p, ".rds", "-long.csv"))
    summerr::backup_file(sub(x = pth_p, ".rds", "-wide.csv"))

    dat_p %>%
      tidyr::unnest(.data$path_to_files) %>%
      dplyr::select(!c(tidyselect::any_of(c("findex", "gindex", "pivot", "file",
                                            "well_let", "well_num")),
                       tidyselect::starts_with("sub_"))) %>%
      dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.double),
                                  round, digits = 4)) %>%
      readr::write_csv(file = sub(x = pth_p, ".rds", "-long.csv"))

    dat_p %>%
      dplyr::select(!c(tidyselect::any_of(c("findex", "gindex", "pivot", "file",
                                            "well_let", "well_num",
                                            # exclude in addition
                                            "path_to_files")),
                       tidyselect::starts_with("sub_"))) %>%
      tidyr::pivot_wider(names_from = "ion", values_from = c("intensity", "percent",
                                                             "mass")) %>%
      dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.double),
                                  round, digits = 4)) %>%
      readr::write_csv(file = sub(x = pth_p, ".rds", "-wide.csv"))

    # export to object

    all_s[[curr_group_idx]] <- dat_s
    all_p[[curr_group_idx]] <- dat_p

  } ### end of for each group loop

  names(all_s) <- data_in_path$path_to_group
  names(all_p) <- data_in_path$path_to_group

  summerr::log_line("=")

  list(spectra = all_s, peaks = all_p)

}
