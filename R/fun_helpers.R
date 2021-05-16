#  HELPERS ---------------------------------------------------------------------

utils::globalVariables(".")

#' Interpret characters as well names
#'
#' @param well Character vector.
#' @param as.tibble If \code{TRUE}, a \code{\link[dplyr:tibble]{tibble}} is
#' returned instead of a list.
#' @param to.upper Should well letters be converted to uppercase if they are not?
#' @param zero.padding Number of digits the well number should be padded to.
#'
#' @return A list with \code{well_let}, \code{well_num} and \code{well} or a
#' tibble with the corresponding column names.
#'
#' @examples
#' as_well(c("a1", "B2", "Q23"))
#' as_well("23a")
#' as_well("n_2")
#'
#' @export
as_well <- function(well, as.tibble = FALSE, to.upper = TRUE, zero.padding = 2) {

  well_raw <- stringr::str_extract(well, "[:alnum:]+")

  well_num <- stringr::str_extract(well_raw, "[0-9]+")
  well_let <- stringr::str_extract(well_raw, stringr::regex("[A-Z]+", ignore_case = TRUE))

  defects <- mapply(function(x, y) any(is.na(x), is.na(y)), well_let, well_num)

  if (to.upper == TRUE) well_let <- toupper(well_let)

  well_num <- sprintf(paste0("%0", zero.padding, "d"), as.numeric(well_num))
  well_tot <- paste0(well_let, well_num)

  well_let[defects] <- NA_character_
  well_num[defects] <- NA_character_
  well_tot[defects] <- NA_character_

  res <- list(well_let = well_let, well_num = well_num, well = well_tot)

  if (as.tibble == TRUE) {

    res <- dplyr::as_tibble(res)

  }

  res

}

#  DIRECTORY NAVIGATION --------------------------------------------------------

#' Express file paths in their canonical form -- Truly
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
normalizePath <- function(path) {

  log_debugging("received", sQuote(path))

  norm_path <- stringr::str_replace(
    base::normalizePath(path, winslash = .Platform$file.sep, mustWork = FALSE),
    # remove trailing path separator which is not accepted on Windows
    pattern = paste0(.Platform$file.sep, "$"), replacement = ""
  )

  log_debugging("returned", sQuote(norm_path))

  norm_path

}

#' Prompt to select from a list of options
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

  if (length(items) == 1) return(items)

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

  item

}

#' Interactively choose a directory
#'
#' This is a wrapper to select a directory \code{path} to process. Depending on
#' the mode R is invoked from, choosing is interactive and may use the RStudio IDE.
#'
#' @param path A character vector of length 1 to start browsing; defaults to the
#' current working directory.
#' @inheritParams rstudioapi::selectDirectory
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

  path

}

#' Interactively choose a single file
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
#' @param suffix A character to match the file extension, e.g. "csv".
#' @param filetype A human-readable file type designation. Along with \code{suffix}
#' will be used to construct \code{filter} in \code{\link[rstudioapi:selectFile]{rstudioapi::selectFile}}.
#' @inheritParams rstudioapi::selectDirectory
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

  dir_files

}

#  GENERAL FILE IMPORT AND EXPORT ----------------------------------------------

#' Import a (plate) layout from an Excel file
#'
#' A (square plate) layout assigning each cell (well) to a specific content with
#' metadata to be taken from columns and/or rows parallel to the plate design.
#'
#' @param file A path or URL to an xlsx file or workbook.
#' @inheritParams openxlsx::read.xlsx
#' @param data_upper_left A string specifying the top left corner (i.e. cell)
#' in Excel coordinates of the plate.
#' @param index_row A string specifying the row (in Excel coordinates) where the
#' plate column index (e.g. running 1 through 24) is found.
#' @param index_col A string specifying the column (in Excel coordinates) where
#' the plate rows index (e.g. running A through P) is found.
#' @param meta_row A named character vector to specify additional metadata to be
#' assigned to each plate column.
#' @param meta_col A named character vector to specify additional metadata to be
#' assigned to each plate row.
#' @param plate_nrow Number of rows to import starting from \code{data_upper_left}.
#' @param plate_ncol Number of columns to import starting from \code{data_upper_left}.
#'
#' @details
#' The content of each Excel cell of the plate is read as a string. Merged cells
#' are allowed and will be unmerged (see \code{\link[openxlsx:read.xlsx]{read.xlsx}}).
#'
#' When importing metadata, it is assumed that \code{meta_row} and \code{meta_col}
#' run parallel to the plate. Offset is allowed. The category names of the metadata
#' must be specified explicitly during the setup; they are not imported.
#'
#' In the example below, the first cell of the acutal plate is "B3". Three metadata
#' columns are given: One in row "1" (here: Concentration), which is applied column-wise,
#' and two parallel to the rows in column "Z" (here: Duplicate) and "AA" (here: Source).
#' The name tags highlighted in yellow are not imported since they are not parallel to
#' the actual plate rows. So is the text "xyz" shown in red.
#'
#' \figure{platelayout_maldi.png}{options: width=600 alt="A sample plate layout provided in \code{summerrmass}."}
#'
#' @return
#' A \code{\link[tibble:tibble]{tibble}} in long form with the columns \code{well},
#' \code{well_let} (letters, row index of the plate), \code{well_num} (integers
#' as strings, column index of the plate), \code{content} and a column for each
#' metadata specified.
#'#'
#' @examples
#' layout_file <- system.file("extdata", "platelayout_maldi.xlsx", package = "summerrmass")
#'
#' import_layout_from_excel(layout_file,
#'   # metadata stored in row number "1" (Excel coordinates) contains concentration
#'   meta_row = c(Concentration = 1),
#'   # metadata stored in column "Z" (Excel coordinates) contains rowwise metadata;
#'   # missing values are "NA", the order in meta_col determines the column arrangement
#'   meta_col = c(Source = "AA", Duplicates = "Z"))
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
import_layout_from_excel <- function(
  file,
  sheet = 1,
  data_upper_left = "B3",
  index_row = "2",
  index_col = "A",
  meta_row  = c(concentration = "1"),
  meta_col  = character(),
  plate_nrow = 16,
  plate_ncol = 24
) {

  # convert excel coordinates to R coordinates

  from.excel <- function(s) {

    # from: https://stackoverflow.com/questions/34537243

    s_upper <- toupper(s)
    # Convert string to a vector of single letters
    s_split <- unlist(strsplit(s_upper, split=""))
    # Convert each letter to the corresponding number
    s_number <- sapply(s_split, function(x) {which(LETTERS == x)})
    # Derive the numeric value associated with each letter
    numbers <- 26^((length(s_number)-1):0)
    # Calculate the column number
    column_number <- sum(s_number * numbers)
    column_number

  }

  # only the first match to a continuous string of numerals or letters is matched
  # and interpreted as row/column indexes
  from.excel.row <- function(x) as.numeric(stringr::str_extract(x, "[0-9]+"))
  from.excel.col <- function(x) from.excel(stringr::str_extract(
    x, stringr::regex("[A-Z]+", ignore_case = TRUE)))

  index_row <- from.excel.row(index_row)
  index_col <- from.excel.col(index_col)

  data_row <- from.excel.row(data_upper_left)
  data_col <- from.excel.col(data_upper_left)

  meta_row <- lapply(meta_row, from.excel.row)
  meta_col <- lapply(meta_col, from.excel.col)

  plate <- openxlsx::read.xlsx(file, fillMergedCells = TRUE, colNames = FALSE)
  # fillMergedCells does not work when rows and cols specification is given; thus
  # need to separate from the following code
  datap <- plate[
    data_row:(data_row + plate_nrow - 1),
    data_col:(data_col + plate_ncol - 1)] %>%
    # set row and column names according to the values specified by the user within
    # the excel sheet; this allows for more flexibility when adjusting plate layout
    magrittr::set_rownames(plate[data_row:(data_row + plate_nrow - 1), index_col]) %>%
    magrittr::set_colnames(sprintf("%02d", as.numeric(
      plate[index_row, data_col:(data_col + plate_ncol - 1)]))) %>%
    #
    tibble::as_tibble(rownames = "well_let") %>%
    # make sure all entries (including NA) are of type character
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                as.character)) %>%
    # make long form
    tidyr::pivot_longer(cols = -.data$well_let, names_to = "well_num",
                        values_to = "content") %>%
    tidyr::unite(.data$well_let, .data$well_num, col = "well", sep = "",
                 remove = FALSE)

  # for each metadata row or column we assume that it is parallel to the plate

  get_meta_row <- function(i) tibble::tibble(
    # well numbers from the index row
    well_num = sprintf("%02d", as.numeric(
      plate[index_row, data_col:(data_col + plate_ncol - 1)])),
    # data from the metadata row
    meta = as.character(plate[i, data_col:(data_col + plate_ncol - 1)])
  )

  get_meta_col <- function(i) tibble::tibble(
    # well letters from the index col
    well_let = plate[data_row:(data_row + plate_nrow - 1), index_col],
    # data from the metadata col
    meta = as.character(plate[data_row:(data_row + plate_nrow - 1), i])
  )

  if (length(meta_row) > 0) datap <- dplyr::left_join(datap, dplyr::bind_rows(lapply(
    as.list(meta_row), get_meta_row), .id = "mtype") %>%
      tidyr::pivot_wider(id_cols = "well_num", names_from = .data$mtype, values_from = .data$meta),
    by = "well_num")

  if (length(meta_col) > 0) datap <- dplyr::left_join(datap, dplyr::bind_rows(lapply(
    as.list(meta_col), get_meta_col), .id = "mtype") %>%
      tidyr::pivot_wider(id_cols = "well_let", names_from = .data$mtype, values_from = .data$meta),
    by = "well_let")

  attr(datap, "file") <- file

  datap

}

#' Import an experiment layout from nested directories
#'
#' Some data is organized in folders that contain files with similar or even
#' identical names. Given a list of paths pointing to those files, the layout
#' of the experiment is established from the nesting of folders.
#'
#' @param paths A list character vector or list of file paths. See Details.
#' @param pivot A \link[base:regex]{regular expression} describing the
#' name of a single folder in each tree up to which "groups" and from which
#' "replicates" are established. See Details.
#' @param relative_to If not \code{NULL}, this subpath is hidden from the paths
#' for all operations; "" to suppress hiding; \code{NULL} will hide the subpath
#' that is common in all elements of \code{paths}.
#'
#' @details
#' \code{paths} can be a vector such as \code{c("path1", "path2", "path3")}
#' or a list of character vectors, e.g. as a result of averaging data, such as
#' \code{list(c("path1", "path2"), "path3")}. The latter case is usually not
#' exposed to the user; the algorithm applies to "path1" and "path3".
#'
#' \code{pivot} must be a unique match for each path. Nesting above and within
#' the matched folder can differ from path to path; care is taken to handle the
#' grouping accordingly.
#'
#' If \code{pivot} is a regular expression with lookahead or lookbehind, these
#' elements are kept in the path.
#'
#' If \code{pivot} has multiple matches in the path, it is advisable to call this
#' function with \code{relative_to = (common path)} since it will be removed from
#' the paths before a match is sought for. Alternatively, \code{relative_to = NULL}
#' will automatically consume the longest shared path between all \code{paths}.
#'
#' \code{relative_to} is expanded (like all \code{paths}) before the regular
#' expression is looked for.
#'
#' \code{relative_to} is preserved as \code{attr(., "dir")} for future use.
#'
#' \subsection{Sample groups by nesting parent folders}{
#'
#' Sample groups are determined from the enclosing (parent) folders. For example,
#' in
#'
#' \preformatted{
#' /common1/folder1/folderA/0_A1/...
#'                       ../0_A2/...
#'               ../folderB/0_A1/...
#' /common1/folder2/folderA/0_A1/...
#'               ../folderB/0_A1/...
#'                         /0_A2/...
#' }
#'
#' two nested groupings are identified: (1) "folder1" and "folder2" as \code{grp_0},
#' and (2) "folderA" and "folderB" as \code{grp_1}. The common path "common1"
#' will not be used as a grouping variable. The first parent is always used as a
#' grouping variable. The full (unique) grouping is returned under \code{group}.
#'
#' Grouping can allow to anaylze multiple directories simultaneously with their
#' own set of parameters enclosed at an appropriate level of nesting.
#'
#' } % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
#'
#' \subsection{Replicates by nesting subfolders}{
#'
#' Sample replicates are determined from the enclosed (child) folders. For example,
#' in
#'
#' \preformatted{
#' ../0_A1/1/targetfile
#'        /2/targetfile
#' ../0_A2/1/targetfile
#' }
#'
#' the first pivot ("0_A1") contains two replicates, the second pivot ("0_A2") one.
#'
#' } % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
#'
#' @return
#' A tibble with the experiment layout as determined from the \code{paths}
#' with columns
#' \code{grp_0}, ..., \code{grp_N} specifiying the sample groups,
#' \code{sub_1}, ..., \code{sub_N} specifiying the subfolder nesting,
#' \code{pivot},
#' \code{replicate} and \code{n_replicates}, based on \code{sub_1},
#' the \code{path} of the file relative to \code{relative_to} and its original
#' position in \code{paths} as \code{findex}.
#'
#' @examples
#' import_layout_from_paths(c("folderA/0_A1/1/file.x", "folderA/0_A1/2/file.x",
#'   "folderA/0_A2/1/file.x", "folderB/0_A1/1/file.x"))
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
import_layout_from_paths <- function(paths, pivot = "[0-9]_[A-Z]+[0-9]+",
                                     relative_to = getwd()) {

  # this transforms both cases into a plain character vector, for which only the
  # first element in a nested list will be kept

  datad <- sapply(paths, "[[", 1) %>% normalizePath() %>%
    tibble::as_tibble()

  # to avoid spurious matching of the regex to a common base directory, remove
  # the relative portion first

  if (is.null(relative_to)) {

    relative_to <- ""

    # determine common basis and replace with "."

    folderlist <- stringr::str_split(datad$value, pattern = .Platform$file.sep,
                                     simplify = TRUE)

    foldersame <- apply(folderlist, MARGIN = 2, FUN = dplyr::n_distinct)

    # browser()

    if (any(foldersame == 1)) {

      # Note: Beware that which(foldersame == 1) may be true for elements after
      # the pivot has been identified. This is why we do it more "complicated".
      #
      # Note: This will omit the last common element, which is intended in such
      # cases in which also the pivot itself is identical for all elements (and
      # only differences in replicates exist); such that it is not stripped off.

      foldersame <- folderlist[1, which(cumsum(abs(diff(foldersame))) == 0)]

      if (length(foldersame) > 0) relative_to <- paste0(
        foldersame, collapse = .Platform$file.sep)

    }

  }

  if (relative_to != "") {

    # browser()

    # process the relative path in much the same way as V1 was processed

    relative_to <- normalizePath(relative_to)

    datad$value <- sapply(datad$value, sub, pattern = relative_to,
                          replacement = ".", fixed = TRUE)

  }

  log_debugging("assessed paths relative to", sQuote(relative_to))

  datad <- datad %>%
    dplyr::mutate(pivot = stringr::str_extract(.data$value, pivot)) %>%
    tidyr::separate(.data$value, into = c("V1", "V2"), sep = pivot, fill = "left") %>%
    # remove trailing path separator which is not accepted under Windows
    dplyr::mutate(V1 = stringr::str_remove(
      string = .data$V1, pattern = paste0(.Platform$file.sep, "$")))

  # this is critical

  stopifnot(ncol(datad) == 3)

  # browser()

  datad <- datad %>%
    dplyr::mutate(
      nest_level = stringr::str_count(.data$V1, pattern = .Platform$file.sep),
      subs_level = stringr::str_count(.data$V2, pattern = .Platform$file.sep)
    ) %>%
    # unnest the pivot groups
    tidyr::separate(.data$V1, into = paste0("grp_", 0:max(c(0, .$nest_level),
                                                          na.rm = TRUE)),
                    sep = .Platform$file.sep, fill = "right", extra = "drop",
                    remove = FALSE) %>%
    # unnest the pivot replicates
    tidyr::separate(.data$V2, into = c(NA, "sub_1", "V3"),
                    sep = .Platform$file.sep, fill = "left", extra = "merge") %>%
    tidyr::separate(.data$V3, into = paste0("sub_", 2:max(c(2, .$subs_level),
                                                          na.rm = TRUE)),
                    sep = .Platform$file.sep, fill = "left", extra = "drop") %>%
    # tidy
    dplyr::mutate_all(list(~ dplyr::na_if(., ""))) %>%
    dplyr::rename(group = "V1") %>%
    dplyr::rename(file  = paste0("sub_", max(.$subs_level))) %>%
    # remove trailing path separators
    dplyr::mutate(group = stringr::str_replace(.data$group, paste0(.Platform$file.sep,
                                                                   "$"), "")) %>%
    # add group-relative file path
    dplyr::bind_cols(path = sapply(paths, function(x) x[[1]])) %>%
    # preserve the file order index
    dplyr::mutate(findex = dplyr::row_number()) %>%
    # preserve the group index
    dplyr::group_by(dplyr::across(tidyselect::starts_with("grp_"))) %>%
    dplyr::mutate(gindex = dplyr::cur_group_id()) %>%
    # sort by well in alphabetical order (in case needed); replicates are at
    # level sub_1; if no subfolder exists, sub_1 will equal "file", which is
    # acceptable
    dplyr::arrange(.data$pivot, .data$sub_1, .by_group = TRUE) %>%
    # group replicates per well
    dplyr::group_by(.data$pivot, .add = TRUE) %>%
    # add replicate information
    dplyr::mutate(replicate = .data$sub_1,
                  n_replicates = dplyr::n_distinct(.data$replicate)) %>%
    dplyr::group_by(.data$replicate, .add = TRUE)

  log_debugging(object = datad)

  if (any(lengths(paths) > 1)) {

    datad$n_replicates <- lengths(paths)
    datad$replicate[which(lengths(paths) > 1)] <- NA
    # not sure if this may cause complications later
    datad$path <- paths

  }

  # if grp_0 is not the only group, drop it

  if (max(datad$nest_level, na.rm = TRUE) > 1) {

    datad <- datad %>% dplyr::ungroup(.data$grp_0) %>% dplyr::select(!.data$grp_0)

  }

  datad <- datad %>%
    dplyr::select(!tidyselect::any_of(c("nest_level", "subs_level"))) %>%
    dplyr::select(dplyr::group_vars(datad),
                  tidyselect::any_of("n_replicates"),
                  .data$findex, .data$gindex, tidyselect::everything())

  attr(datad, "dir") <- relative_to

  datad

}

#' Display a layout with ggplot2
#'
#' The function returns the basic layer to be modified with further geoms by
#' the user, depending on the purpose.
#'
#' @param layout A plate layout.
#' @param ... Arguments passed to the \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile()}}
#' aesthetic.
#'
#' @examples
#' library(ggplot2)
#'
#' layout_file <- system.file("extdata", "platelayout_maldi.xlsx", package = "summerrmass")
#'
#' plate_384 <- import_layout_from_excel(layout_file, meta_row = c(Concentration = 1))
#' display_plate_layout(plate_384)  # all defined wells
#' display_plate_layout(plate_384, fill = content)
#' display_plate_layout(plate_384, fill = as.numeric(Concentration))
#' display_plate_layout(plate_384, fill = content, alpha = as.numeric(Concentration)) +
#'   labs(alpha = "concentration")
#'
#' @importFrom rlang .data
#'
#' @export
display_plate_layout <- function(layout, ...) {

  ggplot2::ggplot(layout, ggplot2::aes(x = .data$well_num, y = forcats::fct_rev(.data$well_let))) +
    ggplot2::geom_tile(ggplot2::aes(...)) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() + ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.box = "vertical",
      legend.box.just = "left",
      legend.position = "bottom"
    )

}
