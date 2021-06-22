#' MALDI Peak Analysis                                              TEMPLATE A01
#'
#' analysis by:
#'          on: <<TODAY>>
#'
#' template by: Benjamin Buchmuller
#'          on: 210622
#'
#' <<RVERSION>>
#'
#' -----------------------------------------------------------------------------
#' BASIC USAGE
#' Change the parameters below to reflect your needs. Then save and "source" the
#' document.
#' -----------------------------------------------------------------------------

# parameters analysis:

trim_spectra_lower_bound = 2420
trim_spectra_upper_bound = 2445

detect_these_ions <- c(mC = 2425, hmC = 2441, fC = 2439)  # m/z values
signal_to_noise_ratio <- 3

negative_control <- "DMSO"
positive_control <- "DFOA"  # to omit empty plots, put the same as in `negative_control`

normalize_before_fitting <- TRUE
default_levels_negative <- c(mC = 0, hmC = 100, fC = 100, `hmC + fC` = 100)  # percentages

concentration_unit <- "\U00B5M"  # micro sign is "\U00B5"

# parameters plotting:

plot_ncol <- 2
plot_nrow <- 6
plot_each_compound <- TRUE  # this make take a while if TRUE

# advanced settings:

layout_file = NULL  # prompt for layout file if NULL; see ?summerrmass::maldi_batch

#' -----------------------------------------------------------------------------

library(summerrmass)
library(tidyverse, quietly = TRUE)

log_line("=")

data_ic50 <- list()

data_maldi <- maldi_batch(
  path  = NULL,  # will prompt for the direcotry to analyze
  layout_file = layout_file,
  pivot = "[0-9]_[A-Z]+[0-9]+",  # regex to match the "well" folders
  # pre-processing of spectra
  FUN_spect = maldi_average_by_well,
  MoreArgs_spect = list(pivot = "[0-9]_[A-Z]+[0-9]+",
                        final_trim_range = c(trim_spectra_lower_bound, trim_spectra_upper_bound),
                        method_baseline = "SNIP",
                        method_average  = "mean"),
  # detecting peaks
  FUN_peaks = maldi_find_peaks_by_well,
  MoreArgs_peaks = list(pivot = "[0-9]_[A-Z]+[0-9]+",
                        mass_list = detect_these_ions,
                        tolerance_assignment = 0.5,
                        SNR = signal_to_noise_ratio,
                        method = "MAD"),
  # drawing spectra
  MoreArgs_draw = list(ncol = plot_ncol, nrow = plot_nrow),
  # associating metadata
  FUN_import_layout = import_layout_from_excel,
  MoreArgs_layout = list(sheet = 1,
                         index_row = "2",
                         index_col = "A",
                         data_upper_left = "B3",
                         plate_nrow = 16,
                         plate_ncol = 24,
                         meta_row = list(
                           # metadata stored in rows
                           concentration = "1"),
                         meta_col = list(
                           # metadata stored in columns
                         ))
)

for (group in seq_along(data_maldi$peaks)) {

  group_name <- names(data_maldi$peaks)[[group]]

  curr_p <- data_maldi$peaks[[group]]
  curr_s <- data_maldi$spectra[[group]]

  # Make sure a plate layout has been assigned that contains the metadata column
  # "concentration". If not, the following should be modified.

  if (all(c("content", "concentration") %in% colnames(curr_p))) {

    # Brush-up output using content/concentration information

    log_task("brushing up spectra in sample group", sQuote(group_name))

    pdf(file = file.path(attr(curr_s, "dir"), "overview_by_compound.pdf"),
        width = 21.5 / 2.54, height = 30.5 / 2.54, paper = "a4")

    op <- graphics::par(no.readonly = TRUE)

    graphics::par(mfcol = c(plot_nrow, plot_ncol), mar = c(0, 3, 0, 1), oma = c(3, 2, 3, 2))

    for (content in unique(curr_p$content)) {

      dat_p <- curr_p %>%
        dplyr::filter(content == content) %>%
        dplyr::group_by(findex, well, content, concentration) %>%
        dplyr::summarize(mass = list(mass), intensity = list(intensity),
                         n_replicates = max(n_replicates), .groups = "keep") %>%
        # sort by increasing concentration
        dplyr::mutate(concentration = as.numeric(concentration)) %>%
        dplyr::arrange(dplyr::desc(concentration))

      for (i in arrange_on_page(dat_p$findex, byrow = TRUE)) {

        if (is.na(i)) {

          plot.new()
          next

        }

        fi <- dat_p$findex[[i]]

        maldi_draw_spectrum(curr_s[[fi]], x = dat_p$mass[[i]], y = dat_p$intensity[[i]],
                            xaxt = c("n", "s")[(i %in% get_border_indices(
                              dat_p$findex, border = "b", byrow = TRUE) + 1)],
                            SNR = signal_to_noise_ratio)

        graphics::title(paste0(dat_p$well[[i]], c("", "*")[(dat_p$n_replicates[[i]] > 1) + 1]),
                        line = -1.5)

        if (i == 1) graphics::mtext(content, line = 1, side = 3,
                                    outer = TRUE, adj = 0)

      }

      summerr:::rm_silently(i, fi, dat_p)

    }

    graphics::par(op)
    dev.off()

    summerr:::rm_silently(content)

    # IC50 fitting

    log_task("fitting IC50 for sample group", sQuote(group_name))

    # --- BGN CUSTOM CODE ---
    #
    curr_p <- curr_p %>%
      dplyr::group_by(group, content, ion) %>%
      # transform mC to "product formation"
      dplyr::mutate(
        ion = forcats::fct_relabel(ion, ~ sub("^mC$", "hmC + fC", .x)),
        percent = case_when(ion == "hmC + fC" ~ 100 - percent,
                            TRUE ~ percent))
    #
    # --- END CUSTOM CODE ---

    # normalization to levels observed in negative_control

    if (normalize_before_fitting) {

      log_process("average peak intensities in", sQuote(negative_control))

      if (negative_control %in% curr_p$content) {

        negative_control.levels <- curr_p %>%
          dplyr::filter(content %in% negative_control) %>%
          dplyr::group_by(ion) %>%
          dplyr::summarize(levels_control_negative = mean(percent, na.rm = TRUE),
                           .groups = "drop")

      } else {

        log_process("not present; using defaults")

        negative_control.levels <- curr_p %>%
          dplyr::group_by(ion) %>%
          dplyr::summarise() %>%
          dplyr::left_join(tibble::as_tibble(default_levels_negative, rownames = "ion"),
                           by = "ion") %>%
          dplyr::rename(levels_control_negative = "value")

      }

      log_object(negative_control.levels)

    }

    if (normalize_before_fitting) {

      curr_p <- curr_p %>%
        dplyr::left_join(negative_control.levels, by = "ion") %>%
        # express percentages relative to the negative before fitting; may cause
        # some values to exceed 100% and eventually fail the upper bounds for
        # fitting top plateau
        dplyr::mutate(percent = percent / levels_control_negative * 100)

    }

    data_ic50[[group]] <- curr_ic50 <- curr_p %>%
      dplyr::mutate(concentration = as.numeric(concentration)) %>%
      model_cleanly_groupwise(fit_IC50, formula = percent ~ concentration,
                              limits_lower = c(0, 5), limits_upper = c(5, 100),
                              newdata = data.frame(concentration = 10^seq(
                                -3, 3, length.out = 100)))

    log_process("writing to disk")

    curr_ic50 %>%
      dplyr::select(dplyr::group_vars(.), tidy) %>%
      tidyr::unnest(tidy) %>%
      dplyr::mutate(across(any_of(c("p.value")), scales::scientific)) %>%
      dplyr::mutate(across(where(is.double), round, digits = 3)) %>%
      readr::write_csv(file = file.path(
        attr(curr_s, "dir"),
        paste0(format(Sys.time(), "%y%m%d"), "-fitted_IC50_estimates.csv")))

    # assemble one plot that will be written to several devices

    curr_plot <- curr_ic50 %>%
      dplyr::filter(ion == "hmC + fC") %>%
      dplyr::filter(lengths(model) != 0)

    if (nrow(curr_plot) == 0) {

      log_process("there is nothing to plot")

    } else {

      log_process("rendering plots")

      curr_plot <- curr_plot %>%
        # make sure that empty facets are not dropped, but left empty
        dplyr::mutate(content = factor(
          content, unique(c(content, positive_control, negative_control))),
          content = forcats::fct_relevel(content,
                                         unique(c(positive_control, negative_control)), after = Inf)) %>%
        ggplot2::ggplot(ggplot2::aes(x = concentration)) +
        # original data points
        ggplot2::geom_pointrange(data = . %>%
                                   dplyr::select(group_vars(.), data) %>%
                                   tidyr::unnest(data),
                                 ggplot2::aes(y = percent),
                                 stat = "summary", fun.data = ggplot2::mean_se,
                                 fatten = 1) +
        # fitted data points
        ggplot2::geom_path(data = . %>%
                             dplyr::select(group_vars(.), augment_new) %>%
                             tidyr::unnest(augment_new),
                           ggplot2::aes(y = .fitted)) +
        # add IC50 values
        ggplot2::geom_text(data = . %>%
                             dplyr::select(group_vars(.), tidy) %>%
                             tidyr::unnest(tidy) %>%  dplyr::filter(term == "IC50"),
                           ggplot2::aes(
                             # y = 110, x = 1,  ## centered on top
                             y = 0, x = 0,
                             label = paste("  ",
                               round(estimate, -floor(log10(std.error / 10))), "\U00B1",
                               round(std.error, -floor(log10(std.error / 10))), concentration_unit)),
                           size = grid::convertUnit(unit(10, "pt"), "mm", valueOnly = TRUE),
                           # hjust = 0.5, vjust = 1  ## centered on top
                           hjust = 0, vjust = 0
        ) +
        # labels
        labs(y = ifelse(negative_control %in% curr_p$content,
                        paste0("% hmC + fC (normalized to ", negative_control, ")"),
                        "% hmC + fC"),
             x = paste("compound concentration /", concentration_unit)) +
        # scales
        ggplot2::scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
        ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), expand = c(0.05, 0.05)) +
        ggplot2::coord_cartesian(ylim = c(-5, 110)) +
        # facets
        ggplot2::facet_wrap(ggplot2::vars(content), drop = FALSE) +
        # theme
        ggplot2::theme_linedraw(base_size = 12) + ggplot2::theme(
          panel.grid = ggplot2::element_blank()
        )

      tryCatch({

        print(curr_plot)

      }, error = function(e) {

        # typically fails with
        #
        # grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
        # polygon edge not found
        #
        # source unclear; however, repeating seems to solve the issue, else
        # epic fail

        graphics.off()

        print(curr_plot)

      })

      # plot: all fits

      pdf(file = file.path(attr(curr_s, "dir"),
                           paste0(format(Sys.time(), "%y%m%d"), "-fitted_IC50_estimates.pdf")),
          height = 21.5 / 2.54, width = 30.5 / 2.54, paper = "a4")

      print(curr_plot)

      dev.off()

      # plot: fits by compound

      curr_plot_fun <- function(p) {

        curr_plot + ggforce::facet_wrap_paginate(ggplot2::vars(content), drop = FALSE,
                                                 nrow = 1, ncol = 1, page = p)

      }

      if (plot_each_compound == TRUE) for (p in seq_len(ggforce::n_pages(curr_plot_fun(0)))) {

        ggplot2::ggsave(path = file.path(attr(curr_s, "dir")),
                        filename = paste0(format(Sys.time(), "%y%m%d"), "-fitted_IC50_for_",
                                          ggplot2::ggplot_build(curr_plot)$layout$layout$content[[p]], ".png"),
                        plot = print(curr_plot_fun(p)),
                        height = 7, width = 7, units = "cm", dpi = 900)

      }

    }

  }

  # clean up loop variables
  summerr:::rm_silently(curr_p, curr_s, curr_ic50, op, p, curr_plot_fun)

  log_line("-")

}

summerr:::rm_silently(group, group_name)

if (length(data_ic50) > 0) {

  # plot: estimates as bargraph

  curr_plot <- data_ic50 %>%
    dplyr::bind_rows(.id = "sample_group") %>%
    dplyr::group_by(sample_group, .add = TRUE) %>%
    dplyr::select(dplyr::group_vars(.), "tidy") %>%
    tidyr::unnest(tidy) %>%
    dplyr::filter(ion == "hmC + fC", term == "IC50",
                  estimate < 500, std.error < estimate,
                  !(content %in% c(positive_control, negative_control)),
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(content, estimate, mean),
                                 y = 1 + estimate, fill = sample_group)) +
    ggplot2::geom_bar(
      stat = "identity",
      position = ggplot2::position_dodge(preserve = "total"), width = 0.5) +
    ggplot2::geom_linerange(#data = . %>% filter(std.error < 0.5 * estimate),
      ggplot2::aes(ymin = 1 + estimate - std.error, ymax = 1 + estimate + std.error),
      position = ggplot2::position_dodge(preserve = "total", width = 0.5)) +
    # labels
    ggplot2::labs(fill = NULL, y = paste("IC50 /", concentration_unit)) +
    # scales
    ggplot2::scale_y_continuous() +
    ggplot2::coord_flip() +
    # theme
    ggplot2::theme_linedraw(base_size = 12) + ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  print(curr_plot)

  ggsave(plot = curr_plot, path = getwd(),
         filename = paste0(format(Sys.time(), "%y%m%d"), "-fitted_IC50.pdf"),
         width = 21.5, height = 30.5, units = "cm")

  rm(curr_plot)

}

log_task("finished! Have a great day")

log_line("=")
