# Study: ST02_Disability_Insurance_Equity
# Script: 00_setup.R
# Author: Nichodemus Werre Amollo
# Date: 2026-04-05
# Purpose: Shared setup, paths, and helper functions for the ST02 workflow.

message("=== SECTION 0: Setup ===")

suppressPackageStartupMessages({
  library(haven)
  library(tidyverse)
  library(survey)
  library(labelled)
  library(broom)
  library(flextable)
  library(officer)
})

options(
  survey.lonely.psu = "adjust",
  scipen = 999
)

find_study_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  repeat {
    required_dirs <- c("01_protocol", "03_scripts", "04_tables", "05_figures", "06_manuscript")
    if (all(dir.exists(file.path(current_dir, required_dirs)))) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("Unable to locate the ST02 study root from the current working directory.")
    }

    current_dir <- parent_dir
  }
}

study_root <- find_study_root()
research_root <- normalizePath(file.path(study_root, "..", ".."), winslash = "/", mustWork = TRUE)
dhs_root <- file.path(research_root, "01_DHS_Data", "KDHS_2022")

paths <- list(
  study_root = study_root,
  research_root = research_root,
  dhs_root = dhs_root,
  protocol_dir = file.path(study_root, "01_protocol"),
  data_notes_dir = file.path(study_root, "02_data_notes"),
  scripts_dir = file.path(study_root, "03_scripts"),
  tables_dir = file.path(study_root, "04_tables"),
  figures_dir = file.path(study_root, "05_figures"),
  manuscript_dir = file.path(study_root, "06_manuscript"),
  derived_dir = file.path(study_root, "07_derived_data"),
  logs_dir = file.path(study_root, "08_logs"),
  errors_log = file.path(study_root, "08_logs", "errors_log.txt"),
  execution_log = file.path(study_root, "08_logs", "st02_execution_log.txt")
)

for (dir_path in unname(paths[c("derived_dir", "logs_dir")])) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

append_log <- function(text, also_message = FALSE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] %s", timestamp, text)
  cat(line, "\n", file = paths$execution_log, append = TRUE)
  if (also_message) {
    message(line)
  }
  invisible(line)
}

append_error <- function(section_name, error_text) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] %s: %s", timestamp, section_name, error_text)
  cat(line, "\n", file = paths$errors_log, append = TRUE)
  cat(line, "\n", file = paths$execution_log, append = TRUE)
  invisible(line)
}

save_rds_output <- function(object, file_name) {
  saveRDS(object, file.path(paths$derived_dir, file_name))
}

to_chr <- function(x) {
  stringr::str_squish(stringr::str_to_lower(as.character(labelled::to_factor(x))))
}

yn_flag <- function(x, yes = "yes", no = "no") {
  label_chr <- to_chr(x)
  dplyr::case_when(
    label_chr == yes ~ 1L,
    label_chr == no ~ 0L,
    TRUE ~ NA_integer_
  )
}

sex_label <- function(x) {
  label_chr <- to_chr(x)
  dplyr::case_when(
    label_chr == "female" ~ "Women",
    label_chr == "male" ~ "Men",
    TRUE ~ NA_character_
  )
}

clean_label <- function(x) {
  output <- stringr::str_to_title(to_chr(x))
  output[output %in% c("Don'T Know", "Na")] <- NA_character_
  output
}

education_label <- function(x) {
  label_chr <- to_chr(x)
  dplyr::case_when(
    label_chr == "no education, preschool/early childhood education" ~ "No education",
    label_chr == "primary" ~ "Primary",
    label_chr == "secondary" ~ "Secondary",
    label_chr == "higher" ~ "Higher",
    TRUE ~ NA_character_
  )
}

difficulty_level <- function(x) {
  label_chr <- to_chr(x)
  dplyr::case_when(
    label_chr %in% c(
      "no difficulty",
      "no difficulty seeing",
      "no difficulty hearing",
      "no difficulty communicating",
      "no difficulty remembering/concentrating",
      "no difficulty walking or climbing",
      "no difficulty washing or dressing"
    ) ~ "No difficulty",
    label_chr == "some difficulty" ~ "Some difficulty",
    label_chr == "a lot of difficulty" ~ "A lot of difficulty",
    stringr::str_detect(label_chr, "^cannot") ~ "Cannot do at all",
    TRUE ~ NA_character_
  )
}

make_design <- function(data, weight_var = "weight") {
  survey::svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = stats::as.formula(paste0("~", weight_var)),
    data = data,
    nest = TRUE
  )
}

weighted_binary <- function(data, var, weight_var = "weight") {
  data_use <- data %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[weight_var]]), !is.na(psu), !is.na(strata))

  if (nrow(data_use) == 0) {
    return(tibble::tibble(
      unweighted_n = 0L,
      est = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_
    ))
  }

  design <- make_design(data_use, weight_var = weight_var)
  estimate <- survey::svymean(stats::as.formula(paste0("~", var)), design, na.rm = TRUE)
  ci <- suppressWarnings(stats::confint(estimate))

  tibble::tibble(
    unweighted_n = nrow(data_use),
    est = as.numeric(stats::coef(estimate)[1]),
    ci_low = ci[1, 1],
    ci_high = ci[1, 2]
  )
}

fmt_pct_ci <- function(est, lo, hi, digits = 1) {
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
    100 * est,
    100 * lo,
    100 * hi
  )
}

fmt_mean_ci <- function(est, lo, hi, digits = 1) {
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
    est,
    lo,
    hi
  )
}

fmt_apr_ci <- function(est, lo, hi, digits = 2) {
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
    est,
    lo,
    hi
  )
}

weighted_mean_stat <- function(data, var, weight_var = "weight") {
  data_use <- data %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[weight_var]]), !is.na(psu), !is.na(strata))

  if (nrow(data_use) == 0) {
    return(tibble::tibble(
      unweighted_n = 0L,
      est = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_
    ))
  }

  design <- make_design(data_use, weight_var = weight_var)
  estimate <- survey::svymean(stats::as.formula(paste0("~", var)), design, na.rm = TRUE)
  ci <- suppressWarnings(stats::confint(estimate))

  tibble::tibble(
    unweighted_n = nrow(data_use),
    est = as.numeric(stats::coef(estimate)[1]),
    ci_low = ci[1, 1],
    ci_high = ci[1, 2]
  )
}

weighted_mean <- weighted_mean_stat

weighted_level <- function(data, var, level_value, weight_var = "weight") {
  data_use <- data %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[weight_var]]), !is.na(psu), !is.na(strata)) %>%
    dplyr::mutate(.indicator = as.integer(as.character(.data[[var]]) == level_value))

  if (nrow(data_use) == 0) {
    return(tibble::tibble(
      unweighted_n = 0L,
      est = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_
    ))
  }

  design <- make_design(data_use, weight_var = weight_var)
  estimate <- survey::svymean(~.indicator, design, na.rm = TRUE)
  ci <- suppressWarnings(stats::confint(estimate))

  tibble::tibble(
    unweighted_n = nrow(data_use),
    est = as.numeric(stats::coef(estimate)[1]),
    ci_low = ci[1, 1],
    ci_high = ci[1, 2]
  )
}

build_publication_flextable <- function(data,
                                        footer_lines = c("Source: Kenya DHS 2022. Survey-weighted estimates."),
                                        font_size = 9,
                                        header_labels = NULL,
                                        spanner_values = NULL,
                                        spanner_widths = NULL) {
  ft <- flextable::flextable(data)

  if (!is.null(header_labels)) {
    ft <- flextable::set_header_labels(ft, values = header_labels)
  }

  if (!is.null(spanner_values) && !is.null(spanner_widths)) {
    ft <- flextable::add_header_row(
      ft,
      values = spanner_values,
      colwidths = spanner_widths,
      top = TRUE
    )
  }

  ft <- ft %>%
    flextable::set_table_properties(layout = "autofit", opts_word = list(split = TRUE)) %>%
    flextable::autofit() %>%
    flextable::theme_booktabs() %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::align(j = seq_len(ncol(data)), align = "center", part = "header") %>%
    flextable::align(j = 1, align = "left", part = "header") %>%
    flextable::valign(valign = "top", part = "all") %>%
    flextable::padding(padding = 3, part = "all")

  if (length(footer_lines) > 0) {
    ft <- flextable::add_footer_lines(ft, values = footer_lines) %>%
      flextable::merge_h(part = "footer") %>%
      flextable::italic(part = "footer") %>%
      flextable::fontsize(size = 8, part = "footer") %>%
      flextable::align(align = "left", part = "footer")
  }

  ft
}

save_table_bundle <- function(data,
                              csv_path,
                              docx_path,
                              caption_text,
                              footer_lines = c("Source: Kenya DHS 2022. Survey-weighted estimates."),
                              font_size = 9,
                              header_labels = NULL,
                              spanner_values = NULL,
                              spanner_widths = NULL) {
  readr::write_csv(data, csv_path)

  table_doc <- build_publication_flextable(
    data = data,
    footer_lines = footer_lines,
    font_size = font_size,
    header_labels = header_labels,
    spanner_values = spanner_values,
    spanner_widths = spanner_widths
  )

  officer::read_docx() %>%
    officer::body_add_par(value = caption_text, style = "Normal") %>%
    flextable::body_add_flextable(table_doc) %>%
    print(target = docx_path)

  invisible(list(csv = csv_path, docx = docx_path))
}
