# Study: ST03_NCD_Insurance_Service_Use
# Script: 00_setup.R
# Author: Nichodemus Werre Amollo
# Date: 2026-03-29
# Purpose: Load packages, define paths, and provide shared helper functions for the ST03 reproducible workflow.

message("=== SECTION 0: Setup ===")

suppressPackageStartupMessages({
  library(haven)
  library(tidyverse)
  library(survey)
  library(labelled)
  library(flextable)
  library(officer)
  library(ggplot2)
  library(broom)
})

options(
  survey.lonely.psu = "adjust",
  scipen = 999
)

find_study_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  repeat {
    required_dirs <- c("01_protocol", "03_scripts", "06_manuscript")
    if (all(dir.exists(file.path(current_dir, required_dirs)))) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("Unable to locate the ST03 study root from the current working directory.")
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
  scripts_dir = file.path(study_root, "03_scripts"),
  tables_dir = file.path(study_root, "04_tables"),
  figures_dir = file.path(study_root, "05_figures"),
  manuscript_dir = file.path(study_root, "06_manuscript"),
  derived_dir = file.path(study_root, "07_derived_data"),
  logs_dir = file.path(study_root, "08_logs"),
  errors_log = file.path(study_root, "08_logs", "errors_log.txt"),
  execution_log = file.path(study_root, "08_logs", "st03_execution_log.txt")
)

for (dir_path in unname(paths[c(
  "tables_dir",
  "figures_dir",
  "manuscript_dir",
  "derived_dir",
  "logs_dir"
)])) {
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

write_lines_output <- function(lines, file_name) {
  readr::write_lines(lines, file.path(paths$derived_dir, file_name))
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
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

problem_flag <- function(x) {
  label_chr <- to_chr(x)
  dplyr::case_when(
    label_chr == "big problem" ~ 1L,
    label_chr %in% c("not a big problem", "no problem") ~ 0L,
    TRUE ~ NA_integer_
  )
}

eligible_flag <- function(x) {
  label_chr <- to_chr(x)
  dplyr::case_when(
    label_chr == "eligible" ~ 1L,
    label_chr == "not eligible" ~ 0L,
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
  output[output %in% c("Don't Know", "Dk", "Na")] <- NA_character_
  output
}

collapse_education <- function(x) {
  label_chr <- clean_label(x)
  dplyr::case_when(
    label_chr == "No Education" ~ "No education",
    label_chr == "Primary" ~ "Primary",
    label_chr %in% c("Secondary", "Higher") ~ "Secondary+",
    TRUE ~ label_chr
  )
}

collapse_employment <- function(x) {
  label_chr <- clean_label(x)
  dplyr::case_when(
    label_chr == "Not Working" ~ "Not working",
    label_chr %in% c("Professional/Technical/Managerial", "Clerical") ~ "Professional/clerical",
    label_chr %in% c("Sales", "Services", "Household And Domestic") ~ "Sales/services",
    label_chr %in% c(
      "Agricultural - Self Employed",
      "Agriculture - Self Employed",
      "Agricultural - Employee",
      "Agriculture - Employee"
    ) ~ "Agriculture",
    label_chr %in% c("Skilled Manual", "Unskilled Manual") ~ "Manual",
    TRUE ~ "Other/unknown"
  )
}

collapse_province <- function(x) {
  region_chr <- to_chr(x)
  dplyr::case_when(
    region_chr %in% c("mombasa", "kwale", "kilifi", "tana river", "lamu", "taita taveta") ~ "Coast",
    region_chr %in% c("garissa", "wajir", "mandera") ~ "North Eastern",
    region_chr %in% c("marsabit", "isiolo", "meru", "tharaka nithi", "embu", "kitui", "machakos", "makueni") ~ "Eastern",
    region_chr %in% c("nyandarua", "nyeri", "kirinyaga", "murang'a", "kiambu") ~ "Central",
    region_chr %in% c(
      "turkana", "west pokot", "samburu", "trans nzoia", "uasin gishu",
      "elgeyo marakwet", "nandi", "baringo", "laikipia", "nakuru",
      "narok", "kajiado", "kericho", "bomet"
    ) ~ "Rift Valley",
    region_chr %in% c("kakamega", "vihiga", "bungoma", "busia") ~ "Western",
    region_chr %in% c("siaya", "kisumu", "homa bay", "migori", "kisii", "nyamira") ~ "Nyanza",
    region_chr == "nairobi" ~ "Nairobi",
    TRUE ~ NA_character_
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
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[weight_var]]))

  if (nrow(data_use) == 0) {
    return(tibble::tibble(
      unweighted_n = 0L,
      est = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      formatted = NA_character_
    ))
  }

  design <- make_design(data_use, weight_var = weight_var)
  estimate <- survey::svymean(stats::as.formula(paste0("~", var)), design, na.rm = TRUE)
  ci <- suppressWarnings(stats::confint(estimate))

  tibble::tibble(
    unweighted_n = nrow(data_use),
    est = as.numeric(stats::coef(estimate)[1]),
    ci_low = ci[1, 1],
    ci_high = ci[1, 2],
    formatted = fmt_pct_ci(est, ci_low, ci_high)
  )
}

weighted_mean_stat <- function(data, var, weight_var = "weight") {
  data_use <- data %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[weight_var]]))

  if (nrow(data_use) == 0) {
    return(tibble::tibble(
      unweighted_n = 0L,
      est = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      formatted = NA_character_
    ))
  }

  design <- make_design(data_use, weight_var = weight_var)
  estimate <- survey::svymean(stats::as.formula(paste0("~", var)), design, na.rm = TRUE)
  ci <- suppressWarnings(stats::confint(estimate))

  tibble::tibble(
    unweighted_n = nrow(data_use),
    est = as.numeric(stats::coef(estimate)[1]),
    ci_low = ci[1, 1],
    ci_high = ci[1, 2],
    formatted = fmt_mean_ci(est, ci_low, ci_high)
  )
}

weighted_mean <- weighted_mean_stat

weighted_level <- function(data, var, level_value, weight_var = "weight") {
  data_use <- data %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[weight_var]])) %>%
    dplyr::mutate(.indicator = as.integer(as.character(.data[[var]]) == level_value))

  if (nrow(data_use) == 0) {
    return(tibble::tibble(
      unweighted_n = 0L,
      est = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      formatted = NA_character_
    ))
  }

  design <- make_design(data_use, weight_var = weight_var)
  estimate <- survey::svymean(~.indicator, design, na.rm = TRUE)
  ci <- suppressWarnings(stats::confint(estimate))

  tibble::tibble(
    unweighted_n = nrow(data_use),
    est = as.numeric(stats::coef(estimate)[1]),
    ci_low = ci[1, 1],
    ci_high = ci[1, 2],
    formatted = fmt_pct_ci(est, ci_low, ci_high)
  )
}

weighted_by_group <- function(data, group_var, outcome_var, weight_var = "weight") {
  groups <- sort(unique(as.character(stats::na.omit(data[[group_var]]))))
  purrr::map_dfr(groups, function(group_value) {
    data_group <- data %>% dplyr::filter(as.character(.data[[group_var]]) == group_value)
    stat <- weighted_binary(data_group, outcome_var, weight_var = weight_var)
    tibble::tibble(
      group = group_value,
      unweighted_n = stat$unweighted_n,
      estimate = stat$est,
      ci_low = stat$ci_low,
      ci_high = stat$ci_high,
      formatted = stat$formatted
    )
  })
}

weighted_concentration_indices <- function(data, y_var, weight_var = "weight", rank_var = "wealth_rank", bootstrap_n = 1000) {
  data_use <- data %>%
    dplyr::filter(!is.na(.data[[y_var]]), !is.na(.data[[rank_var]]), !is.na(.data[[weight_var]])) %>%
    dplyr::mutate(
      y = .data[[y_var]],
      rank_x = .data[[rank_var]],
      wt = .data[[weight_var]]
    )

  if (nrow(data_use) < 25 || mean(data_use$y, na.rm = TRUE) == 0) {
    return(tibble::tibble(
      measure = c("Standard CI", "Erreygers CI"),
      estimate = c(NA_real_, NA_real_),
      ci_low = c(NA_real_, NA_real_),
      ci_high = c(NA_real_, NA_real_)
    ))
  }

  calculate_indices <- function(input_data) {
    ranked_data <- input_data %>%
      dplyr::arrange(rank_x) %>%
      dplyr::mutate(
        w_norm = wt / sum(wt),
        frac_rank = cumsum(w_norm) - 0.5 * w_norm
      )

    mu <- stats::weighted.mean(ranked_data$y, ranked_data$wt)
    if (!is.finite(mu) || mu == 0) {
      return(NA_real_)
    }

    covariance_value <- stats::weighted.mean(
      (ranked_data$y - mu) * (ranked_data$frac_rank - 0.5),
      ranked_data$wt
    )

    standard_ci <- 2 * covariance_value / mu
    c(
      standard = standard_ci,
      erreygers = 4 * mu * standard_ci
    )
  }

  set.seed(20260318)
  point_estimate <- calculate_indices(data_use)
  bootstrap_values <- replicate(bootstrap_n, {
    sample_index <- sample.int(nrow(data_use), replace = TRUE)
    calculate_indices(data_use[sample_index, , drop = FALSE])
  })

  if (is.null(dim(bootstrap_values))) {
    bootstrap_values <- matrix(bootstrap_values, nrow = 2)
    rownames(bootstrap_values) <- c("standard", "erreygers")
  }

  tibble::tibble(
    measure = c("Standard CI", "Erreygers CI"),
    estimate = c(point_estimate["standard"], point_estimate["erreygers"]),
    ci_low = c(
      as.numeric(stats::quantile(bootstrap_values["standard", is.finite(bootstrap_values["standard", ])], 0.025, na.rm = TRUE)),
      as.numeric(stats::quantile(bootstrap_values["erreygers", is.finite(bootstrap_values["erreygers", ])], 0.025, na.rm = TRUE))
    ),
    ci_high = c(
      as.numeric(stats::quantile(bootstrap_values["standard", is.finite(bootstrap_values["standard", ])], 0.975, na.rm = TRUE)),
      as.numeric(stats::quantile(bootstrap_values["erreygers", is.finite(bootstrap_values["erreygers", ])], 0.975, na.rm = TRUE))
    )
  )
}

weighted_ci_estimate <- function(data, y_var, weight_var = "weight", rank_var = "wealth_rank", bootstrap_n = 1000) {
  standard_ci <- weighted_concentration_indices(
    data = data,
    y_var = y_var,
    weight_var = weight_var,
    rank_var = rank_var,
    bootstrap_n = bootstrap_n
  ) %>%
    dplyr::filter(measure == "Standard CI")

  c(
    ci = standard_ci$estimate[[1]],
    ci_low = standard_ci$ci_low[[1]],
    ci_high = standard_ci$ci_high[[1]]
  )
}

extract_model_table <- function(model, model_label, measure) {
  if (is.null(model)) {
    return(tibble::tibble())
  }

  broom::tidy(model) %>%
    dplyr::mutate(
      effect = exp(estimate),
      ci_low = exp(estimate - 1.96 * std.error),
      ci_high = exp(estimate + 1.96 * std.error),
      model = model_label,
      measure = measure
    ) %>%
    dplyr::select(model, measure, term, estimate, std.error, statistic, p.value, effect, ci_low, ci_high)
}

extract_effect <- function(model_table, model_label, term_label, digits = 2) {
  row <- model_table %>%
    dplyr::filter(model == model_label, term == term_label)

  if (nrow(row) == 0) {
    return(NA_character_)
  }

  sprintf(
    paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
    row$effect[[1]],
    row$ci_low[[1]],
    row$ci_high[[1]]
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

st03_table_display_specs <- list(
  table1 = list(
    header_labels = c(
      characteristic = "Characteristic",
      "Women n" = "n",
      Women = "Estimate\n(95% CI)",
      "Men n" = "n",
      Men = "Estimate\n(95% CI)",
      "Pooled n" = "n",
      Pooled = "Estimate\n(95% CI)"
    ),
    spanner_values = c("", "Women", "Men", "Pooled"),
    spanner_widths = c(1, 2, 2, 2)
  ),
  table2 = list(
    header_labels = c(
      sex = "Sex",
      diagnosis_profile = "Diagnosis profile",
      n = "n",
      any_insurance = "Any insurance\n% (95% CI)",
      nhif = "NHIF coverage\n% (95% CI)",
      treatment_gap = "Treatment gap\n% (95% CI)"
    )
  ),
  table3 = list(
    header_labels = c(
      Covariate = "Covariate",
      "Women APR (95% CI)" = "Women\nAPR (95% CI)",
      "Men APR (95% CI)" = "Men\nAPR (95% CI)",
      "Pooled APR (95% CI)" = "Pooled\nAPR (95% CI)"
    )
  ),
  table4 = list(
    header_labels = c(
      "Insurance status" = "Insurance type",
      Insured = "Covered",
      "Barrier type" = "Barrier type",
      "Unweighted n" = "n",
      "Prevalence % (95% CI)" = "Weighted prevalence\n% (95% CI)"
    )
  ),
  table5 = list(
    header_labels = c(
      Outcome = "Outcome",
      Subgroup = "Subgroup",
      "Standard CI (95% CI)" = "Standard CI\n(95% CI)",
      "Erreygers-corrected CI (95% CI)" = "Erreygers-corrected CI\n(95% CI)"
    )
  ),
  table_s2 = list(
    header_labels = c(
      "Outcome definition" = "Outcome definition",
      "Women prevalence % (95% CI)" = "Women prevalence\n% (95% CI)",
      "Men prevalence % (95% CI)" = "Men prevalence\n% (95% CI)",
      "Pooled prevalence % (95% CI)" = "Pooled prevalence\n% (95% CI)",
      "Women APR for insured vs uninsured (95% CI)" = "Women APR for insured\nvs uninsured (95% CI)",
      "Men APR for insured vs uninsured (95% CI)" = "Men APR for insured\nvs uninsured (95% CI)",
      "Pooled APR for insured vs uninsured (95% CI)" = "Pooled APR for insured\nvs uninsured (95% CI)"
    )
  )
)

write_session_info <- function(file_name = "st03_session_info.txt") {
  output_path <- file.path(paths$logs_dir, file_name)
  sink(output_path)
  print(sessionInfo())
  sink()
  invisible(output_path)
}

append_log("Setup loaded for ST03 reproducible workflow.")

message("=== SECTION 0 COMPLETE ===")
