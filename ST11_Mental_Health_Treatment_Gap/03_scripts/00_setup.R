# Study: ST11_Mental_Health_Treatment_Gap
# Script: 00_setup.R
# Purpose: Shared setup, paths, and helper functions for the ST11 workflow.
# Outcome of interest: the depression/anxiety treatment gap (diagnosed but untreated)
#                      and its socioeconomic / insurance gradient, KDHS 2022.

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
      stop("Unable to locate the ST11 study root from the current working directory.")
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
  data_notes_dir = file.path(study_root, "02_data_notes"),
  scripts_dir = file.path(study_root, "03_scripts"),
  tables_dir = file.path(study_root, "04_tables"),
  figures_dir = file.path(study_root, "05_figures"),
  manuscript_dir = file.path(study_root, "06_manuscript"),
  derived_dir = file.path(study_root, "07_derived_data"),
  logs_dir = file.path(study_root, "08_logs"),
  errors_log = file.path(study_root, "08_logs", "errors_log.txt"),
  execution_log = file.path(study_root, "08_logs", "st11_execution_log.txt")
)

for (dir_path in unname(paths[c("derived_dir", "logs_dir", "tables_dir", "figures_dir")])) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

append_log <- function(text, also_message = FALSE) {
  line <- sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), text)
  cat(line, "\n", file = paths$execution_log, append = TRUE)
  if (also_message) message(line)
  invisible(line)
}

append_error <- function(section_name, error_text) {
  line <- sprintf("[%s] %s: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), section_name, error_text)
  cat(line, "\n", file = paths$errors_log, append = TRUE)
  cat(line, "\n", file = paths$execution_log, append = TRUE)
  invisible(line)
}

save_rds_output <- function(object, file_name) {
  saveRDS(object, file.path(paths$derived_dir, file_name))
}

# ---- labelled-value helpers --------------------------------------------------

to_chr <- function(x) stringr::str_squish(stringr::str_to_lower(as.character(labelled::to_factor(x))))

yn_flag <- function(x, yes = "yes", no = "no") {
  label_chr <- to_chr(x)
  dplyr::case_when(
    label_chr == yes ~ 1L,
    label_chr == no ~ 0L,
    TRUE ~ NA_integer_
  )
}

clean_label <- function(x) {
  output <- stringr::str_to_title(to_chr(x))
  output[output %in% c("Don'T Know", "Na", "Missing")] <- NA_character_
  output
}

education_label <- function(x) {
  l <- to_chr(x)
  dplyr::case_when(
    stringr::str_detect(l, "higher|college|university|tertiary") ~ "Higher",
    stringr::str_detect(l, "secondary") ~ "Secondary",
    stringr::str_detect(l, "primary") ~ "Primary",
    stringr::str_detect(l, "no education|none|preschool|early childhood") ~ "No education",
    TRUE ~ NA_character_
  )
}

wealth_label <- function(x) {
  label_chr <- to_chr(x)
  dplyr::case_when(
    stringr::str_detect(label_chr, "poorest") ~ "Poorest",
    stringr::str_detect(label_chr, "poorer") ~ "Poorer",
    stringr::str_detect(label_chr, "middle") ~ "Middle",
    stringr::str_detect(label_chr, "richer") ~ "Richer",
    stringr::str_detect(label_chr, "richest") ~ "Richest",
    TRUE ~ NA_character_
  )
}

wealth_rank_num <- function(wealth_lab) {
  dplyr::recode(wealth_lab,
    "Poorest" = 1, "Poorer" = 2, "Middle" = 3, "Richer" = 4, "Richest" = 5,
    .default = NA_real_)
}

# ---- survey-design helpers ---------------------------------------------------

make_design <- function(data, weight_var = "weight") {
  survey::svydesign(
    ids = ~psu, strata = ~strata,
    weights = stats::as.formula(paste0("~", weight_var)),
    data = data, nest = TRUE
  )
}

weighted_binary <- function(data, var, weight_var = "weight") {
  data_use <- data %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[weight_var]]), !is.na(psu), !is.na(strata))
  if (nrow(data_use) == 0) {
    return(tibble::tibble(unweighted_n = 0L, est = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  design <- make_design(data_use, weight_var = weight_var)
  estimate <- survey::svymean(stats::as.formula(paste0("~", var)), design, na.rm = TRUE)
  ci <- suppressWarnings(stats::confint(estimate))
  tibble::tibble(
    unweighted_n = nrow(data_use),
    est = as.numeric(stats::coef(estimate)[1]),
    ci_low = ci[1, 1], ci_high = ci[1, 2]
  )
}

fmt_pct_ci <- function(est, lo, hi, digits = 1) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"), 100 * est, 100 * lo, 100 * hi)
}

fmt_apr_ci <- function(est, lo, hi, digits = 2) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"), est, lo, hi)
}

# ---- concentration index (binary outcome, ranked by wealth) ------------------
# Standard (Wagstaff) concentration index with the convenient covariance formula,
# Erreygers correction for the bounded binary outcome, and a cluster (PSU)
# bootstrap confidence interval.

concentration_index <- function(data, outcome, rank_var = "wealth_rank",
                                 weight_var = "weight", psu_var = "psu",
                                 n_boot = 200, seed = 2026) {
  d <- data %>%
    dplyr::filter(!is.na(.data[[outcome]]), !is.na(.data[[rank_var]]),
                  !is.na(.data[[weight_var]]), !is.na(.data[[psu_var]]))
  if (nrow(d) < 30 || length(unique(d[[rank_var]])) < 2) {
    return(tibble::tibble(n = nrow(d), CI = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
                          Erreygers = NA_real_, E_low = NA_real_, E_high = NA_real_))
  }

  # work on plain numeric vectors for speed
  W <- as.numeric(d[[weight_var]]); Y <- as.numeric(d[[outcome]]); R <- as.numeric(d[[rank_var]])

  ci_point <- function(w, y, r) {
    o <- order(r); w <- w[o]; y <- y[o]
    wn <- w / sum(w)
    frank <- cumsum(wn) - 0.5 * wn          # weighted fractional rank
    mu <- sum(wn * y)
    if (mu <= 0) return(c(CI = NA_real_, E = NA_real_))
    CI <- (2 / mu) * sum(wn * y * frank) - 1
    E <- 4 * mu * CI                        # Erreygers (binary outcome range = 1)
    c(CI = CI, E = E)
  }

  point <- ci_point(W, Y, R)

  set.seed(seed)
  idx_by_cluster <- split(seq_along(W), d[[psu_var]])  # precompute row indices per PSU
  clusters <- names(idx_by_cluster)
  boots <- matrix(NA_real_, nrow = n_boot, ncol = 2)
  for (b in seq_len(n_boot)) {
    rows <- unlist(idx_by_cluster[sample(clusters, length(clusters), replace = TRUE)], use.names = FALSE)
    boots[b, ] <- ci_point(W[rows], Y[rows], R[rows])
  }
  q <- function(v) stats::quantile(v, c(0.025, 0.975), na.rm = TRUE)
  ci_q <- q(boots[, 1]); e_q <- q(boots[, 2])

  tibble::tibble(
    n = nrow(d),
    CI = point[["CI"]], CI_low = ci_q[[1]], CI_high = ci_q[[2]],
    Erreygers = point[["E"]], E_low = e_q[[1]], E_high = e_q[[2]]
  )
}

# ---- publication table helpers (shared style) --------------------------------

build_publication_flextable <- function(data,
                                        footer_lines = c("Source: Kenya DHS 2022. Survey-weighted estimates."),
                                        font_size = 9, header_labels = NULL) {
  ft <- flextable::flextable(data)
  if (!is.null(header_labels)) ft <- flextable::set_header_labels(ft, values = header_labels)
  ft <- ft %>%
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

  # Sizing must come last: a later theme or autofit call discards it.
  #
  # set_table_properties(layout = "autofit") makes flextable emit no <w:gridCol>
  # entries at all, so Word is free to collapse the columns -- that is what
  # wrapped headers one character per line. autofit() alone sizes to content with
  # no page awareness and can run well past the text width. So autofit for the
  # column proportions, then rescale those proportions to fill the page exactly.
  ft <- flextable::autofit(ft)
  .w <- dim(ft)$widths
  if (length(.w) > 0 && is.finite(sum(.w)) && sum(.w) > 0) {
    ft <- flextable::width(ft, width = .w * (6.27 / sum(.w)))
  }
  ft <- flextable::set_table_properties(
    ft, layout = "fixed", opts_word = list(split = FALSE, keep_with_next = TRUE)
  )

  ft
}

save_table_bundle <- function(data, csv_path, docx_path, caption_text,
                              footer_lines = c("Source: Kenya DHS 2022. Survey-weighted estimates."),
                              font_size = 9, header_labels = NULL) {
  readr::write_csv(data, csv_path)
  table_doc <- build_publication_flextable(data, footer_lines = footer_lines,
                                            font_size = font_size, header_labels = header_labels)
  officer::read_docx() %>%
    officer::body_add_par(value = caption_text, style = "Normal") %>%
    flextable::body_add_flextable(table_doc) %>%
    print(target = docx_path)
  invisible(list(csv = csv_path, docx = docx_path))
}
