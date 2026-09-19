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

`%||%` <- function(x, y) if (is.null(x)) y else x

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

# KDHS records "don't know" and "missing" for the cost items as 999998 / 999999
# and their 9999998 / 9999999 analogues, so any value at or above 999998 is a
# code rather than a shilling amount and has to be dropped before the amount is
# used. Values are Kenyan shillings.
dhs_amount <- function(x) {
  value <- as.numeric(x)
  dplyr::if_else(value >= 999998, NA_real_, value)
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

# Domain estimation.
#
# Filtering a data frame and calling svydesign() on the remainder discards the
# PSUs that contribute no observations to the domain, which is not the same
# calculation as a domain estimate from the full design: the variance should be
# computed with the parent design's PSU and stratum structure, including the
# PSUs that contribute nothing. The difference is small in large domains and not
# small in the sparse ones this study reports, such as insured adults with severe
# functional difficulty.
#
# The parent design is registered once per script, and every subsequent estimate
# is a subset() of it, matched on the row identifier carried in the analytic
# dataset. Call sites keep passing data frames; only the variance calculation
# behind them changes.
.st02_design_state <- new.env(parent = emptyenv())

register_parent_design <- function(data, weight_var = "weight") {
  if (!".row_id" %in% names(data)) {
    stop("The analytic dataset must carry .row_id before a parent design is registered.")
  }
  assign("design", make_design(data, weight_var = weight_var), envir = .st02_design_state)
  assign("weight_var", weight_var, envir = .st02_design_state)
  invisible(TRUE)
}

parent_design <- function() {
  if (!exists("design", envir = .st02_design_state)) NULL else get("design", envir = .st02_design_state)
}

# Design for one domain. Falls back to rebuilding when no parent has been
# registered or the rows cannot be matched, so the helpers still work standalone.
domain_design <- function(data, weight_var = "weight") {
  parent <- parent_design()
  if (is.null(parent) || !".row_id" %in% names(data) ||
      !identical(weight_var, get("weight_var", envir = .st02_design_state))) {
    return(make_design(data, weight_var = weight_var))
  }
  domain_ids <- data$.row_id
  design <- subset(parent, .row_id %in% domain_ids)

  # Variables derived after the parent design was registered, such as an
  # indicator built for one table row, do not exist in the parent's variable
  # frame. They are carried across by row identifier so that a domain estimate
  # can still be taken from the parent design rather than from a rebuilt one.
  derived <- setdiff(names(data), names(design$variables))
  if (length(derived) > 0) {
    position <- match(design$variables$.row_id, data$.row_id)
    for (variable_name in derived) {
      design$variables[[variable_name]] <- data[[variable_name]][position]
    }
  }
  design
}

# Logit-transformed confidence interval for a survey proportion.
#
# The Wald interval returned by confint(svymean()) is computed on the proportion
# scale and is unbounded, so in small domains it can run outside [0, 1]. In this
# study the severe-disability insured payment cell (p = 0.91, n = 19) produced an
# upper limit of 103.1%, which clipped when plotted and left a stray error bar.
# svyciprop(method = "logit") respects the unit interval and is the standard
# choice for survey proportions; in large domains it is numerically almost
# identical to Wald, so headline estimates are essentially unchanged and only
# small-n cells move.
svy_prop_ci <- function(design, formula_txt) {
  prop <- try(
    suppressWarnings(
      survey::svyciprop(stats::as.formula(formula_txt), design,
                        method = "logit", level = 0.95)
    ),
    silent = TRUE
  )
  if (!inherits(prop, "try-error")) {
    ci_vec <- as.numeric(attr(prop, "ci"))
    return(list(est = as.numeric(prop)[1],
                ci_low = max(0, min(1, ci_vec[1])),
                ci_high = max(0, min(1, ci_vec[2]))))
  }
  # Degenerate domain: fall back to Wald, truncated to the unit interval so that
  # downstream plots cannot break.
  est <- survey::svymean(stats::as.formula(formula_txt), design, na.rm = TRUE)
  ci_w <- suppressWarnings(stats::confint(est))
  list(est = as.numeric(stats::coef(est)[1]),
       ci_low = max(0, min(1, ci_w[1, 1])),
       ci_high = max(0, min(1, ci_w[1, 2])))
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

  design <- domain_design(data_use, weight_var = weight_var)
  r <- svy_prop_ci(design, paste0("~ I(", var, " == 1)"))

  tibble::tibble(
    unweighted_n = nrow(data_use),
    est = r$est,
    ci_low = r$ci_low,
    ci_high = r$ci_high
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

  design <- domain_design(data_use, weight_var = weight_var)
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

  design <- domain_design(data_use, weight_var = weight_var)
  r <- svy_prop_ci(design, "~ I(.indicator == 1)")

  tibble::tibble(
    unweighted_n = nrow(data_use),
    est = r$est,
    ci_low = r$ci_low,
    ci_high = r$ci_high
  )
}

# Design-based confidence limits use the survey degrees of freedom rather than a
# normal approximation. With 1,691 PSUs in 92 strata the two are numerically very
# close here, but the t reference is the correct one for a survey model and it is
# what the p values reported alongside these limits already use.
tidy_apr_design <- function(model, design) {
  df_design <- survey::degf(design)
  broom::tidy(model) %>%
    dplyr::mutate(
      apr = exp(estimate),
      ci_low = exp(estimate - stats::qt(0.975, df_design) * std.error),
      ci_high = exp(estimate + stats::qt(0.975, df_design) * std.error),
      df = df_design
    ) %>%
    dplyr::select(term, apr, ci_low, ci_high, p.value, df)
}

# Replicate-weight version of the marginal contrast.
#
# The delta-method version below propagates uncertainty in the fitted
# coefficients but treats the weighted covariate distribution as fixed, so its
# intervals are model-based. Refitting the model in each bootstrap replicate
# carries both the coefficient uncertainty and the uncertainty in the covariate
# distribution through the design. Both are reported; the comparison is in the
# supplementary material.
# Replicate weights for the parent sample, built once and reused.
#
# Order matters here. as.svrepdesign() resamples the PSUs that are present in
# the object it is given, so converting an already-subsetted domain design
# resamples only the PSUs that contribute rows to that domain: the replicates
# are drawn from the domain rather than from the sample the domain sits in.
# Building the replicate weights from the parent and subsetting afterwards
# resamples the whole parent PSU set, which is the quantity a domain estimate
# needs.
#
# Verified against survey 4.4.2: subset() on a survey.design2 drops the
# excluded rows rather than retaining them at zero weight (the per-row
# fpc$sampsize is what preserves the parent stratum sizes), and subset() on a
# svyrep.design behaves the same way. Neither route leaves zero-weight parent
# rows behind, so nothing downstream may rely on that.
#
# The replicate design is cached because every specification and outcome should
# use the same set of replicate weights, and rebuilding 500 replicates per call
# would dominate the run time.
.st02_repdesign_cache <- new.env(parent = emptyenv())

parent_replicate_design <- function(replicates, seed, type = "subbootstrap") {
  parent <- parent_design()
  if (is.null(parent)) return(NULL)
  key <- paste(type, replicates, seed, sep = "_")
  if (!exists(key, envir = .st02_repdesign_cache)) {
    set.seed(seed)
    assign(
      key,
      survey::as.svrepdesign(parent, type = type, replicates = replicates),
      envir = .st02_repdesign_cache
    )
  }
  get(key, envir = .st02_repdesign_cache)
}

standardised_contrast_replicate <- function(formula, design, exposure,
                                            family = quasipoisson(link = "log"),
                                            replicates = 500, seed = 20260919) {
  parent_rep <- parent_replicate_design(replicates, seed)

  if (is.null(parent_rep)) {
    # Standalone use with no registered parent: the domain is all there is.
    set.seed(seed)
    rep_design <- survey::as.svrepdesign(design, type = "subbootstrap", replicates = replicates)
  } else {
    rep_design <- subset(parent_rep, .row_id %in% design$variables$.row_id)
    # Variables built after the parent was registered (the outcome indicator for
    # one specification, say) are not in the parent's variable frame; carry them
    # across by row identifier, as domain_design() does for the Taylor designs.
    derived <- setdiff(names(design$variables), names(rep_design$variables))
    if (length(derived) > 0) {
      position <- match(rep_design$variables$.row_id, design$variables$.row_id)
      for (variable_name in derived) {
        rep_design$variables[[variable_name]] <- design$variables[[variable_name]][position]
      }
    }
  }

  sampling_weights <- stats::weights(rep_design, "sampling")
  rep_weights <- stats::weights(rep_design, "analysis")
  # Defensive: subsetting drops rows in survey 4.4.2, so this is normally the
  # whole frame. It stays so that a survey version which zeroes instead of drops
  # cannot silently feed zero-weight rows into the fit.
  in_domain <- sampling_weights > 0
  model_frame <- rep_design$variables[in_domain, , drop = FALSE]
  sampling_weights <- sampling_weights[in_domain]
  rep_weights <- rep_weights[in_domain, , drop = FALSE]
  terms_obj <- stats::terms(stats::as.formula(formula), data = model_frame)

  mm_1 <- {
    d <- model_frame; d[[exposure]] <- 1
    stats::model.matrix(stats::delete.response(terms_obj), data = d)
  }
  mm_0 <- {
    d <- model_frame; d[[exposure]] <- 0
    stats::model.matrix(stats::delete.response(terms_obj), data = d)
  }

  contrast_from_weights <- function(weights_vec) {
    fit <- suppressWarnings(stats::glm(stats::as.formula(formula), data = model_frame,
                                       weights = weights_vec, family = family))
    beta <- stats::coef(fit)
    w <- weights_vec / sum(weights_vec)
    p1 <- sum(w * exp(as.vector(mm_1 %*% beta)))
    p0 <- sum(w * exp(as.vector(mm_0 %*% beta)))
    c(difference = p1 - p0, ratio = p1 / p0)
  }

  point <- contrast_from_weights(sampling_weights)
  replicate_estimates <- t(apply(rep_weights, 2, contrast_from_weights))
  # The analysis-weight matrix is the largest object here: one column per
  # replicate over every domain row. It is not needed once the replicate
  # estimates exist, and on a memory-constrained machine holding it across the
  # nine outcome-by-specification fits is what pushes the session into swap.
  rm(rep_weights, mm_1, mm_0, model_frame)
  gc(verbose = FALSE)
  scale_factor <- rep_design$scale
  rscales <- rep_design$rscales
  variance <- apply(replicate_estimates, 2, function(x) {
    sum(rscales * (x - mean(x))^2) * scale_factor
  })
  standard_errors <- sqrt(variance)
  df_design <- survey::degf(design)
  t_crit <- stats::qt(0.975, df_design)

  tibble::tibble(
    difference = point[["difference"]],
    difference_ci_low = point[["difference"]] - t_crit * standard_errors[["difference"]],
    difference_ci_high = point[["difference"]] + t_crit * standard_errors[["difference"]],
    ratio = point[["ratio"]],
    ratio_ci_low = point[["ratio"]] - t_crit * standard_errors[["ratio"]],
    ratio_ci_high = point[["ratio"]] + t_crit * standard_errors[["ratio"]],
    replicates = replicates
  )
}

# Marginal standardisation (g-computation) over the observed covariate
# distribution of the analytic sample, weighted by the survey weights. Every
# record is set first to exposed and then to unexposed, predictions are averaged
# under each setting, and the contrast is taken on both the difference and the
# ratio scale. Standard errors come from the delta method applied to the model
# variance-covariance matrix, so they carry the design through the fitted model.
standardised_contrast <- function(model, design, exposure, value_1 = 1, value_0 = 0) {
  data_model <- design$variables
  weights_vec <- stats::weights(design)
  if (is.null(weights_vec) || length(weights_vec) != nrow(data_model)) {
    weights_vec <- rep(1, nrow(data_model))
  }
  # Standardise over the domain the model was fitted to: rows carrying weight and
  # complete on every model term.
  keep <- weights_vec > 0 &
    !is.na(stats::predict(model, newdata = data_model, type = "link"))
  data_model <- data_model[keep, , drop = FALSE]
  weights_vec <- weights_vec[keep]

  build_mm <- function(set_value) {
    newdata <- data_model
    newdata[[exposure]] <- set_value
    stats::model.matrix(stats::delete.response(stats::terms(model)), data = newdata)
  }

  beta <- stats::coef(model)
  vcov_beta <- stats::vcov(model)
  mm_1 <- build_mm(value_1)
  mm_0 <- build_mm(value_0)
  mu_1 <- exp(as.vector(mm_1 %*% beta))
  mu_0 <- exp(as.vector(mm_0 %*% beta))
  w <- weights_vec / sum(weights_vec)
  p1 <- sum(w * mu_1)
  p0 <- sum(w * mu_0)

  grad_1 <- as.vector(crossprod(mm_1, w * mu_1))
  grad_0 <- as.vector(crossprod(mm_0, w * mu_0))
  grad_diff <- grad_1 - grad_0
  se_diff <- sqrt(as.numeric(t(grad_diff) %*% vcov_beta %*% grad_diff))
  grad_log_ratio <- grad_1 / p1 - grad_0 / p0
  se_log_ratio <- sqrt(as.numeric(t(grad_log_ratio) %*% vcov_beta %*% grad_log_ratio))

  df_design <- survey::degf(design)
  t_crit <- stats::qt(0.975, df_design)

  tibble::tibble(
    prevalence_exposed = p1,
    prevalence_unexposed = p0,
    difference = p1 - p0,
    difference_ci_low = (p1 - p0) - t_crit * se_diff,
    difference_ci_high = (p1 - p0) + t_crit * se_diff,
    ratio = p1 / p0,
    ratio_ci_low = exp(log(p1 / p0) - t_crit * se_log_ratio),
    ratio_ci_high = exp(log(p1 / p0) + t_crit * se_log_ratio),
    df = df_design
  )
}

build_publication_flextable <- function(data,
                                        footer_lines = c("Source: Kenya DHS 2022. Survey-weighted estimates."),
                                        font_size = 9,
                                        header_labels = NULL,
                                        spanner_values = NULL,
                                        spanner_widths = NULL,
                                        # A4 (11906 twips) less the 1-inch margins of the
                                        # reference document = 9026 twips = 6.27 in.
                                        page_width_in = 6.27) {
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
    flextable::theme_booktabs() %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::align(j = seq_len(ncol(data)), align = "center", part = "header") %>%
    flextable::align(j = 1, align = "left", part = "header") %>%
    flextable::valign(valign = "top", part = "all") %>%
    flextable::padding(padding = 3, part = "all")

  # Numeric-looking columns centre; the label column stays left.
  if (ncol(data) > 1) {
    ft <- flextable::align(ft, j = seq(2, ncol(data)), align = "center", part = "body")
  }

  # Sizing, and it has to come last because any later theme or autofit call
  # discards it.
  #
  # Two traps here, both of which produced unreadable tables.
  #
  # set_table_properties(layout = "autofit") makes flextable emit NO <w:gridCol>
  # entries at all. Word is then free to size columns however it likes and
  # collapses them to their minimum, which is what wrapped headers one character
  # per line. The fixed layout is required so that real column widths are written.
  #
  # autofit() on its own sizes columns to their content with no page awareness.
  # For a seven-column table like Table 1 that came to roughly 14,800 twips
  # against an A4 text width of about 9,000, so Word squeezed it back down.
  #
  # So: autofit for the relative proportions, then rescale to exactly fill the
  # available width. Narrow tables widen, over-wide tables shrink, and the
  # content-derived column proportions are preserved either way.
  ft <- flextable::autofit(ft)
  w <- dim(ft)$widths
  if (length(w) > 0 && is.finite(sum(w)) && sum(w) > 0) {
    # Proportional rescaling alone squeezed the narrow columns. When a wide table
    # is scaled down to the text width, a column holding "37,233" can end up
    # narrower than that string and Word then breaks the number after the comma.
    # Short columns are given a floor first and the remaining width is shared
    # among the rest in proportion to their content.
    min_width_in <- 0.58
    content_width <- vapply(data, function(column) max(nchar(as.character(column)), 0), numeric(1))
    header_width <- if (!is.null(header_labels)) {
      vapply(names(data), function(nm) nchar(as.character(header_labels[[nm]] %||% nm)), numeric(1))
    } else {
      vapply(names(data), nchar, numeric(1))
    }
    narrow <- pmax(content_width, 0) <= 8 & header_width <= 18
    if (any(narrow) && !all(narrow)) {
      floors <- pmin(pmax(w[narrow], min_width_in), page_width_in / 4)
      remaining <- page_width_in - sum(floors)
      wide_w <- w[!narrow]
      if (remaining > 0 && sum(wide_w) > 0) {
        w[narrow] <- floors
        w[!narrow] <- wide_w * (remaining / sum(wide_w))
      } else {
        w <- w * (page_width_in / sum(w))
      }
    } else {
      w <- w * (page_width_in / sum(w))
    }
    ft <- flextable::width(ft, width = w)
  }
  ft <- flextable::set_table_properties(
    ft,
    layout = "fixed",
    # split = TRUE lets a long table flow onto the next page instead of being
    # pushed whole, which is what left most of a page blank above Table 6.
    # paginate() below repeats the header row on each page it reaches.
    opts_word = list(split = TRUE, keep_with_next = TRUE)
  )

  # Repeat the header on every page a long table spills onto, and keep the
  # caption with the table. The manuscript wrapper also calls paginate(); doing
  # it here as well is idempotent and means the standalone table exports under
  # 04_tables/ get repeating headers too, which they previously did not.
  ft <- flextable::paginate(ft, init = TRUE, hdr_ftr = TRUE)


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
