# =============================================================================
# make_reference_doc.R
# =============================================================================
# Generates a custom Word reference document for journal manuscript formatting.
#
# The default Quarto/Pandoc DOCX reference document uses Word's built-in
# "Title" style at ~36pt, which produces an enormous title page. This script
# creates a reference_doc.docx where:
#   - Title style  : 14 pt, bold, left-aligned (journal standard)
#   - Author style : 11 pt, normal weight, left-aligned
#   - Abstract heading: 11 pt, bold
#   - Heading 1    : 12 pt, bold (section headings)
#   - Heading 2    : 11 pt, bold italic
#   - Heading 3    : 11 pt, italic
#   - Body Text    : 11 pt, Times New Roman, 1.5 line spacing
#   - Margins      : 2.54 cm all sides (standard A4)
#
# Usage (run once from the project root or any manuscript folder):
#   Rscript Research/02_Studies/_TEMPLATES/make_reference_doc.R
#
# The output file is written to:
#   Research/02_Studies/_TEMPLATES/reference_doc.docx
#
# All manuscript.qmd files reference this via:
#   format:
#     docx:
#       reference-doc: ../../_TEMPLATES/reference_doc.docx
# =============================================================================

# ── 0. Dependencies ───────────────────────────────────────────────────────────
if (!requireNamespace("officer",  quietly = TRUE)) install.packages("officer")
if (!requireNamespace("xml2",     quietly = TRUE)) install.packages("xml2")
if (!requireNamespace("here",     quietly = TRUE)) install.packages("here")

library(officer)
library(xml2)

# ── 1. Output path ────────────────────────────────────────────────────────────
# Resolve output path relative to this script's location.
# Works whether script is run from project root or the _TEMPLATES folder.
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE)),
  error = function(e) {
    # When run interactively or via Rscript without sys.frame, fall back to
    # the _TEMPLATES directory relative to the working directory.
    candidates <- c(
      file.path("Research", "02_Studies", "_TEMPLATES"),
      file.path("02_Studies", "_TEMPLATES"),
      "_TEMPLATES",
      "."
    )
    for (d in candidates) {
      if (dir.exists(d)) return(normalizePath(d))
    }
    normalizePath(".")
  }
)

out_path <- file.path(script_dir, "reference_doc.docx")
message("Output will be written to: ", out_path)

# ── 2. Create a blank document as the base ────────────────────────────────────
doc <- read_docx()

# ── 3. Helper: find a style node by styleId ───────────────────────────────────
find_style_node <- function(doc, style_id) {
  styles_xml <- doc$styles$styles
  xml_find_first(
    styles_xml,
    paste0(".//w:style[@w:styleId='", style_id, "']")
  )
}

# ── 4. Helper: set w:sz and w:szCs within a style's w:rPr ────────────────────
# half_pt: font size in half-points (e.g. 14pt = 28 half-points)
set_style_font_size <- function(style_node, half_pt) {
  rPr <- xml_find_first(style_node, ".//w:rPr")
  if (inherits(rPr, "xml_missing")) {
    # Create rPr if it doesn't exist
    rPr <- xml_add_child(style_node, "w:rPr")
  }

  # w:sz
  sz <- xml_find_first(rPr, "w:sz")
  if (inherits(sz, "xml_missing")) {
    sz <- xml_add_child(rPr, "w:sz")
  }
  xml_set_attr(sz, "w:val", as.character(half_pt))

  # w:szCs (complex script size — must match)
  szCs <- xml_find_first(rPr, "w:szCs")
  if (inherits(szCs, "xml_missing")) {
    szCs <- xml_add_child(rPr, "w:szCs")
  }
  xml_set_attr(szCs, "w:val", as.character(half_pt))

  invisible(style_node)
}

# ── 5. Helper: set paragraph alignment within a style's w:pPr ────────────────
# align: "left", "center", "right", "both"
set_style_alignment <- function(style_node, align) {
  pPr <- xml_find_first(style_node, "w:pPr")
  if (inherits(pPr, "xml_missing")) {
    pPr <- xml_add_child(style_node, "w:pPr")
  }
  jc <- xml_find_first(pPr, "w:jc")
  if (inherits(jc, "xml_missing")) {
    jc <- xml_add_child(pPr, "w:jc")
  }
  xml_set_attr(jc, "w:val", align)
  invisible(style_node)
}

# ── 6. Helper: set bold on/off within a style's w:rPr ────────────────────────
set_style_bold <- function(style_node, bold = TRUE) {
  rPr <- xml_find_first(style_node, ".//w:rPr")
  if (inherits(rPr, "xml_missing")) {
    rPr <- xml_add_child(style_node, "w:rPr")
  }
  b <- xml_find_first(rPr, "w:b")
  if (bold) {
    if (inherits(b, "xml_missing")) xml_add_child(rPr, "w:b")
  } else {
    if (!inherits(b, "xml_missing")) xml_remove(b)
    # Explicitly suppress bold inheritance
    bOff <- xml_find_first(rPr, "w:b[@w:val='false']")
    if (inherits(bOff, "xml_missing")) {
      b_node <- xml_add_child(rPr, "w:b")
      xml_set_attr(b_node, "w:val", "false")
    }
  }
  invisible(style_node)
}

# ── 7. Helper: set line spacing within a style's w:pPr ───────────────────────
# line: spacing in 240ths of a line (240 = single, 360 = 1.5, 480 = double)
set_style_line_spacing <- function(style_node, line = 360, line_rule = "auto") {
  pPr <- xml_find_first(style_node, "w:pPr")
  if (inherits(pPr, "xml_missing")) {
    pPr <- xml_add_child(style_node, "w:pPr")
  }
  spacing <- xml_find_first(pPr, "w:spacing")
  if (inherits(spacing, "xml_missing")) {
    spacing <- xml_add_child(pPr, "w:spacing")
  }
  xml_set_attr(spacing, "w:line",      as.character(line))
  xml_set_attr(spacing, "w:lineRule",  line_rule)
  invisible(style_node)
}

# ── 8. Helper: set font name within a style's w:rPr ──────────────────────────
set_style_font <- function(style_node, font_name) {
  rPr <- xml_find_first(style_node, ".//w:rPr")
  if (inherits(rPr, "xml_missing")) {
    rPr <- xml_add_child(style_node, "w:rPr")
  }
  rFonts <- xml_find_first(rPr, "w:rFonts")
  if (inherits(rFonts, "xml_missing")) {
    rFonts <- xml_add_child(rPr, "w:rFonts")
  }
  xml_set_attr(rFonts, "w:ascii",    font_name)
  xml_set_attr(rFonts, "w:hAnsi",   font_name)
  xml_set_attr(rFonts, "w:eastAsia", font_name)
  invisible(style_node)
}

# Remove any paragraph border from a style, such as Word's default Title underline
remove_style_paragraph_border <- function(style_node) {
  pPr <- xml_find_first(style_node, "w:pPr")
  if (inherits(pPr, "xml_missing")) {
    pPr <- xml_add_child(style_node, "w:pPr")
  }
  pBdr <- xml_find_first(pPr, "w:pBdr")
  if (!inherits(pBdr, "xml_missing")) {
    xml_remove(pBdr)
  }
  invisible(style_node)
}

# ── 9. Apply style modifications ─────────────────────────────────────────────

# --- Title style: 14pt, bold, left-aligned -----------------------------------
title_node <- find_style_node(doc, "Title")
if (!inherits(title_node, "xml_missing")) {
  set_style_font_size(title_node, 28L)   # 14pt = 28 half-points
  set_style_bold(title_node, TRUE)
  set_style_alignment(title_node, "left")
  set_style_font(title_node, "Times New Roman")
  remove_style_paragraph_border(title_node)
  message("  Modified: Title -> 14pt, bold, left-aligned")
} else {
  message("  WARNING: Title style not found — using default")
}

# --- Subtitle style: 12pt, not bold, left-aligned ----------------------------
subtitle_node <- find_style_node(doc, "Subtitle")
if (!inherits(subtitle_node, "xml_missing")) {
  set_style_font_size(subtitle_node, 24L)  # 12pt
  set_style_bold(subtitle_node, FALSE)
  set_style_alignment(subtitle_node, "left")
  set_style_font(subtitle_node, "Times New Roman")
  message("  Modified: Subtitle -> 12pt, no bold, left-aligned")
}

# --- Author style: 11pt, not bold, left-aligned ------------------------------
author_node <- find_style_node(doc, "Author")
if (!inherits(author_node, "xml_missing")) {
  set_style_font_size(author_node, 22L)  # 11pt
  set_style_bold(author_node, FALSE)
  set_style_alignment(author_node, "left")
  set_style_font(author_node, "Times New Roman")
  message("  Modified: Author -> 11pt, no bold, left-aligned")
}

# --- Date style: 11pt, not bold, left-aligned --------------------------------
date_node <- find_style_node(doc, "Date")
if (!inherits(date_node, "xml_missing")) {
  set_style_font_size(date_node, 22L)
  set_style_bold(date_node, FALSE)
  set_style_alignment(date_node, "left")
  set_style_font(date_node, "Times New Roman")
  message("  Modified: Date -> 11pt, no bold, left-aligned")
}

# --- Heading 1: 12pt, bold, left-aligned, no extra space before --------------
h1_node <- find_style_node(doc, "heading1")
if (inherits(h1_node, "xml_missing")) h1_node <- find_style_node(doc, "Heading1")
if (!inherits(h1_node, "xml_missing")) {
  set_style_font_size(h1_node, 24L)  # 12pt
  set_style_bold(h1_node, TRUE)
  set_style_alignment(h1_node, "left")
  set_style_font(h1_node, "Times New Roman")
  message("  Modified: Heading 1 -> 12pt, bold")
}

# --- Heading 2: 11pt, bold italic, left-aligned ------------------------------
h2_node <- find_style_node(doc, "heading2")
if (inherits(h2_node, "xml_missing")) h2_node <- find_style_node(doc, "Heading2")
if (!inherits(h2_node, "xml_missing")) {
  set_style_font_size(h2_node, 22L)  # 11pt
  set_style_bold(h2_node, TRUE)
  set_style_alignment(h2_node, "left")
  set_style_font(h2_node, "Times New Roman")
  message("  Modified: Heading 2 -> 11pt, bold")
}

# --- Heading 3: 11pt, italic, left-aligned -----------------------------------
h3_node <- find_style_node(doc, "heading3")
if (inherits(h3_node, "xml_missing")) h3_node <- find_style_node(doc, "Heading3")
if (!inherits(h3_node, "xml_missing")) {
  set_style_font_size(h3_node, 22L)  # 11pt
  set_style_alignment(h3_node, "left")
  set_style_font(h3_node, "Times New Roman")
  message("  Modified: Heading 3 -> 11pt")
}

# --- Normal / Body Text: 11pt, Times New Roman, 1.5 spacing ------------------
normal_node <- find_style_node(doc, "Normal")
if (!inherits(normal_node, "xml_missing")) {
  set_style_font_size(normal_node, 22L)  # 11pt
  set_style_font(normal_node, "Times New Roman")
  set_style_line_spacing(normal_node, line = 360L, line_rule = "auto")  # 1.5
  set_style_alignment(normal_node, "both")  # justified
  message("  Modified: Normal -> 11pt, Times New Roman, 1.5 spacing, justified")
}

# ── 10. Set document-level page margins (A4, 2.54 cm = 1440 twips) ───────────
# officer stores sections in the document body
tryCatch({
  doc <- body_set_default_section(
    doc,
    prop_section(
      page_size   = page_size(width = 8.27, height = 11.69, orient = "portrait"),
      page_margins = page_mar(
        top    = 1,   # inches (~ 2.54 cm)
        bottom = 1,
        left   = 1,
        right  = 1,
        header = 0.5,
        footer = 0.5
      )
    )
  )
  message("  Set: A4 page with 1-inch margins")
}, error = function(e) {
  message("  NOTE: Could not set page margins via body_set_default_section — ",
          "margins will use Pandoc defaults. Error: ", conditionMessage(e))
})

# ── 11. Write the output file ─────────────────────────────────────────────────
tryCatch({
  print(doc, target = out_path)
  message("\n✅  reference_doc.docx written to:\n    ", out_path)
  message("\nNext steps:")
  message("  1. In each manuscript.qmd, add under format: docx:")
  message("       reference-doc: ../../_TEMPLATES/reference_doc.docx")
  message("  2. Render: quarto render manuscript.qmd --to docx")
}, error = function(e) {
  message("\n❌  Failed to write reference_doc.docx: ", conditionMessage(e))
  message("    Check that the output directory exists and is writeable.")
  stop(e)
})
