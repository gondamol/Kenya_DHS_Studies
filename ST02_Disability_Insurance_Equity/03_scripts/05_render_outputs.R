# Study: ST02_Disability_Insurance_Equity
# Script: 05_render_outputs.R
# Author: Nichodemus Werre Amollo
# Date: 2026-09-19
# Purpose: Render the manuscript and its companion documents, then repair the
#          OOXML defects the render leaves behind. Two of them make Word refuse
#          the file outright: a table cell whose last block-level child is a
#          nested table, which is what Quarto's flextable caption wrapper
#          produces, and property containers such as w:pPr and w:rPr whose
#          children are out of schema order. The repair is therefore part of
#          producing a submittable document and not an optional extra, and this
#          script fails loudly rather than reporting success without it.

message("=== SECTION 5: Render and repair documents ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

quarto_bin <- Sys.which("quarto")
if (!nzchar(quarto_bin)) {
  candidates <- c(
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin", "quarto.exe"),
    "C:/Program Files/Quarto/bin/quarto.exe",
    "/usr/local/bin/quarto"
  )
  quarto_bin <- candidates[file.exists(candidates)][1]
}
if (is.na(quarto_bin) || !nzchar(quarto_bin)) {
  stop("Quarto was not found. Install it or put it on PATH before rendering.")
}

documents <- c("manuscript.qmd", "supplementary_tables.qmd", "strobe_checklist.qmd", "cover_letter.qmd")

# The work is wrapped in a function so that on.exit() can restore the working
# directory. At the top level of a sourced script it cannot: source() evaluates
# each expression inside withVisible(), so a handler registered there fires as
# soon as that one expression finishes. Inside a real function it behaves
# normally and runs on an error as well as on a clean return, which matters
# because a failed render used to leave the session in the manuscript directory.
render_and_repair <- function() {
  # The document names are relative to the manuscript directory, and so are the
  # _quarto.yml and the CSL path inside them. Rendering therefore has to happen
  # with that directory as the working directory: called from the study root, as
  # the workflow runner does, quarto was simply not finding the .qmd and the
  # script reported a render failure with no other explanation.
  previous_wd <- setwd(paths$manuscript_dir)
  on.exit(setwd(previous_wd), add = TRUE)

  for (document in documents) {
    append_log(paste("Rendering", document), also_message = TRUE)
    status <- system2(quarto_bin, c("render", shQuote(document)),
                      stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(status, "status")) && attr(status, "status") != 0) {
      # Quarto's own diagnosis is the only thing that makes a render failure
      # actionable, and it was previously discarded with the captured output.
      for (line in utils::tail(as.character(status), 40)) {
        append_log(paste("  quarto:", line), also_message = TRUE)
      }
      append_error("SECTION 5", paste("quarto render failed for", document))
      stop("quarto render failed for ", document)
    }
  }

  python_bin <- Sys.which("python")
  if (!nzchar(python_bin)) python_bin <- Sys.which("python3")
  if (!nzchar(python_bin)) {
    # Not a warning. Without the repair the rendered .docx files do not open in
    # Word at all, so continuing would report a successful render and hand over
    # documents that cannot be submitted.
    append_error("SECTION 5", "Python was not found, so the DOCX repair step could not run")
    stop("Python was not found. The rendered .docx files still carry the defects ",
         "that make Word refuse to open them, so the render is not usable. ",
         "Install Python or put it on PATH and run this script again.")
  }

  rendered <- list.files(paths$manuscript_dir, pattern = "[.]docx$", full.names = TRUE)
  rendered <- rendered[!grepl("_V0[.]docx$", rendered)]
  # Word writes a "~$name.docx" owner file beside any document it currently has
  # open. It is not a document, and handing it to the schema fixer either errors
  # or writes nonsense over it, so it is excluded here.
  rendered <- rendered[!grepl("^~[$]", basename(rendered))]
  fixer <- file.path(paths$scripts_dir, "05_fix_docx_schema_order.py")

  report <- system2(python_bin, c(shQuote(fixer), shQuote(rendered)), stdout = TRUE, stderr = TRUE)
  for (line in report) append_log(line, also_message = TRUE)

  # The fixer re-reads the bytes it wrote and exits non-zero if any package
  # still violates an invariant. Logging that output without checking the status
  # was how a failed repair could pass for a successful one.
  fixer_status <- attr(report, "status")
  if (!is.null(fixer_status) && fixer_status != 0) {
    append_error("SECTION 5", "the DOCX repair step failed; the rendered documents are not submittable")
    stop("05_fix_docx_schema_order.py exited with status ", fixer_status,
         ". The rendered .docx files have not been repaired and Word will refuse them.")
  }

  invisible(TRUE)
}

render_and_repair()

message("=== SECTION 5 COMPLETE ===")
