"""
make_reference_doc.py
=====================
Generates reference_doc.docx - a custom Word reference document for
journal manuscript formatting in Quarto.

The default Quarto/Pandoc DOCX reference document uses Word's built-in
"Title" style at ~36 pt, producing an enormous title page. This script
creates a reference_doc.docx where every style is tuned to match the
appearance expected by medical/public-health journals (e.g. BMC series):

  Title      : 14 pt, bold, left-aligned, black
  Subtitle   : 11 pt, not bold, left-aligned, black
  Author     : 11 pt, not bold, left-aligned
  Date       : 11 pt, not bold, left-aligned
  Heading 1  : 12 pt, bold, left-aligned, no caps
  Heading 2  : 11 pt, bold italic, left-aligned
  Heading 3  : 11 pt, italic, left-aligned
  Normal     : 11 pt, Times New Roman, 1.15 spacing, justified
  Body Text  : 11 pt, Times New Roman, 1.15 spacing, justified
  First Para : same as Normal but no indent
  Caption    :  9 pt, italic, centred
  Footnote   :  9 pt, normal

Page: A4, 2.54 cm (1 inch) margins on all sides.

Usage (run once from any directory inside the project):
    python Research/02_Studies/_TEMPLATES/make_reference_doc.py

Output:
    Research/02_Studies/_TEMPLATES/reference_doc.docx

All manuscript.qmd files then reference it via:
    format:
      docx:
        reference-doc: ../../_TEMPLATES/reference_doc.docx
"""

import os
import sys
from pathlib import Path

try:
    from docx import Document
    from docx.enum.text import WD_ALIGN_PARAGRAPH
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    from docx.shared import Cm, Inches, Pt, RGBColor
except ImportError:
    print("ERROR: python-docx is not installed.")
    print("Install it with:  pip install python-docx")
    sys.exit(1)

# ---------------------------------------------------------------------------
# 0. Resolve output path
# ---------------------------------------------------------------------------
script_path = Path(__file__).resolve()
out_path = script_path.parent / "reference_doc.docx"

print(f"Output will be written to:\n  {out_path}\n")

# ---------------------------------------------------------------------------
# 1. Helper utilities
# ---------------------------------------------------------------------------

BLACK = RGBColor(0, 0, 0)
FONT = "Times New Roman"


def set_run_font(run, name=FONT, size_pt=11, bold=False, italic=False, color=BLACK):
    run.font.name = name
    run.font.size = Pt(size_pt)
    run.font.bold = bold
    run.font.italic = italic
    run.font.color.rgb = color


def style_paragraph_format(
    para_fmt,
    align=WD_ALIGN_PARAGRAPH.LEFT,
    space_before_pt=0,
    space_after_pt=6,
    line_spacing=1.15,
    first_line_indent=None,
):
    """Apply paragraph-level formatting to a ParagraphFormat object."""
    para_fmt.alignment = align
    para_fmt.space_before = Pt(space_before_pt)
    para_fmt.space_after = Pt(space_after_pt)
    para_fmt.line_spacing = line_spacing  # float = multiple
    if first_line_indent is not None:
        para_fmt.first_line_indent = first_line_indent


def apply_style(
    style,
    font_name=FONT,
    font_size_pt=11,
    bold=False,
    italic=False,
    align=WD_ALIGN_PARAGRAPH.LEFT,
    space_before_pt=0,
    space_after_pt=6,
    line_spacing=1.15,
    color=BLACK,
):
    """One-stop helper to set both font and paragraph properties on a style."""
    # --- font ---
    f = style.font
    f.name = font_name
    f.size = Pt(font_size_pt)
    f.bold = bold
    f.italic = italic
    f.color.rgb = color

    # --- paragraph ---
    pf = style.paragraph_format
    pf.alignment = align
    pf.space_before = Pt(space_before_pt)
    pf.space_after = Pt(space_after_pt)
    pf.line_spacing = line_spacing  # float multiple


def remove_caps(style):
    """Explicitly turn off ALL_CAPS on a style's rPr element."""
    rPr = style.element.get_or_add_rPr()
    caps_tag = qn("w:caps")
    # Remove any existing caps element
    for caps in rPr.findall(caps_tag):
        rPr.remove(caps)
    # Add w:caps w:val="0" to explicitly disable
    caps_el = OxmlElement("w:caps")
    caps_el.set(qn("w:val"), "0")
    rPr.append(caps_el)


def remove_small_caps(style):
    rPr = style.element.get_or_add_rPr()
    tag = qn("w:smallCaps")
    for el in rPr.findall(tag):
        rPr.remove(el)
    el = OxmlElement("w:smallCaps")
    el.set(qn("w:val"), "0")
    rPr.append(el)


def remove_paragraph_border(style):
    """Remove any paragraph border from a style, such as Word's default Title underline."""
    pPr = style.element.get_or_add_pPr()
    border_tag = qn("w:pBdr")
    for border in pPr.findall(border_tag):
        pPr.remove(border)


def set_page_margins(
    doc, top=1.0, bottom=1.0, left=1.0, right=1.0, header=0.5, footer=0.5
):
    """Set page margins in inches on the first (default) section."""
    section = doc.sections[0]
    section.top_margin = Inches(top)
    section.bottom_margin = Inches(bottom)
    section.left_margin = Inches(left)
    section.right_margin = Inches(right)
    section.header_distance = Inches(header)
    section.footer_distance = Inches(footer)


def set_a4(doc):
    section = doc.sections[0]
    section.page_width = Cm(21.0)
    section.page_height = Cm(29.7)


# ---------------------------------------------------------------------------
# 2. Create document and set page layout
# ---------------------------------------------------------------------------
doc = Document()

set_a4(doc)
set_page_margins(doc, top=1.0, bottom=1.0, left=1.0, right=1.0)

print("  Page layout: A4, 1-inch margins")

# ---------------------------------------------------------------------------
# 3. Normal / default paragraph style
# ---------------------------------------------------------------------------
normal = doc.styles["Normal"]
apply_style(
    normal,
    font_size_pt=11,
    align=WD_ALIGN_PARAGRAPH.JUSTIFY,
    space_before_pt=0,
    space_after_pt=6,
    line_spacing=1.15,
)
print("  Styled: Normal -> 11 pt, Times New Roman, justified, 1.15 spacing")

# ---------------------------------------------------------------------------
# 4. Title style  (the big fix - was ~36 pt)
# ---------------------------------------------------------------------------
title = doc.styles["Title"]
apply_style(
    title,
    font_size_pt=14,
    bold=True,
    align=WD_ALIGN_PARAGRAPH.LEFT,
    space_before_pt=0,
    space_after_pt=6,
    line_spacing=1.15,
)
remove_caps(title)
remove_small_caps(title)
remove_paragraph_border(title)
print("  Styled: Title -> 14 pt, bold, left-aligned  [OK]  (was ~36 pt)")

# ---------------------------------------------------------------------------
# 5. Subtitle style
# ---------------------------------------------------------------------------
try:
    subtitle = doc.styles["Subtitle"]
    apply_style(
        subtitle,
        font_size_pt=11,
        bold=False,
        italic=False,
        align=WD_ALIGN_PARAGRAPH.LEFT,
        space_before_pt=0,
        space_after_pt=6,
        line_spacing=1.15,
    )
    remove_caps(subtitle)
    print("  Styled: Subtitle -> 11 pt, not bold, left-aligned")
except KeyError:
    print("  NOTE: Subtitle style not found, skipping")

# ---------------------------------------------------------------------------
# 6. Author style
# ---------------------------------------------------------------------------
try:
    author = doc.styles["Author"]
    apply_style(
        author,
        font_size_pt=11,
        bold=False,
        align=WD_ALIGN_PARAGRAPH.LEFT,
        space_before_pt=0,
        space_after_pt=4,
        line_spacing=1.15,
    )
    remove_caps(author)
    print("  Styled: Author -> 11 pt, not bold, left-aligned")
except KeyError:
    print("  NOTE: Author style not found, skipping")

# ---------------------------------------------------------------------------
# 7. Date style
# ---------------------------------------------------------------------------
try:
    date_style = doc.styles["Date"]
    apply_style(
        date_style,
        font_size_pt=11,
        bold=False,
        align=WD_ALIGN_PARAGRAPH.LEFT,
        space_before_pt=0,
        space_after_pt=4,
        line_spacing=1.15,
    )
    remove_caps(date_style)
    print("  Styled: Date -> 11 pt, not bold, left-aligned")
except KeyError:
    print("  NOTE: Date style not found, skipping")

# ---------------------------------------------------------------------------
# 8. Abstract title style (if it exists)
# ---------------------------------------------------------------------------
try:
    abstract_title = doc.styles["Abstract Title"]
    apply_style(
        abstract_title,
        font_size_pt=11,
        bold=True,
        align=WD_ALIGN_PARAGRAPH.LEFT,
        space_before_pt=6,
        space_after_pt=3,
        line_spacing=1.15,
    )
    remove_caps(abstract_title)
    print("  Styled: Abstract Title -> 11 pt, bold, left-aligned")
except KeyError:
    pass  # Not present in all Pandoc reference docs

# ---------------------------------------------------------------------------
# 9. Heading 1
# ---------------------------------------------------------------------------
h1 = doc.styles["Heading 1"]
apply_style(
    h1,
    font_size_pt=12,
    bold=True,
    italic=False,
    align=WD_ALIGN_PARAGRAPH.LEFT,
    space_before_pt=12,
    space_after_pt=3,
    line_spacing=1.15,
)
remove_caps(h1)
remove_small_caps(h1)
print("  Styled: Heading 1 -> 12 pt, bold, left-aligned, no caps")

# ---------------------------------------------------------------------------
# 10. Heading 2
# ---------------------------------------------------------------------------
h2 = doc.styles["Heading 2"]
apply_style(
    h2,
    font_size_pt=11,
    bold=True,
    italic=True,
    align=WD_ALIGN_PARAGRAPH.LEFT,
    space_before_pt=8,
    space_after_pt=2,
    line_spacing=1.15,
)
remove_caps(h2)
remove_small_caps(h2)
print("  Styled: Heading 2 -> 11 pt, bold italic, left-aligned, no caps")

# ---------------------------------------------------------------------------
# 11. Heading 3
# ---------------------------------------------------------------------------
h3 = doc.styles["Heading 3"]
apply_style(
    h3,
    font_size_pt=11,
    bold=False,
    italic=True,
    align=WD_ALIGN_PARAGRAPH.LEFT,
    space_before_pt=6,
    space_after_pt=2,
    line_spacing=1.15,
)
remove_caps(h3)
remove_small_caps(h3)
print("  Styled: Heading 3 -> 11 pt, italic, left-aligned")

# ---------------------------------------------------------------------------
# 12. Heading 4 (used for subgroup labels in some manuscripts)
# ---------------------------------------------------------------------------
try:
    h4 = doc.styles["Heading 4"]
    apply_style(
        h4,
        font_size_pt=11,
        bold=True,
        italic=False,
        align=WD_ALIGN_PARAGRAPH.LEFT,
        space_before_pt=4,
        space_after_pt=2,
        line_spacing=1.15,
    )
    remove_caps(h4)
    print("  Styled: Heading 4 -> 11 pt, bold, left-aligned")
except KeyError:
    pass

# ---------------------------------------------------------------------------
# 13. Body Text (explicit body-text style, mirrors Normal)
# ---------------------------------------------------------------------------
try:
    body_text = doc.styles["Body Text"]
    apply_style(
        body_text,
        font_size_pt=11,
        align=WD_ALIGN_PARAGRAPH.JUSTIFY,
        space_before_pt=0,
        space_after_pt=6,
        line_spacing=1.15,
    )
    print("  Styled: Body Text -> 11 pt, justified, 1.15 spacing")
except KeyError:
    pass

# ---------------------------------------------------------------------------
# 14. First Paragraph (no indent variant)
# ---------------------------------------------------------------------------
try:
    first_para = doc.styles["First Paragraph"]
    apply_style(
        first_para,
        font_size_pt=11,
        align=WD_ALIGN_PARAGRAPH.JUSTIFY,
        space_before_pt=0,
        space_after_pt=6,
        line_spacing=1.15,
    )
    first_para.paragraph_format.first_line_indent = Inches(0)
    print("  Styled: First Paragraph -> 11 pt, no indent")
except KeyError:
    pass

# ---------------------------------------------------------------------------
# 15. Caption style (for table / figure captions)
# ---------------------------------------------------------------------------
try:
    caption = doc.styles["Caption"]
    apply_style(
        caption,
        font_size_pt=9,
        italic=True,
        align=WD_ALIGN_PARAGRAPH.LEFT,
        space_before_pt=3,
        space_after_pt=6,
        line_spacing=1.0,
    )
    print("  Styled: Caption -> 9 pt, italic, left-aligned")
except KeyError:
    pass

# ---------------------------------------------------------------------------
# 16. Footnote Text
# ---------------------------------------------------------------------------
try:
    footnote = doc.styles["Footnote Text"]
    apply_style(
        footnote,
        font_size_pt=9,
        align=WD_ALIGN_PARAGRAPH.LEFT,
        space_before_pt=0,
        space_after_pt=3,
        line_spacing=1.0,
    )
    print("  Styled: Footnote Text -> 9 pt")
except KeyError:
    pass

# ---------------------------------------------------------------------------
# 17. Block Text / Block Quotation (for abstract indented blocks)
# ---------------------------------------------------------------------------
for style_name in ("Block Text", "Block Quotation", "Compact"):
    try:
        bq = doc.styles[style_name]
        apply_style(
            bq,
            font_size_pt=11,
            align=WD_ALIGN_PARAGRAPH.JUSTIFY,
            space_before_pt=0,
            space_after_pt=6,
            line_spacing=1.15,
        )
        print(f"  Styled: {style_name} -> 11 pt, justified")
    except KeyError:
        pass

# ---------------------------------------------------------------------------
# 18. Add a dummy paragraph so the file is valid and non-empty
# ---------------------------------------------------------------------------
p = doc.add_paragraph()
p.style = doc.styles["Normal"]

# ---------------------------------------------------------------------------
# 19. Save
# ---------------------------------------------------------------------------
try:
    doc.save(str(out_path))
    print(f"\n[OK]  reference_doc.docx written to:\n    {out_path}")
    print("\nNext steps:")
    print("  Each manuscript.qmd already has:")
    print("    format:")
    print("      docx:")
    print("        reference-doc: ../../_TEMPLATES/reference_doc.docx")
    print("  Simply run:  quarto render manuscript.qmd")
except Exception as e:
    print(f"\n[ERROR]  Failed to save: {e}")
    sys.exit(1)
