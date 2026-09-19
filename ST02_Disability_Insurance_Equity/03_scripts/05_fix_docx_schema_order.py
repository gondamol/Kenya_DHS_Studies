"""Repair OOXML child ordering in rendered .docx files.

Word validates a document against the WordprocessingML schema when it opens it.
Several of the property containers, in particular w:pPr, w:rPr, w:tcPr, w:trPr and
w:tblPr, are declared as *sequences*: their child elements must appear in a fixed
order. The order is not cosmetic, and a document that violates it is the class of
file Word offers to open in repair mode rather than opening normally.

Renders of this manuscript violated it in two places. flextable writes paragraph
and run properties in its own order (for example w:jc before w:pStyle, and w:i
before w:b), which accounts for every violation inside a table, and the shared
reference document carries six run-property blocks in styles.xml with the same
problem. Neither changes how the document looks; both make Word ask to repair it.

There is a second, independent defect with the same symptom. A table cell is
declared to end with a block-level element, and Word requires that element to be
a paragraph: a w:tc whose last block child is a nested w:tbl is rejected. The
caption wrapper that Quarto builds around a flextable puts the real table last
inside the wrapper cell, so six cells in this manuscript ended that way, and the
file was already unopenable before the ordering pass ran. This script appends a
one-point empty paragraph after any nested table that would otherwise be last in
its cell.

Apart from those closing paragraphs the script adds nothing: no other element is
added, removed or re-attributed, and the rendered content is unchanged.

Usage:
    python 03_scripts/05_fix_docx_schema_order.py 06_manuscript/manuscript.docx [more.docx ...]

Run it after `quarto render`. It is idempotent; running it on an already-ordered
file reports zero changes and rewrites nothing.
"""

from __future__ import annotations

import re
import shutil
import sys
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET

W = "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}"

# Child order for each sequence container, from the ECMA-376 schema.
SCHEMA_ORDER = {
    "pPr": [
        "pStyle", "keepNext", "keepLines", "pageBreakBefore", "framePr", "widowControl",
        "numPr", "suppressLineNumbers", "pBdr", "shd", "tabs", "suppressAutoHyphens",
        "kinsoku", "wordWrap", "overflowPunct", "topLinePunct", "autoSpaceDE",
        "autoSpaceDN", "bidi", "adjustRightInd", "snapToGrid", "spacing", "ind",
        "contextualSpacing", "mirrorIndents", "suppressOverlap", "jc", "textDirection",
        "textAlignment", "textboxTightWrap", "outlineLvl", "divId", "cnfStyle", "rPr",
        "sectPr", "pPrChange",
    ],
    "rPr": [
        "rStyle", "rFonts", "b", "bCs", "i", "iCs", "caps", "smallCaps", "strike",
        "dstrike", "outline", "shadow", "emboss", "imprint", "noProof", "snapToGrid",
        "vanish", "webHidden", "color", "spacing", "w", "kern", "position", "sz", "szCs",
        "highlight", "u", "effect", "bdr", "shd", "fitText", "vertAlign", "rtl", "cs",
        "em", "lang", "eastAsianLayout", "specVanish", "oMath", "rPrChange",
    ],
    "tcPr": [
        "cnfStyle", "tcW", "gridSpan", "hMerge", "vMerge", "tcBorders", "shd", "noWrap",
        "tcMar", "textDirection", "tcFitText", "vAlign", "hideMark", "headers",
        "cellIns", "cellDel", "cellMerge", "tcPrChange",
    ],
    "trPr": [
        "cnfStyle", "divId", "gridBefore", "gridAfter", "wBefore", "wAfter", "cantSplit",
        "trHeight", "tblHeader", "tblCellSpacing", "jc", "hidden", "ins", "del",
        "trPrChange",
    ],
    "tblPr": [
        "tblStyle", "tblpPr", "tblOverlap", "bidiVisual", "tblStyleRowBandSize",
        "tblStyleColBandSize", "tblW", "jc", "tblCellSpacing", "tblInd", "tblBorders",
        "shd", "tblLayout", "tblCellMar", "tblLook", "tblCaption", "tblDescription",
        "tblPrChange",
    ],
}

PARTS_TO_FIX = re.compile(
    r"^word/(document|styles|numbering|footnotes|endnotes|settings)\.xml$|"
    r"^word/(header|footer)\d*\.xml$"
)


def local_name(element: ET.Element) -> str:
    return element.tag.split("}")[-1]


ROOT_TAG = re.compile(r"<([A-Za-z_][\w.-]*(?::[A-Za-z_][\w.-]*)?)((?:\s[^<>]*)?)>", re.S)


def root_start_tag(xml_text):
    """The document element's start tag, skipping the declaration, PIs and comments."""
    position = 0
    while position < len(xml_text):
        next_tag = xml_text.find("<", position)
        if next_tag == -1:
            return None
        if xml_text.startswith("<?", next_tag):
            position = xml_text.find("?>", next_tag) + 2
        elif xml_text.startswith("<!--", next_tag):
            position = xml_text.find("-->", next_tag) + 3
        elif xml_text.startswith("<!", next_tag):
            position = xml_text.find(">", next_tag) + 1
        else:
            return ROOT_TAG.match(xml_text, next_tag)
    return None


def declared_prefixes(start_tag):
    return dict(re.findall(r'xmlns:([A-Za-z0-9_.-]+)="([^"]+)"', start_tag))


def restore_namespace_declarations(original, rewritten):
    """Put back xmlns declarations that ElementTree dropped as unused.

    ElementTree emits a namespace declaration only for namespaces some node in
    the tree actually uses. register_namespace() fixes which prefix is chosen,
    not whether a declaration is written at all. styles.xml declares w14 and,
    after Quarto's render, uses it nowhere, so the declaration was dropped while
    the literal attribute mc:Ignorable="w14" survived. A prefix named in
    mc:Ignorable that resolves to no in-scope declaration is invalid under the
    Markup Compatibility part of ECMA-376, and Word answers it by offering to
    repair the file: the same symptom this script exists to remove,
    reintroduced by the removal.

    Rather than special-casing w14, every declaration carried by the original
    document element is restored.
    """
    original_root = root_start_tag(original)
    new_root = root_start_tag(rewritten)
    if original_root is None or new_root is None:
        return rewritten

    before = declared_prefixes(original_root.group(0))
    after = declared_prefixes(new_root.group(0))
    missing = {prefix: uri for prefix, uri in before.items() if prefix not in after}
    if not missing:
        return rewritten

    additions = "".join(' xmlns:%s="%s"' % (p, u) for p, u in missing.items())
    insert_at = new_root.end() - 1
    if rewritten[insert_at - 1] == "/":
        insert_at -= 1
    return rewritten[:insert_at] + additions + rewritten[insert_at:]


def check_markup_compatibility(xml_text, part_name):
    """Fail loudly if an mc: attribute names a prefix that is not in scope."""
    root = root_start_tag(xml_text)
    if root is None:
        return
    declared = set(declared_prefixes(root.group(0)))
    for attribute in ("Ignorable", "ProcessContent", "MustUnderstand"):
        found = re.search(r'mc:%s\s*=\s*"([^"]*)"' % attribute, root.group(0))
        if not found:
            continue
        undeclared = [x for x in found.group(1).split() if x not in declared]
        if undeclared:
            raise RuntimeError(
                "%s: mc:%s names undeclared prefix(es) %s; Word would ask to "
                "repair this file." % (part_name, attribute, ", ".join(undeclared))
            )


def register_namespaces(xml_text: str) -> None:
    """Keep the original prefixes, so attributes such as mc:Ignorable stay valid."""
    for prefix, uri in re.findall(r'xmlns:([A-Za-z0-9_.-]+)="([^"]+)"', xml_text):
        try:
            ET.register_namespace(prefix, uri)
        except ValueError:
            pass


BLOCK_LEVEL = {W + "p", W + "tbl"}


def close_cells_with_paragraph(root: ET.Element) -> int:
    """Give every table cell a paragraph as its last block-level child.

    ECMA-376 models a table cell's content as block-level elements ending in a
    paragraph. A cell whose last block child is a nested table is invalid, and
    Word's response is the repair prompt rather than a specific complaint. The
    inserted paragraph is set to one point with no spacing after it, so it
    occupies as little vertical space as a paragraph can.
    """
    added = 0
    for cell in root.iter(W + "tc"):
        children = list(cell)
        last_block = None
        for position, child in enumerate(children):
            if child.tag in BLOCK_LEVEL:
                last_block = position
        if last_block is None:
            insert_at = len(children)
        elif children[last_block].tag == W + "tbl":
            insert_at = last_block + 1
        else:
            continue

        paragraph = ET.Element(W + "p")
        properties = ET.SubElement(paragraph, W + "pPr")
        spacing = ET.SubElement(properties, W + "spacing")
        spacing.set(W + "after", "0")
        spacing.set(W + "line", "240")
        spacing.set(W + "lineRule", "auto")
        run_properties = ET.SubElement(properties, W + "rPr")
        size = ET.SubElement(run_properties, W + "sz")
        size.set(W + "val", "2")
        cell.insert(insert_at, paragraph)
        added += 1
    return added


def reorder_part(xml_bytes: bytes) -> tuple[bytes, int, int]:
    xml_text = xml_bytes.decode("utf-8")
    register_namespaces(xml_text)
    root = ET.fromstring(xml_text)

    cells_closed = close_cells_with_paragraph(root)
    changes = 0
    for container, order in SCHEMA_ORDER.items():
        index = {name: position for position, name in enumerate(order)}
        for element in root.iter(W + container):
            children = list(element)
            if len(children) < 2:
                continue
            # Unknown children keep their relative position at the end, rather than
            # being dropped or moved ahead of elements the schema does place.
            def sort_key(pair):
                position, child = pair
                return (index.get(local_name(child), len(order)), position)

            ordered = [child for _, child in sorted(enumerate(children), key=sort_key)]
            if ordered != children:
                element[:] = ordered
                changes += 1

    if changes == 0 and cells_closed == 0:
        return xml_bytes, 0, 0

    body = ET.tostring(root, encoding="utf-8", xml_declaration=False).decode("utf-8")
    body = restore_namespace_declarations(xml_text, body)
    declaration = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n'
    return (declaration + body).encode("utf-8"), changes, cells_closed


def fix_docx(path: Path) -> tuple[int, int]:
    source = zipfile.ZipFile(path)
    entries = [(item, source.read(item.filename)) for item in source.infolist()]
    source.close()

    total_changes = 0
    total_cells = 0
    rebuilt = []
    for item, data in entries:
        if PARTS_TO_FIX.match(item.filename):
            data, changes, cells_closed = reorder_part(data)
            total_changes += changes
            total_cells += cells_closed
        if item.filename.endswith(".xml"):
            check_markup_compatibility(data.decode("utf-8", "replace"), item.filename)
        rebuilt.append((item, data))

    if total_changes == 0 and total_cells == 0:
        return 0, 0

    backup = path.with_suffix(path.suffix + ".preorder")
    shutil.copy2(path, backup)
    with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as target:
        for item, data in rebuilt:
            target.writestr(item, data)
    backup.unlink()
    verify_package(path)
    return total_changes, total_cells


def verify_package(path: Path) -> None:
    """Re-read what was written and assert the invariants this script exists for.

    A repair script that can quietly emit an unopenable file is worse than no
    repair script, because the render log then reports success. Both defects
    seen in this project are cheap to test for, so they are tested for on the
    bytes that were actually written.
    """
    with zipfile.ZipFile(path) as package:
        for name in package.namelist():
            data = package.read(name)
            if name.endswith(".xml"):
                check_markup_compatibility(data.decode("utf-8", "replace"), name)
            if not PARTS_TO_FIX.match(name):
                continue
            root = ET.fromstring(data)
            for cell in root.iter(W + "tc"):
                blocks = [c for c in cell if c.tag in BLOCK_LEVEL]
                if blocks and blocks[-1].tag == W + "tbl":
                    raise RuntimeError(
                        f"{path.name}/{name}: a table cell still ends with a nested "
                        "table; Word would refuse to open this file."
                    )
            for container, order in SCHEMA_ORDER.items():
                for element in root.iter(W + container):
                    positions = [
                        order.index(local_name(child))
                        for child in element
                        if local_name(child) in order
                    ]
                    if positions != sorted(positions):
                        raise RuntimeError(
                            f"{path.name}/{name}: w:{container} children are still "
                            "out of schema order."
                        )


def main(argv: list[str]) -> int:
    if len(argv) < 2:
        print(__doc__)
        return 2
    for name in argv[1:]:
        path = Path(name)
        if not path.exists():
            print(f"missing: {path}")
            return 1
        changes, cells_closed = fix_docx(path)
        print(
            f"{path.name}: reordered {changes} property blocks, "
            f"closed {cells_closed} table cells with a paragraph"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
