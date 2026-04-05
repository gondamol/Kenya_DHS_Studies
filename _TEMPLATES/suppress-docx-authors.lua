-- suppress-docx-authors.lua
-- =============================================================================
-- Pandoc / Quarto Lua filter
--
-- Problem: When a Quarto manuscript has both:
--   (a) structured `author:` metadata in the YAML front matter, AND
--   (b) an explicit author / affiliation block written in the document body,
-- Pandoc renders the YAML authors a second time at the top of the DOCX
-- (using Word's "Author" paragraph style), producing a duplicate author line.
--
-- Fix: For DOCX output only, nil out the `author` and `date` metadata fields
-- before Pandoc builds the title block. The title (and abstract, keywords,
-- etc.) are left intact so they still render correctly.
-- The body-level author block — with superscript affiliation numbers,
-- affiliation addresses, and correspondence line — provides the visible
-- author information in the final document.
--
-- HTML and PDF outputs are unaffected: this filter is a no-op for any
-- format other than docx / docx-based formats.
-- =============================================================================

local author_blocks = nil

local function append_markdown_inlines(target, text)
  if text == nil or text == "" then
    return
  end

  local has_leading_space = text:match("^%s") ~= nil
  local has_trailing_space = text:match("%s$") ~= nil
  local trimmed = text:gsub("^%s+", ""):gsub("%s+$", "")

  if has_leading_space and #target > 0 then
    target:insert(pandoc.Space())
  end

  if trimmed == "" then
    if has_trailing_space then
      target:insert(pandoc.Space())
    end
    return
  end

  local parsed = pandoc.read(trimmed, "markdown").blocks

  for _, block in ipairs(parsed) do
    if block.t == "Para" or block.t == "Plain" then
      for _, inline in ipairs(block.content) do
        target:insert(inline)
      end
    end
  end

  if has_trailing_space then
    target:insert(pandoc.Space())
  end
end

local function parse_author_line(line)
  local inlines = pandoc.List()
  local pos = 1

  while true do
    local start_pos, end_pos, sup_text = line:find("<sup>(.-)</sup>", pos)

    if start_pos == nil then
      append_markdown_inlines(inlines, line:sub(pos))
      break
    end

    append_markdown_inlines(inlines, line:sub(pos, start_pos - 1))
    inlines:insert(pandoc.Superscript({ pandoc.Str(sup_text) }))
    pos = end_pos + 1
  end

  return pandoc.Para(inlines)
end

local function load_docx_author_block()
  local handle = io.open("docx_author_block.md", "r")
  if handle == nil then
    return nil
  end

  local contents = handle:read("*a")
  handle:close()

  if contents == nil or not contents:match("%S") then
    return nil
  end

  local blocks = pandoc.List()

  contents = contents:gsub("\r\n", "\n")

  for line in contents:gmatch("[^\n]+") do
    if line:match("%S") then
      blocks:insert(parse_author_line(line))
    end
  end

  return blocks
end

function Meta(meta)
  -- Only act when the output format is DOCX
  if FORMAT:match("docx") then
    -- Remove the author list so Pandoc does not render the "Author" style block
    meta.author = nil
    -- Remove the date so the "Date" style block is also suppressed
    meta.date   = nil
    -- If a local author-block file exists, cache it for insertion before body text
    author_blocks = load_docx_author_block()
  end
  return meta
end

function Pandoc(doc)
  if FORMAT:match("docx") and author_blocks ~= nil then
    local merged_blocks = pandoc.List()

    for _, block in ipairs(author_blocks) do
      merged_blocks:insert(block)
    end

    for _, block in ipairs(doc.blocks) do
      merged_blocks:insert(block)
    end

    doc.blocks = merged_blocks
  end

  return doc
end
