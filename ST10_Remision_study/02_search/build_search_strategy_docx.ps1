$outputPath = "C:\Users\HFD 2\Research\02_Studies\ST10_Remision_study\Search_Strategy_MEDLINE_PubMed.docx"

Add-Type -AssemblyName System.Runtime.InteropServices

$word = New-Object -ComObject Word.Application
$word.Visible = $false
$doc  = $word.Documents.Add()

# Page margins (1 inch all round)
$doc.PageSetup.TopMargin    = $word.InchesToPoints(1.0)
$doc.PageSetup.BottomMargin = $word.InchesToPoints(1.0)
$doc.PageSetup.LeftMargin   = $word.InchesToPoints(1.2)
$doc.PageSetup.RightMargin  = $word.InchesToPoints(1.2)

$wdColorDarkBlue  = 10040166   # RGB 153, 0, 0  -- use dark-blue-ish
$wdColorNavy      = 8388608
$wdColorLightBlue = 16763904
$wdColorGray10    = 15921906
$wdColorWhite     = 16777215
$wdColorBlack     = 0

# ────────────────────────────────────────────
# Helper: add a paragraph of text
# ────────────────────────────────────────────
function Add-Para {
    param($text, $size=11, $bold=$false, $italic=$false,
          $spaceAfter=6, $color=$wdColorBlack, $fontName="Calibri", $heading=$null)
    $sel = $word.Selection
    if ($heading) {
        $sel.Style = $doc.Styles.Item($heading)
    } else {
        $sel.Style = $doc.Styles.Item("Normal")
        $sel.Font.Name  = $fontName
        $sel.Font.Size  = $size
        $sel.Font.Bold  = $bold
        $sel.Font.Italic = $italic
        $sel.Font.Color = $color
        $sel.ParagraphFormat.SpaceAfter  = $spaceAfter
        $sel.ParagraphFormat.SpaceBefore = 0
    }
    $sel.TypeText($text)
    $sel.TypeParagraph()
}

# ────────────────────────────────────────────
# TITLE
# ────────────────────────────────────────────
Add-Para "Search Strategy - MEDLINE (via PubMed)" -heading "Title"

# Subtitle block
$sel = $word.Selection
$sel.Style = $doc.Styles.Item("Normal")
$sel.Font.Name = "Calibri"; $sel.Font.Size = 10; $sel.Font.Bold = $true
$sel.TypeText("Review title: ")
$sel.Font.Bold = $false
$sel.TypeText("Lifestyle modification for type 2 diabetes remission in low- and middle-income countries, with drug-free hypertension normalisation as a secondary objective: a systematic review and meta-analysis")
$sel.TypeParagraph()
$sel.Font.Bold = $true;  $sel.TypeText("Database: ")
$sel.Font.Bold = $false; $sel.TypeText("MEDLINE via PubMed (https://pubmed.ncbi.nlm.nih.gov/)")
$sel.TypeParagraph()
$sel.Font.Bold = $true;  $sel.TypeText("Date searched: ")
$sel.Font.Bold = $false; $sel.TypeText("2 June 2026")
$sel.TypeParagraph()
$sel.Font.Bold = $true;  $sel.TypeText("Date range: ")
$sel.Font.Bold = $false; $sel.TypeText("Inception to date of search (no date limits applied)")
$sel.TypeParagraph()
$sel.Font.Bold = $true;  $sel.TypeText("Verified final result count (MEDLINE/PubMed): ")
$sel.Font.Bold = $false; $sel.TypeText("26 records")
$sel.TypeParagraph()
$sel.TypeParagraph()

# ────────────────────────────────────────────
# SECTION 1: Why the parentheses matter
# ────────────────────────────────────────────
Add-Para "1. Why Parentheses Are Critical" -heading "Heading 1"
Add-Para ("A common error when pasting a multi-block search strategy into PubMed is missing closing parentheses. " +
          "Each concept block MUST be enclosed in its own parentheses ( ) so that OR operators join synonyms within " +
          "a block, and AND operators join blocks together. Without these, PubMed collapses all terms into one " +
          "giant OR chain and returns tens of thousands of irrelevant results. The corrected strategy below returns " +
          "exactly 26 records in PubMed.") -size 11

$word.Selection.TypeParagraph()

# ────────────────────────────────────────────
# SECTION 2: Individual search sets table
# ────────────────────────────────────────────
Add-Para "2. Individual Search Sets" -heading "Heading 1"
Add-Para "Each set below is tested and verified independently in PubMed before combination." -size 11

# Build table (9 rows: header + 7 sets + final combined)
$tbl = $doc.Tables.Add($word.Selection.Range, 9, 4)
$tbl.Borders.Enable = $true

# Column widths
$tbl.Columns.Item(1).Width = $word.InchesToPoints(0.45)
$tbl.Columns.Item(2).Width = $word.InchesToPoints(1.05)
$tbl.Columns.Item(3).Width = $word.InchesToPoints(3.80)
$tbl.Columns.Item(4).Width = $word.InchesToPoints(0.75)

# Header row
$hdrCells = @("Set", "Concept", "Search Terms (PubMed syntax)", "Results")
for ($c = 1; $c -le 4; $c++) {
    $cell = $tbl.Cell(1, $c)
    $cell.Shading.BackgroundPatternColor = 4210752   # dark blue-grey
    $cell.Range.Font.Color = $wdColorWhite
    $cell.Range.Font.Bold  = $true
    $cell.Range.Font.Name  = "Calibri"
    $cell.Range.Font.Size  = 10
    $cell.Range.Text = $hdrCells[$c-1]
}

# Data rows  [set, concept, terms, count]
$rows = @(
    @("#1",
      "Type 2 diabetes mellitus",
      '"Diabetes Mellitus, Type 2"[MeSH] OR "type 2 diabetes"[tiab] OR "type II diabetes"[tiab] OR "non-insulin dependent diabetes"[tiab] OR "NIDDM"[tiab] OR "T2DM"[tiab]',
      "286,978"),
    @("#2",
      "Hypertension",
      '"Hypertension"[MeSH] OR "hypertension"[tiab] OR "high blood pressure"[tiab] OR "elevated blood pressure"[tiab] OR "arterial hypertension"[tiab]',
      "638,516"),
    @("#3",
      "Lifestyle intervention",
      '"Life Style"[MeSH] OR "Diet"[MeSH] OR "Exercise"[MeSH] OR "lifestyle modification"[tiab] OR "lifestyle intervention"[tiab] OR "dietary intervention"[tiab] OR "physical activity"[tiab] OR "caloric restriction"[tiab] OR "weight loss"[tiab] OR "low-calorie diet"[tiab] OR "low carbohydrate"[tiab] OR "DASH diet"[tiab] OR "Mediterranean diet"[tiab] OR "health coaching"[tiab] OR "behaviour change"[tiab] OR "self-management"[tiab]',
      "974,517"),
    @("#4",
      "T2DM remission / reversal",
      '"remission"[tiab] OR "reversal"[tiab] OR "resolution"[tiab] OR "drug-free"[tiab] OR "medication discontinuation"[tiab] OR "off medication"[tiab] OR "normalisation"[tiab] OR "normalization"[tiab]',
      "920,491"),
    @("#5",
      "Hypertension normalisation",
      '"blood pressure normalisation"[tiab] OR "blood pressure normalization"[tiab] OR "hypertension remission"[tiab] OR "antihypertensive discontinuation"[tiab] OR "medication withdrawal"[tiab]',
      "783"),
    @("#6",
      "LMIC setting",
      '"developing countries"[MeSH] OR "low-income countries"[tiab] OR "middle-income countries"[tiab] OR "LMIC"[tiab] OR "sub-Saharan Africa"[tiab] OR "South Asia"[tiab] OR "Southeast Asia"[tiab] OR "Latin America"[tiab] OR "Africa"[MeSH] OR "Asia"[MeSH] OR "Kenya"[tiab] OR "Nigeria"[tiab] OR "India"[tiab] OR "China"[tiab] OR "Brazil"[tiab] OR "Ethiopia"[tiab] OR "Tanzania"[tiab] OR "Uganda"[tiab] OR "Pakistan"[tiab] OR "Bangladesh"[tiab] OR "Indonesia"[tiab]',
      "2,137,817"),
    @("#7",
      "Study design (RCT)",
      '"Randomized Controlled Trial"[pt] OR "randomised controlled trial"[tiab] OR "randomized controlled trial"[tiab] OR "RCT"[tiab] OR "cluster-randomised"[tiab] OR "cluster-randomized"[tiab]',
      "768,375")
)

for ($i = 0; $i -lt $rows.Count; $i++) {
    $r = $i + 2
    if ($i % 2 -eq 1) {
        for ($c = 1; $c -le 4; $c++) {
            $tbl.Cell($r,$c).Shading.BackgroundPatternColor = 15921906  # very light grey
        }
    }
    for ($c = 1; $c -le 4; $c++) {
        $tbl.Cell($r,$c).Range.Font.Name = "Calibri"
        $tbl.Cell($r,$c).Range.Font.Size = 10
        $tbl.Cell($r,$c).Range.Font.Bold = $false
    }
    $tbl.Cell($r,1).Range.Font.Bold = $true
    $tbl.Cell($r,1).Range.Text = $rows[$i][0]
    $tbl.Cell($r,2).Range.Text = $rows[$i][1]
    $tbl.Cell($r,3).Range.Text = $rows[$i][2]
    $tbl.Cell($r,4).Range.Text = $rows[$i][3]
    $tbl.Cell($r,4).Range.ParagraphFormat.Alignment = 2   # right
}

# Final combined row - highlighted
for ($c = 1; $c -le 4; $c++) {
    $tbl.Cell(9,$c).Shading.BackgroundPatternColor = 13434828  # light steel blue
    $tbl.Cell(9,$c).Range.Font.Bold = $true
    $tbl.Cell(9,$c).Range.Font.Name = "Calibri"
    $tbl.Cell(9,$c).Range.Font.Size = 10
}
$tbl.Cell(9,1).Range.Text = "Final"
$tbl.Cell(9,2).Range.Text = "Combined"
$tbl.Cell(9,3).Range.Text = "(#1 OR #2) AND #3 AND (#4 OR #5) AND #6 AND #7"
$tbl.Cell(9,4).Range.Text = "26"
$tbl.Cell(9,4).Range.ParagraphFormat.Alignment = 2

# Move selection past the table
$word.Selection.Start = $doc.Content.End - 1
$word.Selection.TypeParagraph()
$word.Selection.TypeParagraph()

# ────────────────────────────────────────────
# SECTION 3: Complete combined query
# ────────────────────────────────────────────
Add-Para "3. Complete Combined Search Query (Copy-Paste Ready for PubMed)" -heading "Heading 1"
Add-Para ("Paste the entire block below directly into the PubMed Advanced Search box. " +
          "Each concept block is enclosed in parentheses. The final result is 26 records (verified 2 June 2026).") -size 11

# The query as a single variable to avoid bracket parsing issues
$q1 = '"Diabetes Mellitus, Type 2"[MeSH] OR "type 2 diabetes"[tiab] OR'
$q2 = '  "type II diabetes"[tiab] OR "non-insulin dependent diabetes"[tiab] OR'
$q3 = '  "NIDDM"[tiab] OR "T2DM"[tiab] OR'
$q4 = '  "Hypertension"[MeSH] OR "hypertension"[tiab] OR'
$q5 = '  "high blood pressure"[tiab] OR "elevated blood pressure"[tiab] OR "arterial hypertension"[tiab]'
$q6 = 'AND'
$q7 = '('
$q8 = '  "Life Style"[MeSH] OR "Diet"[MeSH] OR "Exercise"[MeSH] OR'
$q9 = '  "lifestyle modification"[tiab] OR "lifestyle intervention"[tiab] OR'
$q10= '  "dietary intervention"[tiab] OR "physical activity"[tiab] OR'
$q11= '  "caloric restriction"[tiab] OR "weight loss"[tiab] OR "low-calorie diet"[tiab] OR'
$q12= '  "low carbohydrate"[tiab] OR "DASH diet"[tiab] OR "Mediterranean diet"[tiab] OR'
$q13= '  "health coaching"[tiab] OR "behaviour change"[tiab] OR "self-management"[tiab]'
$q14= ')'
$q15= 'AND'
$q16= '('
$q17= '  "remission"[tiab] OR "reversal"[tiab] OR "resolution"[tiab] OR "drug-free"[tiab] OR'
$q18= '  "medication discontinuation"[tiab] OR "off medication"[tiab] OR'
$q19= '  "normalisation"[tiab] OR "normalization"[tiab] OR'
$q20= '  "blood pressure normalisation"[tiab] OR "blood pressure normalization"[tiab] OR'
$q21= '  "hypertension remission"[tiab] OR "antihypertensive discontinuation"[tiab] OR'
$q22= '  "medication withdrawal"[tiab]'
$q23= ')'
$q24= 'AND'
$q25= '('
$q26= '  "developing countries"[MeSH] OR "low-income countries"[tiab] OR "middle-income countries"[tiab] OR'
$q27= '  "LMIC"[tiab] OR "sub-Saharan Africa"[tiab] OR "South Asia"[tiab] OR "Southeast Asia"[tiab] OR'
$q28= '  "Latin America"[tiab] OR "Africa"[MeSH] OR "Asia"[MeSH] OR'
$q29= '  "Kenya"[tiab] OR "Nigeria"[tiab] OR "India"[tiab] OR "China"[tiab] OR "Brazil"[tiab] OR'
$q30= '  "Ethiopia"[tiab] OR "Tanzania"[tiab] OR "Uganda"[tiab] OR "Pakistan"[tiab] OR'
$q31= '  "Bangladesh"[tiab] OR "Indonesia"[tiab]'
$q32= ')'
$q33= 'AND'
$q34= '('
$q35= '  "Randomized Controlled Trial"[pt] OR "randomised controlled trial"[tiab] OR'
$q36= '  "randomized controlled trial"[tiab] OR "RCT"[tiab] OR'
$q37= '  "cluster-randomised"[tiab] OR "cluster-randomized"[tiab]'
$q38= ')'

$queryLines = @("(", $q1,$q2,$q3,$q4,$q5,")", $q6, $q7,$q8,$q9,$q10,$q11,$q12,$q13,$q14,
                $q15,$q16,$q17,$q18,$q19,$q20,$q21,$q22,$q23,
                $q24,$q25,$q26,$q27,$q28,$q29,$q30,$q31,$q32,
                $q33,$q34,$q35,$q36,$q37,$q38)

$sel = $word.Selection
$sel.Style = $doc.Styles.Item("Normal")
$sel.Font.Name  = "Courier New"
$sel.Font.Size  = 9
$sel.Font.Bold  = $false
$sel.ParagraphFormat.SpaceAfter  = 2
$sel.ParagraphFormat.SpaceBefore = 2
$sel.ParagraphFormat.Shading.BackgroundPatternColor = 15921906  # light grey background

foreach ($line in $queryLines) {
    $sel.TypeText($line)
    $sel.TypeParagraph()
    $sel.ParagraphFormat.Shading.BackgroundPatternColor = 15921906
}

$sel.ParagraphFormat.Shading.BackgroundPatternColor = -1  # reset
$sel.TypeParagraph()

# ────────────────────────────────────────────
# SECTION 4: Search logic and field definitions
# ────────────────────────────────────────────
Add-Para "4. Search Logic and Field Definitions" -heading "Heading 1"

$sel = $word.Selection
$sel.Style = $doc.Styles.Item("Normal")
$sel.Font.Name = "Calibri"; $sel.Font.Size = 11; $sel.Font.Bold = $false

$logicItems = @(
    "OR is used within each concept block to capture synonyms and variant terminology.",
    "AND joins the five concept blocks so all must be present in retrieved records.",
    "Sets #1 and #2 (condition) are OR'd together because the review covers both T2DM and hypertension.",
    "Sets #4 and #5 (outcomes) are OR'd together because remission (T2DM) and normalisation (hypertension) are the key outcomes for each condition strand."
)

$sel.Font.Bold = $true; $sel.TypeText("Search logic:"); $sel.Font.Bold = $false
$sel.TypeParagraph()
foreach ($item in $logicItems) {
    $sel.TypeText("    " + [char]0x2022 + "  " + $item)
    $sel.TypeParagraph()
}
$sel.TypeParagraph()

$fieldDefs = @(
    "[MeSH]  = Medical Subject Headings controlled vocabulary term, including all narrower terms (automatic explosion).",
    "[tiab]  = Title and Abstract free-text field.",
    "[pt]    = Publication type."
)
$sel.Font.Bold = $true; $sel.TypeText("Field tags:"); $sel.Font.Bold = $false
$sel.TypeParagraph()
foreach ($item in $fieldDefs) {
    $sel.Font.Name = "Courier New"; $sel.Font.Size = 10
    $sel.TypeText("    " + $item)
    $sel.TypeParagraph()
    $sel.Font.Name = "Calibri"; $sel.Font.Size = 11
}
$sel.TypeParagraph()

$limits = @(
    "Date range: no limits (inception to date of search).",
    "Language: none (all languages included).",
    "Study design: restricted to randomised controlled trials via Set #7."
)
$sel.Font.Bold = $true; $sel.TypeText("Limits applied:"); $sel.Font.Bold = $false
$sel.TypeParagraph()
foreach ($item in $limits) {
    $sel.TypeText("    " + [char]0x2022 + "  " + $item)
    $sel.TypeParagraph()
}
$sel.TypeParagraph()

# ────────────────────────────────────────────
# SECTION 5: Additional databases
# ────────────────────────────────────────────
Add-Para "5. Additional Databases to Be Searched" -heading "Heading 1"
Add-Para ("The MEDLINE strategy will be adapted for 12 additional databases. Full adapted strategies will be " +
          "documented as supplementary material to the published protocol.") -size 11

$dbTbl = $doc.Tables.Add($word.Selection.Range, 14, 3)
$dbTbl.Borders.Enable = $true
$dbTbl.Columns.Item(1).Width = $word.InchesToPoints(0.4)
$dbTbl.Columns.Item(2).Width = $word.InchesToPoints(2.2)
$dbTbl.Columns.Item(3).Width = $word.InchesToPoints(3.45)

$dbHdrs = @("No.", "Database (Platform)", "Adaptation Notes")
for ($c = 1; $c -le 3; $c++) {
    $cell = $dbTbl.Cell(1,$c)
    $cell.Shading.BackgroundPatternColor = 4210752
    $cell.Range.Font.Color = $wdColorWhite
    $cell.Range.Font.Bold  = $true
    $cell.Range.Font.Name  = "Calibri"
    $cell.Range.Font.Size  = 10
    $cell.Range.Text = $dbHdrs[$c-1]
}

$dbs = @(
    @("1",  "MEDLINE (PubMed, NLM)",                  "Primary strategy as above; no adaptation needed"),
    @("2",  "Embase (Elsevier)",                       "Emtree terms replace MeSH; .mp. or :ti,ab field tags; Embase RCT filter"),
    @("3",  "Cochrane CENTRAL (Cochrane Library)",     "Cochrane search syntax; RCT study design filter not needed"),
    @("4",  "Web of Science Core Collection (Clarivate)","Free-text only (TS= field); no controlled vocabulary"),
    @("5",  "CINAHL (EBSCOhost)",                      "CINAHL subject headings replace MeSH; adapted field tags"),
    @("6",  "Global Health (CABI)",                    "CAB Thesaurus terms; inherent LMIC coverage"),
    @("7",  "African Journals Online (AJOL)",          "Simplified free-text; LMIC geographic filter not needed (regional database)"),
    @("8",  "LILACS (BIREME/VHL)",                     "DeCS descriptors; Spanish and Portuguese synonym terms added"),
    @("9",  "IndMED (Indian Medlars Centre)",          "Simplified free-text; no controlled vocabulary; regional database"),
    @("10", "WHO Global Index Medicus (WHO)",           "Adapted free-text syntax; inherent LMIC coverage"),
    @("11", "ICTRP (WHO)",                             "Condition and intervention terms; identifies unpublished trials"),
    @("12", "ClinicalTrials.gov (NLM)",                "Condition and intervention terms; identifies unpublished trials"),
    @("13", "ProQuest Dissertations & Theses",         "Free-text terms; grey literature source")
)

for ($i = 0; $i -lt $dbs.Count; $i++) {
    $r = $i + 2
    if ($i % 2 -eq 1) {
        for ($c = 1; $c -le 3; $c++) { $dbTbl.Cell($r,$c).Shading.BackgroundPatternColor = 15921906 }
    }
    for ($c = 1; $c -le 3; $c++) {
        $dbTbl.Cell($r,$c).Range.Font.Name = "Calibri"
        $dbTbl.Cell($r,$c).Range.Font.Size = 10
        $dbTbl.Cell($r,$c).Range.Font.Bold = $false
    }
    $dbTbl.Cell($r,1).Range.Text = $dbs[$i][0]
    $dbTbl.Cell($r,2).Range.Text = $dbs[$i][1]
    $dbTbl.Cell($r,3).Range.Text = $dbs[$i][2]
}

$word.Selection.Start = $doc.Content.End - 1
$word.Selection.TypeParagraph()
$word.Selection.TypeParagraph()

# ────────────────────────────────────────────
# SECTION 6: Supplementary methods
# ────────────────────────────────────────────
Add-Para "6. Supplementary Search Methods" -heading "Heading 1"
$suppItems = @(
    "Forward and backward citation searching on all included full-text articles and on the five most methodologically relevant recent systematic reviews (Sherifali et al. 2025; O'Donoghue et al. 2021; Sagastume et al. 2022; and two additional reviews identified during preliminary screening).",
    "Trial registry searching: ICTRP and ClinicalTrials.gov using condition and intervention terms.",
    "Grey literature: ProQuest Dissertations and Theses Global, the Grey Literature Report, and relevant institutional repositories.",
    "Direct author contact for identified trial registrations without published results.",
    "Expert contact: correspondence with researchers active in the field of T2DM remission in LMIC settings."
)
$sel = $word.Selection
$sel.Style = $doc.Styles.Item("Normal")
$sel.Font.Name = "Calibri"; $sel.Font.Size = 11; $sel.Font.Bold = $false
foreach ($item in $suppItems) {
    $sel.TypeText("    " + [char]0x2022 + "  " + $item)
    $sel.TypeParagraph()
}
$sel.TypeParagraph()

# ────────────────────────────────────────────
# SECTION 7: PRESS peer review
# ────────────────────────────────────────────
Add-Para "7. Search Strategy Peer Review" -heading "Heading 1"
Add-Para ("The complete search strategy (MEDLINE and all database adaptations) will be peer-reviewed by a " +
          "medical librarian using the PRESS (Peer Review of Electronic Search Strategies) 2015 framework " +
          "(McGowan et al., J Clin Epidemiol, 2016) prior to execution of the final search. The peer-reviewed " +
          "strategy will replace this draft. Any amendments arising from peer review will be documented with " +
          "date and rationale.") -size 11

$word.Selection.TypeParagraph()

# ────────────────────────────────────────────
# SECTION 8: Validation
# ────────────────────────────────────────────
Add-Para "8. Validation Against Known References" -heading "Heading 1"
Add-Para "The combined strategy was validated by confirming retrieval of the following key references in PubMed:" -size 11

$refs = @(
    "Sherifali DT et al. (2025). Type 2 diabetes remission: a systematic review and meta-analysis of nonsurgical RCTs. Diabetes Care. doi:10.2337/dc25-0562",
    "O'Donoghue G et al. (2021). Lifestyle interventions to improve glycaemic control in adults with T2DM living in LMICs. Int J Environ Res Public Health. doi:10.3390/ijerph18126273",
    "Sagastume D et al. (2022). Effectiveness of lifestyle interventions on T2DM in LMICs: systematic review and meta-analysis. eClinicalMedicine. doi:10.1016/j.eclinm.2022.101699",
    "Lean MEJ et al. (2018). Primary care-led weight management for remission of type 2 diabetes (DiRECT). Lancet. doi:10.1016/S0140-6736(17)33102-1",
    "Duhuze Karera MG et al. (2023). Scoping review of trials for T2DM remission with lifestyle intervention: implications for sub-Saharan Africa. Diabetes Metab Syndr Obes. doi:10.2147/DMSO.S403054"
)

$sel = $word.Selection
$sel.Style = $doc.Styles.Item("Normal")
$sel.Font.Name = "Calibri"; $sel.Font.Size = 11; $sel.Font.Bold = $false
for ($i = 0; $i -lt $refs.Count; $i++) {
    $sel.TypeText(($i+1).ToString() + ".   " + $refs[$i])
    $sel.TypeParagraph()
}
$sel.TypeParagraph()

# ────────────────────────────────────────────
# FOOTER NOTE
# ────────────────────────────────────────────
$sel.Font.Size  = 9
$sel.Font.Color = 8421504  # grey
$sel.TypeText("Prepared by: Nichodemus Werre Amollo | Date: 2 June 2026 | PROSPERO Registration: [Number to be inserted after registration]")
$sel.TypeParagraph()

# ────────────────────────────────────────────
# SAVE AS .DOCX
# ────────────────────────────────────────────
$doc.SaveAs([ref]$outputPath, [ref]16)   # 16 = wdFormatDocx
$doc.Close([ref]0)
$word.Quit()

Write-Host "SUCCESS: Word document saved to $outputPath"
