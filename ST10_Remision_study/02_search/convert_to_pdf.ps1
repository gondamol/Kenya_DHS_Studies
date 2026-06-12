$source = "C:\Users\HFD 2\Research\02_Studies\ST10_Remision_study\Search_Strategy_MEDLINE_PubMed.html"
$output = "C:\Users\HFD 2\Research\02_Studies\ST10_Remision_study\Search_Strategy_MEDLINE_PubMed.pdf"

$word = New-Object -ComObject Word.Application
$word.Visible = $false
$doc = $word.Documents.Open($source, $false, $true)
$doc.SaveAs([ref]$output, [ref]17)  # 17 = wdFormatPDF
$doc.Close([ref]0)
$word.Quit()
Write-Host "PDF created at: $output"
