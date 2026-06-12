$word = New-Object -ComObject Word.Application
$word.Visible = $false
$inputPath = "C:\Users\HFD 2\Research\02_Studies\ST10_Remision_study\SR_Protocol_LMIC_Remission_v0.docx"
$outputPath = "C:\Users\HFD 2\Research\02_Studies\ST10_Remision_study\SR_Protocol_LMIC_Remission_v0.txt"
$doc = $word.Documents.Open($inputPath)
$doc.SaveAs([ref]$outputPath, [ref]2)
$doc.Close([ref]0)
$word.Quit()
Write-Host "Done"
