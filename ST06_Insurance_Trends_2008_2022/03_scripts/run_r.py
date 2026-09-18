import subprocess
import sys

r_script = r"c:\Users\HFD 2\Research\02_Studies\ST06_Insurance_Trends_2008_2022\03_scripts\debug_2022.R"
r_path = r"C:\Users\HFD 2\AppData\Local\Programs\R\R-4.5.1\bin\Rscript.exe"

print("Running debug_2022.R to check 2022 PR encoding...")
result = subprocess.run([r_path, r_script], capture_output=True, text=True)
print("STDOUT:", result.stdout)
print("STDERR:", result.stderr)
print("Return code:", result.returncode)