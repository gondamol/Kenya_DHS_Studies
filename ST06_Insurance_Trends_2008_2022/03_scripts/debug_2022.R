library(haven)
library(dplyr)
pr22 <- read_dta("c:/Users/HFD 2/Research/01_DHS_Data/KDHS_2022/PR_Person_Recode/KEPR8CFL.DTA",
  col_select = c(hv104, hv105, sh27))
cat("nrow:", nrow(pr22), "\n")
cat("hv104 vals:", paste(unique(as.character(as_factor(pr22$hv104))), collapse = "|"), "\n")
cat("fem15_49:", sum(pr22$hv105 >= 15 & pr22$hv105 <= 49 & as.character(as_factor(pr22$hv104)) == "female", na.rm = TRUE), "\n")