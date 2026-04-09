# Authors: Linette Page and Sofia Granados
# Date created: April 8 2026
# Last edited: April 8 2026 SG
# Description: This code cleans the replication data from Acemoglu et al 2020, specifically the variables to capture judicial capacity and ranking officials per municipality for our ignorability assumption
# Input: Replication data available in https://doi.org/10.3886/E111542V1
# Output: unit is municipality-year; one column for each variable

# 0. Setup -----------

library(haven)

data <- read_dta("~/Dropbox/Page and Granados/data/in/acemoglu_2020/Data/FPMunyear.dta") 