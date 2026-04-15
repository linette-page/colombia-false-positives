# Authors: Linette Page and Sofia Granados
# Date created: April 8 2026
# Last edited: April 14 2026 SG
# Description: This code cleans the replication data from Acemoglu et al 2020, specifically the variables to capture judicial capacity and ranking officials per municipality for our ignorability assumption
# Input: Replication data available in https://doi.org/10.3886/E111542V1
# Output: unit is municipality-year; one column for each variable

# 0. Setup -----------

library(haven)
library(tidyverse)
library(labelled) # To get labels imported from Stata

acemoglu_data <- read_dta("~/Dropbox/Page and Granados/data/in/acemoglu_2020/Data/FPMunyear.dta") 
# This data set is the main from Acemoglu et al's paper. It is a yearly municipal panel data from 2000 to 2010. It includes all the information of the main variables and the set of controls used in the paper's estimations.

# 1. Identifying relevant variables ----

# From this paper, we need the SOO variables for our conditional ignorability criteria: Brigades rank and judicial inefficiency

data <- acemoglu_data |> 
  select(codmpio = M_code, #Mpio 
         year,
         justice_percent, ## Judicial inefficiency: They construct the measures using data from complaints against public functionaries. From the Inspector General Office (Procuraduria) --> this is an event-based dataset of complaints against public servants from 1995 to 2011.
         rank_w_any, # Colonel in charge (weighted share) - Brigades rank: They reconstruct the army structure and identify colonel ranks by brigade from 1991 to 2010 with an original dataset from webpage archives. Specifically, they used the Colombian army's web-page https://www.ejercito.mil.co/conozcanos/organigrama/unidades militares which includes information about brigade leadership and jurisdictions, together with old versions of the webpage to reconstruct historical ranks as crawled by the Internet Archives Way Back Machine https://archive.org/web/. In cases where neither source provided information on ranks by brigade, they searched for news in El Tiempo's digital archive to identify ranks. --> Can we replicate this???? 
         # "We compute a weighted share using the population of all municipalities under a brigade’s jurisdiction as weights to recognize that larger brigades may be more important"
         rank_s_any, # Colonel in charge (unweighted share)
         rank_w_bri, # Colonel in charge regular (weighted share)
         rank_w_mob, # Colonel in charge mob (weighted share)
         rank_any,# Colonel in charge (dummy)
         lat1, # Municipality latitude - might be helpful if we decide to do maps
         lon1, # Municipality longitude - might be helpful if we decide to do maps
         fp, # Number cases false positives - False positives: To double check with the construction of our data - They use the same source as we do: to measure false positives they use CINEP's Data Bank on Human Rights and Political Violence (Banco de datos de DDHH y violencia politica), and specifically the special report on false positives (CINEP, 2009). 
         execution, # Number executions false positives (fp cases)
         fpd, # Indicator false positives
         tp, # Number causalties true positives
         tpd, # Indicator true positives
         events # Number cases true positives (tp cases)
         )

# 2. Save dataset ----

write_csv(data, "data/out/clean_acemoglu.csv")
