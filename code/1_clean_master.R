# Authors: Linette Page and Sofia Granados
# Date created: May 30 2025
# Last edited: Apr 14 2026 SG
# Description: This code merges and cleans the result of all datasets for covariates, violence data and trust
# Input: covariates, trust, violence
# Output: municipality-year level 

# Index:
#   0. Setup
#   1. Getting datasets
#   2. Merging

## 0. Setup ----------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(haven) # For .dta


## 1. Getting all datasets ------------------------------------------------------

  ### Violence (CNMH) -----
  violence <- read_csv("data/out/clean_violence.csv")
  length(unique(violence$codmpio)) # 1127 Municipalities 
  length(unique(violence$year)) # 80 years
  violence <- violence |> 
    mutate(codmpio = as.numeric(codmpio)) 
  
  ### Violence (CEV-JEP) ----
  violence_cev_jep <- read_csv("data/out/clean_violence_CEV_JEP.csv")
  length(unique(violence_cev_jep$codmpio)) # 1119 Municipalities 
  length(unique(violence_cev_jep$year)) # 34 years
  violence_cev_jep <- violence_cev_jep |> 
    mutate(codmpio = as.numeric(codmpio)) 

  ### Covariates -----
  covariates <- read_csv("data/out/clean_covariates.csv")
  length(unique(covariates$codmpio)) # 1115 Municipalities 
  length(unique(covariates$year)) # 35 years
    
  covariates <- covariates |>
    mutate(codmpio = as.numeric(codmpio)) 
  
  ### Trust ----
  trust <- read_csv("data/out/clean_trust.csv")
  length(unique(trust$codmpio)) # 1121 Municipalities 
  trust <- trust |> 
    mutate(codmpio = as.numeric(codmpio))
  
  ### Acemoglu ----
  acemoglu <- read_csv("data/out/clean_acemoglu.csv")
  length(unique(acemoglu$codmpio)) # 1119 Municipalities 
  
## 2. Merging -------------------------------------------------------------------
 
  full_dataset <- violence |> 
    full_join(violence_cev_jep, by = join_by(codmpio, year), relationship = "one-to-one")|>
    full_join(covariates, by = join_by(codmpio, year), relationship = "one-to-one")|>
    full_join(trust, by = join_by(codmpio)) |> 
    full_join(acemoglu, by = join_by(codmpio, year)) |> 
    select(dpto, mpio = mpio.y, codmpio, everything())
  
  ### Cleaning municipality identifiers ------
  
  # Filling info we currently have
  
  full_dataset <- full_dataset |>
    group_by(codmpio) |> 
    fill(dpto, .direction = "downup") |> # Fills info from dpto to the whole municipality
    fill(mpio, .direction = "downup") |> # Fills info from mpio to the whole municipality
    ungroup()  # Ungroups before selecting
  
  # Missing 
  df_missing <- full_dataset |> 
    filter(is.na(mpio))
  
  # Clean municipality names that are missing:
  
  full_dataset <- full_dataset |> 
    mutate( mpio = case_when(
      codmpio == 15114 ~ "BUSBANZÁ",
      codmpio ==  15162 ~ "CERINZA",
      codmpio ==  15189 ~ "CIÉNEGA",
      codmpio ==  15224 ~ "CUCAITA",
      codmpio ==  15293 ~ "GACHANTIVÁ",
      codmpio ==  15476 ~ "MOTAVITA",
      codmpio ==  15638 ~ "SÁCHICA",
      codmpio ==  15676 ~ "SAN MIGUEL DE SEMA",
      codmpio ==  15723 ~ "SATIVASUR",
      codmpio ==  15764 ~ "SORACÁ",
      codmpio ==  15808 ~ "TINJACÁ",
      codmpio ==  15820 ~ "TÓPAGA",
      codmpio ==  15879 ~ "VIRACACHÁ",
      codmpio ==  25200 ~ "COGUA",
      codmpio ==  25407 ~ "LENGUAZAQUE",
      codmpio ==  25781 ~ "SUTATAUSA",
      codmpio ==  25898 ~ "ZIPACoN",
      codmpio ==  68872 ~ "VILLANUEVA",
      codmpio ==  88564 ~ "PROVIDENCIA Y SANTA CATALINA",
      codmpio ==  91460 ~ "MIRITI - PARANÁ",
      codmpio ==  91540 ~ "PUERTO NARIÑO",
      codmpio ==  94885 ~ "LA GUADALUPE",
      codmpio ==  94886 ~ "CACAHUAL",
      codmpio ==  94887 ~ "PANA PANA",
      codmpio ==  97777 ~ "PAPUNAHUA", 
      TRUE ~ as.character(mpio)  # Preserve existing values
    ))
  
  # Double check missing: 0 
  
  df_missing <- full_dataset |> 
    filter(is.na(mpio))
  
  # Fill again
  
  full_dataset <- full_dataset |> 
    group_by(codmpio) |> 
    fill(dpto, .direction = "downup") |> # Fills info from dpto to the whole municipality
    fill(mpio, .direction = "downup") |> # Fills info from mpio to the whole municipality
    ungroup()  # Ungroups before selecting
  
  # Now, department
  
  df_missing <- full_dataset |> 
    filter(is.na(dpto))
  
  # Some rows do not have information so they were named as "SIN INFORMACION"
  full_dataset <- full_dataset |> 
    filter(
      dpto != "SIN INFORMACION"
    )
  
  # Verify 
  df_missing <- full_dataset |> 
    filter(is.na(dpto))
  
  # Verifying pop_total: All years previous 1993 will have the 1993 population
  
  full_dataset <- full_dataset |> 
    group_by(codmpio) |> 
    mutate(pop_1993 = pop_total[year == 1993]) |> # Create a 1993 baseline for each group
    mutate(pop_total = if_else(year < 1993, pop_1993, pop_total)) |> # Fill in missing (or all) years before 1993 with the 1993 value
    select(-pop_1993) |> 
    ungroup()
  
  full_dataset <- full_dataset |> 
    mutate(pop_total = case_when(
      is.na(pop_total) ~ NA_real_,              # if population is NA
      pop_total == 0 ~ NA_real_,                # If population is 0
      TRUE ~ as.numeric(pop_total)
    ) 
    ) |> 
    group_by(codmpio) |> 
    # Find the first non-NA population value for each municipality
    arrange(year) |> 
    mutate(first_non_na_pop = pop_total[which(!is.na(pop_total))[1]]) |> 
    # Replace values before first data year with that first value
    mutate(pop_total = if_else(year < min(year[!is.na(pop_total)]), first_non_na_pop, pop_total)) |> 
    select(-first_non_na_pop) |> 
    ungroup()
  
  # Filter for only before 2019
  full_dataset <- full_dataset |> 
    filter(year < 2019 & year > 1978)
  
### Saving data -----------------------------  
write_csv(full_dataset, "data/out/full_dataset.csv")