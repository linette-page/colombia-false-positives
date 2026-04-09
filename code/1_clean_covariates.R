# Authors: Linette Page and Sofia Granados
# Date created: May 25 2025 
# Last edited: May 30 2025 15:48 SG
# Description: This code cleans data from Panel Municipal CEDE and Presidential
# Elections for the construction of covariates
# Input: Panel_municipal_caracteristicas, panel_municipal_education, 
# panel_municipal_buen_gobierno, presidential_elections_2002 and 2006
# Output: municipality-year level covariates

# Index:
#   0. Setup
#   1. Panel: General Characteristics
#   2. Panel: Education 
#   3. Support for Uribe
#   4. Merge

## 1. Setup ----------------------------------------------------------------------

library(tidyverse)
library(haven) # For .dta

# Working directory for all to work - Comment as needed!!!!
setwd("/Users/sofiagranados/Library/CloudStorage/Dropbox/Page and Granados") # For SG
#setwd("Dropbox/Page and Granados") # For LP


## 1. Panel: General Characteristics ---------------------------------------------

  # Getting the data

  general_charact <- read_dta("data/in/covariates/Sociodemographic/PANEL_CARACTERISTICAS_GENERALES(2024).dta")

  # Renaming and selecting variables of interest:
  
  general_charact <- general_charact |> 
    mutate(
      codmpio = as.character(codmpio)) |>
    rename(year = ano,
           pop_total = pobl_tot, # This is census data from 1998, 2005 and 2008 and projections for the rest of years
           prop_rural = indrural, # Rural population / Total population
           gdp_pcap_cons2005 = pib_percapita_cons, # GDP Per capita constant to 2005
           mpi = IPM  # Multidimensional Poverty Index (1993 and 2005) For how is it calculated - See annex
           ) |> 
    select(codmpio, year, pop_total, prop_rural, gdp_pcap_cons2005, mpi, gini)

## 2. Panel: Education -----------------------------------------------------------
  
  # Getting the data
  
  education <- read_dta("data/in/covariates/Sociodemographic/PANEL_DE_EDUCACION(2023).dta")
  
  # Renaming and selecting variables of interest:
  
    # These variables are available for 1993 and 2005 (mostly) 
  
  education <- education |> 
    mutate(
      codmpio = as.character(codmpio)) |> 
    rename( year = ano, 
            avg_schooling = anos_est_mun,
            literacy = ind_alfa # Rate of 15 y/o + who read and write
            ) |> 
    select(codmpio, year, avg_schooling, literacy)
  

## 3. Support for Uribe--------------------------------------
  
  # Getting the data
  pres_elections_2002 <- read_dta("data/in/covariates/Elections/2002_Presidencia.dta") # Data for first round 2002 presidential elections

  pres_elections_2006 <- read_dta("data/in/covariates/Elections/2006_Presidencia.dta") # Data for first round 2006 presidential elections
  
  #Renaming, creating and selecting variables of interest for 2002 
  
  pres_elections_2002 <- pres_elections_2002 |>
    group_by(codmpio) |> 
    mutate( 
      total_votes_2002 = sum(votos)) # Create a total votes to calculate proportion
  
  pres_elections_2002 <- pres_elections_2002 |>
    filter(primer_apellido == "URIBE") |> # Keeping only Uribe 
    mutate(
      codmpio = as.character(codmpio),
      uribe_2002_votes = votos,
      uribe_2002_proportion = votos/total_votes_2002) |>  #Proportion of Uribe votes over total votes (including null and blank votes)
    select(codmpio, total_votes_2002, uribe_2002_votes, uribe_2002_proportion)
  
  
  #Renaming, creating and selecting variables of interest for 2006
  
  pres_elections_2006 <- pres_elections_2006 |>
    group_by(codmpio) |> 
    mutate( 
      total_votes_2006 = sum(votos)) # Create a total votes to calculate proportion
  
  pres_elections_2006 <- pres_elections_2006 |>
    filter(primer_apellido == "URIBE") |> # Keeping only Uribe 
    mutate(
      codmpio = as.character(codmpio),
      uribe_2006_votes = votos,
      uribe_2006_proportion = votos/total_votes_2006) |>  #Proportion of Uribe votes over total votes (including null and blank votes)
    rename(
      mpio = municipio,
      dpto = departamento
    ) |> 
    select(codmpio, mpio, dpto, total_votes_2006, uribe_2006_votes, uribe_2006_proportion)
  
  # Merging both elections so it's easier to merge
  
  uribe_support <-  left_join(pres_elections_2002, pres_elections_2006, by = join_by(codmpio))
  
## Merge all ------------------------------------------------------------------
  
  covariates <- uribe_support |> 
    left_join(education, by = join_by(codmpio), relationship = "one-to-many") |> 
    left_join(general_charact, by = join_by(codmpio, year)) |> 
    filter(year >= 1930) |> 
    select(dpto, mpio, codmpio, everything())

# Save final output -----------------------------------------------------------
  
 # write_csv(covariates, "data/out/clean_covariates.csv")
  
rm(list = ls())
  
  
