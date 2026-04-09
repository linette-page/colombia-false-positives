
# Authors: Linette Page and Sofia Granados
# Date created: May 25 2025
# Last edited: May 25 2025 4:48 LP
# Description: This code cleans the 2016 peace plebiscite data
# Input: plebiscite data
# Output: unit is municipality; proportion of yes votes to not-yes votes

# index:
#   packages
#   load data
#   reshape data
#   calculate proportion
#   save

# cleaning trust =============================================================

###packages----
library(tidyverse)

###load data: 4487 obs and 15 var----
plebiscite <- read_csv("Dropbox/Page and Granados/data/in/trust/2016_Plebiscite.csv")

###reshape data----
clean_trust <- plebiscite |>
  select(codmpio, nomparti, votos) |> #select variables
  filter(codmpio != "000NA") |> #drop municipality named "000NA"
  pivot_wider( #one row for each municipality
    names_from = nomparti,
    values_from = votos
  )

###calculate proportion----
clean_trust <- clean_trust |>
  mutate(
    total = NO + SI + `VOTOS NULOS` + `VOTOS NO MARCADOS`,
    plebi_yes_prop = SI / total
  ) |>
  select(codmpio, plebi_yes_prop) #select variables

###save----
#write_csv(clean_trust, "Dropbox/Page and Granados/data/out/clean_trust.csv")

rm(plebiscite)
