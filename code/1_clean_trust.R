
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
library(haven)

###load data: 4487 obs and 15 var----
plebiscite <- read_csv("Dropbox/Page and Granados/data/in/trust/2016_Plebiscite.csv")
plebiscite <- read_csv("../../data/in/trust/2016_Plebiscite.csv") #linette

###reshape data----
clean_peace <- plebiscite |>
  select(codmpio, nomparti, votos) |> #select variables
  filter(codmpio != "000NA") |> #drop municipality named "000NA"
  pivot_wider( #one row for each municipality
    names_from = nomparti,
    values_from = votos
  )

###calculate proportion----
clean_peace <- clean_trust |>
  mutate(
    total = NO + SI + `VOTOS NULOS` + `VOTOS NO MARCADOS`,
    plebi_yes_prop = SI / total
  ) |>
  select(codmpio, plebi_yes_prop, total) #select variables

# cleaning trust (military) =============================================================

MAPS <- readRDS("../../data/in/trust/MAPS/MAPS public dataset Wave 1+2.rds") #linette
dane <- read_excel("../../data/in/trust/MAPS/MAPS versión pública - Codebook.xlsx", range = "C42:D1163")

clean_military <- MAPS |>
  mutate(codmpio = as.numeric(MAPS$p2_cod))

zap_labels(clean_military$p2_cod) #type of data

clean_military <- clean_military |>
  select(codmpio, p19_a, p19_f, p19_u) |>
  mutate(
    trust_pres = case_when(
      p19_a == "Don’t know" ~ NA,
      p19_a == "No answer" ~ NA,
      p19_a == "Not at all" ~ 1,
      p19_a == "Very little" ~ 2,
      p19_a == "Somewhat" ~ 3,
      p19_a == "A lot" ~ 4,
    ),
    trust_armed = case_when(
      p19_f == "Don’t know" ~ NA,
      p19_f == "No answer" ~ NA,
      p19_f == "Not at all" ~ 1,
      p19_f == "Very little" ~ 2,
      p19_f == "Somewhat" ~ 3,
      p19_f == "A lot" ~ 4,
    ),
    trust_inst = case_when(
      p19_u == "Don’t know" ~ NA,
      p19_u == "No answer" ~ NA,
      p19_u == "Not at all" ~ 1,
      p19_u == "Very little" ~ 2,
      p19_u == "Somewhat" ~ 3,
      p19_u == "A lot" ~ 4,
    )
  ) |>
  select(codmpio, trust_pres, trust_armed, trust_inst) #only 73 unique municipalities

#zap_labels(MAPS$p19_a)

# p2_cod #dane
# 
# p19_a #trust the president
# p19_c #mayor
# p19_d #judges
# p19_e #colombian national police
# p19_f #armed forces
# p19_g #Juntas de Acción Comunal
# p19_h #ethnic/indigenous authorities
# p19_o #tv/radio
# p19_u #state institutions
# Don’t know	-8
# No answer	-7
# Not at all	1
# Very little	2
# Somewhat	3
# A lot	4
# 
# p20_b #20b. During the last 6 months, have you contacted a politician, the mayor or...
# Don’t know	-8
# No answer	-7
# No	0
# Yes	1

# merge =============================================================
clean_trust <- clean_peace |> 
  left_join(clean_military, by = join_by(codmpio))

###save----
#write_csv(clean_trust, "../data/out/clean_trust.csv")

rm(plebiscite)
