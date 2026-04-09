# Authors: Linette Page and Sofia Granados
# Date created: May 25 2025 
# Last edited: June 3 2025 18:48 SG
# Description: This code cleans the CNMH violence data
# Input: CNMH by victim data; one dataset per 11 types of violence and CEV - JEP - HRDAG a set of dataset per 4 prioritized cases (homicide, disappearances, kidnapping and recruitment)
# Output: unit is municipality-year; one column for each TYPE-PERPETRATOR (27 total)

# cleaning violence from CNMH ==========================================================

##packages----
library(tidyverse)
library(openxlsx)


##load files----
paths <- list.files("data/in/violence/cases", 
                    pattern = "[.]xlsx$", full.names = TRUE) #all file names

###naming files----
types <- c() #extract violence type from file names
for (i in 1:length(paths)) {
  types <- c(types, str_sub(paths[i], -14, -13))
}

#renaming matrix
map <- data.frame(find = c("VS", "SE", "RU", "MI", "MA", "DF", "DB", "AT", "AS", "AP", "AB"),
                  replace = c("sexual", "kidnapping", "recruitment", "mine", "massacre", 
                            "forced_disappearence", "property_damage", "terrorist", 
                            "selective_assasination", "population_attack", "war_action"))
                            

types <- as.character(map[match(types, map$find), "replace"])

# #loads all data in at once
# for (i in 1:length(types)) {
#   assign(types[i], read.xlsx(paths[i]))
# }


##loop----
all_missing <- data.frame(matrix(ncol = 11, nrow = 4)) #missing data matrix
final_df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), #empty final dataset
                     c("codmpio", "mpio", "year"))

for (i in 1:length(types)) {
  
  ###skip----
  if(i %in% c(1, 2)) next #skipping war_action and population_attack because no perpetrator variable
 
  ###load----
  data <- read.xlsx(paths[i]) #load each dataset and name it based on type
  
  ###create new clean dataset----
  data_clean <- data |> 
    select(Código.DANE.de.Municipio, Municipio, Año, Presunto.Responsable, #select variables
           Total.de.Víctimas.del.Caso) |>
    rename( #rename
      codmpio = Código.DANE.de.Municipio,
      mpio = Municipio,
      year = Año,
      perp = Presunto.Responsable,
      num_vics = Total.de.Víctimas.del.Caso
    ) |>
    filter(!(codmpio %in% c("00000", "05000", #remove missing data
                            "EXVEN", "EXPER", "EXPAN", "EXECU", "EX000")) 
           & year != "0000" 
           & !(perp %in% c("SIN INFORMACIÓN", "NINGUNO", "OTRO", "OTRO ¿CUÁL?"))) |>
    mutate(perp = case_when( #sort by perpetrator
      perp %in% c("GRUPO PARAMILITAR") ~ "paramilitary",
      perp %in% c("AGENTE DEL ESTADO") ~ "state",
      perp %in% c("GUERRILLA") ~ "guerrilla",
      TRUE ~ NA
    )) |>
    drop_na(perp) #remove if not identified
    
  ###missing data----
  all_missing[1, i] <- types[i] #add to matrix of missing data
  all_missing[2, i] <- nrow(data) #total data
  all_missing[3, i] <- nrow(data_clean) #length after cleaning

  ###victims calculated----
  data_calculate <- data_clean |>
    group_by(codmpio, mpio, year, perp) |>
    summarize(
      total_victims_count = sum(num_vics) #get number of victims per municipality-year
    ) |>
    pivot_wider(
      names_from = perp,
      values_from = total_victims_count
    )
  
  all_missing[4, i] <- nrow(data_calculate) #length after pivoting
  
  for (n in 4:ncol(data_calculate)) { #add type of violence to column names
    colnames(data_calculate)[n] <- paste0(types[i], "_", colnames(data_calculate)[n])
  }
    
  ###merge----
  final_df <- merge(final_df, data_calculate, by = c("codmpio", "mpio", "year"), #Maybe it should be full join?
                    all = TRUE, suffixes = c(".x", types[i]))
    
    
  #assign(types[i], data_calculate) #assign datasets
}

#Identifying soure
final_df <- final_df |> 
  rename_with(
    ~ paste0(.x, "_cnmh"),
    .cols = setdiff(names(final_df), c("year", "codmpio"))
  ) |> 
  select(-mpio_cnmh)

#20666 obs, 29 variables
#there is no terrorist_state

# Falsos Positivos ============================================
## According to CNMH, state perpetrated falsos positivos of Selective Assassinations (AS), Massacres (MA), Forced Disappearences (DF) and Sexual Violence (VS) (SV during the FP)

## Repeating LP's loop for Falsos Positivos

##load files----
paths <- list.files("data/in/violence/cases", 
                    pattern = "[.]xlsx$", full.names = TRUE) #all file names

###naming files----
types <- c() #extract violence type from file names
for (i in 1:length(paths)) {
  types <- c(types, str_sub(paths[i], -14, -13))
}

#renaming matrix
map <- data.frame(find = c("VS", "SE", "RU", "MI", "MA", "DF", "DB", "AT", "AS", "AP", "AB"),
                  replace = c("sexual", "kidnapping", "recruitment", "mine", "massacre", 
                              "forced_disappearence", "property_damage", "terrorist", 
                              "selective_assasination", "population_attack", "war_action"))


types <- as.character(map[match(types, map$find), "replace"])

# #loads all data in at once
# for (i in 1:length(types)) {
#   assign(types[i], read.xlsx(paths[i]))
# }


##loop----
all_missing <- data.frame(matrix(ncol = 11, nrow = 4)) #missing data matrix
final_df_fp <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), #empty final dataset
                     c("codmpio", "mpio", "year"))

for (i in 1:length(types)) {
  
  ###skip----
  if(i %in% c(1, 2, 4, 5, 8, 9, 10)) next #skipping everything that will have falsos positivos:  Selective Assassinations (AS - 4), Massacres (MA - 7), Forced Disappearences (DF - 6) and Sexual Violence (VS - 11) 
  
  ###load----
  data_fp <- read.xlsx(paths[i]) #load each dataset and name it based on type
  
  ###create new clean dataset----
  data_clean_fp <- data_fp |> 
    select(Código.DANE.de.Municipio, Municipio, Año, Presunto.Responsable, #select variables
           Total.de.Víctimas.del.Caso, Modalidad) |> 
    filter(Modalidad == "FALSO POSITIVO") |> # Filtering for only Falsos Positivos
    select(-Modalidad) |> 
    rename( #rename
      codmpio = Código.DANE.de.Municipio,
      mpio = Municipio,
      year = Año,
      perp = Presunto.Responsable,
      num_vics = Total.de.Víctimas.del.Caso
    ) |>
    filter(!(codmpio %in% c("00000", "05000", #remove missing data
                            "EXVEN", "EXPER", "EXPAN", "EXECU", "EX000")) 
           & year != "0000" 
           & !(perp %in% c("SIN INFORMACIÓN", "NINGUNO", "OTRO", "OTRO ¿CUÁL?"))) |>
    mutate(perp = case_when( #sort by perpetrator
      perp %in% c("GRUPO PARAMILITAR") ~ "paramilitary",
      perp %in% c("AGENTE DEL ESTADO") ~ "state",
      perp %in% c("GUERRILLA") ~ "guerrilla",
      TRUE ~ NA
    )) |>
    drop_na(perp) |> #remove if not identified
    filter(perp == "state")# Filter for only Falsos Positivos by the state (there's 1 case by paramilitaries)

  ###victims calculated----
  data_calculate_fp <- data_clean_fp |>
    group_by(codmpio, mpio, year)|>
    summarize(
      !!paste0(types[i], "_fal_pos") := sum(num_vics) #get number of victims per municipality-year and name based o type
    ) 
  
  ###merge----
  final_df_fp <- merge(final_df_fp, data_calculate_fp, by = c("codmpio", "mpio", "year"), 
                    all = TRUE, suffixes = c(".x", types[i]))
  
  
  #assign(types[i], data_calculate) #assign datasets
}

clean_violence <- full_join(final_df,final_df_fp, by = join_by(codmpio, year)) #Both from cnmh (all and falsos positivos only)

write_csv(clean_violence, "data/out/clean_violence.csv")


rm(paths, types, i, missing, all, add, map, data, data_clean, data_calculate, n)
rm(forced_disappearence, kidnapping, massacre, mine, population_attack, 
   property_damage, recruitment, selective_assasination, sexual, terrorist, war_action)


##Things to consider: 
#who is the perpetrator of violence? 
  #currently usingPresunto.Responsable

#a version of the code that includes "-" ie multiple actors
# mutate(perp = case_when( #sort by perpetrator
#   perp %in% c("GRUPO PARAMILITAR", "GRUPO PARAMILITAR - GUERRILLA") ~ "paramilitary",
#   perp %in% c("AGENTE DEL ESTADO", "AGENTE DEL ESTADO - GRUPO PARAMILITAR",
#               "AGENTE DEL ESTADO - GRUPO POSDESMOVILIZACIÓN", 
#               "AGENTE DEL ESTADO - GUERRILLA") ~ "state",
#   perp %in% c("GUERRILLA", "GRUPO POSDESMOVILIZACIÓN - GUERRILLA") ~ "guerrilla",
#   TRUE ~ NA
# ))

#other actors not included:
#POSDESMOVILIZACIÓN; GRUPO ARMADO NO IDENTIFICADO

#Victims
#Currently using Total.de.Víctimas.del.Caso; doesn't distinguish between civilians and combatants

#war_action variable
# has information on civilians involved and state involved
#use as robustness checks - we shouldn't have the same effect because responsibility isn't clear
#use lesionados.civiles and total.civiles

#population_attack variable
#currently not using


# 2. cleaning violence from CEV - JEP - HRDAG  ==========================================================

## Disappearances ----

### load files----
paths_diss <- list.files("data/in/violence/CEV-JEP-HRDAG/Disappearences.csv", 
                    pattern = "[.]csv$", full.names = TRUE) #all file names

### Clean and reshape ----
  
  disapp_temp <- read.csv(paths_diss[1]) |> # Load each df from the folder
    mutate(
      perp = case_when( # Recoding to translate and integrate all guerrillas
        p_str == "GUE-ELN" ~ "Dis_Guerr_any",
        p_str == "GUE-FARC" ~ "Dis_Guerr_any",
        p_str == "GUE-OTRO" ~ "Dis_Guerr_any",
        p_str == "EST" ~ "Dis_State",
        p_str == "PARA" ~ "Dis_Param",
        p_str == "multiple" ~ "Dis_Multiple",
        p_str == "OTRO" ~ "Dis_Other",
        TRUE ~ NA_character_
      )
    ) |> 
    select(perp, yy_hecho, muni_code_hecho, p_str_imputed) |> # Only what we need for year-mpio-responsible level and for droping imputed observations
    # 172097 victims  
    filter(p_str_imputed == "FALSE") |>  # We are selecting only the cases that had information for the perpetrator (were not imputed)
    # 50810 Victims
    select(-p_str_imputed) |> # We don't need this anymore
    group_by(muni_code_hecho, yy_hecho, perp) |> 
    summarize(total_victims = n(), .groups = "drop") |> # Total victims at the year-mpio-responsible level
    pivot_wider(
      names_from = perp,
      values_from = total_victims
    )

  # Get total by type
  disapp_temp <- disapp_temp |> 
    rowwise() |> #"Group" by row
    mutate(Dis_total = sum(c_across(c("Dis_Guerr_any", "Dis_State", "Dis_Param", "Dis_Multiple", "Dis_Other")), na.rm = TRUE)) 

## Homicides ----

### load files----
paths_hom <- list.files("data/in/violence/CEV-JEP-HRDAG/Homicides.csv", 
                         pattern = "[.]csv$", full.names = TRUE) #all file names


### Clean and reshape ----

  hom_temp <- read.csv(paths_hom[1]) |> # Load each df from the folder
    mutate(
      perp = case_when( # Recoding to translate and integrate all guerrillas
        p_str == "GUE-ELN" ~ "Hom_Guerr_any",
        p_str == "GUE-FARC" ~ "Hom_Guerr_any",
        p_str == "GUE-OTRO" ~ "Hom_Guerr_any",
        p_str == "EST" ~ "Hom_State",
        p_str == "PARA" ~ "Hom_Param",
        p_str == "multiple" ~ "Hom_Multiple",
        p_str == "OTRO" ~ "Hom_Other",
        TRUE ~ NA_character_
      )
    ) |> 
    select(perp, yy_hecho, muni_code_hecho, p_str_imputed) |> # Only what we need for year-mpio-responsible level and for droping imputed observations
    # 554369 victims  
    filter(p_str_imputed == "FALSE") |>  # We are selecting only the cases that had information for the perpetrator (were not imputed)
    # 191692 Victims
    select(-p_str_imputed) |> # We don't need this anymore
    group_by(muni_code_hecho, yy_hecho, perp) |> 
    summarize(total_victims = n(), .groups = "drop") |> # Total victims at the year-mpio-responsible level
    pivot_wider(
      names_from = perp,
      values_from = total_victims
    )
  
  # Get total by type
  hom_temp <- hom_temp |> 
    rowwise() |> #"Group" by row
    mutate(Hom_total = sum(c_across(c("Hom_Guerr_any", "Hom_State", "Hom_Param", "Hom_Multiple", "Hom_Other")), na.rm = TRUE)) 

## Kidnapping ----

### load files----
paths_kid <- list.files("data/in/violence/CEV-JEP-HRDAG/Kidnapping", 
                        pattern = "[.]csv$", full.names = TRUE) #all file names

### Clean and reshape ----

  kid_temp <- read.csv(paths_kid[1]) |> # Load each df from the folder
    mutate(
      perp = case_when( # Recoding to translate and integrate all guerrillas
        p_str == "GUE-ELN" ~ "Kid_Guerr_any",
        p_str == "GUE-FARC" ~ "Kid_Guerr_any",
        p_str == "GUE-OTRO" ~ "Kid_Guerr_any",
        p_str == "EST" ~ "Kid_State",
        p_str == "PARA" ~ "Kid_Param",
        p_str == "multiple" ~ "Kid_Multiple",
        p_str == "OTRO" ~ "Kid_Other",
        TRUE ~ NA_character_
      )
    ) |> 
    select(perp, yy_hecho, muni_code_hecho, p_str_imputed) |> # Only what we need for year-mpio-responsible level and for droping imputed observations
    # 55874 victims  
    filter(p_str_imputed == "FALSE") |>  # We are selecting only the cases that had information for the perpetrator (were not imputed)
    # 37236 Victims
    select(-p_str_imputed) |> # We don't need this anymore
    group_by(muni_code_hecho, yy_hecho, perp) |> 
    summarize(total_victims = n(), .groups = "drop") |> # Total victims at the year-mpio-responsible level
    pivot_wider(
      names_from = perp,
      values_from = total_victims
    )
  
  # Get total by type
  kid_temp <- kid_temp |> 
    rowwise() |> #"Group" by row
    mutate(Kid_total = sum(c_across(c("Kid_Guerr_any", "Kid_State", "Kid_Param", "Kid_Multiple", "Kid_Other")), na.rm = TRUE)) 
  
  
## Recruitment ----

### load files----
paths_Rec <- list.files("data/in/violence/CEV-JEP-HRDAG/Recruitment", 
                        pattern = "[.]csv$", full.names = TRUE) #all file names

### Clean and reshape ----

  Rec_temp <- read.csv(paths_Rec[1]) |> # Load each df from the folder
    mutate(
      perp = case_when( # Recoding to translate and integrate all guerrillas
        p_str == "GUE-ELN" ~ "Rec_Guerr_any",
        p_str == "GUE-FARC" ~ "Rec_Guerr_any",
        p_str == "GUE-OTRO" ~ "Rec_Guerr_any",
        p_str == "EST" ~ "Rec_State",
        p_str == "PARA" ~ "Rec_Param",
        p_str == "multiple" ~ "Rec_Multiple",
        p_str == "OTRO" ~ "Rec_Other",
        TRUE ~ NA_character_
      )
    ) |> 
    select(perp, yy_hecho, muni_code_hecho, p_str_imputed) |> # Only what we need for year-mpio-responsible level and for droping imputed observations
    # 19226 victims  
    filter(p_str_imputed == "FALSE") |>  # We are selecting only the cases that had information for the perpetrator (were not imputed)
    # 14874 Victims
    select(-p_str_imputed) |> # We don't need this anymore
    group_by(muni_code_hecho, yy_hecho, perp) |> 
    summarize(total_victims = n(), .groups = "drop") |> # Total victims at the year-mpio-responsible level
    pivot_wider(
      names_from = perp,
      values_from = total_victims
    )
  
  # Get total by type
  Rec_temp <- Rec_temp |> 
    rowwise() |> #"Group" by row
    mutate(Rec_total = sum(c_across(c("Rec_Guerr_any", "Rec_State", "Rec_Param", "Rec_Multiple", "Rec_Other")), na.rm = TRUE)) 
  
## General dataset ----

violence_CEV_JEP <- full_join(hom_temp, kid_temp, by = join_by(muni_code_hecho, yy_hecho))

violence_CEV_JEP <- full_join(violence_CEV_JEP, Rec_temp, by = join_by(muni_code_hecho, yy_hecho))

violence_CEV_JEP <- full_join(violence_CEV_JEP, disapp_temp, by = join_by(muni_code_hecho, yy_hecho))


## Saving CEV_JEP dataset -----

violence_CEV_JEP <- violence_CEV_JEP |> 
  rename( year = yy_hecho,
          codmpio = muni_code_hecho)

violence_CEV_JEP <- violence_CEV_JEP |> 
  rename_with(
    ~ paste0(.x, "_jep"),
    .cols = setdiff(names(violence_CEV_JEP), c("year", "codmpio"))
  )

write_csv(violence_CEV_JEP, "data/out/clean_violence_CEV_JEP.csv")