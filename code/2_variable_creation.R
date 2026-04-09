# Authors: Linette Page and Sofia Granados
# Date created: May 30 2025 
# Last edited: June 03 2025 4:44 LP
# Description: This code creates the variables for estimation
# Input: full_dataset
# Output: municipality-year level covariates ready for estimation

# Index:
#   1. Setups
#   2. Peace Support
#   3. Exposure to Violence (ETV)
#     3.1. ETV 1 Disappearances and Homicides (JEP)
#     3.2. ETV 2 All (JEP)
#     3.3. ETV 3 Falsos Positivos (CNMH)
#     3.4. ETV 4 Massacre, Selective Assasination and Forced Disappearence (CNMH)
#     3.5. ETV 5 All (CNMH)
#   4.  Merging all ETV (municipality level)
#   5. Covariates
#     5.1 Sociodemographic covariates
#     5.2 Addressing missing covariates
#     5.3 Violence covariates


## 1. Setup ----------------------------------------------------------------------

suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(haven))

# Getting master dataset

full_dataset <- read_csv("data/out/full_dataset.csv")
glimpse(full_dataset)

## 2. Peace Support ------------------------------------------------------------

### Proportion of Yes/all votes for Peace in 2016 by municipality (i) 

### It's plebi_yes_prop - It's ready

### Summary statistics
summary(full_dataset$plebi_yes_prop)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.1586  0.3732  0.4856  0.5058  0.6152  0.9506      71 

### Weird that mean is 0.5058 for yes!!! verify we're using the correct side or 1-x VERIFIED

### Count missing values: 71 (Two municipalities - Morichal and La Guadalupe)
### These very small town don't have info for result. Could not find explanation

sum(is.na(full_dataset$plebi_yes_prop))
df_missing <- full_dataset |> 
  filter(is.na(plebi_yes_prop))

### Create final dataset for estimation
final_data <- full_dataset |> 
  select(dpto, mpio, codmpio, plebi_yes_prop) |> 
  group_by(dpto, mpio, codmpio) |> 
  summarize(trust_plebi_yes = mean(plebi_yes_prop))

## 3. Exposure To Violence (ETV) ------------------------------------------------------------
# Proportion of (total victims x 100,000) / total population from 2003 to 2011 by municipality (i)

  ### Filtering for treatment period
  full_dataset_treat <- full_dataset |> 
    filter(year > 2003 & year < 2011 )  
  
  ### 3.1. ETV 1 - Disappearances and Homicides (JEP) -----------------------------------------------------------------------
  # Total victims of the state -> Total victims of the state for all types of violence (sum across cols) “Disappearances”, “homicides”
  
df_etv_dh <- full_dataset_treat |> 
    rowwise() |> #"Group" by row
    mutate(tot_state_vic_d_h = sum(c_across(c("Dis_State_jep", "Hom_State_jep")), na.rm = TRUE)) |> # Should NAs be 0 or NAs????? This confuses me - I think they should be zero because estimation with NAs will probably overestimate. However, we are assuming that NA means no victims, it could be misreport.
    ungroup() |> 
    group_by(codmpio) |> 
    summarise(etv_dh = (sum(tot_state_vic_d_h) * 10000)/mean(pop_total))
    # mutate(etv_dh = (tot_state_vic_d_h * 10000) / pop_total)

  summary(df_etv_dh$etv_dh)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.000   0.000   1.031   3.825   4.159 101.500 

  ### 3.2. ETV 2 - All (JEP) -----------------------------------------------------------------------
  # Total victims -> Total victims for some kinds violence (sum across some cols) “Disappearences”, “homicides”, “kidnapping”, “recruitment”
  
  df_etv_all <- full_dataset_treat |>
    rowwise() |> #"Group" by row
    mutate(tot_state_vic_all = sum(c_across(c("Dis_State_jep", "Hom_State_jep", "Rec_State_jep", "Kid_State_jep")), na.rm = TRUE)) |> # Should NAs be 0 or NAs????? This confuses me - I think they should be zero because estimation with NAs will probably overestimate. However, we are assuming that NA means no victims, it could be misreport.
    group_by(codmpio) |> 
    summarise(etv_all_jep = (sum(tot_state_vic_all) * 10000)/mean(pop_total))  # Victims rate per 10,000 inhabitants  # mutate(etv_dh = (sum across years in tot_state_vic_d_h * 10000) / mean across years pop_total)
   
  
  summary(df_etv_all$etv_all)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.000   0.000   1.105   4.035   4.387 101.500 

  ### 3.3. ETV 3 - Falsos Positivos (JEP) -----------------------------------------------------------------------
  # Total victims -> Total victims for falsos positivos (sum across falsos positivos cols) 
 
 # etv_false_pos
  df_etv_false_p <- full_dataset_treat |>
    rowwise() |> #"Group" by row
    mutate(tot_false_pos = sum(c_across(c("selective_assasination_fal_pos", "forced_disappearence_fal_pos", "massacre_fal_pos", "sexual_fal_pos")), na.rm = TRUE)) |> # Should NAs be 0 or NAs????? This confuses me - I think they should be zero because estimation with NAs will probably overestimate. However, we are assuming that NA means no victims, it could be misreport.
    group_by(codmpio) |> 
    summarise(etv_false_pos = (sum(tot_false_pos) * 10000)/mean(pop_total))  # Victims rate per 10,000 inhabitants  # mutate(etvetv_false_pos_dh = (sum across years in tot_false_pos * 10000) / mean across years pop_total)
  
  summary(df_etv_false_p$etv_false_pos)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.0000  0.0000  0.0000  0.6010  0.0597 27.4957 

  ### 3.4 ETV 4 Massacre, Selective Assassination and Forced Disappearance (CNMH) ----------------------------------------------------------------------
  # Total victims -> Total state Massacre, Selective Assassination and Forced Disappearances victims (sum across cols) 
  
  df_etv_cnmh <- full_dataset_treat |>
    rowwise() |> #"Group" by row
    mutate(tot_state_vic_cnmh = sum(c_across(c("massacre_state_cnmh", "selective_assasination_state_cnmh", "forced_disappearence_state_cnmh")), na.rm = TRUE)) |> # Should NAs be 0 or NAs????? This confuses me - I think they should be zero because estimation with NAs will probably overestimate. However, we are assuming that NA means no victims, it could be misreport.
    group_by(codmpio) |> 
    summarise(etv_cnmh = (sum(tot_state_vic_cnmh) * 10000)/mean(pop_total))  # Victims rate per 10,000 inhabitants  # mutate(etv_all_cnmh = (sum across years in tot_state_vic_all_cnmh * 10000) / mean across years pop_total)
  
  
  summary(df_etv_cnmh$etv_cnmh)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.000   0.000   0.000   2.106   2.004  97.087 
  
  ### 3.5. ETV 5 All (CNMH) -----------------------------------------------------------------------
  # Total victims -> Total state "sexual", "kidnapping", "recruitment", "mine", "massacre", "forced_disappearance", "property_damage", "selective_assasination" victims (sum across all cnmh cols) 

  df_etv_all_cnmh <- full_dataset_treat |>
    rowwise() |> #"Group" by row
    mutate(tot_state_vic_all_cnmh = sum(c_across(c("sexual_state_cnmh", "kidnapping_state_cnmh", "recruitment_state_cnmh", "mine_state_cnmh", "massacre_state_cnmh", "forced_disappearence_state_cnmh", "property_damage_state_cnmh", "selective_assasination_state_cnmh")), na.rm = TRUE)) |> # Should NAs be 0 or NAs????? This confuses me - I think they should be zero because estimation with NAs will probably overestimate. However, we are assuming that NA means no victims, it could be misreport.
    group_by(codmpio) |> 
    summarise(etv_all_cnmh = (sum(tot_state_vic_all_cnmh) * 10000)/mean(pop_total))  # Victims rate per 10,000 inhabitants  # mutate(etv_all_cnmh = (sum across years in tot_state_vic_all_cnmh * 10000) / mean across years pop_total)
  
  
  summary(df_etv_all_cnmh$etv_all_cnmh)
  
# 4. Merging all ETV and trust (municipality level) --------------------------------
  
  final_data <- final_data |> 
    full_join(df_etv_dh, by = join_by(codmpio)) |> # Merging trust with JEP datasets
    full_join(df_etv_all, by = join_by(codmpio)) |> 
    full_join(df_etv_false_p, by = join_by(codmpio)) |> 
    full_join(df_etv_cnmh, by = join_by(codmpio)) |> 
    full_join(df_etv_all_cnmh, by = join_by(codmpio)) |> 
    ungroup()


# 5. Covariates ----------------------------------------------------------------

## 5.1 clean NAs in covariates
full_dataset <- read_csv("data/out/full_dataset.csv")
  
covariates <- c("codmpio", "uribe_2002_proportion", "uribe_2006_proportion", 
                "avg_schooling", "literacy", "pop_total", "prop_rural", 
                "gdp_pcap_cons2005", "mpi", "gini") #all covariates to add

merging <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), #empty merging dataset
                    covariates)

muni <- full_dataset |> #get unique list of all municipalities
  dplyr::select(codmpio) |>
  distinct() |>
  as.list()
muni <- muni[[1]] #unlist

for (j in muni) { #loop through municipalities
  sub <- full_dataset |>
    filter(codmpio == j & year %in% c(2002, 2005, 1993, 2018)) |> #all years that have covariate information
    dplyr::select(year, covariates) |>
    mutate( #select covariates and years
      avg_schooling = if_else(is.na(avg_schooling[year == 1993]) == F, #choose 1993; if missing then 2005
                              avg_schooling[year == 1993], avg_schooling[year == 2005]), 
      literacy = literacy[year == 1993],
      gini = gini[year == 1993],
      gdp_pcap_cons2005 = gdp_pcap_cons2005[year == 2005],
      mpi = if_else(is.na(mpi[year == 2005]) == F, #choose 2005, if missing then 2018
                    mpi[year == 2005], mpi[year == 2018])
    ) |>
    filter(year == 2002) |> #select row
    dplyr::select(-year)
  merging <- bind_rows(merging, sub) #add to main merging dataset
}

final_data <- final_data |> 
  left_join(merging, by = join_by(codmpio))

final_data <- final_data |>
  mutate(log_pop = log(pop_total))

#missing
final_data |>
  summarise_all(~ sum(is.na(.)))

## 5.2 Missing covariates ------
merging |> summarise_all(~ sum(is.na(.)))

# #mpi - has 2018
# merging |> filter(is.na(mpi)) |> dplyr::select(codmpio)
# full_dataset |> filter(codmpio == 97511) |> dplyr::select(year, mpi)

# #avg_schooling: has 2005
# merging |> filter(is.na(avg_schooling)) |> dplyr::select(codmpio)
# full_dataset |> filter(codmpio == 97511) |> dplyr::select(year, avg_schooling)

#gini all 75 are NA
merging |> filter(is.na(gini)) |> dplyr::select(codmpio)
full_dataset |> filter(codmpio == 86568) |> dplyr::select(year, gini)

#gdp_pcap_cons2005 - all 62 are NA
merging |> filter(is.na(gdp_pcap_cons2005)) |> dplyr::select(codmpio)
full_dataset |> filter(codmpio == 86757) |> dplyr::select(year, gdp_pcap_cons2005)

#uribe_2006_proportion: votes and total are all 0
merging |> filter(is.na(uribe_2006_proportion)) |> dplyr::select(codmpio)
full_dataset |> filter(codmpio == 94885) |> dplyr::select(year, uribe_2006_votes, uribe_2006_proportion, total_votes_2006)

#uribe 2002: votes and total are all 0
merging |> filter(is.na(uribe_2002_proportion)) |> dplyr::select(codmpio)
full_dataset |> filter(codmpio == 50686) |> dplyr::select(year, uribe_2002_votes, uribe_2002_proportion, total_votes_2002)


## 5.3 Violence covariates --------------------------------------------------------------------
### 5.3.1 Presence of state actors pre-treatment (1978 - 2003) --------------------------------

#### Getting the full_dataset
  full_dataset <- read_csv("data/out/full_dataset.csv")
  glimpse(full_dataset)
  
####  Filtering for PRE-treatment period
  full_dataset_pretreat <- full_dataset |> 
    filter(year <= 2002 & year >= 1978)  

### Total victims -> Total victims for some kinds violence (sum across some cols) “Disappearences”, “homicides”, “kidnapping”, “recruitment” for pretreatment period
  
  df_state_etv_pre <- full_dataset_pretreat |>
    rowwise() |> #"Group" by row
    mutate(tot_state_vic_all = sum(c_across(c("Dis_State_jep", "Hom_State_jep", "Rec_State_jep", "Kid_State_jep")), na.rm = TRUE)) |> # Should NAs be 0 or NAs????? This confuses me - I think they should be zero because estimation with NAs will probably overestimate. However, we are assuming that NA means no victims, it could be misreport.
    group_by(codmpio) |> 
    summarise(etv_state_pre = (sum(tot_state_vic_all) * 10000)/mean(pop_total))  # Victims rate per 10,000 inhabitants  # mutate(etv_dh = (sum across years in tot_state_vic_d_h * 10000) / mean across years pop_total)
  
  
  summary(df_state_etv_pre$etv_state_pre)
  
###  5.3.2 Presence non-state actors (1978 - 2003): ----------------------------------

####  Guerrilla (FARC and ELN) and Paramilitary (not including multiple or other) ----------------------------
  
  df_nonstate_etv_pre <- full_dataset_pretreat |>
    rowwise() |> #"Group" by row
    mutate(tot_nonstate_vic_all = sum(c_across(c("Dis_Guerr_any_jep", "Hom_Guerr_any_jep", "Rec_Guerr_any_jep", "Kid_Guerr_any_jep", "Dis_Param_jep", "Hom_Param_jep", "Rec_Param_jep", "Kid_Param_jep")), na.rm = TRUE)) |> 
    group_by(codmpio) |> 
    summarise(etv_nonstate_pre = (sum(tot_nonstate_vic_all) * 10000)/mean(pop_total),# Victims rate per 10,000 inhabitants  # mutate(etv_dh = (sum across years in tot_state_vic_d_h * 10000) / mean across years pop_total)
              presence_nonstate_pre = ifelse(etv_nonstate_pre != 0, 1, 0)) # Dummy of presence or not
  
  
  summary(df_nonstate_etv_pre$presence_nonstate_pre) # 96% municipalities have had presence. Continuous might be a better control

##### Guerrillas (Both FARC and ELN) ----------------------------------

  df_guerr_etv_pre <- full_dataset_pretreat |>
    rowwise() |> #"Group" by row
    mutate(tot_guerr_vic_all = sum(c_across(c("Dis_Guerr_any_jep", "Hom_Guerr_any_jep", "Rec_Guerr_any_jep", "Kid_Guerr_any_jep")), na.rm = TRUE)) |> 
    group_by(codmpio) |> 
    summarise(etv_guerr_pre = (sum(tot_guerr_vic_all) * 10000)/mean(pop_total),# Victims rate per 10,000 inhabitants  # mutate(etv_dh = (sum across years in tot_state_vic_d_h * 10000) / mean across years pop_total)
              presence_guerr_pre = ifelse(etv_guerr_pre != 0, 1, 0)) # Dummy of presence or not
  
  summary(df_guerr_etv_pre$presence_guerr_pre)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
  # 0.0000  1.0000  1.0000  0.8744  1.0000  1.0000 
  
  summary(df_guerr_etv_pre$etv_guerr_pre)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.000   2.814  12.684  33.640  43.317 485.596 
  
##### Paramilitary -------------------------------------------------------------

  df_param_etv_pre <- full_dataset_pretreat |>
    rowwise() |> #"Group" by row
    mutate(tot_param_vic_all = sum(c_across(c("Dis_Param_jep", "Hom_Param_jep", "Rec_Param_jep", "Kid_Param_jep")), na.rm = TRUE)) |> 
    group_by(codmpio) |> 
    summarise(etv_param_pre = (sum(tot_param_vic_all) * 10000)/mean(pop_total),# Victims rate per 10,000 inhabitants  # mutate(etv_dh = (sum across years in tot_state_vic_d_h * 10000) / mean across years pop_total)
              presence_param_pre = ifelse(etv_param_pre != 0, 1, 0)) # Dummy of presence or not
  
  
  summary(df_param_etv_pre$etv_param_pre)  
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.000   2.814  12.684  33.640  43.317 485.596 
     
  summary(df_param_etv_pre$presence_param_pre)   
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.0000  1.0000  1.0000  0.8744  1.0000  1.0000

###  5.3.3 Historical conflict and Seguridad Democractica: ----------------------------------
  
  # Getting the data from Panel CEDE
 
  violence_panel <- read_dta("data/in/covariates/Non-state actor presence/PANEL_CONFLICTO_Y_VIOLENCIA(2024).dta")
  
  sum(is.na(full_dataset$plebi_yes_prop))
  df_missing <- full_dataset |> 
    filter(is.na(plebi_yes_prop))
  
  ### Create final dataset for estimation
  cede_conflict <- violence_panel |> 
    filter(ano >= 1993 & ano <= 2012) |> # Availability of the data
    select(codmpio, d_refuerzo, conflicto) |> 
    group_by(codmpio) |> 
    summarize(dem_security = mean(d_refuerzo, na.rm = TRUE), #D_refuerzo: Seguridad democrática dummy 
              hist_land_conflict = mean(conflicto, na.rm = TRUE)) #Historical land conflict (1900 - 1931)
  
  summary(cede_conflict$dem_security)  
  
  summary(cede_conflict$hist_land_conflict)  
  
  ###  5.3.3 Historical conflict and Seguridad Democractica: ----------------------------------
  
  estimation_data <- final_data |> # Dataset with sociodemographic covariates 
    left_join(df_state_etv_pre, by = join_by(codmpio)) |> #Pre-treatment state ETV
    left_join(df_nonstate_etv_pre, by = join_by(codmpio)) |> # Pre-treatment Nonstate actors presence
    left_join(df_guerr_etv_pre, by = join_by(codmpio)) |># Pre-treatment Guerrilla presence
    left_join(df_param_etv_pre, by = join_by(codmpio)) |> # Pre-treatment Paramilitary presence
    left_join(cede_conflict, by = join_by(codmpio)) # Historical conflict variables from Panel CEDE
  
# 6. Save

  ## Save dataset
  
  #write_csv(estimation_data, "data/out/estimation_data.csv")
  
  ## Looking at the result:
  summary_report <- final_data |>
    select(-c("mpio","dpto","codmpio")) |>
    summarise(across(
      everything(),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        var  = ~var(.x, na.rm = TRUE),
        min  = ~min(.x, na.rm = TRUE),
        max  = ~max(.x, na.rm = TRUE),
        nas  = ~sum(is.na(.x))
      ),
      .names = "{.col}_{.fn}"
    ))|>
    pivot_longer(
      everything(),
      names_to = c("variable", "stat"),
      names_sep = "_(?=[^_]+$)"  # splits at last underscore
    ) |>
    pivot_wider(
      names_from = stat,
      values_from = value
    )
  