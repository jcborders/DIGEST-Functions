# James Borders - 9/12/22

# The below functions simulate swallowing data (sim_swallowing_data)
# and then calculates DIGEST safety scores 
# per Starmer et al. (2021) - Adaptation and Validation of the DIGEST for FEES

# Load Packages
library(tidyverse) # data wrangling
library(simstudy) # simulate data
set.seed(2022) # simulation reproducibility

# Simulate fake data set to illustrate use
sim_swallowing_data <- function(between_variance, 
                                sample_size,
                                trials){
  # Simulate IDDSI 0 Data
  pvar <- defData(
    # specify between-subject variation
    varname = "participant_variation",
    formula = 0,
    variance = between_variance,
    dist = "normal"
  )
  
  defA_iddsi_0 <-
    defDataAdd(varname = "pas_raw",
               # specify formula to compute PAS
               formula = "1.05 + participant_variation",
               dist = "nonrandom")
  
  dd <- genData(sample_size, pvar) # 3 participants
  
  dd <-
    genCluster(dd,
               # specify 10 trials per participant
               cLevelVar = "participant_variation",
               numIndsVar = trials,
               level1ID = "total_trials")
  
  dd <- addColumns(defA_iddsi_0, dd)
  
  pas_distribution_iddsi_0 <- # probabilities for each PAS score (1-8)
    c(0.323, 0.015, 0.256, 0.0076, 0.233, 0.015, 0.0526, 0.0978)
  
  data_iddsi_0 <-
    genOrdCat(
      # add PAS outcome
      dd,
      adjVar = "pas_raw",
      baseprobs = pas_distribution_iddsi_0,
      catVar = "pas",
      idname = "total_trials"
    )
  
  data_iddsi_0 <- data_iddsi_0 |>
    select(id, pas)  # select only ID and PAS scores
  
  
  data_iddsi_0$pas <-
    as.numeric(data_iddsi_0$pas) # convert PAS to numeric
  
  data_iddsi_0 <-
    data_iddsi_0 |> # simulate vocal fold and subglottic residue with beta distribution (skewed right) for appropriate cases
    group_by(id) |>
    mutate(
      vocal_folds_severity_rating = case_when(pas == 5 ~ rbeta(n(), 1, 9),
                                              pas != 5 ~ NA_real_),
      subglottis_severity_rating = case_when(pas == 7 |
                                               pas == 8 ~ rbeta(n(), 1, 9),
                                             pas != 7 ~ NA_real_),
      percent_pharyngeal_residue = rbeta(n(), 1, 10)
    ) |>
    add_column(IDDSI = 0)
  
  
  ## Simulate iddsi_4 Data
  defA_iddsi_4 <-
    defDataAdd(varname = "pas_raw",
               # specify formula to compute PAS
               formula = "1.05 + participant_variation",
               dist = "nonrandom")
  
  dd_iddsi_4 <- genData(sample_size, pvar) # 3 participants
  
  dd_iddsi_4 <-
    genCluster(
      dd_iddsi_4,
      # specify 10 trials per participant
      cLevelVar = "participant_variation",
      numIndsVar = trials,
      level1ID = "total_trials"
    )
  
  dd_iddsi_4 <- addColumns(defA_iddsi_4, dd_iddsi_4)
  
  pas_distribution_iddsi_4 <-
    # probabilities for each PAS score (1-8)
    c(0.7, 0.015, 0.2, 0.0076, 0.366, 0.01, 0.01, 0.01)
  
  data_iddsi_4 <-
    genOrdCat(
      # add PAS outcome
      dd_iddsi_4,
      adjVar = "pas_raw",
      baseprobs = pas_distribution_iddsi_4,
      catVar = "pas",
      idname = "total_trials"
    )
  
  data_iddsi_4 <- data_iddsi_4 |>
    select(id, pas)  # select only ID and PAS scores
  
  
  data_iddsi_4$pas <-
    as.numeric(data_iddsi_4$pas) # convert PAS to numeric
  
  data_iddsi_4 <-
    data_iddsi_4 |> # simulate vocal fold and subglottic residue with beta distribution (skewed right) for appropriate cases
    group_by(id) |>
    mutate(
      vocal_folds_severity_rating = case_when(pas == 5 ~ rbeta(n(), 1, 9),
                                              pas != 5 ~ NA_real_),
      subglottis_severity_rating = case_when(pas == 7 |
                                               pas == 8 ~ rbeta(n(), 1, 9),
                                             pas != 7 ~ NA_real_),
      percent_pharyngeal_residue = rbeta(n(), 1, 6)
    ) |>
    add_column(IDDSI = 4)
  
  ## Simulate IDDSI 7 Data
  defA_iddsi_7 <-
    defDataAdd(varname = "pas_raw",
               # specify formula to compute PAS
               formula = "1.05 + participant_variation",
               dist = "nonrandom")
  
  dd_iddsi_7 <- genData(sample_size, pvar) # 3 participants
  
  dd_iddsi_7 <-
    genCluster(
      dd_iddsi_7,
      # specify 10 trials per participant
      cLevelVar = "participant_variation",
      numIndsVar = trials,
      level1ID = "total_trials"
    )
  
  dd_iddsi_7 <- addColumns(defA_iddsi_7, dd_iddsi_7)
  
  pas_distribution_iddsi_7 <-
    # probabilities for each PAS score (1-8)
    c(0.7, 0.015, 0.2, 0.0076, 0.366, 0.01, 0.01, 0.01)
  
  data_iddsi_7 <-
    genOrdCat(
      # add PAS outcome
      dd_iddsi_7,
      adjVar = "pas_raw",
      baseprobs = pas_distribution_iddsi_7,
      catVar = "pas",
      idname = "total_trials"
    )
  
  data_iddsi_7 <- data_iddsi_7 |>
    select(id, pas)  # select only ID and PAS scores
  
  
  data_iddsi_7$pas <-
    as.numeric(data_iddsi_7$pas) # convert PAS to numeric
  
  data_iddsi_7 <-
    data_iddsi_7 |> # simulate vocal fold and subglottic residue with beta distribution (skewed right) for appropriate cases
    group_by(id) |>
    mutate(
      vocal_folds_severity_rating = case_when(pas == 5 ~ rbeta(n(), 1, 10),
                                              pas != 5 ~ NA_real_),
      subglottis_severity_rating = case_when(pas == 7 |
                                               pas == 8 ~ rbeta(n(), 1, 10),
                                             pas != 7 ~ NA_real_),
      percent_pharyngeal_residue = rbeta(n(), 1, 5)
    ) |>
    add_column(IDDSI = 7)
  
  # Combine columns
  data <- rbind(data_iddsi_0, data_iddsi_4, data_iddsi_7)
}

# Simulate data
data <- sim_swallowing_data(
  between_variance = 1,
  sample_size = 20,
  trials = 3
)

# Function to calculate DIGEST safety scores, arguments specified below
digest_safety <- function(data, # put data frame title here
                          id, # put name of id grouping variable here
                          pas, # put name of PAS outcome variable here
                          vocal_folds_severity_rating, # put name of VF severity rating here
                          subglottis_severity_rating # put name of subglottis severity rating here
) {
  data_x <- data |> 
    group_by(id, IDDSI) |> 
    summarise(num_trials = n()) # calculate # of trials for each consistency
  
  x <- inner_join(data, data_x) # join # of trials summary with full data set
  
  x2 <- x |> 
    group_by(id, IDDSI, .drop = F) |> # calculate % of PAS for each grouping by consistency
    mutate(perc_3_4 = sum(pas == 3 | pas == 4, na.rm = T)/num_trials,
           perc_5_6 = sum(pas == 3 | pas == 4, na.rm = T)/num_trials,
           perc_7_8 = sum(pas == 3 | pas == 4, na.rm = T)/num_trials) |> 
    group_by(id) |> 
    mutate(binary_airway_invasion = case_when(pas > 2 ~ 1, # counts number of PAS > 2 events per participant
                                              pas <= 2 ~ 0),
           max_pas = max(pas, na.rm = T), # max PAS
           freq_3_4 = sum(pas == 3 | pas == 4, na.rm = T), # overall frequencies for PAS 3 & 4
           freq_5_6 = sum(pas == 5 | pas == 6, na.rm = T), # overall frequencies for PAS 5 & 6
           freq_7_8 = sum(pas == 7 | pas == 8, na.rm = T)) # overall frequency of PAS 7 or 8
  
  x3 <- x2 |> # calculates airway invasion across multiple consistencies
    group_by(id, IDDSI) |> 
    mutate(freq_airway_invasion = sum(binary_airway_invasion, na.rm = T),
           airway_invasion_by_consistency = case_when(freq_airway_invasion >= 1 ~ 1,
                                                      freq_airway_invasion < 1 ~ 0)
    ) |> 
    dplyr::slice(1) |> 
    group_by(id) |> 
    summarise(sum(airway_invasion_by_consistency, na.rm = T)) |> 
    mutate(airway_invasion_multiple_consistencies = case_when(
      `sum(airway_invasion_by_consistency, na.rm = T)` > 1 ~ "yes",
      `sum(airway_invasion_by_consistency, na.rm = T)` <= 1 ~ "no")) |> 
    dplyr::select(id, airway_invasion_multiple_consistencies)
  
  x4 <- inner_join(x2, x3)
  
  x5 <- x4 |> # summary info on whether someone had intermittent or chronic frequency on at least 1 consistency
    group_by(id, IDDSI) |> 
    dplyr::slice(1) |> 
    group_by(id) |> 
    mutate(pas_3_4_chronic_pre = case_when(perc_3_4 < .50 ~ 0, # count number of times >= 50% events
                                           perc_3_4 >= .50 ~ 1),
           pas_5_6_chronic_pre = case_when(perc_5_6 < .50 ~ 0,
                                           perc_5_6 >= .50 ~ 1),
           pas_7_8_chronic_pre = case_when(perc_7_8 < .50 ~ 0,
                                           perc_7_8 >= .50 ~ 1),
           pas_3_4_chronic_pre_sum = sum(pas_3_4_chronic_pre, na.rm = T), # add up freq of these events for each consistency
           pas_5_6_chronic_pre_sum = sum(pas_5_6_chronic_pre, na.rm = T),
           pas_7_8_chronic_pre_sum = sum(pas_7_8_chronic_pre, na.rm = T)
    ) |> 
    dplyr::slice(1) |> 
    mutate(pas_3_4_chronic = case_when(pas_3_4_chronic_pre_sum <= 1 ~ "intermittent",
                                       pas_3_4_chronic_pre_sum >1 ~ "chronic"),
           pas_5_6_chronic = case_when(pas_5_6_chronic_pre_sum <= 1 ~ "intermittent",
                                       pas_5_6_chronic_pre_sum >1 ~ "chronic"),
           pas_7_8_chronic = case_when(pas_7_8_chronic_pre_sum <= 1 ~ "intermittent",
                                       pas_7_8_chronic_pre_sum >1 ~ "chronic")
    ) |>
    dplyr::select(id, pas_3_4_chronic, pas_5_6_chronic, pas_7_8_chronic)
  
  x6 <- inner_join(x4, x5)
  
  x6 |> 
    dplyr::select(id, pas, max_pas, vocal_folds_severity_rating, 
                  subglottis_severity_rating, freq_3_4, freq_5_6, freq_7_8,
                  airway_invasion_multiple_consistencies, pas_3_4_chronic,
                  pas_5_6_chronic, pas_7_8_chronic) |> 
    drop_na(pas) |> 
    group_by(id) |> 
    top_n(1, pas) |>  # keep rows based on max PAS
    mutate( # calculate max values for VF and subglottis for when > 2 trials with same max PAS score
      vocal_folds_severity_rating_max = case_when(
        max_pas >= 5 | max_pas <= 6 ~ max(vocal_folds_severity_rating)), # is this combo of VF or subglottis or deepest landmark???
      subglottis_severity_rating_max = case_when(max_pas >= 7 | max_pas <= 8 ~ max(subglottis_severity_rating))
    ) |> 
    dplyr::slice(1) |> 
    mutate(safety_score = case_when(
      max_pas == 1 | max_pas == 2 ~ 0,
      freq_3_4 == 1 & max_pas == 3 | max_pas == 4 ~ 0, # PAS 3-4, single event
      freq_3_4 > 1 & max_pas == 3 | max_pas == 4 ~ 1, # PAS 3-4, multiple events
      freq_5_6 == 1 & vocal_folds_severity_rating_max <= .25 & max_pas == 5 | max_pas == 6 ~ 1, # PAS 5-6, single, not gross
      freq_5_6 == 1 & vocal_folds_severity_rating_max > .25 & max_pas == 5 | max_pas == 6 ~ 2, # PAS 5-6, single event, gross
      freq_5_6 > 1 & max_pas == 5 | max_pas == 6 ~ 2, # PAS 5-6, multiple events
      freq_7_8 == 1 & subglottis_severity_rating_max <= .25 & freq_5_6 < 1 & max_pas == 7 | max_pas == 8 ~ 1, # PAS 7-8, single event, not gross, no PAS 5 or 6
      freq_7_8 == 1 & subglottis_severity_rating_max <= .25 & freq_5_6 >= 1 & max_pas == 7 | max_pas == 8 ~ 2, # PAS 7-8, single event, not gross, additional PAS 5 or 6's
      freq_7_8 > 1 & pas_7_8_chronic == "intermittent" & subglottis_severity_rating_max <= .25 & max_pas == 7 | max_pas == 8 ~ 2, # PAS 7-8, less than 50%, not gross
      freq_7_8 > 1 & pas_7_8_chronic == "chronic" | airway_invasion_multiple_consistencies == "yes" & subglottis_severity_rating_max <= .25 & max_pas == 7 | max_pas == 8 ~ 3, # PAS 7-8, chronic, not gross
      freq_7_8 >= 1 & pas_7_8_chronic == "intermittent" & subglottis_severity_rating_max > .25 & max_pas == 7 | max_pas == 8 ~ 3, # PAS 7-8, less than 50%, gross
      freq_7_8 > 1 & pas_7_8_chronic == "chronic" | airway_invasion_multiple_consistencies == "yes" & subglottis_severity_rating_max > .25 & max_pas == 7 | max_pas == 8 ~ 4 # PAS 7-8, more than 50%, gross
    )
    ) |> 
    select(id, max_pas, safety_score)
}

# Use function to derive DIGEST safety scores
digest_safety(data = data, # note that arguments use the same names as the data for simplicity
              id = id,
              pas = pas,
              vocal_folds_severity_rating = vocal_folds_severity_rating, 
              subglottis_severity_rating = subglottis_severity_rating)


# Function to calculate DIGEST efficiency scores, arguments specified below
digest_safety <- function(data, # put data frame title here
                          id, # put name of id grouping variable here
                          pas, # put name of PAS outcome variable here
                          oropharyngeal_severity_rating, # put name of oro severity rating here
                          hypopharyngeal_severity_rating, # put name of hypo severity rating here
                          laryngeal_vestibule_severity_rating, # put name of LV severity rating here
                          vocal_folds_severity_rating, # put name of VF severity rating here
                          subglottis_severity_rating # put name of subglottis severity rating here
) {
  
  df1 <- data |> # calculate efficiency groupings by consistency
    group_by(id, IDDSI) |> 
    mutate(residue_average_score = mean(c(oropharyngeal_severity_rating, 
                                          hypopharyngeal_severity_rating
                                          # epiglottis_severity_rating,
                                          # laryngeal_vestibule_severity_rating,
                                          # vocal_folds_severity_rating,
                                          # subglottis_severity_rating
                                          ), na.rm = T),
           max_residue_score = max(residue_average_score)
           ) |> 
    dplyr::slice(1) |> 
    mutate(efficiency_group_1 = case_when(residue_average_score < .10 ~ 1),
           efficiency_group_2 = case_when(residue_average_score >= .10 & residue_average_score <= .33 ~ 1),
           efficiency_group_3 = case_when(residue_average_score >= .34 & residue_average_score <= .66 ~ 1),
           efficiency_group_4 = case_when(residue_average_score < .66 ~ 1)
           ) |> 
    dplyr::select(id, IDDSI, max_residue_score, efficiency_group_1, efficiency_group_2, efficiency_group_3, efficiency_group_4)
  
  df2 <- df1 |> # count number of consistencies administered per participant
    group_by(id) |> 
    summarise(number_of_consistencies = n())
  
  df3 <- inner_join(df1, df2)
  
  df4 <- df3 |> 
    group_by(id) |> 
    mutate(efficiency_grade = case_when(max_residue_score < .10 ~ 0,
                                        max_residue_score >= .10 & max_residue_score <= .33 ~ 1,
                                        IDDSI == 7 & max_residue_score >= .34 & max_residue_score <= .66 ~ 2,
                                        IDDSI == 0 | IDDSI == 4 & max_residue_score >= .34 & max_residue_score <= .66 ~ 3,
                                        efficiency_group_4/number_of_consistencies > 0 ~ 3,
                                        efficiency_group_4/number_of_consistencies == 1 ~ 4)
           ) |> 
    dplyr::slice(1)
  
  
}





