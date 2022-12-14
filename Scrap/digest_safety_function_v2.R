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
                                             pas != 7 ~ NA_real_)
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
                                             pas != 7 ~ NA_real_)
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
      vocal_folds_severity_rating = case_when(pas == 5 ~ rbeta(n(), 1, 9),
                                              pas != 5 ~ NA_real_),
      subglottis_severity_rating = case_when(pas == 7 |
                                               pas == 8 ~ rbeta(n(), 1, 9),
                                             pas != 7 ~ NA_real_)
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
  # First calculate number of trials for each consistency
  data_trials <- data |> 
    group_by(id, IDDSI) |> 
    summarise(num_trials = n()) |> 
    pivot_wider(names_from = "IDDSI",
                values_from = "num_trials") |> 
    rename(IDDSI_0_trials = `0`,
           IDDSI_4_trials = `4`,
           IDDSI_7_trials = `7`)
  
  # Add these columns to full dataset
  data2 <- full_join(data, data_trials, by = "id")
  
  # Calculate frequency/percentage of airway invasion for each consistency grouping
  freq_df <- data2 |> 
    group_by(id) |> 
    mutate(binary_airway_invasion = case_when(pas > 2 ~ 1,
                                              pas <= 2 ~ 0)) |>  # when PAS > 2 give 1 otherwise 0
    group_by(id, IDDSI) |> 
    summarise(freq_airway_invasion = sum(binary_airway_invasion)) |> 
    pivot_wider(names_from = "IDDSI",
                values_from = "freq_airway_invasion") |> 
    rename(IDDSI_0_freq = `0`,
           IDDSI_4_freq = `4`,
           IDDSI_7_freq = `7`) |> 
    mutate(chronic = case_when(IDDSI_0_freq > 0 & IDDSI_4_freq > 0 | IDDSI_0_freq > 0 & IDDSI_7_freq > 0 | IDDSI_4_freq > 0 & IDDSI_7_freq > 0 ~ 1))
  
  # Add these columns to full dataset
  data3 <- full_join(data2, freq_df, by = "id")
  
  # Calculate DIGEST safety scores
  data3 |> 
    group_by(id) |> 
    mutate(max_pas = max(pas), # max PAS
           freq_airway_invasion = sum(pas > 2), # frequency PAS greater than 2
           freq_5_6 = sum(pas == 5 | pas == 6), # frequencies for PAS 5 & 6
           freq_7_8 = sum(pas == 7 | pas == 8), # frequency of PAS 7 or 8
           perc_7_8_iddsi_0 = freq_7_8/IDDSI_0_trials,
           pas_7_8_frequency = case_when(pas == 7 | pas == 8 & IDDSI == 0 ~ 1,
                                         pas == 7 | pas == 8 & IDDSI > 0 ~ 1),
           intermittent_IDDSI_0 = IDDSI_0_freq/IDDSI_0_trials,
           intermittent_IDDSI_4 = IDDSI_4_freq/IDDSI_4_trials,
           intermittent_IDDSI_7 = IDDSI_7_freq/IDDSI_7_trials
    ) |> 
    top_n(1, pas) |>  # keep rows based on max PAS
    mutate( # calculate max values for VF and subglottis for when > 2 trials with same max PAS score
      vocal_folds_severity_rating_max = case_when(
        max_pas >= 5 | max_pas <= 6 ~ max(vocal_folds_severity_rating)), # is this combo of VF or subglottis or deepest landmark???
      subglottis_severity_rating_max = case_when(max_pas >= 7 | max_pas <= 8 ~ max(subglottis_severity_rating))
    ) |> 
    dplyr::slice(1) |> 
    mutate(safety_score = case_when(
      max_pas == 1 | max_pas == 2 ~ 0,
      freq_airway_invasion == 1 & max_pas == 3 | max_pas == 4 ~ 0, # PAS 3-4, single event
      freq_airway_invasion > 1 & max_pas == 3 | max_pas == 4 ~ 1, # PAS 3-4, multiple events
      freq_airway_invasion == 1 & vocal_folds_severity_rating_max <= .25 & max_pas == 5 | max_pas == 6 ~ 1, # PAS 5-6, single, not gross
      freq_airway_invasion == 1 & vocal_folds_severity_rating_max > .25 & max_pas == 5 | max_pas == 6 ~ 2, # PAS 5-6, single event, gross - CHECK THIS
      freq_airway_invasion > 1 & max_pas == 5 | max_pas == 6 ~ 2, # PAS 5-6, multiple events
      freq_airway_invasion == 1 & subglottis_severity_rating_max <= .25 & freq_5_6 < 1 & max_pas == 7 | max_pas == 8 ~ 1, # PAS 7-8, single event, not gross, no PAS 5 or 6
      freq_airway_invasion == 1 & subglottis_severity_rating_max <= .25 & freq_5_6 >= 1 & max_pas == 7 | max_pas == 8 ~ 2, # PAS 7-8, single event, not gross, additional PAS 5 or 6's
      freq_airway_invasion > 1 & intermittent_IDDSI_0 < 0.50 | intermittent_IDDSI_4 < 0.50 | intermittent_IDDSI_4 < 0.50 & subglottis_severity_rating_max <= .25 & max_pas == 7 | max_pas == 8 ~ 2, # PAS 7-8, less than 50%, not gross
      freq_airway_invasion > 1 & intermittent_IDDSI_0 >= 0.50 | chronic == 1 & subglottis_severity_rating_max <= .25 & max_pas == 7 | max_pas == 8 ~ 3, # PAS 7-8, chronic, not gross
      freq_airway_invasion >= 1 & intermittent_IDDSI_0 < 0.50 | intermittent_IDDSI_0 < 0.50 & subglottis_severity_rating_max > .25 & max_pas == 7 | max_pas == 8 ~ 3, # PAS 7-8, less than 50%, gross
      freq_airway_invasion > 1 & intermittent_IDDSI_0 >= 0.50 | chronic == 1 & subglottis_severity_rating_max > .25 & max_pas == 7 | max_pas == 8 ~ 4 # PAS 7-8, more than 50%, gross
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

  