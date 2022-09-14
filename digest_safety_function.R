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
                                treatment_effect,
                                sample_size,
                                trials){

  pvar <- defData( # specify between-subject variation
    varname = "participant_variation",
    formula = 0,
    variance = between_variance,
    dist = "normal"
  )
  
  defA <- defDataAdd(varname = "pas_raw", # specify formula to compute PAS
                     formula = "1.05 + participant_variation",
                     dist = "nonrandom")
  
  dd <- genData(sample_size, pvar) # 3 participants
  
  dd <-
    genCluster(dd, # specify 10 trials per participant
               cLevelVar = "participant_variation",
               numIndsVar = trials,
               level1ID = "total_trials")
  
  dd <- addColumns(defA, dd)
  
  pas_distribution <- # probabilities for each PAS score (1-8)
    c(0.323, 0.015, 0.256, 0.0076, 0.233, 0.015, 0.0526, 0.0978) 
  
  data <-
    genOrdCat( # add PAS outcome
      dd,
      adjVar = "pas_raw",
      baseprobs = pas_distribution,
      catVar = "pas",
      idname = "total_trials"
    )
  
  data <- data |> 
    select(id, pas) # select only ID and PAS scores
  
  data$pas <- as.numeric(data$pas) # convert PAS to numeric
  
  data <- data |> # simulate vocal fold and subglottic residue with beta distribution (skewed right) for appropriate cases
    group_by(id) |> 
    mutate(vocal_folds_severity_rating = case_when(pas == 5 ~ rbeta(n(), 1, 9),
                                                   pas != 5 ~ NA_real_),
           subglottis_severity_rating = case_when(pas == 7 | pas == 8 ~ rbeta(n(), 1, 9),
                                                  pas != 7 ~ NA_real_)
    )
}

# Simulate data
data <- sim_swallowing_data(
  between_variance = 1,
  sample_size = 20,
  trials = 10
)

# Function to calculate DIGEST safety scores, arguments specified below
digest_safety <- function(data, # put data frame title here
                          id, # put name of id grouping variable here
                          pas, # put name of PAS outcome variable here
                          vocal_folds_severity_rating, # put name of VF severity rating here
                          subglottis_severity_rating # put name of subglottis severity rating here
                          ) {
  data |> 
  group_by(id) |> 
  mutate(max_pas = max(pas), # max PAS
         freq_3_4 = sum(pas == 3 | pas == 4), # frequencies for PAS 3 & 4
         freq_5_6 = sum(pas == 5 | pas == 6), # frequencies for PAS 5 & 6
         freq_7_8 = sum(pas == 7 | pas == 8), # frequencies for PAS 7 & 8
         num_trials = n(), # count number of trials per participant
         perc_7_8 = freq_7_8/num_trials,
  ) |> 
  top_n(1, pas) |>  # keep rows based on max PAS
  mutate( # calculate max values for VF and subglottis for when > 2 trials with same max PAS score
    vocal_folds_severity_rating_max = case_when(
      max_pas >= 5 | max_pas <= 6 ~ max(vocal_folds_severity_rating)),
    subglottis_severity_rating_max = case_when(max_pas >= 7 | max_pas <= 8 ~ max(subglottis_severity_rating))
  ) |> 
  dplyr::slice(1) |> 
  mutate(safety_score = case_when(
    freq_3_4 == 0 & max_pas == 1 | max_pas == 2 ~ 0,
    freq_3_4 == 1 & max_pas == 3 | max_pas == 4 ~ 0, # PAS 3-4, single event
                                  freq_3_4 > 1 & max_pas == 3 | max_pas == 4 ~ 1, # PAS 3-4, multiple events
                                  freq_5_6 == 1 & vocal_folds_severity_rating_max <= 25 & max_pas == 5 | max_pas == 6 ~ 1, # PAS 5-6, single, not gross
                                  freq_5_6 == 1 & vocal_folds_severity_rating_max > 25 & max_pas == 5 | max_pas == 6 ~ 2, # PAS 5-6, single event, gross
                                  freq_5_6 > 1 & max_pas == 5 | max_pas == 6 ~ 2, # PAS 5-6, multiple events
                                  freq_7_8 == 1 & subglottis_severity_rating_max < 25 & freq_5_6 < 1 & max_pas == 7 | max_pas == 8 ~ 1, # PAS 7-8, single event, not gross, no PAS 5 or 6
                                  freq_7_8 == 1 & subglottis_severity_rating_max < 25 & freq_5_6 >= 1 & max_pas == 7 | max_pas == 8 ~ 2, # PAS 7-8, single event, not gross, additional PAS 5 or 6's
                                  freq_7_8 > 1 & perc_7_8 < .50 & subglottis_severity_rating_max < 25 & max_pas == 7 | max_pas == 8 ~ 2, # PAS 7-8, less than 50%, not gross
                                  freq_7_8 > 1 & perc_7_8 >= .50 & subglottis_severity_rating_max < 25 & max_pas == 7 | max_pas == 8 ~ 3, # PAS 7-8, more than 50%, not gross
                                  freq_7_8 >= 1 & perc_7_8 < .50 & subglottis_severity_rating_max >= 25 & max_pas == 7 | max_pas == 8 ~ 3, # PAS 7-8, less than 50%, gross
                                  freq_7_8 > 1 & perc_7_8 >= .50 & subglottis_severity_rating_max >= 25 & max_pas == 7 | max_pas == 8 ~ 4 # PAS 7-8, more than 50%, gross
                                  )
  ) |> 
  select(id, max_pas, freq_3_4, freq_5_6, freq_7_8, perc_7_8, 
         vocal_folds_severity_rating_max, subglottis_severity_rating_max, safety_score)
}

# Use function to derive DIGEST safety scores
digest_safety(data = data, # note that arguments use the same names as the data for simplicity
              id = id,
              pas = pas,
              vocal_folds_severity_rating = vocal_folds_severity_rating, 
              subglottis_severity_rating = subglottis_severity_rating)

  