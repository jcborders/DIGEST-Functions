
# First calculate number of trials for each consistency
data_trials <- data |> 
  group_by(id, IDDSI) |> 
  summarise(num_trials = n()) |> 
  pivot_wider(names_from = "IDDSI",
              values_from = "num_trials") |> 
  rename(IDDSI_0_trials = `0`,
         IDDSI_4_trials = `4`)

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
         IDDSI_4_freq = `4`) |> 
  mutate(chronic = case_when(IDDSI_0_freq > 0 & IDDSI_4_freq > 0 ~ 1))

# Add these columns to full dataset
data3 <- full_join(data2, freq_df, by = "id")

# Calculate DIGEST safety scores
data3 |> 
  group_by(id) |> 
  mutate(max_pas = max(pas), # max PAS
         freq_airway_invasion = sum(pas > 2), # frequency PAS greater than 2
         freq_5_6 = sum(pas == 5 | pas == 6), # frequencies for PAS 5 & 6
         freq_7_8 = sum(pas == 7 | pas == 8), # frequency of PAS 7 or 8
         perc_7_8_thin = freq_7_8/IDDSI_0_trials,
         pas_7_8_frequency = case_when(pas == 7 | pas == 8 & IDDSI == 0 ~ 1,
                                       pas == 7 | pas == 8 & IDDSI > 0 ~ 1),
         intermittent_IDDSI_0 = IDDSI_0_freq/IDDSI_0_trials,
         intermittent_IDDSI_4 = IDDSI_4_freq/IDDSI_4_trials
  ) |> 
  top_n(1, pas) |>  # keep rows based on max PAS
  mutate( # calculate max values for VF and subglottis for when > 2 trials with same max PAS score
    vocal_folds_severity_rating_max = case_when(
      max_pas >= 5 | max_pas <= 6 ~ max(vocal_folds_severity_rating)),
    subglottis_severity_rating_max = case_when(max_pas >= 7 | max_pas <= 8 ~ max(subglottis_severity_rating))
  ) |> 
  dplyr::slice(1) |> 
  mutate(safety_score = case_when(
    max_pas == 1 | max_pas == 2 ~ 0,
    freq_airway_invasion == 1 & max_pas == 3 | max_pas == 4 ~ 0, # PAS 3-4, single event
    freq_airway_invasion > 1 & max_pas == 3 | max_pas == 4 ~ 1, # PAS 3-4, multiple events
    freq_airway_invasion == 1 & vocal_folds_severity_rating_max <= 25 & max_pas == 5 | max_pas == 6 ~ 1, # PAS 5-6, single, not gross
    freq_airway_invasion == 1 & vocal_folds_severity_rating_max > 25 & max_pas == 5 | max_pas == 6 ~ 2, # PAS 5-6, single event, gross
    freq_airway_invasion > 1 & max_pas == 5 | max_pas == 6 ~ 2, # PAS 5-6, multiple events
    freq_airway_invasion == 1 & subglottis_severity_rating_max < 25 & freq_5_6 < 1 & max_pas == 7 | max_pas == 8 ~ 1, # PAS 7-8, single event, not gross, no PAS 5 or 6
    freq_airway_invasion == 1 & subglottis_severity_rating_max < 25 & freq_5_6 >= 1 & max_pas == 7 | max_pas == 8 ~ 2, # PAS 7-8, single event, not gross, additional PAS 5 or 6's
    freq_airway_invasion > 1 & intermittent_IDDSI_0 < 0.50 | intermittent_IDDSI_0 < 0.50 & subglottis_severity_rating_max < 25 & max_pas == 7 | max_pas == 8 ~ 2, # PAS 7-8, less than 50%, not gross
    freq_airway_invasion > 1 & intermittent_IDDSI_0 >= 0.50 | chronic == 1 & subglottis_severity_rating_max < 25 & max_pas == 7 | max_pas == 8 ~ 3, # PAS 7-8, chronic, not gross
    freq_airway_invasion >= 1 & intermittent_IDDSI_0 < 0.50 | intermittent_IDDSI_0 < 0.50 & subglottis_severity_rating_max >= 25 & max_pas == 7 | max_pas == 8 ~ 3, # PAS 7-8, less than 50%, gross
    freq_airway_invasion > 1 & intermittent_IDDSI_0 >= 0.50 | chronic == 1 & subglottis_severity_rating_max >= 25 & max_pas == 7 | max_pas == 8 ~ 4 # PAS 7-8, more than 50%, gross
  )
  ) |> 
  select(id, max_pas, safety_score)






