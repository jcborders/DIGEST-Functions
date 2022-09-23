data_x <- data |> 
  group_by(id, IDDSI) |> 
  summarise(num_trials = n()) # calculate # of trials for each consistency

x <- inner_join(data, data_x) # join # of trials summary with full data set

x2 <- x |> 
  group_by(id, IDDSI, .drop = F) |> # calculate % of PAS for each grouping by consistency
  mutate(perc_3_4 = sum(pas == 3 | pas == 4)/num_trials,
         perc_5_6 = sum(pas == 3 | pas == 4)/num_trials,
         perc_7_8 = sum(pas == 3 | pas == 4)/num_trials) |> 
  group_by(id) |> 
  mutate(binary_airway_invasion = case_when(pas > 2 ~ 1, # counts number of PAS > 2 events per participant
                                            pas <= 2 ~ 0),
         max_pas = max(pas), # max PAS
         freq_3_4 = sum(pas == 3 | pas == 4), # overall frequencies for PAS 3 & 4
         freq_5_6 = sum(pas == 5 | pas == 6), # overall frequencies for PAS 5 & 6
         freq_7_8 = sum(pas == 7 | pas == 8)) # overall frequency of PAS 7 or 8
         
x3 <- x2 |> # calculates airway invasion across multiple consistencies
  group_by(id, IDDSI) |> 
  mutate(freq_airway_invasion = sum(binary_airway_invasion),
         airway_invasion_by_consistency = case_when(freq_airway_invasion >= 1 ~ 1,
                                                    freq_airway_invasion < 1 ~ 0)
         ) |> 
  dplyr::slice(1) |> 
  group_by(id) |> 
  summarise(sum(airway_invasion_by_consistency)) |> 
  mutate(airway_invasion_multiple_consistencies = case_when(
    `sum(airway_invasion_by_consistency)` > 1 ~ "yes",
    `sum(airway_invasion_by_consistency)` <= 1 ~ "no")) |> 
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
         pas_3_4_chronic_pre_sum = sum(pas_3_4_chronic_pre), # add up freq of these events for each consistency
         pas_5_6_chronic_pre_sum = sum(pas_5_6_chronic_pre),
         pas_7_8_chronic_pre_sum = sum(pas_7_8_chronic_pre)
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

# Use function to derive DIGEST safety scores
digest_safety(data = data, # note that arguments use the same names as the data for simplicity
              id = id,
              pas = pas,
              vocal_folds_severity_rating = vocal_folds_severity_rating, 
              subglottis_severity_rating = subglottis_severity_rating)
