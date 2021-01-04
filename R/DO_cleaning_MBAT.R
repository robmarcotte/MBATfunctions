# DO_cleaning_18to20
#
# Function to clean up noldus data with a data frame of known errors.
#
# Library dependencies: dplyr
DO_cleaning_MBAT = function(noldus_data, manual_fix){

  # Provide METs based on the Behavior and Primary Activity
  behavior_mets = do_fix_MBAT_METs %>% select(Behavior, MET) %>% rename(Behavior_METs = MET)
  noldus_data = left_join(noldus_data, behavior_mets)

  activity_mets = do_fix_MBAT_METs %>% select(Behavior, MET) %>%
    rename(Modifier_1 = Behavior,
           Activity_METs = MET)
  noldus_data = left_join(noldus_data, activity_mets)

  noldus_data$Activity_METs = ifelse(noldus_data$Activity_METs == 'Behavior', noldus_data$Behavior_METs, noldus_data$Activity_METs)

  # Private time indices have both Behavior for both Behavior and Modifier_1 METs. Assign the Laundry METs
  noldus_data$Behavior_METs[which(noldus_data$Behavior_METs == 'Behavior')] = noldus_data$Activity_METs[which(noldus_data$Behavior_METs == 'Behavior')]

  omit_obs = do_fix_MBAT %>% select(Behavior,Modifier_1, Omit_Me, Reason)

  noldus_data = left_join(noldus_data, omit_obs)

  noldus_data = noldus_data %>% mutate(Behavior_METs = as.numeric(Behavior_METs),
                                       Activity_METs = as.numeric(Activity_METs),
                                       METs = ifelse(Behavior_METs>=Activity_METs, Behavior_METs, Activity_METs)) %>%
    select(-Behavior_METs, -Activity_METs)

  return(noldus_data)
}
