# DO_descriptives
#
# Function to extract the relevant DO information from the Noldus excel file
#
# Library dependencies: readxl, stringr, dplyr

DO_descriptives = function(filepath, keep_state_points = F){

  file_data = read_xlsx(filepath, col_names = T, n_max = 1)
  col_total = ncol(file_data)
  file_data = read_xlsx(filepath, col_names = T,
                        col_types = c('date','date','date','numeric','date','date',
                                      'numeric','numeric','numeric',
                                      rep('text', col_total-9)))

  # Remove columns that contain irrelevant information
  if(keep_state_points == F){
    file_data = file_data %>% filter(Event_Type == 'State start')
  } else {
    file_data = file_data %>% filter(Event_Type == 'State start' | Event_Type == 'State point')
  }

  file_data = file_data %>% select(Date_Time_Absolute_dmy_hmsf, Duration_sf, Behavior, starts_with('Modifier'))

  old_colnames = colnames(file_data)
  important_names_indices = str_which(old_colnames, paste('Behavior','Modifier', sep = '|'))

  # Create data frame that needs to be populated
  all_coded_variables = c('Behavior','Modifier_1','Computer use.','Phone use.','Watching TV.', 'Doing laundry.',
                          'Vacuuming.','Cooking.','Doing dishes/meal clean up.','Calisthenics.','Biking.',
                          'Reading.','Writing.','Walkie Talkie use.','Eating.','Drinking.','Brushing teeth.',
                          'Taking medication.','Headphones.','Glasses.','Fixing hair.','Putting on jewelry.',
                          'Bending down.','Location.')

  # Create character vector of all possible locations
  locations = c('Kitchen','Living room','Bedroom','Hallway','Bathroom','Laundry room')

  new_colnames = c(old_colnames[1:(important_names_indices[1]-1)],all_coded_variables)

  full_modifier_noldus = data.frame(matrix(NA, nrow = nrow(file_data), ncol = length(new_colnames), ))
  colnames(full_modifier_noldus) = new_colnames

  full_modifier_noldus[,12:ncol(full_modifier_noldus)] = apply(full_modifier_noldus[,12:ncol(full_modifier_noldus)], 2, as.character)


  for(i in 1:length(old_colnames)){

    # Check if it exists in the original exported data
    if(old_colnames[i] %in% new_colnames){
      new_index = str_which(new_colnames, old_colnames[i])

      full_modifier_noldus[,new_index] = file_data[,i]
    } else { # Otherwise check the column contents
      unique_obs = unique(file_data[,i], na.rm = T)
      unique_obs = data.frame(na.omit(unique_obs))

      if(nrow(unique_obs) == 1){ # If the column contents is only one thing, likely a non-mutually exclusive modifier
        modifier = as.character(unique_obs[1])
        new_index = str_which(new_colnames,modifier)

        full_modifier_noldus[,new_index] = file_data[,i]
      } else {

        if(names(file_data[i]) == 'Behavior'){ # Check if the column name exported is Behavior
          new_index = str_which(new_colnames, 'Behavior')

          full_modifier_noldus[,new_index] = file_data[,i]
        } else {
          if(any(unlist(unique_obs) %in% locations)){ # Check if the coded content matches any of the Location options in the coding scheme
            new_index = str_which(new_colnames,'Location.')
            full_modifier_noldus[,new_index] = file_data[,i]
          } else { # The remaining option has to be Main Activity
            if(names(file_data[i]) == 'Modifier_1'){
              new_index = str_which(new_colnames,'Modifier_1')
              full_modifier_noldus[,new_index] = file_data[,i]
            }
          }
        }
      }
    }
  }

  full_modifier_noldus$Duration_sf = as.numeric(full_modifier_noldus$Duration_sf)

  full_modifier_noldus = cbind(Filepath = basename(filepath), full_modifier_noldus)

  return(full_modifier_noldus)

}
