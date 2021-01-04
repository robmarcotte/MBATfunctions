# DO_collapse_MBAT
#
# Function to collapse hundredth of seconds observations to second-by-second observations
#
# Library dependencies: dplyr, MOCAfunctions

DO_collapse_MBAT = function(noldus_data, multi_task = F){
  noldus_data$index = rep(seq(1, ceiling(nrow(noldus_data)/100), by = 1), each= 100)[1:nrow(noldus_data)]

  if(multi_task == F){

    new_data  = data.frame(Filename = noldus_data$Filepath[1],
                           Timestamp = noldus_data$Timestamp[seq(1, nrow(noldus_data), by = 100)],
                           Behavior = tapply(noldus_data$Behavior, INDEX = noldus_data$index, FUN =  MOCAfunctions::majority_string),
                           Modifier_1 = tapply(noldus_data$Modifier_1, INDEX = noldus_data$index, FUN =  MOCAfunctions::majority_string),
                           METs = tapply(noldus_data$METs, INDEX = noldus_data$index, FUN =  MOCAfunctions::majority_string),
                           Location = tapply(noldus_data$Location., INDEX = noldus_data$index, FUN =  MOCAfunctions::majority_string),
                           Omit_me = tapply(noldus_data$Omit_Me, noldus_data$index, max, na.rm = T))
  } else {

    new_data = data.frame(index = seq(1, ceiling(nrow(noldus_data)/100), by = 1), stringsAsFactors = F)
    noldus_data$index = rep(seq(1, ceiling(nrow(noldus_data)/100), by = 1), each= 100)[1:nrow(noldus_data)]

    for(i in 1:ncol(noldus_data)){
      original_colnames = colnames(noldus_data)
      new_data = data.frame(index = seq(1, ceiling(nrow(noldus_data)/100), by = 1),
                            collapse_info = tapply(noldus_data[,i], noldus_data$index, FUN = MOCAfunctions::majority_string, piece = 'coded_strings'), stringsAsFactors = F)
      noldus_data = left_join(noldus_data, new_data)
      colnames(noldus_data) = c(original_colnames, colnames(noldus_data)[i])
    }
  }

  return(new_data)

}
