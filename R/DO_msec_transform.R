# DO_msec_transform
#
# Function to expand noldus data observations to hundredths of a second observations
# Accepts Noldus data that has been processed using the DO_descriptives function
#
# noldus_data = Noldus data previously processed using the DO_descriptives function
#
# Returns a data frame with the Noldus data expanded to hundredths of a second
#
# Library dependencies: stringr, readxl, dplyr, lubridate

DO_msec_transform <- function(noldus_data){
  DO.data.1 <- noldus_data #read File
  secs <- ceiling(round(DO.data.1$Duration_sf*100)) # round duration of obs to nearest hundredth of a second (hence * 100)

  Timestamp = rep(seq(DO.data.1$Date_Time_Absolute_dmy_hmsf[1], DO.data.1$Date_Time_Absolute_dmy_hmsf[1]+sum(secs)/100, by = 1), each = 100)[1:sum(secs)]

  big.DO.1 <- data.frame(Timestamp = Timestamp,
                         Behavior=as.character(rep(DO.data.1$Behavior,times=secs)),
                         Modifier_1=as.character(rep(DO.data.1$Modifier_1,times=secs)), stringsAsFactors = F)

  modifiers = str_which(colnames(noldus_data), '\\.')

  for(i in 1:length(modifiers)){
    modifier_data = as.vector(DO.data.1[,modifiers[i]])
    modifier_data[which(is.na(modifier_data))] = 'NA'

    modifier_data <- rep(unlist(modifier_data), times=secs)
    big.DO.1 = bind_cols(big.DO.1, as.character(modifier_data))
    colnames(big.DO.1)[ncol(big.DO.1)] = colnames(noldus_data)[modifiers[i]]
  }

  big.DO.1 = cbind(Filepath = DO.data.1$Filepath[1], big.DO.1)

  return(big.DO.1)

}
