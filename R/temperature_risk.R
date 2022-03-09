
#' Temperature Risk
#' 
#' This function determines the level of risk associated with given temperature events to urban dwellers
#' @param temperature (C)
#' @param age (years)
#' @param income ($/year)
#' @return risk (high, med, low), extreme_events

temperature_risk = function(temperature, age, income) {
  # set up equations for extreme temperature scenarios
  extreme_temperature = case_when(temperature_df$tmax < 28 ~ "low", temperature_df$tmax >=28 & temperature_df$tmax <32 ~ "med", temperature_df$tmax >= 32 ~ "high")
  
  # add column for extreme_temperature to temperature data frame
  temperature_df$extreme_temperature <- extreme_temperature
  
  #create a single data_frame of age and income for each individual
  age_income = data.frame(age, income)
  
  # create age and income thresholds using case_when
  age_threshold = case_when(age >= 60 ~ "high", age < 60 & age >= 45 ~ "med", age < 45 ~ "low")
  income_threshold = case_when(income <= 35000 ~ "low", income > 35000 & income < 50000 ~ "med", income >= 50000 ~ "high" )
  
  # add columns for age_threshold and income_threshold to age_income data frame
  age_income$age_threshold <- age_threshold
  age_income$income_threshold <- income_threshold
  
  #set up risk as an array
  risk = as.data.frame(matrix(nrow=length(temperature_df$tmax), ncol=n))
  
  #set up array for risk calculations
  risk_calc = as.data.frame(matrix(nrow = n, ncol = 4))
  #rename risk_calc columns
  names(risk_calc)[1] <- "low_risk"
  names(risk_calc)[2] <- "med_risk"
  names(risk_calc)[3] <- "high_risk"
  names(risk_calc)[4] <- "extreme_events"
  # set up risk scenarios by looping each individual through temperature
  for(i in 1:n){
    risk[,i] = case_when (income_threshold[i] == "high" | age_threshold[i] == "low" | extreme_temperature == "low" ~ "low",
                          extreme_temperature == "med" &
                            age_threshold[i] != "low" & income_threshold[i] != "high" ~ "med",
                          extreme_temperature == "high" & age_threshold[i] != "low" & income_threshold[i] != "high" ~ "high")
    risk_calc[i,1] = sum(risk[,i]=="low")
    risk_calc[i,2] = sum(risk[,i]=="med")
    risk_calc[i,3] = sum(risk[,i]=="high")
    risk_calc[i,4] = sum(extreme_temperature!="low")
  }
  return(list(risk_low = sum(risk == "low", na.rm = TRUE), risk_med = sum(risk == "med", na.rm = TRUE), risk_high = sum(risk == "high", na.rm = TRUE), total_extreme_events = sum(extreme_temperature != "low"), summary(risk_calc)))
}
