
#' Temperature Risk
#' 
#' This function determines the level of risk associated with given temperature events to urban dwellers
#' @param temperature (C)
#' @param age (years)
#' @param income ($/year)
#' @return risk (high, med, low), extreme_events

temperature_risk = function(temperature, age, income) {
  # set up equations for extreme temperature scenarios
  extreme_temperature = case_when(temperature < 32 ~ "low", temperature > 32 & temperature <= 37 ~ "med", temperature >37 ~ "high")
  
  #set up age threshold
  age_threshold = case_when(age >= 60 ~ "high", age < 60 & age >= 45 ~ "med", age < 45 ~ "low")
  
  #set up low-income threshold
  income_threshold = case_when(income <= 35000 ~ "low", income > 35000 & income < 50000 ~ "med", income >= 50000 ~ "high" )
  
  # set up risk scenarios 
  risk = case_when (income_threshold == "high" | age_threshold == "low" | extreme_temperature == "low" ~ "low",
                    extreme_temperature == "med" &
                      age_threshold != "high"|"med" & income_threshold != "low"|"med" ~ "med",
                    extreme_temperature == "high" & age_threshold != "high"|"med" & income_threshold != "low"|"med" ~ "high")
  return(list(risk=risk, total_extreme_events = sum(extreme_temperature != "med"|"high")))
}
