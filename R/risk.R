#' Risk - tempertaure
#' 
#' This function aims to compute a level of risk associated with extreme temperature events
#' We are using an index that scores between 2 and 6, where higher numbers indcate more risk
#' @param temp (in celsius (C))
#' @param age (risk in years (years))
#' @return associated_risk 
#' @return days of risk
#'
risk = function(temp, age) {
  # equation for extreme temp
  temp_risk = case_when(temp < 20 ~ "low", temp >= 20 & temp <= 29 ~ "medium", temp >29 ~ "high")
  
  days_risk = case_when(temp >29 ~ "high")
  
  # age risk
  age_risk = case_when(age >= 60 ~ "high", age < 60 & age >= 35 ~ "medium", age < 35 ~ "low")
  
  # age error checking
  
  if(age_risk < 0)
    return("Can't measure risk if you don't exist!")
  
  #temp risk
           
  if(temp_risk == "high" && age_risk == "high")
    return(6)
  
  if(temp_risk == "medium" && age_risk == "high")
    return(5)
  
  if(temp_risk == "low" && age_risk == "high")
    return(4)
  
  if(temp_risk == "high" && age_risk == "medium")
    return(5)
  
  if(temp_risk == "high" && age_risk == "low")
    return(4)
  
  if(temp_risk == "low" && age_risk == "medium")
    return(3)
  
  if(temp_risk == "medium" && age_risk == "low")
    return(3)
  
  if(temp_risk == "low" && age_risk == "low")
    return(2)
  
  if(temp_risk == "medium" && age_risk == "medium")
    return(4)
  
  if(temp_risk == "medium" && age_risk == "medium")
    return(4)
  
  age_risk = sum(summary(temp_risk))
  
  risk_sum = sum(summary(days_risk))
  
  risk_list <- list(total_age = age_risk, total_num = risk_sum)
  return(risk_list)
  
 
}


  
 



