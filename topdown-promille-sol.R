# PREREQUISITES ----------------------------------------------------------------

library(here)
library(tidyverse)
library(checkmate)
library(testthat)

# PROMILLE FUNCTION ------------------------------------------------------------

tell_me_how_drunk <- function(age, 
                              sex = c("male", "female"), 
                              height, 
                              weight, 
                              drinking_time, 
                              drinks) {
  
  lookup_alc <- data.frame(
    key = c("massn", "hoibe", "wein", "schnaps"),
    volume = c(1000, 500, 200, 40),
    ethanol = c(0.06, 0.06, 0.11, 0.4)
  )
  
  amount_consumed <- data.frame(drinks) %>% 
    gather() %>% 
    left_join(lookup_alc, by = "key") %>% 
    mutate(amount_alc = value * volume * ethanol * 0.8) %>% 
    summarise(sum(amount_alc)) %>% 
    as.numeric()
  
  get_gkw <- function(sex, age, height, weight) {
    
    male <- sex == "male"
    intercept <- ifelse(male, 2.447, 0.203)
    b_age <- ifelse(male, -0.09516, -0.07)
    b_height <- ifelse(male, 0.1074, 0.1069)
    b_weight <- ifelse(0.3362, 0.2466)
    
    intercept + b_age * age + b_height * height + b_weight * weight
    
  }
  
  get_detox <- function(drinking_time) 0.15 * (diff(drinking_time) - 1)
  
  gkw <- get_gkw(sex, age, height, weight)
  detox <- get_detox(drinking_time)
  
  0.8 * amount_consumed / (1.055 * gkw) - detox
  
}

drinks <- list("massn" = 2, "schnaps" = 3)
tell_me_how_drunk(
  age = 26, 
  sex = "female", 
  height = 174, 
  weight = 65, 
  drinks = drinks,
  drinking_time = c(2, 6)
)
