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
  
  drinks_df <- data.frame(drinks) %>% 
    gather() %>% 
    left_join(lookup_alc, by = "key") %>% 
    rename("drink" = key, "amount" = value) %>% 
    mutate(amount_alc = amount * volume * ethanol * 0.8)

  
}

drinks <- list("massn" = 2, "schnaps" = 3)
data.frame(drinks) %>% gather()
