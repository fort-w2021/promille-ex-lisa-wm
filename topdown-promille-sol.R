# PREREQUISITES ----------------------------------------------------------------

library(here)
library(tidyverse)
library(checkmate)
library(testthat)

# PROBLEM STRUCTURE ------------------------------------------------------------

# Goal: calculate degree of noimnotdrunk, taking into account decomposition
# during drinking
# 
# c_final = c - 0.15(t - 1)
#
# --> t: drinking time in hours
#     t = timediff(beginning, end) [h]
# --> c: blood alc level w/o decomposition
#     c = 0.8 * amount_consumed / (1.055 * gkw)
#     --> amount_consumed = volume * ethanol * 0.8
#     --> gkw = linfun(sex * (age, height, weight))

# HELPER FUNCTIONS -------------------------------------------------------------

# Age checks

find_impostor <- function(age, drinks) {
  
  hard_stuff <- any(names(drinks) == "schnaps")
  ifelse(age < 16 | (age >= 16 & age < 18 & hard_stuff), TRUE, FALSE)
  
}

# Amount hammered home

get_intake <- function(drinks) {
  
  lookup_alc <- data.frame(
    drink = accepted_drinks,
    volume = c(1000, 500, 200, 40),
    ethanol = c(0.06, 0.06, 0.11, 0.4)
  )

  drinks <- unlist(drinks)
  drinks <- tapply(drinks, names(drinks), sum)

  data.frame(count = drinks) %>%
    rownames_to_column("drink") %>% 
    left_join(lookup_alc, by = "drink") %>% 
    mutate(amount_alc = count * volume * ethanol * 0.8) %>%
    summarise(sum(amount_alc)) %>%
    as.numeric()
  
}

# Body water

get_water <- function(sex, age, height, weight) {
  
    weights <- case_when(
      sex == "male" ~ c(2.447, -0.09516, 0.1074, 0.3362),
      sex == "female" ~ c(0.203, -0.07, 0.1069, 0.2466)
    )
    features <- matrix(c(1, age, height, weight))
    as.numeric(weights %*% features)
    
}

# Blood alcohol level after W/W formula

apply_ww_formula <- function(amount_consumed, gkw, time_passed) {
  
  c_pretox <- 0.8 * amount_consumed / (1.055 * gkw)
  ifelse(
    time_passed >= 1,
    max(0, c_pretox - 0.15 * (time_passed - 1)),
    c_pretox)

}

# PROMILLE FUNCTION ------------------------------------------------------------

tell_me_how_drunk <- function(age, 
                              sex = c("male", "female"), 
                              height, 
                              weight, 
                              drinking_time, 
                              drinks) {
  
  # Check input formats and convert sex to supported format, if possible
  
  assert_count(age)
  assert_count(height)
  assert_count(weight)
  
  accepted_drinks <<- c("massn", "hoibe", "wein", "schnaps")
  assert_names(names(unlist(drinks)), subset.of = accepted_drinks)
  assert_integerish(unlist(drinks))
  
  assert_posixct(drinking_time)
  if (diff(drinking_time) < 0) stop("end must not lie before beginning")

  # If you're queer, drink more beer...
  sex <- match.arg(tolower(sex), c("female", "male")) 
  
  # Check drinking age
  
  if (find_impostor(age, drinks)) {
    warning("stop stealing your brother's id, that's illegal")
  }
  
  # Calculate alcohol intake, body water and time passed
  
  amount_consumed <- get_intake(drinks)
  gkw <- get_water(sex, age, height, weight)
  happy_hour <- 
    as.numeric(difftime(drinking_time[2], drinking_time[1], units = "hours"))
  
  # Calculate blood alcohol level incl. decomposition
  
  apply_ww_formula(amount_consumed, gkw, happy_hour)
  
}

# TESTS ------------------------------------------------------------------------

test_file(here("topdown-promille-tests.R"))
