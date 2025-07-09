### Purpose: Prepare HRS data for examples
### Author:  S Bauldry
### Date:    May 17, 2025

### Working directory and libraries
setwd("~/desktop")
library(tidyverse)
library(haven)
library(panelr)

### Load RAND HRS longitudinal file
d1 <- read_dta("randhrs1992_2022v1.dta")

### Identify analysis variables
### use waves 13-16 which correspond to years 2016-2022
ti    <- c("hhidpn", "raracem", "rahispan", "ragender", "hacohort")
tv    <- c("agey_b", "cesd", "drinkd")
wv    <- paste0("r", 13:16)
tv_ls <- unlist( outer(wv, tv, paste0) )
vars  <- c(ti, tv_ls)

### Extract analysis variables and prepare time-invariant variables
d2 <- d1 |>
  select(all_of( vars )) |>
  filter(hacohort == 7) |> # late baby-boomers
  mutate(fem = as.factor( ifelse(ragender == 2, 1, 0) ),  # women
         rce = as.factor( case_when(
           rahispan == 1 ~ 1,       # Hispanic
           raracem  == 2 ~ 2,       # non-Hispanic Black
           raracem  == 1 ~ 3,       # non-Hispanic White
           raracem  == 3 ~ 4)) ) |> # other race/ethnicity 
  select(-c(ragender, rahispan, raracem, hacohort))

### Reshape to long form
d3 <- long_panel(d2, prefix = "r", begin = 13, end = 16, label_location = "beginning") |>
  rename(age = agey_b) |>
  mutate(drinkd = as.numeric(drinkd))

### Select analysis sample
# baseline sample of R age 51-65
d4 <- d3 |> filter(age > 50 & age <= 65)
length(d4$hhidpn)
n_distinct(d4$hhidpn)

# drop missing demographic characteristics and focal variables
d5 <- d4 |> drop_na(c(fem, rce, age, cesd, drinkd))
length(d5$hhidpn)
n_distinct(d5$hhidpn)

### Create balanced panel for ease of examples
d6 <- d5 |>
  group_by(id) |>
  filter(n() == 4) |>
  ungroup() |>
  select(-id)
length(d6$hhidpn)
n_distinct(d6$hhidpn)

### Saving long-form data for fixed effects examples
summary(d6)
write_csv(d6, "mcapsi-model-specification-long-hrs-data.csv")


### Preparing data for latent growth curve
# identify R between 53 and 55 at Wave 13
ids <- d6 |>
  filter(wave == 13, age >= 53, age <= 55) |>
  pull(hhidpn)

# Reshape to wide
d7 <- d6 |>
  filter(hhidpn %in% ids) |>
  mutate(wave = wave - 12) |>
  pivot_wider(id_cols = c(hhidpn, fem, rce), names_from = wave, values_from = c(age, cesd, drinkd))

# Saving wide-form data for analysis
write_csv(d7, "mcapsi-model-specification-wide-hrs-data.csv")


### Preparing wide form data for dynamic model - using same data as with fixed effects examples
d8 <- d6 |>
  mutate(wave = wave - 12) |>
  pivot_wider(id_cols = c(hhidpn), names_from = wave, values_from = c(age, cesd, drinkd))
write_csv(d8, "mcapsi-model-specification-wide-2-hrs-data.csv")