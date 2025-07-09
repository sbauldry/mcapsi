### Purpose: Prepare examples for module 7
### Author:  S Bauldry
### Date:    May 17, 2025

### Working directory and libraries
library(tidyverse) # general purpose package
library(lme4)      # package for multilevel models
library(plm)       # package for panel models
library(lavaan)    # package for structural equation models
library(patchwork) # package for combining plots



# ---------------------------------------------------------------- #
# 7.2 Fixed Effects Models
# ---------------------------------------------------------------- #

### Read prepared long-form data
url1 <- "https://raw.githubusercontent.com/sbauldry/mcapsi/main/mcapsi-model-specification-long-hrs-data.csv"
d1 <- read_csv(url1)

### Respondent-level descriptives
between_person_stats <- d1 |>
  group_by(wave) |>
  summarise(cesd_mean   = mean(cesd),
            cesd_sd     = sd(cesd),
            drinkd_mean = mean(drinkd),
            drinkd_sd   = sd(drinkd))
between_person_stats

demographics <- d1 |>
  distinct(hhidpn, .keep_all = TRUE) |>
  select(hhidpn, fem, rce) |>
  mutate(
    sex  = factor(fem, levels = c(0, 1), labels = c("Male", "Female")),
    race = factor(rce, levels = c(1, 2, 3, 4), labels = c("Hispanic", "Black", "White", "other"))
  )

f1a <- ggplot(demographics, aes(x = sex, fill = sex)) +
  geom_bar(aes(y = after_stat(count/sum(count)))) +
  scale_y_continuous(name = "proportion", limits = c(0, 0.6)) +
  scale_fill_grey() +
  theme_light() +
  theme(legend.position = "none",
        axis.text  = element_text(size = 18),
        axis.title = element_text(size = 20))

f1b <- ggplot(demographics, aes(x = race, fill = race)) +
  geom_bar(aes(y = after_stat(count/sum(count)))) +
  scale_y_continuous(name = "proportion", limits = c(0, 0.6)) +
  scale_fill_grey() +
  theme_light() +
  theme(legend.position = "none",
        axis.text  = element_text(size = 18),
        axis.title = element_text(size = 20))

f1 <- f1a + f1b
f1
ggsave("f1.jpg", f1)



### Within-person descriptives
within_person_stats <- d1 |>
  group_by(hhidpn) |>
  summarise(
    # within-person variation in CESD
    cesd_mean   = mean(cesd),
    cesd_sd     = sd(cesd),
    cesd_change = max(cesd) - min(cesd) > 0,
    
    # within-person variation in drinking
    drinkd_mean   = mean(drinkd),
    drinkd_sd     = sd(drinkd),
    drinkd_change = max(drinkd) - min(drinkd) > 0
  )

# summary of within-person variation
within_summary <- within_person_stats |>
  summarise(
    n_individuals = n(),
    
    # CESD within-person variation
    cesd_avg_within_sd  = mean(cesd_sd),
    cesd_pct_change     = mean(cesd_change)*100,
    
    # Drinking within-person variation
    drinkd_avg_within_sd = mean(drinkd_sd),
    drinkd_pct_change    = mean(drinkd_change)*100
  )
within_summary

# ICCs
m1_cesd  <- lmer(cesd ~ 1 + (1 | hhidpn), data = d1)
summary(m1_cesd)
vc_cesd  <- VarCorr(m1_cesd)
icc_cesd <- vc_cesd$hhidpn[1]/(vc_cesd$hhidpn[1] + attr(vc_cesd, "sc")^2)

m1_drinkd  <- lmer(drinkd ~ 1 + (1 | hhidpn), data = d1)
vc_drinkd  <- VarCorr(m1_drinkd)
icc_drinkd <- vc_drinkd$hhidpn[1]/(vc_drinkd$hhidpn[1] + attr(vc_drinkd, "sc")^2)
icc_drinkd



### Standard multilevel regression model adjusting for clustering
fe_m1 <- lmer(drinkd ~ cesd + (1 | hhidpn), data = d1)
summary(fe_m1)

### Unit fixed effects model
fe_m2 <- plm(drinkd ~ cesd, data = d1, index = c("hhidpn", "wave"), model = "within")
summary(fe_m2)

### Two-way fixed effects model
fe_m3 <- plm(drinkd ~ cesd, data = d1, index = c("hhidpn", "wave"), model = "within", effect = "twoways")
summary(fe_m3)

### Two-way fixed effects model with sex interaction
fe_m4 <- plm(drinkd ~ cesd + cesd:fem, data = d1, index = c("hhidpn", "wave"), model = "within", effect = "twoways")
summary(fe_m4)



# ---------------------------------------------------------------- #
#7.3 Random Effects Models
# ---------------------------------------------------------------- #

### Read prepared wide-form data
url2 <- "https://raw.githubusercontent.com/sbauldry/mcapsi/main/mcapsi-model-specification-wide-hrs-data.csv"
d2 <- read_csv(url2) |>
  mutate(
    sex = factor(fem, levels = c(0, 1), labels = c("Male", "Female")),
    race = factor(rce, levels = c(1, 2, 3, 4), labels = c("Hispanic", "Black", "White", "other")),
    hsp = ifelse(rce == 1, 1, 0),
    blk = ifelse(rce == 2, 1, 0),
    oth = ifelse(rce == 3, 1, 0))

### Respondent-level descriptives
drinking <- d2 |>
  select(drinkd_1, drinkd_2, drinkd_3, drinkd_4) |>
  summarise_all(list( mean = ~mean(.), sd = ~sd(.) ))
drinking

demographics2 <- d2 |>
  distinct(hhidpn, .keep_all = TRUE) |>
  select(hhidpn, sex, race)

f2a <- ggplot(demographics2, aes(x = sex, fill = sex)) +
  geom_bar(aes(y = after_stat(count/sum(count)))) +
  scale_y_continuous(name = "proportion", limits = c(0, 0.6)) +
  scale_fill_grey() +
  theme_light() +
  theme(legend.position = "none",
        axis.text  = element_text(size = 18),
        axis.title = element_text(size = 20))

f2b <- ggplot(demographics, aes(x = race, fill = race)) +
  geom_bar(aes(y = after_stat(count/sum(count)))) +
  scale_y_continuous(name = "proportion", limits = c(0, 0.6)) +
  scale_fill_grey() +
  theme_light() +
  theme(legend.position = "none",
        axis.text  = element_text(size = 18),
        axis.title = element_text(size = 20))

f2 <- f2a + f2b
f2
ggsave("f2.jpg", f2)



### Plot of individual trajectories
d3 <- d2 |>
  select(hhidpn, sex, race, drinkd_1, drinkd_2, drinkd_3, drinkd_4) |>
  pivot_longer(cols = c(drinkd_1, drinkd_2, drinkd_3, drinkd_4), names_to = "wave", values_to = "drink") |>
  mutate(wave = as.numeric(str_extract(wave, "\\d+")))

f3 <- ggplot(d3, aes(x = wave, y = drink, group = hhidpn)) +
  geom_line(alpha = 0.3, color = "gray") +
  geom_smooth(aes(group = 1), method = "loess", se = T, color = "red", linewidth = 1.2) +
  scale_x_continuous(breaks = 1:4, labels = 1:4) +
  labs(x = "Wave", y = "days drinking") +
  theme_light() +
  theme(axis.text  = element_text(size = 18),
        axis.title = element_text(size = 20))
f3
ggsave("f3.jpg", f3)



### Separate plots by gender and race/ethnicity
f4 <- ggplot(d3, aes(x = wave, y = drink, group = hhidpn)) +
  geom_line(alpha = 0.3, color = "gray") +
  geom_smooth(aes(group = 1), method = "loess", se = T, color = "red", linewidth = 1.2) +
  facet_wrap(~sex) +
  scale_x_continuous(breaks = 1:4, labels = 1:4) +
  labs(x = "Wave", y = "days drinking") +
  theme_light() +
  theme(axis.text  = element_text(size = 18),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 20))
f4
ggsave("f4.jpg", f4)

f5 <- ggplot(d3, aes(x = wave, y = drink, group = hhidpn)) +
  geom_line(alpha = 0.3, color = "gray") +
  geom_smooth(aes(group = 1), method = "loess", se = T, color = "red", linewidth = 1.2) +
  facet_wrap(~race) +
  scale_x_continuous(breaks = 1:4, labels = 1:4) +
  labs(x = "Wave", y = "days drinking") +
  theme_light() +
  theme(axis.text  = element_text(size = 18),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 20))
f5
ggsave("f5.jpg", f5)


### Fit unconditional linear growth model
lgm_m1 <- '
  Int =~ 1*drinkd_1 + 1*drinkd_2 + 1*drinkd_3 + 1*drinkd_4
  Slp =~ 0*drinkd_1 + 1*drinkd_2 + 2*drinkd_3 + 3*drinkd_4
'
fit1 <- growth(lgm_m1, data = d2)
summary(fit1)
summary(fit1, fit.measures = T, rsquare = T)


### Fit conditional linear latent growth model
lgm_m2 <- '
  Int =~ 1*drinkd_1 + 1*drinkd_2 + 1*drinkd_3 + 1*drinkd_4
  Slp =~ 0*drinkd_1 + 1*drinkd_2 + 2*drinkd_3 + 3*drinkd_4
  Int ~ fem + hsp + blk + oth
  Slp ~ fem + hsp + blk + oth
'
fit2 <- growth(lgm_m2, data = d2)
summary(fit2, rsquare = T)


### Fit conditional linear latent growth model with time-varying covariates
lgm_m3 <- '
  Int =~ 1*drinkd_1 + 1*drinkd_2 + 1*drinkd_3 + 1*drinkd_4
  Slp =~ 0*drinkd_1 + 1*drinkd_2 + 2*drinkd_3 + 3*drinkd_4
  Int ~ fem + hsp + blk + oth
  Slp ~ fem + hsp + blk + oth
  drinkd_1 ~ cesd_1
  drinkd_2 ~ cesd_2
  drinkd_3 ~ cesd_3
  drinkd_4 ~ cesd_4
'
fit3 <- growth(lgm_m3, data = d2)
summary(fit3, rsquare = T)




# ---------------------------------------------------------------- #
#7.4 Dynamic Models
# ---------------------------------------------------------------- #

### Read prepared data
url3 <- "https://raw.githubusercontent.com/sbauldry/mcapsi/main/mcapsi-model-specification-wide-2-hrs-data.csv"
d4 <- read_csv(url3)


### Summary of estimates from one-way fixed effects model as comparison point
summary(fe_m2)


### One-way fixed effects model in SEM framework
dm_m1 <- '
  U =~ 1*drinkd_1 + 1*drinkd_2 + 1*drinkd_3 + 1*drinkd_4
  drinkd_1 ~ a*cesd_1
  drinkd_2 ~ a*cesd_2
  drinkd_3 ~ a*cesd_3
  drinkd_4 ~ a*cesd_4
  U ~~ cesd_1
  U ~~ cesd_2
  U ~~ cesd_3
  U ~~ cesd_4
  drinkd_1 ~~ b*drinkd_1
  drinkd_2 ~~ b*drinkd_2
  drinkd_3 ~~ b*drinkd_3
  drinkd_4 ~~ b*drinkd_4
'
dm_fit1 <- sem(dm_m1, data = d4, information = "observed")
summary(dm_fit1)


### Fixed effects model with state effects
dm_m2 <- '
  U =~ 1*drinkd_2 + 1*drinkd_3 + 1*drinkd_4
  drinkd_2 ~ a*drinkd_1 + b*cesd_2
  drinkd_3 ~ a*drinkd_2 + b*cesd_3
  drinkd_4 ~ a*drinkd_3 + b*cesd_4
  U ~~ drinkd_1
  U ~~ cesd_2
  U ~~ cesd_3
  U ~~ cesd_4
'
dm_fit2 <- sem(dm_m2, data = d4, information = "observed")
summary(dm_fit2)
