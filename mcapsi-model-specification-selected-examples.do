*** Purpose: illustrate selected examples from module 7 on model specification
*** Author:  S Bauldry
*** Date:    July 13, 2025


*** Read prepared data for FE models
local url1 "https://raw.githubusercontent.com/sbauldry/mcapsi/main/mcapsi-model-specification-long-hrs-data.csv"
import delimited using `url1', clear

* Define the panel data
xtset hhidpn wave

* Fit a one-way fixed effects model
xtreg drinkd cesd, fe

* Fit a two-way fixed effects model
xtreg drinkd cesd i.wave, fe

* Fit a two-way fixed effects model with gender interaction
xtreg drinkd c.cesd##i.fem i.wave, fe



*** Read prepared for for latent growth models
* note: I don't recommend using Stata for latent growth models due to hidden identification parameterizations
local url2 "https://raw.githubusercontent.com/sbauldry/mcapsi/main/mcapsi-model-specification-wide-hrs-data.csv"
import delimited using `url2', clear

* Fit an unconditional linear growth model
sem (drinkd_1 <- Int@1 Slp@0 _cons@0) ///
    (drinkd_2 <- Int@1 Slp@1 _cons@0) ///
	(drinkd_3 <- Int@1 Slp@2 _cons@0) ///
	(drinkd_4 <- Int@1 Slp@3 _cons@0), means(Int Slp)
estat gof, stats(all)
	
	
* Fit a conditional linear growth model
qui tab rce, gen(r)
sem (drinkd_1 <- Int@1 Slp@0) ///
    (drinkd_2 <- Int@1 Slp@1) ///
	(drinkd_3 <- Int@1 Slp@2) ///
	(drinkd_4 <- Int@1 Slp@3) ///
	(fem r1 r2 r4-> Int Slp), cov(e.Int*e.Slp)
	
* Fit a linear growth model with time-varying covariates
* note: these estimates differ from R / need to check identification constraints
sem (drinkd_1 <- Int@1 Slp@0 cesd_1) ///
    (drinkd_2 <- Int@1 Slp@1 cesd_2) ///
	(drinkd_3 <- Int@1 Slp@2 cesd_3) ///
	(drinkd_4 <- Int@1 Slp@3 cesd_4) ///
	(fem r1 r2 r4-> Int Slp), cov(e.Int*e.Slp)
	


*** Read prepared for for dynamic models
* note: I don't recommend using Stata for these types of dynamic models due to hidden identification parameterizations
local url3 "https://raw.githubusercontent.com/sbauldry/mcapsi/main/mcapsi-model-specification-wide-2-hrs-data.csv"
import delimited using `url3', clear

* Fit a fixed effects model in SEM framework
sem (drinkd_1 <- U@1 cesd_1@a _cons@b, var(e.drinkd_1@c)) ///
    (drinkd_2 <- U@1 cesd_2@a _cons@b, var(e.drinkd_2@c)) ///
	(drinkd_3 <- U@1 cesd_3@a _cons@b, var(e.drinkd_3@c)) ///
	(drinkd_4 <- U@1 cesd_4@a _cons@b, var(e.drinkd_4@c))
	
* note: see Allison et al. (2017) Appendix B for a discussion of the contortions
* needed to get Stata to fit a fixed effects model with state effects in the
* SEM framework
