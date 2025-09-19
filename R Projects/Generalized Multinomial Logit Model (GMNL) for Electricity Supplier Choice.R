# ⚡ Discrete Choice Modeling of Electricity Suppliers with Generalized Multinomial Logit (GMNL)
# Author: Viriya Gunawan Lim
#
# 📌 Project Description
# This project estimates a Generalized Multinomial Logit (GMNL) or Mixed Logit with Integrated Lognormal Scale (MIXL-ILS)
# model using the Electricity dataset from the mlogit package, implemented
# through the gmnl package.
#
# The dataset is panel data, with:
# - 361 individuals
# - 4,308 observations
# - Each individual faced 12 hypothetical choice scenarios
#
# In each scenario, consumers chose between 4 electricity suppliers described by:
# - Price scheme: fixed (pf), time-of-day (tod), or seasonal (seas)
# - Contract length (cl)
# - Local supplier (loc)
# - Well-known supplier (wk)
#
# Since the design considered unlabeled alternatives,
# alternative-specific constants (ASCs) are set to zero.
#
# 🛠 Steps
# 1. Load and preprocess data:
#    - Convert from wide to long format for estimation
#    - Use a subsample (1:3000) to reduce computation time
# 2. Estimate baseline Multinomial Logit (MNL)
#    - Includes cl, loc, wk, tod, seas
# 3. Perform Hausman–McFadden Test
#    - Checks the Independence of Irrelevant Alternatives (IIA) assumption
# 4. Estimate GMNL (MIXL-ILS) with gmnl
#    - Random coefficients for cl, loc, wk, tod, seas
#    - Allows heterogeneity in consumer preferences and scale
#    - Estimates covariance and correlation between random coefficients
#
# ✅ Insights
# - MNL provides a baseline, but IIA is restrictive
# - MIXL-ILS captures taste heterogeneity and realistic substitution patterns
# - Panel data improves modeling of repeated choices by the same individuals


library(gmnl)
library(mlogit)
library(dplyr)
library(ggplot2)

data("Electricity", package = "mlogit")
head(Electricity)
View(Electricity)

Elect <- mlogit.data(Electricity, id.var = "id", choice = "choice",
                      varying = 3:26, shape = "wide", sep = "")
head(Elect)

View(Elect)

# Full model: Includes all alternatives
full_model <- mlogit(choice ~ cl + loc + wk + tod + seas, data = Elect, reflevel = "1")

# Reduced model: Excludes one alternative (e.g., "alt = 4")
reduced_model <- mlogit(choice ~ cl + loc + wk + tod, data = Elect,
                        reflevel = "1", alt.subset = c("1", "2", "3"))

# Perform Hausman-McFadden test
hm_test <- hmftest(full_model, reduced_model)

# Output the results
print(hm_test)


#MIXL-ILS is better so we will use it. 

Elec.gmnl <- gmnl(choice ~ cl + loc + wk + tod + seas| 0, data = Elect,
                 subset = 1:3000,
                 model = 'gmnl',
                 R = 500,
                 panel = TRUE,
                 ranp = c(cl = "n", loc = "n", wk = "n", tod = "n", seas = "n"),
                 correlation = TRUE)
summary(Elec.gmnl)
cov.gmnl(Elec.gmnl)
se.cov.gmnl(Elec.gmnl)
se.cov.gmnl(Elec.gmnl, sd = TRUE)
cor.gmnl(Elec.gmnl)


# Count of chosen alternatives
table(Elect$choice)

# Proportion of choices
prop.table(table(Elect$choice))