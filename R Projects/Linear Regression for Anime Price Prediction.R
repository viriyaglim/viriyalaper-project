#############################################################
# 🎥 Anime Score Prediction with Linear Regression
# Author: Viriya Gunawan Lim
#
# 📌 Project Description
# This project applies **multiple linear regression** to predict
# anime scores based on features like ranking, popularity, source,
# and rating. Several models are tested by removing variables step by step
# to compare their explanatory power.
#############################################################

# Load required libraries
library(psych)      # correlation and multivariate analysis
library(readr)      # data import
library(ggplot2)    # visualization
library(dplyr)      # data wrangling
library(broom)      # tidy model outputs
library(ggpubr)     # publication-ready plots

#############################################################
# 🔹 Data Preparation
#############################################################

# Load dataset
dataf <- Dataset_Final_Model_Linear_Anime

# Create dummy variables for categorical features
dataf$Lightnovel   <- ifelse(dataf$Source == "Light novel", 1, 0)
dataf$Manga        <- ifelse(dataf$Source == "Manga", 1, 0)
dataf$Novel        <- ifelse(dataf$Source == "Novel", 1, 0)
dataf$Original     <- ifelse(dataf$Source == "Original", 1, 0)
dataf$Visualnovel  <- ifelse(dataf$Source == "Visual novel", 1, 0)

dataf$PG13 <- ifelse(dataf$Rating == "PG-13 - Teens 13 or older", 1, 0)
dataf$R17  <- ifelse(dataf$Rating == "R - 17+ (violence & profanity)", 1, 0)
dataf$R    <- ifelse(dataf$Rating == "R+ - Mild Nudity", 1, 0)

# Remove unused categorical columns
dataf <- subset(dataf, select = -c(English, Source, Rating))

#############################################################
# 🔹 Model Building
#############################################################

# Model 1: All variables
model1 <- lm(Score ~ Ranked + Members + Episodes + Favorites +
             Lightnovel + Manga + Novel + Original + Visualnovel +
             PG13 + R17 + R, data = dataf)
anova(model1)
summary(model1)

# Model 2: Remove Episodes
model2 <- lm(Score ~ Ranked + Members + Favorites +
             Lightnovel + Manga + Novel + Original + Visualnovel +
             PG13 + R17 + R, data = dataf)
anova(model2)
summary(model2)

# Model 3: Remove Rating dummies
model3 <- lm(Score ~ Ranked + Episodes + Members + Favorites +
             Manga + Novel + Original + Visualnovel, data = dataf)
anova(model3)
summary(model3)

# Model 4: Only age-restricted ratings + popularity
model4 <- lm(Score ~ Ranked + Members + Favorites + R17 + R, data = dataf)
anova(model4)
summary(model4)

# Model 5: Only Ranked, Members, Favorites
model5 <- lm(Score ~ Ranked + Members + Favorites, data = dataf)
anova(model5)
summary(model5)

#############################################################
# 🔹 Model Diagnostics
#############################################################

# Residual diagnostic plots for model 1
par(mfrow = c(2,2))
plot(model1)

# Scatterplot matrix for variables
pairs(~ Score + Ranked + Members + Episodes + Favorites +
        Lightnovel + Manga + Novel + Original + Visualnovel +
        PG13 + R17 + R, data = dataf)

# Correlation matrix (Pearson, Kendall, Spearman)
cor(dataf, method = c("pearson", "kendall", "spearman"))

#############################################################
# ✅ Insights
# - Model 1 is the full model, but may suffer from multicollinearity.
# - Model 2–5 gradually simplify the predictors to test robustness.
# - Key predictors likely: Ranked, Members, Favorites (popularity measures).
# - Diagnostics (residual plots, correlation) help check model fit.
#############################################################
