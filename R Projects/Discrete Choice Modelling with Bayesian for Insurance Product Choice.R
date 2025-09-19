

#title: "🩺 Insurance Product Choice — Bayesian Discrete Choice (R + Stan)"
#author: "Viriya Gunawan Lim"
---

## 📌 Project Summary
#This project models **which insurance product tier** (🎖️ *Bronze*, 🥈 *Silver*, 🥇 *Gold*) a person is most likely to choose based on their demographics and health factors using a **Bayesian Discrete Choice Model** (multinomial logit with random coefficients) fit in **Stan** from **R**.

#Steps:
#1. Load the classic `insurance.csv`.
#2. **Create tiers** by splitting `charges` into three quantiles → *Bronze*, *Silver*, *Gold*.
#3. Reshape the data into **long format** (one row per person × alternative).
#4. Fit a **mixed logit** (random‐coefficients MNL) with **individual heterogeneity** via random slopes and a **scale heterogeneity** term.
#5. Inspect posterior summaries to learn how **age, BMI, sex, smoker, region** relate to product choice.



# Load necessary library
library(dplyr)
library(tidyr)
library(ggplot2)
library(naniar)
library(mlogit)
library(rstan)

# Load the dataset
insurance_data <- read.csv("C:\\Users\\Viriya Gunawan Lim\\Downloads\\insurance.csv")
head(insurance_data)

# Calculate quantile thresholds for charges
quantiles <- quantile(insurance_data$charges, probs = c(0.35, 0.70))

# Classify into Bronze, Silver, and Gold
insurance_data <- insurance_data %>%
  mutate(
    product_tier = case_when(
      charges <= quantiles[1] ~ "Bronze",
      charges <= quantiles[2] ~ "Silver",
      TRUE ~ "Gold"
    )
  )

# View the modified dataset
head(insurance_data)
insurance_data <- insurance_data %>% select(-charges)
head(insurance_data)

#EDA 
# 1. Structure of the dataset
str(insurance_data)

# 2. Summary statistics for each column
summary(insurance_data)

# 3. Check for missing values in each column
missing_values <- colSums(is.na(insurance_data))
print("Missing values in each column:")
print(missing_values)
#NO MISSING VALUES

#Prepare Data

# Add an ID column to uniquely identify each individual
insurance_data <- insurance_data %>%
  mutate(id = row_number())

# Convert data to long format, with one row per alternative for each individual
insurance_long <- insurance_data %>%
  # Replicate data for each alternative
  crossing(alternative = c("Bronze", "Silver", "Gold")) %>%
  # Create a binary choice indicator (1 if chosen, 0 otherwise)
  mutate(choice = ifelse(product_tier == alternative, 1, 0))


# Convert categorical variables to numeric format for modeling
insurance_long <- insurance_long %>%
  mutate(
    sex = as.numeric(as.factor(sex)),        # Convert 'sex' to numeric
    smoker = as.numeric(as.factor(smoker)),  # Convert 'smoker' to numeric
    region = as.numeric(as.factor(region))   # Convert 'region' to numeric
  )

# Create the data list for Stan
data_list <- list(
  N = n_distinct(insurance_long$id),        # Number of individuals
  J = 3,                                    # Number of alternatives (Bronze, Silver, Gold)
  choice = insurance_long %>% filter(choice == 1) %>% pull(alternative),  # Vector of choices
  X_age = matrix(insurance_long$age, ncol = 3, byrow = TRUE),
  X_bmi = matrix(insurance_long$bmi, ncol = 3, byrow = TRUE),
  X_sex = matrix(insurance_long$sex, ncol = 3, byrow = TRUE),
  X_smoker = matrix(insurance_long$smoker, ncol = 3, byrow = TRUE),
  X_region = matrix(insurance_long$region, ncol = 3, byrow = TRUE)
)


#AAAAAAA DCM 


stan_model_code <- "
data {
    int<lower=1> N;                 // Number of individuals
    int<lower=1> J;                 // Number of alternatives
    int<lower=1, upper=J> choice[N]; // Chosen alternative (1, 2, or 3)
    matrix[N, J] X_age;             // Age covariates per alternative
    matrix[N, J] X_bmi;             // BMI covariates per alternative
    matrix[N, J] X_sex;             // Sex covariates per alternative
    matrix[N, J] X_smoker;          // Smoker status covariates per alternative
    matrix[N, J] X_region;          // Region covariates per alternative
}
parameters {
    real<lower=1> pareto_param;       // Shape parameter for Pareto (k > 1)
    vector<lower=0>[N] scale;         // Individual-specific scale parameters (Pareto)

    // Mean and standard deviation for random coefficients
    real B_age_mu;                    
    real B_bmi_mu;                    
    real<lower=0> B_age_sigma;        
    real<lower=0> B_bmi_sigma;
    
    // Coefficients for fixed effects (sex, smoker, region)
    real B_sex;
    real B_smoker;
    real B_region;
    
    // Individual-specific random coefficients for age and bmi
    vector[N] beta_age;               
    vector[N] beta_bmi;
}
model {
    // Priors for mean and std deviation of random coefficients
    B_age_mu ~ normal(0, 1);
    B_bmi_mu ~ normal(0, 1);
    B_age_sigma ~ normal(0, 1);
    B_bmi_sigma ~ normal(0, 1);

    // Priors for individual-specific random coefficients
    beta_age ~ normal(B_age_mu, B_age_sigma);
    beta_bmi ~ normal(B_bmi_mu, B_bmi_sigma);

    // Pareto distribution for scale parameters
    scale ~ pareto(pareto_param, 1);

    for (n in 1:N) {
        vector[J] V;  // Utility for each alternative for individual n
        for (j in 1:J) {
            V[j] = beta_age[n] * X_age[n, j]
                 + beta_bmi[n] * X_bmi[n, j]
                 + B_sex * X_sex[n, j]
                 + B_smoker * X_smoker[n, j]
                 + B_region * X_region[n, j]
                 + scale[n];
        }
        // Logit choice model for the observed choice
        choice[n] ~ categorical_logit(V);
    }
}
"

# Compile and fit the model
fit <- stan(model_code = stan_model_code, data = data_list, iter = 2000, chains = 4)

# Print the model summary
print(fit)

