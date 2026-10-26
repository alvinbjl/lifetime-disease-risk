# v1
library(tidyverse)

data <- tibble(
  age = 1:5,
  pop = c(100, 95, 90, 84, 78),
  c_cancer = c(60, 50, 45, 38, 32),
  c_death = c(20, 10, 12, 9, 8)
  # r_cancer = c(0.60, 0.53, 0.50, 0.45, 0.41),
  # r_death = c(0.20, 0.11, 0.13, 0.11, 0.10)
  ) |> 
  mutate(
    r_cancer = c_cancer/pop,
    r_death = c_death/pop,
    r_hazard = r_cancer + r_death,
    p_surv = exp(-r_hazard),
    p_cancer_or_death = 1-exp(-r_hazard),
    p_cancer = c_cancer/(c_cancer + c_death) * p_cancer_or_death,
    p_surv_cumulative = cumprod(p_surv)
  ) |> 
  view()

# risk <- 
  
data |> 
  select(p_cancer, p_surv_cumulative) |> 
  mutate(p_surv_cumulative = c(1, p_surv_cumulative[1:n()-1]),
         p = p_cancer*p_surv_cumulative) |> 
  select(p) |> 
  sum()
  
# change to funcitons


t <- tibble(a = 1:5, b = 2:6) |> 
  mutate(b = c(1, b[1:n()-1]))


x <- 0
for (i in 1:5) {
  x <- x + i
}
x
  
  1:nrow(data)









# archive ---------------------------------------------------------------------
library(tidyverse)

y1 <- data.frame(
  age_group = c("00-10", "11-20", "21-30"),
  pop = c(100, 110, 90),
  new_cases = c(20, 25, 15),
  death = c(10, 20, 5)
)

y2 <- data.frame(
  age_group = c("00-10", "11-20", "21-30"),
  pop = c(90, 100, 120),
  new_cases = c(10, 20, 40),
  death = c(12, 18, 10)
)

y3 <- data.frame(
  age_group = c("00-10", "11-20", "21-30"),
  pop = c(120, 150, 100),
  new_cases = c(30, 20, 30),
  death = c(5, 10, 20)
)


# step 1: combine to get 3 year total?
y1 + y2

# step 2: get incidence rate, death rate
rate_inc <- data.frame(
  age_group = y1$age_group,
  rate_inc = 
)

rate_death <-

# step 3: Apply to discrete -> continous conversion (cumulative risk?)
prob_cancer_and_alive <- exp(-(rate_inc + rate)*t)
prob_cancer_or_death <- 1 - exp(-()*t)
prob_cancer <- y1/y1+y2 * prob_cancer_or_death
prob_death <- y2/y1+y2 * prob_cancer_or_death

# step 4:


# toydat ------------------------------------------------------------------
y_act <- data.frame(
  person = c("A", "B"),
  year1 = c("cancer", "no cancer"),
  year2 = c("recovered", "cancer")
)

y <- tibble(
  year = c("y1", "y2"),
  new_case = c(1,1),
  pop = c(2,2),
) |> 
  mutate(
    r = new_case/pop
  )

# new test group of newborns
naive <- y$r[1] + (1-y$r[1])*y$r[2]
wun <- 1-exp(-y$r[1]*1)


  








# AI ----------------------------------------------------------------------
# ==============================================================================
# CANCER LIFETIME & AGE-CONDITIONAL RISK METHODOLOGY COMPARISON
# ==============================================================================
# Step 1: Define Sample Epidemiological Dataset (3 Age Groups)
data <- data.frame(
  age_group = c("0-10", "11-20", "21-30"),
  width = c(10, 10, 10),
  pop = c(1000, 900, 600),
  cancer_cases = c(100, 200, 100),       # C_i / R_i
  non_cancer_deaths = c(100, 300, 100)   # D0_i / (M_i - D_i)
)

# Compute Person-Years (PY) and basic rates
data$PY <- data$pop * data$width
data$inc_rate <- data$cancer_cases / data$PY     # r_i
data$death_rate <- data$non_cancer_deaths / data$PY # m0_i

# ------------------------------------------------------------------------------
# METHOD 1: NAIVE CUMULATIVE INCIDENCE
# ------------------------------------------------------------------------------
calc_naive <- function(df) {
  cum_inc <- sum(df$inc_rate * df$width)
  return(list(lifetime_risk = cum_inc))
}

# ------------------------------------------------------------------------------
# METHOD 2: WUN ET AL. (1998) / FEUER ET AL. (1993) DEVCAN DISCRETE LIFE TABLE
# ------------------------------------------------------------------------------
calc_wun <- function(df, l0_start = 100000) {
  n <- nrow(df)
  l <- numeric(n + 1); l[1] <- l0_start   # Total population survivors
  l0 <- numeric(n + 1); l0[1] <- l0_start # Cancer-free survivors
  a <- numeric(n)     # Expected cancer cases
  R <- numeric(n)     # Prevalence ratios
  r0 <- numeric(n)    # Adjusted incidence rates
  
  for (i in 1:n) {
    w <- df$width[i]
    r <- df$inc_rate[i]
    m0 <- df$death_rate[i]
    
    R[i] <- l[i] / l0[i] # Prevalence ratio R_x [2]
    g <- 1 - exp(-w * r)
    g0 <- g * R[i]
    r0[i] <- ifelse(g0 < 1, -log(1 - g0) / w, r * R[i])
    
    lambda <- r0[i] + m0
    a[i] <- l0[i] * (1 - exp(-w * lambda)) * (r0[i] / lambda)
    
    l[i + 1] <- l[i] * exp(-w * m0)
    l0[i + 1] <- l0[i] * exp(-w * lambda)
  }
  
  # Age-conditional risks: sum(a[i:n]) / l0[i]
  cond_risks <- sapply(1:n, function(i) sum(a[i:n]) / l0[i])
  
  return(list(
    lifetime_risk = sum(a) / l0[1],
    adjusted_rates = r0,
    conditional_risks = cond_risks
  ))
}

# ------------------------------------------------------------------------------
# METHOD 3: FAY ET AL. (2003) CONTINUOUS CAUSE-SPECIFIC HAZARD
# ------------------------------------------------------------------------------
calc_fay <- function(df, wun_res) {
  n <- nrow(df)
  S <- numeric(n + 1); S[1] <- 1.0
  fay_cases <- numeric(n)
  
  for (i in 1:n) {
    w <- df$width[i]
    r0 <- wun_res$adjusted_rates[i]
    m0 <- df$death_rate[i]
    lambda <- r0 + m0
    
    # Continuous integral: Int[ lambda_c * S(t) dt ] [3]
    fay_cases[i] <- S[i] - (r0 / lambda) * (1 - exp(-w * lambda))
    S[i + 1] <- S[i] * exp(-w * lambda)
  }
  
  cond_risks <- sapply(1:n, function(i) sum(fay_cases[i:n]) / S[i])
  return(list(
    lifetime_risk = sum(fay_cases),
    conditional_risks = cond_risks
  ))
}

# ------------------------------------------------------------------------------
# METHOD 4: SASIENI ET AL. (2011) ADJUSTED FOR MULTIPLE PRIMARIES (AMP)
# ------------------------------------------------------------------------------
calc_amp <- function(df) {
  n <- nrow(df)
  S_star <- numeric(n + 1); S_star[1] <- 1.0
  amp_cases <- numeric(n)
  
  for (i in 1:n) {
    w <- df$width[i]
    R_cases <- df$cancer_cases[i]
    PY <- df$PY[i]
    D_non_c <- df$non_cancer_deaths[i]
    
    c_rate <- R_cases / PY
    mu_star <- (R_cases + D_non_c) / PY # AMP removal rate [4]
    
    # First-primary cancer probability
    amp_cases[i] <- S_star[i] * (c_rate / mu_star) * (1 - exp(-w * mu_star))
    S_star[i + 1] <- S_star[i] * exp(-w * mu_star) # Removal from S0* [4]
  }
  
  cond_risks <- sapply(1:n, function(i) sum(amp_cases[i:n]) / S_star[i])
  return(list(
    lifetime_risk = sum(amp_cases),
    conditional_risks = cond_risks
  ))
}

# ------------------------------------------------------------------------------
# EXECUTE AND PRINT
# ------------------------------------------------------------------------------
res_naive <- calc_naive(data)
res_wun <- calc_wun(data)
res_fay <- calc_fay(data, res_wun)
res_amp <- calc_amp(data)

cat("\nLIFETIME RISK RESULTS:\n")
cat(sprintf("1. Naive Cumulative: %.2f%%\n", res_naive$lifetime_risk * 100))
cat(sprintf("2. Wun (1998) DevCan: %.2f%%\n", res_wun$lifetime_risk * 100))
cat(sprintf("3. Fay (2003) Contin: %.2f%%\n", res_fay$lifetime_risk * 100))
cat(sprintf("4. Sasieni (2011) AMP: %.2f%%\n", res_amp$lifetime_risk * 100))

