# Group Project: Depression, Comorbidity, and Multiple Linear Regression
# Question 4: SLR Baseline Analysis

# PHQ9 = β0 +β1(PhysicalActivity) + ε

# Load required packages
library(readr)

# Import Dataset
depression_data_clean <- 
  read_csv("data/cleaned_analytic_data.csv")

# Fit simple linear regression:
# phq9_score = Patient Health Questionnaire-9 score-outcome;
# physical_activity_min_week = Minutes of physical activity per week- predictor

fit_phq9_pa <- lm(phq9_score ~ physical_activity_min_week,
                  data = depression_data_clean)
summary(fit_phq9_pa) # coefficients, SEs, t tests, R-squared
confint(fit_phq9_pa) # coefficient confidence intervals


anova(fit_phq9_pa) # regression ANOVA table

anova_results <- anova(fit_phq9_pa)



# In the PHQ9-Physical Activity SLR- 
#P HQ9 Score = 10.8809 - 0.004657(Minutes of physical activity per week)
# the slope estimate is -0.004657 meaning that an additional minute of physical
# activity is associated on average with an approximately 0.004657 decrease in
# PHQ9 score.

# The slope standard error (SE) is 0.0009003 representing the estimated 
# sampling variability of the slope

# The t-statistic is -5.173, meaning that the estimated slope is 5.173 
# standard errors below the null-hypothesized slope of 0

# The two-sided p-value is 2.75e-07 (p < 0.001). Assuming the null hypothesis 
# H0 : β1 = 0 is true, the probability of obtaining a t statistic as extreme or
# more extreme than -5.173 is approximately 0.0000275%. 
# Since p< 0.05, we reject the null hypothesis and conclude that there is 
# sufficient statistical evidence of a linear association between 
# weekly physical activity minutes and PHQ9 depression scores.

# The 95% confidence interval is (-0.006423983, -0.002890716)
# We are 95% confident that the true population slope lies between -0.006423983 
# and -0.002890716. Because the interval excludes 0, the slope is statistically 
# significant at α = 0.05.

# The regression ANOVA F statistic is 26.76 on 1 and 1073 DF, p-value: 2.75e-07

# The R-squared: 0.02433 and Adjusted R-squared:  0.02342

# The residual Standard Error is 3.478 on 1073 degrees of freedom


# Pearson correlation matrix
r_pearson <- cor(
  depression_data_clean[, c(
    "phq9_score",
    "physical_activity_min_week"
  )],
  use = "complete.obs",
  method = "pearson"
)

r_pearson

# The Pearson correlation coefficient between weekly physical activity minutes 
# and PHQ9 scores is r = -0.156, indicating a weak negative linear relationship.


# Verify F = t^2 in SLR
coef_table <- coef(summary(fit_phq9_pa))

slope_t <- coef_table["physical_activity_min_week", "t value"]

# Overall model F statistic from summary()
f_info <- summary(fit_phq9_pa)$fstatistic
model_F <- unname(f_info["value"])

c(
  t_value   = slope_t,
  t_squared = slope_t^2,
  model_F   = model_F
)

# Numerical check
stopifnot(isTRUE(all.equal(
  as.numeric(slope_t^2),
  as.numeric(model_F),
  tolerance = 1e-8
)))

#The squared slope t-statistic (t = -5.173) is approximately 26.76, which 
#equals the overall model F-statistic (F = 26.76).
#This verifies the mathematical relationship F = t^2.


# Verify r = sign(b1) * sqrt(R^2) in SLR
# Extract the slope coefficient
b1 <- coef(fit_phq9_pa)[["physical_activity_min_week"]]

# Extract R-squared
R2 <- summary(fit_phq9_pa)$r.squared

# Extract Pearson correlation from matrix
r_pearson_value <- r_pearson[
  "phq9_score",
  "physical_activity_min_week"
]

# Calculate r using the SLR relationship
r_from_R2 <- sign(b1) * sqrt(R2)

# Compare the results
c(
  slope = b1,
  R_squared = R2,
  Pearson_r = r_pearson_value,
  r_from_R2 = r_from_R2
)

# Numerical verification
stopifnot(isTRUE(all.equal(
  as.numeric(r_pearson_value),
  as.numeric(r_from_R2),
  tolerance = 1e-8
)))

# Residuals versus fitted values
plot(fitted(fit_phq9_pa), resid(fit_phq9_pa), 
     xlab = "Fitted PHQ9 Scores",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values",
     pch = 16)
abline(h = 0, lty = 2)


# Normal Q-Q Plot
qqnorm(
  resid(fit_phq9_pa),
  pch = 16,
  main = "Normal Q-Q Plot"
)

qqline(resid(fit_phq9_pa))

#LINE Assumptions

# Linearity:
# Linear mean function- The residuals vs. fitted values shows an uneven 
# distribution, with most scores concentrated between 9 and 11. 
# However, there is no obvious U-shaped curvature or pattern. Visual assessment
# is difficult due to uneven distribution, but there is insufficient visual 
# evidence to conclude the assumption has been violated.

# Independence
# Independence of residuals cannot be determined using these plots. Assessing
# this assumption requires study design information, such as how participants 
#were sampled and whether observations are influence one another.

# Normality
# Normal Q-Q plot of model residuals shows that most observations follow the
# reference line very closely. There are minor deviations in the tails, but not
# enough to conclude violations. Normality assumption is reasonably satisfied.

# Equal variance (homoscedasticity):
# The residuals vs. fitted values plot suggests a possible mild funnel-shaped 
# pattern, with greater residual spread at higher fitted PHQ9 scores.
# There may be concern of heteroscedasticity and a potential violation of 
# the equal variance assumption. However, the pattern is not definitive 
# based on visual inspection alone.