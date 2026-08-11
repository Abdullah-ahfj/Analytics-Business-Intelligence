# Task A
# ============================================================

# Import dataset
air_data <- read.csv(file.choose())

# View dataset
View(air_data)

# Check dataset dimensions
dim(air_data)

# Check column names
names(air_data)

# Check dataset structure
str(air_data)

# Check missing values
colSums(is.na(air_data))

# Descriptive statistics
summary(air_data)

# Standard deviations
sapply(air_data, sd)




# Normality Testing
# ============================================================

# Individual Shapiro-Wilk tests
shapiro.test(air_data$airport_traffic)
shapiro.test(air_data$avg_income)
shapiro.test(air_data$fuel_price)
shapiro.test(air_data$avg_ticket_fare)
shapiro.test(air_data$flight_frequency)
shapiro.test(air_data$route_distance)
shapiro.test(air_data$passenger_demand)

# Display only p-values
sapply(air_data, function(x) shapiro.test(x)$p.value)





# Create folder for Task A figures
# ============================================================

if (!dir.exists("Task_A_Figures")) {
  dir.create("Task_A_Figures")
}


# Histograms
# ============================================================

png("Task_A_Figures/A01_Histogram_Airport_Traffic.png",
    width = 1200, height = 800, res = 150)

hist(air_data$airport_traffic,
     main = "Distribution of Airport Traffic",
     xlab = "Annual Airport Traffic (Passengers)")

dev.off()


png("Task_A_Figures/A02_Histogram_Average_Income.png",
    width = 1200, height = 800, res = 150)

hist(air_data$avg_income,
     main = "Distribution of Average Income",
     xlab = "Average Income (USD per Year)")

dev.off()


png("Task_A_Figures/A03_Histogram_Fuel_Price.png",
    width = 1200, height = 800, res = 150)

hist(air_data$fuel_price,
     main = "Distribution of Aviation Fuel Price",
     xlab = "Fuel Price (USD per Litre)")

dev.off()


png("Task_A_Figures/A04_Histogram_Average_Ticket_Fare.png",
    width = 1200, height = 800, res = 150)

hist(air_data$avg_ticket_fare,
     main = "Distribution of Average Ticket Fare",
     xlab = "Average Ticket Fare (USD)")

dev.off()


png("Task_A_Figures/A05_Histogram_Flight_Frequency.png",
    width = 1200, height = 800, res = 150)

hist(air_data$flight_frequency,
     main = "Distribution of Flight Frequency",
     xlab = "Scheduled Flights per Day")

dev.off()


png("Task_A_Figures/A06_Histogram_Route_Distance.png",
    width = 1200, height = 800, res = 150)

hist(air_data$route_distance,
     main = "Distribution of Route Distance",
     xlab = "Route Distance (Kilometres)")

dev.off()


png("Task_A_Figures/A07_Histogram_Passenger_Demand.png",
    width = 1200, height = 800, res = 150)

hist(air_data$passenger_demand,
     main = "Distribution of Passenger Demand",
     xlab = "Passenger Demand")

dev.off()


# Q-Q Plot
# ============================================================

png("Task_A_Figures/A08_QQ_Plot_Passenger_Demand.png",
    width = 1200, height = 800, res = 150)

qqnorm(air_data$passenger_demand,
       main = "Q-Q Plot of Passenger Demand",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")

qqline(air_data$passenger_demand)

dev.off()






# CORRELATION ANALYSIS
# ============================================================

# Create folders if they do not already exist
if (!dir.exists("Task_A_Figures")) {
  dir.create("Task_A_Figures")
}

if (!dir.exists("Task_A_Results")) {
  dir.create("Task_A_Results")
}



# 1. Pearson Correlation Matrix
# ------------------------------------------------------------

correlation_matrix <- cor(
  air_data,
  method = "pearson"
)

# Display correlation matrix rounded to 3 decimal places
round(correlation_matrix, 3)



# 2. Correlation of Each Variable with Passenger Demand
# ------------------------------------------------------------

passenger_correlations <- correlation_matrix[, "passenger_demand"]

# Display correlations with passenger demand
round(passenger_correlations, 3)



# 3. Save Correlation Matrix as CSV
# ------------------------------------------------------------

write.csv(
  round(correlation_matrix, 3),
  "Task_A_Results/A03_Correlation_Matrix.csv",
  row.names = TRUE
)



# 4. Pearson Correlation Significance Tests
# ------------------------------------------------------------

# Airport Traffic vs Passenger Demand
cor_airport <- cor.test(
  air_data$airport_traffic,
  air_data$passenger_demand,
  method = "pearson"
)

print(cor_airport)


# Average Income vs Passenger Demand
cor_income <- cor.test(
  air_data$avg_income,
  air_data$passenger_demand,
  method = "pearson"
)

print(cor_income)


# Fuel Price vs Passenger Demand
cor_fuel <- cor.test(
  air_data$fuel_price,
  air_data$passenger_demand,
  method = "pearson"
)

print(cor_fuel)


# Average Ticket Fare vs Passenger Demand
cor_fare <- cor.test(
  air_data$avg_ticket_fare,
  air_data$passenger_demand,
  method = "pearson"
)

print(cor_fare)


# Flight Frequency vs Passenger Demand
cor_flight <- cor.test(
  air_data$flight_frequency,
  air_data$passenger_demand,
  method = "pearson"
)

print(cor_flight)


# Route Distance vs Passenger Demand
cor_distance <- cor.test(
  air_data$route_distance,
  air_data$passenger_demand,
  method = "pearson"
)

print(cor_distance)



# 5. Create Summary Table of Correlation Results
# ------------------------------------------------------------

correlation_results <- data.frame(
  
  Variable = c(
    "Airport Traffic",
    "Average Income",
    "Fuel Price",
    "Average Ticket Fare",
    "Flight Frequency",
    "Route Distance"
  ),
  
  Correlation = c(
    unname(cor_airport$estimate),
    unname(cor_income$estimate),
    unname(cor_fuel$estimate),
    unname(cor_fare$estimate),
    unname(cor_flight$estimate),
    unname(cor_distance$estimate)
  ),
  
  P_Value = c(
    cor_airport$p.value,
    cor_income$p.value,
    cor_fuel$p.value,
    cor_fare$p.value,
    cor_flight$p.value,
    cor_distance$p.value
  )
)


# Add statistical significance
correlation_results$Significant <- ifelse(
  correlation_results$P_Value <= 0.05,
  "Yes",
  "No"
)


# Round values for easier reading
correlation_results$Correlation <- round(
  correlation_results$Correlation,
  3
)

correlation_results$P_Value <- round(
  correlation_results$P_Value,
  5
)


# Display final correlation summary table
print(correlation_results)


# Save final correlation table
write.csv(
  correlation_results,
  "Task_A_Results/A04_Correlation_Results.csv",
  row.names = FALSE
)



# 6. Save Detailed Pearson Correlation Test Results
# ------------------------------------------------------------

capture.output(
  {
    cat("PEARSON CORRELATION ANALYSIS\n")
    cat("============================\n\n")
    
    cat("Airport Traffic vs Passenger Demand\n")
    print(cor_airport)
    
    cat("\nAverage Income vs Passenger Demand\n")
    print(cor_income)
    
    cat("\nFuel Price vs Passenger Demand\n")
    print(cor_fuel)
    
    cat("\nAverage Ticket Fare vs Passenger Demand\n")
    print(cor_fare)
    
    cat("\nFlight Frequency vs Passenger Demand\n")
    print(cor_flight)
    
    cat("\nRoute Distance vs Passenger Demand\n")
    print(cor_distance)
  },
  
  file = "Task_A_Results/A05_Detailed_Correlation_Tests.txt"
)



# SCATTERPLOTS WITH REGRESSION LINES
# ============================================================



# Airport Traffic vs Passenger Demand
# ------------------------------------------------------------

png(
  "Task_A_Figures/A09_Scatterplot_Airport_Traffic.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  air_data$airport_traffic,
  air_data$passenger_demand,
  main = "Airport Traffic vs Passenger Demand",
  xlab = "Annual Airport Traffic (Passengers)",
  ylab = "Passenger Demand",
  pch = 19
)

abline(
  lm(passenger_demand ~ airport_traffic, data = air_data),
  lwd = 2
)

dev.off()



# Average Income vs Passenger Demand
# ------------------------------------------------------------

png(
  "Task_A_Figures/A10_Scatterplot_Average_Income.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  air_data$avg_income,
  air_data$passenger_demand,
  main = "Average Income vs Passenger Demand",
  xlab = "Average Income (USD per Year)",
  ylab = "Passenger Demand",
  pch = 19
)

abline(
  lm(passenger_demand ~ avg_income, data = air_data),
  lwd = 2
)

dev.off()



# Fuel Price vs Passenger Demand
# ------------------------------------------------------------

png(
  "Task_A_Figures/A11_Scatterplot_Fuel_Price.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  air_data$fuel_price,
  air_data$passenger_demand,
  main = "Fuel Price vs Passenger Demand",
  xlab = "Fuel Price (USD per Litre)",
  ylab = "Passenger Demand",
  pch = 19
)

abline(
  lm(passenger_demand ~ fuel_price, data = air_data),
  lwd = 2
)

dev.off()



# Average Ticket Fare vs Passenger Demand
# ------------------------------------------------------------

png(
  "Task_A_Figures/A12_Scatterplot_Average_Ticket_Fare.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  air_data$avg_ticket_fare,
  air_data$passenger_demand,
  main = "Average Ticket Fare vs Passenger Demand",
  xlab = "Average Ticket Fare (USD)",
  ylab = "Passenger Demand",
  pch = 19
)

abline(
  lm(passenger_demand ~ avg_ticket_fare, data = air_data),
  lwd = 2
)

dev.off()



# Flight Frequency vs Passenger Demand
# ------------------------------------------------------------

png(
  "Task_A_Figures/A13_Scatterplot_Flight_Frequency.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  air_data$flight_frequency,
  air_data$passenger_demand,
  main = "Flight Frequency vs Passenger Demand",
  xlab = "Scheduled Flights per Day",
  ylab = "Passenger Demand",
  pch = 19
)

abline(
  lm(passenger_demand ~ flight_frequency, data = air_data),
  lwd = 2
)

dev.off()



# Route Distance vs Passenger Demand
# ------------------------------------------------------------

png(
  "Task_A_Figures/A14_Scatterplot_Route_Distance.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  air_data$route_distance,
  air_data$passenger_demand,
  main = "Route Distance vs Passenger Demand",
  xlab = "Route Distance (Kilometres)",
  ylab = "Passenger Demand",
  pch = 19
)

abline(
  lm(passenger_demand ~ route_distance, data = air_data),
  lwd = 2
)

dev.off()



# END OF CORRELATION ANALYSIS
# ============================================================

cat("\nCorrelation analysis completed successfully.\n")
cat("Results saved in: Task_A_Results\n")
cat("Graphs saved in: Task_A_Figures\n")




# SIMPLE LINEAR REGRESSION
# Passenger Demand predicted by Airport Traffic
# ============================================================

# Create results and figures folders if they do not exist
if (!dir.exists("Task_A_Figures")) {
  dir.create("Task_A_Figures")
}

if (!dir.exists("Task_A_Results")) {
  dir.create("Task_A_Results")
}



# 1. Build Simple Linear Regression Model
# ------------------------------------------------------------

simple_model <- lm(
  passenger_demand ~ airport_traffic,
  data = air_data
)



# 2. Display Model Summary
# ------------------------------------------------------------

summary(simple_model)



# 3. Display Regression Coefficients
# ------------------------------------------------------------

coef(simple_model)



# 4. Display Confidence Intervals
# ------------------------------------------------------------

confint(simple_model)



# 5. Save Full Regression Output
# ------------------------------------------------------------

capture.output(
  {
    cat("SIMPLE LINEAR REGRESSION\n")
    cat("========================\n\n")
    
    cat("Dependent Variable: Passenger Demand\n")
    cat("Independent Variable: Airport Traffic\n\n")
    
    print(summary(simple_model))
    
    cat("\nRegression Coefficients\n")
    print(coef(simple_model))
    
    cat("\n95% Confidence Intervals\n")
    print(confint(simple_model))
  },
  file = "Task_A_Results/A06_Simple_Linear_Regression.txt"
)



# 6. Create Regression Scatterplot
# ------------------------------------------------------------

png(
  "Task_A_Figures/A15_Simple_Regression_Airport_Traffic.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  air_data$airport_traffic,
  air_data$passenger_demand,
  main = "Simple Linear Regression: Airport Traffic vs Passenger Demand",
  xlab = "Airport Traffic",
  ylab = "Passenger Demand",
  pch = 19
)

abline(
  simple_model,
  lwd = 2
)

dev.off()



# 7. Save Regression Diagnostic Plots
# ------------------------------------------------------------

png(
  "Task_A_Figures/A16_Simple_Regression_Diagnostics.png",
  width = 1400,
  height = 1000,
  res = 150
)

par(mfrow = c(2, 2))

plot(simple_model)

dev.off()

par(mfrow = c(1, 1))



# 8. Test Normality of Regression Residuals
# ------------------------------------------------------------

simple_residual_normality <- shapiro.test(
  residuals(simple_model)
)

print(simple_residual_normality)


# Save residual normality result
capture.output(
  simple_residual_normality,
  file = "Task_A_Results/A07_Simple_Model_Residual_Normality.txt"
)



# 9. Extract Important Model Statistics
# ------------------------------------------------------------

simple_summary <- summary(simple_model)

simple_results <- data.frame(
  
  Statistic = c(
    "Intercept",
    "Airport Traffic Coefficient",
    "R-Squared",
    "Adjusted R-Squared",
    "F-Statistic",
    "Model P-Value"
  ),
  
  Value = c(
    coef(simple_model)[1],
    coef(simple_model)[2],
    simple_summary$r.squared,
    simple_summary$adj.r.squared,
    simple_summary$fstatistic[1],
    pf(
      simple_summary$fstatistic[1],
      simple_summary$fstatistic[2],
      simple_summary$fstatistic[3],
      lower.tail = FALSE
    )
  )
)


# Display summary statistics
print(simple_results)


# Save summary statistics
write.csv(
  simple_results,
  "Task_A_Results/A08_Simple_Regression_Summary.csv",
  row.names = FALSE
)



# END OF SIMPLE LINEAR REGRESSION
# ============================================================

cat("\nSimple linear regression completed successfully.\n")
cat("Results saved in: Task_A_Results\n")
cat("Graphs saved in: Task_A_Figures\n")


summary(simple_model)

simple_residual_normality






# MULTIPLE LINEAR REGRESSION
# Passenger Demand predicted by all independent variables
# ============================================================

# Create folders if they do not already exist
if (!dir.exists("Task_A_Figures")) {
  dir.create("Task_A_Figures")
}

if (!dir.exists("Task_A_Results")) {
  dir.create("Task_A_Results")
}


# ------------------------------------------------------------
# 1. Build Multiple Linear Regression Model
# ------------------------------------------------------------

multiple_model <- lm(
  passenger_demand ~ airport_traffic +
    avg_income +
    fuel_price +
    avg_ticket_fare +
    flight_frequency +
    route_distance,
  data = air_data
)


# ------------------------------------------------------------
# 2. Display Full Model Summary
# ------------------------------------------------------------

summary(multiple_model)


# ------------------------------------------------------------
# 3. Display Regression Coefficients
# ------------------------------------------------------------

coef(multiple_model)


# ------------------------------------------------------------
# 4. Display 95% Confidence Intervals
# ------------------------------------------------------------

confint(multiple_model)


# ------------------------------------------------------------
# 5. Save Full Multiple Regression Output
# ------------------------------------------------------------

capture.output(
  {
    cat("MULTIPLE LINEAR REGRESSION\n")
    cat("==========================\n\n")
    
    cat("Dependent Variable: Passenger Demand\n\n")
    
    cat("Independent Variables:\n")
    cat("- Airport Traffic\n")
    cat("- Average Income\n")
    cat("- Fuel Price\n")
    cat("- Average Ticket Fare\n")
    cat("- Flight Frequency\n")
    cat("- Route Distance\n\n")
    
    print(summary(multiple_model))
    
    cat("\nRegression Coefficients\n")
    print(coef(multiple_model))
    
    cat("\n95% Confidence Intervals\n")
    print(confint(multiple_model))
  },
  file = "Task_A_Results/A09_Multiple_Linear_Regression.txt"
)


# ------------------------------------------------------------
# 6. Test Normality of Multiple Regression Residuals
# ------------------------------------------------------------

multiple_residual_normality <- shapiro.test(
  residuals(multiple_model)
)

print(multiple_residual_normality)


# Save residual normality test
capture.output(
  multiple_residual_normality,
  file = "Task_A_Results/A10_Multiple_Model_Residual_Normality.txt"
)


# ------------------------------------------------------------
# 7. Save Multiple Regression Diagnostic Plots
# ------------------------------------------------------------

png(
  "Task_A_Figures/A17_Multiple_Regression_Diagnostics.png",
  width = 1400,
  height = 1000,
  res = 150
)

par(mfrow = c(2, 2))

plot(multiple_model)

dev.off()

par(mfrow = c(1, 1))


# ------------------------------------------------------------
# 8. Extract Important Model Statistics
# ------------------------------------------------------------

multiple_summary <- summary(multiple_model)

multiple_model_p_value <- pf(
  multiple_summary$fstatistic[1],
  multiple_summary$fstatistic[2],
  multiple_summary$fstatistic[3],
  lower.tail = FALSE
)

multiple_results <- data.frame(
  
  Statistic = c(
    "R-Squared",
    "Adjusted R-Squared",
    "Residual Standard Error",
    "F-Statistic",
    "Model P-Value"
  ),
  
  Value = c(
    multiple_summary$r.squared,
    multiple_summary$adj.r.squared,
    multiple_summary$sigma,
    multiple_summary$fstatistic[1],
    multiple_model_p_value
  )
)

print(multiple_results)


# Save model statistics
write.csv(
  multiple_results,
  "Task_A_Results/A11_Multiple_Regression_Summary.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 9. Create Coefficient Summary Table
# ------------------------------------------------------------

coefficient_table <- data.frame(
  Variable = rownames(multiple_summary$coefficients),
  Estimate = multiple_summary$coefficients[, 1],
  Standard_Error = multiple_summary$coefficients[, 2],
  T_Value = multiple_summary$coefficients[, 3],
  P_Value = multiple_summary$coefficients[, 4]
)

coefficient_table$Significant <- ifelse(
  coefficient_table$P_Value <= 0.05,
  "Yes",
  "No"
)

# Round values for easier reading
coefficient_table$Estimate <- round(
  coefficient_table$Estimate,
  4
)

coefficient_table$Standard_Error <- round(
  coefficient_table$Standard_Error,
  4
)

coefficient_table$T_Value <- round(
  coefficient_table$T_Value,
  3
)

coefficient_table$P_Value <- round(
  coefficient_table$P_Value,
  5
)


# Display coefficient table
print(coefficient_table)


# Save coefficient table
write.csv(
  coefficient_table,
  "Task_A_Results/A12_Multiple_Regression_Coefficients.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 10. Compare Simple and Multiple Models
# ------------------------------------------------------------

model_comparison <- data.frame(
  
  Model = c(
    "Simple Linear Regression",
    "Multiple Linear Regression"
  ),
  
  R_Squared = c(
    summary(simple_model)$r.squared,
    summary(multiple_model)$r.squared
  ),
  
  Adjusted_R_Squared = c(
    summary(simple_model)$adj.r.squared,
    summary(multiple_model)$adj.r.squared
  )
)

model_comparison$R_Squared <- round(
  model_comparison$R_Squared,
  4
)

model_comparison$Adjusted_R_Squared <- round(
  model_comparison$Adjusted_R_Squared,
  4
)

print(model_comparison)


# Save model comparison
write.csv(
  model_comparison,
  "Task_A_Results/A13_Model_Comparison.csv",
  row.names = FALSE
)


# ============================================================
# END OF MULTIPLE LINEAR REGRESSION
# ============================================================

cat("\nMultiple linear regression completed successfully.\n")
cat("Results saved in: Task_A_Results\n")
cat("Graphs saved in: Task_A_Figures\n")


summary(multiple_model)

multiple_residual_normality

print(coefficient_table)





# MULTICOLLINEARITY TEST - VIF
# ============================================================

# Install package once if needed
if (!require(car)) {
  install.packages("car")
  library(car)
} else {
  library(car)
}

# Calculate VIF values
vif_results <- vif(multiple_model)

# Display VIF results
print(vif_results)

# Save VIF results
write.csv(
  data.frame(
    Variable = names(vif_results),
    VIF = as.numeric(vif_results)
  ),
  "Task_A_Results/A14_VIF_Results.csv",
  row.names = FALSE
)

print(vif_results)



# REDUCED MULTIPLE REGRESSION MODEL
# Significant predictors only:
# Airport Traffic + Fuel Price
# ============================================================


# ------------------------------------------------------------
# 1. Build Reduced Regression Model
# ------------------------------------------------------------

reduced_model <- lm(
  passenger_demand ~ airport_traffic + fuel_price,
  data = air_data
)


# ------------------------------------------------------------
# 2. Display Reduced Model Summary
# ------------------------------------------------------------

summary(reduced_model)


# ------------------------------------------------------------
# 3. Display Coefficients
# ------------------------------------------------------------

coef(reduced_model)


# ------------------------------------------------------------
# 4. Display 95% Confidence Intervals
# ------------------------------------------------------------

confint(reduced_model)


# ------------------------------------------------------------
# 5. Test Residual Normality
# ------------------------------------------------------------

reduced_residual_normality <- shapiro.test(
  residuals(reduced_model)
)

print(reduced_residual_normality)


# ------------------------------------------------------------
# 6. Save Full Reduced Model Output
# ------------------------------------------------------------

capture.output(
  {
    cat("REDUCED MULTIPLE REGRESSION MODEL\n")
    cat("=================================\n\n")
    
    cat("Dependent Variable: Passenger Demand\n\n")
    
    cat("Predictors:\n")
    cat("- Airport Traffic\n")
    cat("- Fuel Price\n\n")
    
    print(summary(reduced_model))
    
    cat("\nRegression Coefficients\n")
    print(coef(reduced_model))
    
    cat("\n95% Confidence Intervals\n")
    print(confint(reduced_model))
    
    cat("\nResidual Normality Test\n")
    print(reduced_residual_normality)
  },
  file = "Task_A_Results/A15_Reduced_Regression_Model.txt"
)


# ------------------------------------------------------------
# 7. Extract Reduced Model Statistics
# ------------------------------------------------------------

reduced_summary <- summary(reduced_model)

reduced_model_p_value <- pf(
  reduced_summary$fstatistic[1],
  reduced_summary$fstatistic[2],
  reduced_summary$fstatistic[3],
  lower.tail = FALSE
)

reduced_results <- data.frame(
  
  Statistic = c(
    "R-Squared",
    "Adjusted R-Squared",
    "Residual Standard Error",
    "F-Statistic",
    "Model P-Value"
  ),
  
  Value = c(
    reduced_summary$r.squared,
    reduced_summary$adj.r.squared,
    reduced_summary$sigma,
    reduced_summary$fstatistic[1],
    reduced_model_p_value
  )
)

print(reduced_results)


# Save reduced model statistics
write.csv(
  reduced_results,
  "Task_A_Results/A16_Reduced_Model_Summary.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 8. Create Reduced Model Coefficient Table
# ------------------------------------------------------------

reduced_coefficient_table <- data.frame(
  
  Variable = rownames(reduced_summary$coefficients),
  
  Estimate = reduced_summary$coefficients[, 1],
  
  Standard_Error = reduced_summary$coefficients[, 2],
  
  T_Value = reduced_summary$coefficients[, 3],
  
  P_Value = reduced_summary$coefficients[, 4]
)

reduced_coefficient_table$Significant <- ifelse(
  reduced_coefficient_table$P_Value <= 0.05,
  "Yes",
  "No"
)

reduced_coefficient_table$Estimate <- round(
  reduced_coefficient_table$Estimate,
  4
)

reduced_coefficient_table$Standard_Error <- round(
  reduced_coefficient_table$Standard_Error,
  4
)

reduced_coefficient_table$T_Value <- round(
  reduced_coefficient_table$T_Value,
  3
)

reduced_coefficient_table$P_Value <- round(
  reduced_coefficient_table$P_Value,
  5
)

print(reduced_coefficient_table)


# Save coefficient table
write.csv(
  reduced_coefficient_table,
  "Task_A_Results/A17_Reduced_Model_Coefficients.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 9. Compare All Three Regression Models
# ------------------------------------------------------------

final_model_comparison <- data.frame(
  
  Model = c(
    "Simple Model - Airport Traffic",
    "Full Multiple Model - Six Predictors",
    "Reduced Model - Airport Traffic + Fuel Price"
  ),
  
  R_Squared = c(
    summary(simple_model)$r.squared,
    summary(multiple_model)$r.squared,
    summary(reduced_model)$r.squared
  ),
  
  Adjusted_R_Squared = c(
    summary(simple_model)$adj.r.squared,
    summary(multiple_model)$adj.r.squared,
    summary(reduced_model)$adj.r.squared
  ),
  
  Residual_Standard_Error = c(
    summary(simple_model)$sigma,
    summary(multiple_model)$sigma,
    summary(reduced_model)$sigma
  )
)


# Round results
final_model_comparison$R_Squared <- round(
  final_model_comparison$R_Squared,
  4
)

final_model_comparison$Adjusted_R_Squared <- round(
  final_model_comparison$Adjusted_R_Squared,
  4
)

final_model_comparison$Residual_Standard_Error <- round(
  final_model_comparison$Residual_Standard_Error,
  2
)


# Display comparison
print(final_model_comparison)


# Save comparison
write.csv(
  final_model_comparison,
  "Task_A_Results/A18_Final_Model_Comparison.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 10. Save Reduced Model Diagnostic Plots
# ------------------------------------------------------------

png(
  "Task_A_Figures/A18_Reduced_Model_Diagnostics.png",
  width = 1400,
  height = 1000,
  res = 150
)

par(mfrow = c(2, 2))

plot(reduced_model)

dev.off()

par(mfrow = c(1, 1))


# ============================================================
# END OF REDUCED REGRESSION MODEL
# ============================================================

cat("\nReduced regression model completed successfully.\n")
cat("Results saved in: Task_A_Results\n")
cat("Graphs saved in: Task_A_Figures\n")

summary(reduced_model)
reduced_residual_normality
print(final_model_comparison)