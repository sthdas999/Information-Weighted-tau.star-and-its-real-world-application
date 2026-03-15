############################################################
# Scatter Plot: Blood Glucose Level vs Age
# Dataset already in Global Environment: glucose_age_data
############################################################
# Required package
library(KernSmooth)
library(np)

# 1. Check available objects
ls()

# 2. Check column names
colnames(glucose_age_data)

# 3. Convert column names to safe format
names(glucose_age_data) <- make.names(names(glucose_age_data))

# Check updated column names
colnames(glucose_age_data)

############################################################
# 4. Rename important columns
############################################################

# Rename glucose column for convenience
names(glucose_age_data)[names(glucose_age_data)=="Blood.Glucose.Level.BGL."] <- "BGL"

############################################################
# 5. Convert variables to numeric
############################################################

glucose_age_data$Age <- as.numeric(glucose_age_data$Age)
glucose_age_data$BGL <- as.numeric(glucose_age_data$BGL)

############################################################
# 6. Remove rows containing any NA values
############################################################

glucose_age_data <- na.omit(glucose_age_data)

############################################################
# 7. Verify cleaned data
############################################################

summary(glucose_age_data$Age)
summary(glucose_age_data$BGL)
dim(glucose_age_data)

############################################################
# 8. Scatter Plot
############################################################

plot(
  glucose_age_data$Age,
  glucose_age_data$BGL,
  pch = 16,
  cex = 0.6,
  col = rgb(0,0,0,0.5),
  xlab = "Age (years)",
  ylab = "Blood Glucose Level",
  main = "Scatter Plot of Blood Glucose Level vs Age"
)

############################################################
# 9. Save the figure as PDF for LaTeX
############################################################

pdf("glucose_age_scatterplot.pdf", width = 6, height = 4)

plot(
  glucose_age_data$Age,
  glucose_age_data$BGL,
  pch = 16,
  cex = 0.6,
  col = rgb(0,0,0,0.5),
  xlab = "Age (years)",
  ylab = "Blood Glucose Level",
  main = "Scatter Plot of Blood Glucose Level vs Age"
)

dev.off()

############################################################
# Nonparametric Kernel Regression: Glucose vs Age
############################################################

# Clean column names
names(glucose_age_data) <- make.names(names(glucose_age_data))
names(glucose_age_data)[names(glucose_age_data)=="Blood.Glucose.Level.BGL."] <- "BGL"

# Remove missing values
glucose_age_data <- na.omit(glucose_age_data)

# Kernel regression (Nadaraya-Watson estimator)
model <- npreg(BGL ~ Age, data = glucose_age_data)

# Generate grid of age values
age_grid <- seq(min(glucose_age_data$Age),
                max(glucose_age_data$Age),
                length.out = 200)

# Predicted values
fit_values <- predict(model, newdata = data.frame(Age = age_grid))

# Create dataset for plotting
kernel_fit <- data.frame(Age = age_grid, ghat = fit_values)

# Save fitted curve
write.csv(kernel_fit, "kernel_regression_fit.csv", row.names = FALSE)


## Scatterplot of Age vs Estimated Function ##

# Check object
ls()

# Check structure
str(kernel_regression_fit)

# Check column names
names(kernel_regression_fit)

# Reset graphics device
while (!is.null(dev.list())) dev.off()

# Extract variables
Age  <- as.numeric(as.character(kernel_regression_fit$Age))
ghat <- as.numeric(as.character(kernel_regression_fit$ghat))

# Check if values exist
summary(Age)
summary(ghat)

# Remove missing values if created during conversion
ok <- complete.cases(Age, ghat)
Age  <- Age[ok]
ghat <- ghat[ok]

# Draw scatterplot
plot(Age, ghat,
     pch = 19,
     col = "blue",
     xlab = "Age",
     ylab = "ghat",
     main = "Scatterplot of Age vs Estimated Function")

# Fit linear model
fit <- lm(ghat ~ Age, data = kernel_regression_fit)

# Scatterplot
plot(kernel_regression_fit$Age, kernel_regression_fit$ghat,
     pch = 19,
     xlab = "Age",
     ylab = "ghat",
     main = "Scatterplot with Fitted Regression Line")

# Add fitted line
abline(fit, col = "red", lwd = 2)


##  Kernel_Regression_Fit-Blodd_Glucose_Level_vs_Age  ##

# Extract variables
Age  <- as.numeric(as.character(kernel_regression_fit$Age))
ghat <- as.numeric(as.character(kernel_regression_fit$ghat))

# Remove missing values if any
ok <- complete.cases(Age, ghat)
Age  <- Age[ok]
ghat <- ghat[ok]

# Bandwidth selection via cross-validation
bw <- dpill(Age, ghat)

# Nadaraya-Watson kernel regression estimate
fit <- locpoly(Age, ghat,
               bandwidth = bw,
               kernel = "normal",
               degree = 0)

# Scatterplot
plot(Age, ghat,
     pch = 19,
     xlab = "Age",
     ylab = "Blood Glucose Level",
     main = "Kernel Regression Fit")

# Add fitted regression curve
lines(fit$x, fit$y,
      col = "red",
      lwd = 2)
