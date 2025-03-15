myData <-
  read.csv("C:/Users/USER/Desktop/BA Assignment/Prestige_New.csv")
print(myData)
print(myData$income)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


min_income <- min(myData$income, na.rm = TRUE)
max_income <- max(myData$income, na.rm = TRUE)
mean_income <- mean(myData$income, na.rm = TRUE)
median_income <- median(myData$income, na.rm = TRUE)
mode_income <- getmode(myData$income)

print(paste("Min:", min_income))
print(paste("Max:", max_income))
print(paste("Mean:", mean_income))
print(paste("Median:", median_income))
print(paste("Mode:", mode_income))



summary(myData$prestige)
summary(myData$education)
summary(myData$income)




#--------------------------------------------------------

# Load necessary library
install.packages("ggplot2")
library(ggplot2)

# Compute Mean
mean_prestige <- mean(myData$prestige, na.rm = TRUE)
mean_education <- mean(myData$education, na.rm = TRUE)
mean_income <- mean(myData$income, na.rm = TRUE)

# Compute Median
median_prestige <- median(myData$prestige, na.rm = TRUE)
median_education <- median(myData$education, na.rm = TRUE)
median_income <- median(myData$income, na.rm = TRUE)

# Function to calculate Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Compute Mode
mode_prestige <- getmode(myData$prestige)
mode_education <- getmode(myData$education)
mode_income <- getmode(myData$income)

# Print Central Tendency Measures
print(paste("Prestige - Mean:", mean_prestige, " | Median:", median_prestige, " | Mode:", mode_prestige))
print(paste("Education - Mean:", mean_education, " | Median:", median_education, " | Mode:", mode_education))
print(paste("Income - Mean:", mean_income, " | Median:", median_income, " | Mode:", mode_income))



# Bell Curve Function
plot_bell_curve <- function(data, title, color) {
  ggplot(data, aes(x = value)) +
    geom_density(fill = color, alpha = 0.5) + 
    geom_vline(aes(xintercept = mean(value)), color = "red", linetype = "dashed", size = 1) + 
    geom_vline(aes(xintercept = median(value)), color = "blue", linetype = "dotted", size = 1) + 
    ggtitle(title) +
    theme_minimal()
}

# Convert Data to Long Format
prestige_data <- data.frame(value = myData$prestige)
education_data <- data.frame(value = myData$education)
income_data <- data.frame(value = myData$income)

# Plot Bell Curves
plot_prestige <- plot_bell_curve(prestige_data, "Bell Curve of Prestige", "blue")
plot_education <- plot_bell_curve(education_data, "Bell Curve of Education", "green")
plot_income <- plot_bell_curve(income_data, "Bell Curve of Income", "orange")

# Display Plots
print(plot_prestige)
print(plot_education)
print(plot_income)





#-------------------------------------------------------------
# Load necessary libraries
install.packages("ggplot2")  # if not already installed
library(ggplot2)


# Remove rows with missing 'type' values (if any)
data <- myData[!is.na(myData$type), ]

# Define hypotheses:
# H0 (Null Hypothesis): The mean prestige is the same across different occupation types.
# H1 (Alternative Hypothesis): At least one occupation type has a mean prestige that differs significantly.

# Conduct a one-way ANOVA to test the hypothesis
anova_result <- aov(prestige ~ type, data = data)
summary(anova_result)

# Calculate group means for each occupation type
group_means <- aggregate(prestige ~ type, data = data, FUN = mean)
print(group_means)

# Graphical Analysis: Create a boxplot to visualize the distribution of prestige by occupation type
boxplot <- ggplot(data, aes(x = type, y = prestige)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Prestige by Occupation Type",
       x = "Occupation Type",
       y = "Prestige") +
  theme_minimal()
print(boxplot)

# Optional: Create a density plot overlaying mean and median lines for further insights
plot_bell_curve <- function(data, title, color) {
  ggplot(data, aes(x = prestige)) +
    geom_density(fill = color, alpha = 0.5) +
    geom_vline(aes(xintercept = mean(prestige)), color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median(prestige)), color = "blue", linetype = "dotted", size = 1) +
    ggtitle(title) +
    theme_minimal()
}

# Create a density plot for each type (if needed)
density_plots <- lapply(unique(data$type), function(t) {
  subset_data <- data[data$type == t, ]
  plot_bell_curve(subset_data, paste("Density Plot of Prestige for", t), "orange")
})
# Display one of the density plots as an example
print(density_plots[[1]])





#-----------------------------------------------------------------

# Load necessary libraries
install.packages("ggplot2")  # if not already installed
library(ggplot2)


# Remove rows with missing 'type' values (if any)
data <- myData[!is.na(myData$type), ]

# Define hypotheses:
# H0 (Null Hypothesis): The mean prestige is the same across different occupation types.
# H1 (Alternative Hypothesis): At least one occupation type has a mean prestige that differs significantly.

# Conduct a one-way ANOVA to test the hypothesis
anova_result <- aov(prestige ~ type, data = data)
summary(anova_result)

# Calculate group means for each occupation type
group_means <- aggregate(prestige ~ type, data = data, FUN = mean)
print(group_means)

# Graphical Analysis: Create a boxplot to visualize the distribution of prestige by occupation type
boxplot <- ggplot(data, aes(x = type, y = prestige)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Prestige by Occupation Type",
       x = "Occupation Type",
       y = "Prestige") +
  theme_minimal()
print(boxplot)

# Optional: Create a density plot overlaying mean and median lines for further insights
plot_bell_curve <- function(data, title, color) {
  ggplot(data, aes(x = prestige)) +
    geom_density(fill = color, alpha = 0.5) +
    geom_vline(aes(xintercept = mean(prestige)), color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median(prestige)), color = "blue", linetype = "dotted", size = 1) +
    ggtitle(title) +
    theme_minimal()
}

# Create a density plot for each type (if needed)
density_plots <- lapply(unique(data$type), function(t) {
  subset_data <- data[data$type == t, ]
  plot_bell_curve(subset_data, paste("Density Plot of Prestige for", t), "orange")
})
# Display one of the density plots as an example
print(density_plots[[1]])






#-----------------------------------------------------------

# Step 1: State the Hypotheses:
# H0: There is no significant correlation between prestige and education (ρ = 0).
# H1: There is a significant correlation between prestige and education (ρ ≠ 0).

# Step 2: Test for Normality (using Shapiro-Wilk Test)
shapiro_prestige <- shapiro.test(myData$prestige)
shapiro_education <- shapiro.test(myData$education)

print(shapiro_prestige)
print(shapiro_education)

# If both variables are normally distributed (p-value > 0.05), we proceed with Pearson's correlation test.
# Otherwise, consider using Spearman's rank correlation.

# Step 3: Conduct Pearson Correlation Analysis
correlation_result <- cor.test(myData$prestige, myData$education, method = "pearson", na.rm = TRUE)
print(correlation_result)



#------------------------------------------------------------------

# Step 1: State the Hypotheses:
# H0: There is no significant correlation between prestige and income (ρ = 0).
# H1: There is a significant correlation between prestige and income (ρ ≠ 0).

# Step 2: Test for Normality (using Shapiro-Wilk Test)
shapiro_prestige <- shapiro.test(myData$prestige)
shapiro_income <- shapiro.test(myData$income)

print(shapiro_prestige)
print(shapiro_income)

# If both variables are normally distributed (p-value > 0.05), we proceed with Pearson's correlation test.
# Otherwise, we will use Spearman's rank correlation.

# Step 3: Conduct Pearson Correlation Analysis
correlation_result <- cor.test(myData$prestige, myData$income, method = "pearson", na.rm = TRUE)
print(correlation_result)





#----------------------------------------------------------------

# Step 1: State the Hypotheses:
# H0: There is no significant correlation between prestige and percentage of women (ρ = 0).
# H1: There is a significant correlation between prestige and percentage of women (ρ ≠ 0).

# Step 2: Test for Normality (using Shapiro-Wilk Test)
shapiro_prestige <- shapiro.test(myData$prestige)
shapiro_women_percentage <- shapiro.test(myData$women)

print(shapiro_prestige)
print(shapiro_women_percentage)

# If both variables are normally distributed (p-value > 0.05), we proceed with Pearson's correlation test.
# Otherwise, we will use Spearman's rank correlation.

# Step 3: Conduct Pearson Correlation Analysis
correlation_result <- cor.test(myData$prestige, myData$women, method = "pearson", na.rm = TRUE)
print(correlation_result)



#----------------------------------------------------------------------

# Perform Multiple Linear Regression
model <- lm(prestige ~ education + income + women, data = myData)
summary(model)

#-------------------------------------------------------------------
shapiro.test(myData$prestige)
shapiro.test(myData$education)


cor.test(myData$prestige, myData$education, method = "pearson")  # or method = "spearman"

shapiro.test(myData$prestige)
shapiro.test(myData$income)

cor.test(myData$prestige, myData$income, method = "pearson")  # or method = "spearman"

shapiro.test(myData$prestige)
shapiro.test(myData$women)

cor.test(myData$prestige, myData$women, method = "pearson")  # or method = "spearman"



model <- lm(prestige ~ education + income + women, data = myData)
summary(model)

