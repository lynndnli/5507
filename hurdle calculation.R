# Load necessary libraries
install.packages("pscl")
library(pscl)
library(dplyr)
library(lubridate)
library(scales)

# Load your data
data <- read.csv('/Users/Lynn/Downloads/codes/PP_mergedtheta_1214.csv')

# Convert 'Label' to binary 'female' variable
data$gender <- ifelse(data$Label == 'F', 1, 0)


# Count the number of words in each post
data <- data[!is.na(data$tokenz), ]
data$post_length <- sapply(strsplit(data$tokenz, " "), length)

# Selecting the independent variables (IVs) and dependent variable (DV)
IVs <- data[, c('gender','sexism_count','aggressive_count', 'competitive_count', 'incompetitive_count', 'submissive_count','V3','V4')]
DV <- data$incivility_count

# Fit the hurdle model
model <- hurdle(DV ~ ., data = IVs, dist = "poisson")

# Print out the statistics
summary(model)


##############stargazer#############
# Fit the hurdle model
model <- hurdle(DV ~ ., data = IVs, dist = "poisson")

# Print out the statistics using stargazer
stargazer(model, type = "html",
          out = '/Users/Lynn/Downloads/codes/table1.doc',
          dep.var.labels = c("Incivility"), 
          column.labels = c("Model"),
          omit.stat = c("ll", "aic"),
          column.sep.width = "5pt",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          zero.component = TRUE)


# Load required libraries
library(pscl)
library(xtable)











# Calculate McFadden's R-squared
null_logLik <- logLik(hurdle(DV ~ 1, data = IVs, dist = "poisson"))
model_logLik <- logLik(model)
mcFadden_R2 <- 1 - as.numeric(model_logLik) / as.numeric(null_logLik)

# Print McFadden's R-squared
mcFadden_R2

# View the first few rows of the 'months_since_min' column
head(data$months_since_min)

# Load necessary library
library(car)

# Compute VIF
vif_values <- vif(model)

# Print VIF values
print(vif_values)

install.packages("stargazer")
library(stargazer)

vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)

# Create a table using stargazer
stargazer(vif_df, type = "text", title = "Variance Inflation Factors", 
          out = "vif_table.csv", summary = FALSE, align = TRUE)
