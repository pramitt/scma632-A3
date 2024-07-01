setwd("C:\\Users\\prami\\Desktop\\SCMA 632\\data")
library("AER")

# Install packages if not already installed
if (!require(GGally)) install.packages('GGally')
if (!require(VGAM)) install.packages('VGAM')
if (!require(ggplot2)) install.packages('ggplot2')

# Load the installed packages
library(GGally)
library(VGAM)
library(ggplot2)
data("Affairs")
head(Affairs)
unique(Affairs$affairs)
table(Affairs$affairs)

# Fit a Tobit model
library(AER)
fm.tobit <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating, data = Affairs)
fm.tobit <- AER::tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating, data = Affairs)
summary(fm.tobit)
summary(fm.tobit2)

# Read the CSV file
df <- read.csv('NSSO68.csv', header = TRUE)

# Display the names of the columns
dput(names(df))

# Subset the data for state 'AP'
df_ap <- df[df$state_1 == 'AP', ]

# Define the columns of interest
vars <- c("Sector", "hhdsz", "Religion", "Social_Group", "MPCE_URP", "Sex", "Age", "Marital_Status", "Education", "chicken_q", "chicken_v")

# Select the relevant columns
df_ap_p <- df_ap[vars]

# Display the names of the columns in the new dataframe
names(df_ap_p)

# Calculate price
df_ap_p$price <- df_ap_p$chicken_v / df_ap_p$chicken_q

# Check the names of the columns after adding 'price'
names(df_ap_p)

# Display a summary of the new dataframe
summary(df_ap_p)

# Check for missing or infinite values in 'price'
df_ap_p$price[is.na(df_ap_p$price) | is.infinite(df_ap_p$price)] <- 0

# Display the frequency table of 'chicken_q'
head(table(df_ap_p$chicken_q))

# Display the dimensions of the dataframe
dim(df_ap_p)

# Fit the linear model using the corrected dataframe
fit <- lm(chicken_q ~ hhdsz + Religion + MPCE_URP + Sex + Age + Marital_Status + Education + price, data = df_ap_p)
summary(fit)
fit <- lm(chicken_q ~ hhdsz + Religion + MPCE_URP + Sex + Age + Marital_Status + Education + price, data = df_ap_p)
summary(fit)

# Ensure GGally and VGAM packages are loaded
require(ggplot2)
require(GGally)
require(VGAM)

# Visualize pairwise relationships
ggpairs(df_ap_p[, c("chicken_q", "MPCE_URP", "price")])

# Fit Tobit model
m <- vglm(chicken_q ~ hhdsz + Religion + MPCE_URP + Sex + Age + Marital_Status + Education + price, tobit(Lower = 0), data = df_ap_p)
summary(m)

# Calculations
exp(-1.032e+00)
sd(df_ap_p$chicken_q)

