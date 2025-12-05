#Final Code

#Intro
#data loading

install.packages(c("tidyverse", "readxl", "car", "psych"))
library(tidyverse)
library(readxl)
library(car)
library(psych)
library(modelr)
read_xlsx("C:/Users/alexp/Downloads/Abalone dataset.xlsx")
abalone <- read_excel("C:/Users/alexp/Downloads/Abalone dataset.xlsx")

#METHODS

#Dependent variables and scales
#the dependent variable we are trying to find is the rings
abalone$Rings <- as.numeric(abalone$Rings)


#Dependent variables descriptive stats

describe(abalone$Rings)#Gets the descriptive stats
  #variables = 1
  #n = 4177
  #mean = 9.93
  #sd = 3.22
  #median = 9
  #se = 0.05

#Histogram of Rings
  #x = number of rings
  #y = Frequency count
  #Why = shows the distribution and points out any skew
abalone |>
  ggplot(aes(x = Rings))+
  geom_histogram(bins = 30, fill = "lightblue", color = "black")+
  labs(title = "Distribution of Rings",
       x = "Number of Rings",
       y = "Frequency")


#CI 

model <- lm(Rings ~ Length + Diameter + Height +Whole_weight + Shucked_weight +Viscera_weight + Shell_weight, data = abalone)
summary(model)
alpha <- 0.05
confint(model, level = 0.95)


#Dependent variables assumptions checked/plots used 

#QQ plot and line for rings
  #axes descriptions 
    #Theoretical Quantiles = "Perfect Normal Distribution"
    #Sample Quantiles = Actual values from the dataset
    #Why we compare: Looks for linear relationship

abalone |>
  ggplot(aes(sample = Rings)) + stat_qq() +stat_qq_line() +
  labs(title = "QQ Plot for Rings", x = "Theoretical Quantiles", y = "Observed Quantiles")+
  theme_minimal()


#Independent variables and scales

#we take the numeric and factor conversions so that we can take the plots and check assumptions
abalone$Length <- as.numeric(abalone$Length)
abalone$Diameter <- as.numeric(abalone$Diameter)
abalone$Height <- as.numeric(abalone$Height)
abalone$Whole_weight <- as.numeric(abalone$Whole_weight)
abalone$Shucked_weight <- as.numeric(abalone$Shucked_weight)
abalone$Viscera_weight <- as.numeric(abalone$Viscera_weight)
abalone$Shell_weight <- as.numeric(abalone$Shell_weight)
abalone$Sex <- as.factor(abalone$Sex)


#Independent variables descriptive stats

#takes the descriptive stats of all the independent vars.
abalone |> select(Length, Diameter, Height, Whole_weight, Shucked_weight,
                   Viscera_weight, Shell_weight) |> describe()


#Independent variables assumptions checked/plots used

#Histograms for ind. vars. 
#x = measured value
#y = frequency
#Why = find skew for each predictor
#In order to simplify findings I took the facet wrap of all of the variables and labeled them.
#This way we can see the correlation of each variable and the predictors
abalone |>
  pivot_longer(cols = c(Length, Diameter, Height, Whole_weight, Shucked_weight, Viscera_weight, Shell_weight),
               names_to = "Variable", values_to = "Value") |>
  ggplot(aes(x = Value)) + geom_histogram(bins = 25, color = "black", fill = "lightblue") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Histograms of the Continuous Predictors", x = "Measured Value (grams and millimeters)", y = "Abalone Measured")


#QQ plot for ind. vars.
#facet wrap again
#Theoretical Quantiles = "Perfect Normal Distribution"
#Sample Quantiles = "Actual IV values"
#Why: check linearity of predictors 
abalone |>
  pivot_longer(cols = c(Length, Diameter, Height, Whole_weight, Shucked_weight, Viscera_weight, Shell_weight),
               names_to = "Variable", values_to = "Value") |>
  ggplot(aes(sample = Value)) + stat_qq() + stat_qq_line() +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "QQ Plots of Predictors", x = "Theoretical Quantiles", y = "Observed Quantiles") +
  theme_minimal() #This QQ plot shows a very linear relationship 


#RESULTS


#descriptive results
#numeric summaries = means, sd, min, max for dep. var. and ind. vars.
abalone |> 
  summarise(mean_Rings = mean(Rings, na.rm = TRUE),
  sd_Rings = sd(Rings, na.rm = TRUE),
  min_Rings = min(Rings, na.rm = TRUE),
  max_Rings = max(Rings, na.rm = TRUE))

abalone |>
  count(Sex)

#Correlation matrix of key numeric variables
cor(abalone |>
      select(Rings, Length, Diameter, Height, Whole_weight, Shucked_weight, Viscera_weight, Shell_weight), use = "complete.obs")


#Uni and Bivariate descriptive statistics for Dependent Variables

#The Univariate statistics are shown below with a boxplot
#x = Sex, 
#y = Rings, 
#Why = compare dependent variables across groups
ggplot(abalone, aes(x = Sex,y = Rings,fill = Sex)) +
  geom_boxplot()+
  labs(title = "Rings by Sex",x = "Sex",y = "Number of Rings")+
  theme(legend.position = "none")


#The Bivariate statistics have already been done earlier with a histogram 

abalone |>
  ggplot(aes(x = Rings))+
  geom_histogram(bins = 30, fill = "lightblue", color = "black")+
  labs(title = "Distribution of Rings",
       x = "Number of Rings",
       y = "Frequency")



#Uni and Bivariate descriptive statistics for independent variables

#Scatterplot: Rings vs Length
#x axis = Length (mm), y axis = Rings
#Why: visualize linear relationship between predictor and DV
ggplot(abalone, aes(x = Length, y = Rings)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rings vs Length", x = "Length (mm)", y = "Number of Rings") +
  theme_minimal()

#SCATTERPLOT FACET

abalone |>
  pivot_longer(cols = c(Length, Diameter, Height, Whole_weight,Shucked_weight, Viscera_weight, Shell_weight),
    names_to = "Predictor", values_to = "Value") |>
  ggplot(aes(x = Value, y = Rings)) +
  geom_point(alpha = 0.1, color = "darkred") +
  facet_wrap(~ Predictor, scales = "free_x") +
  labs( title = "Predictor Values vs Rings",
    x = "Predictor Value",
    y = "Observed Rings") +
  theme_minimal()

#Univariate stats for ind. vars.
#Boxplot to show the dep. var. and how it compares across ind. vars.
#x = Sex
#y = Rings
#Why = compare dep. var. across categories

ggplot(abalone, aes(x = Sex,y = Rings,fill = Sex)) +
  geom_boxplot()+
  labs(title = "Rings by Sex",x = "Sex",y = "Number of Rings")+
  theme_minimal() + theme(legend.position = "none")

#Regression/F-test/R-Squared

model <- lm(Rings ~ Length + Diameter + Height + Whole_weight +
              Shucked_weight + Viscera_weight + Shell_weight + Sex,data = abalone)
summary(model)


#Residual plot for rings(age)

#x = predicted rings,
#y = residuals
#Why: checks constant variance & linearity between Observed and Predicted values
ggplot(model, aes(.fitted, .resid)) +
  geom_point() + geom_hline(yintercept = 0) +
  labs(title = "Observed Rings vs Predicted Rings", subtitle = "Residual Plot",
       x = "Predicted Rings", y = "Observed Rings") +
  theme_minimal()


#QQ plot of regression residuals
#Why?: checks assumptions

ggplot(model, aes(sample = .resid)) +
  stat_qq() + stat_qq_line() +
  labs(title = "Regression Plot for Residuals", subtite = "QQ Plot",
       x = "Predicted Quantiles",
       y = "Observed Quantiles") +
  theme_minimal()


#Outliers/missing data/ final sample size

colSums(is.na(abalone))
nrow(abalone)

#inferential results with tables (regression/F-test/predictor coefficient)

summary(model)
nrow(abalone)

