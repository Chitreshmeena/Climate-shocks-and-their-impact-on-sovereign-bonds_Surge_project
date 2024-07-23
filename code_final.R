

# Restart your R session to clear any existing objects

# Load the packages
library(writexl)
library(tidyverse)
library(magrittr)
library(stargazer)
library(readxl)
library(foreign)
library(memisc)
library(dplyr)
library(labelled)
library(expss)
library(Hmisc)
library(haven)
library(rio)
library(ggplot2)
library(lessR)
library(tvReg)
library(plm)
library(car)
library(gplots)

new_data <- read_excel("D:\\6th Sem Summer Surge IITK24\\R_Data_and_codes\\datawithnoNA02to16.xlsx")

view(new_data)

data_dev <- read_excel("D:\\6th Sem Summer Surge IITK24\\R_Data_and_codes\\Working paper_data_codes\\dev cont data.xlsx")

view(data_dev)
p1data<-pdata.frame(data_dev,index=c("cname","year"))
is.pbalanced(p1data)

data2_dev <- read_excel("C:\\Users\\lenovo\\Downloads\\final_new_df.xlsx")

view(data2_dev)

p2data<-pdata.frame(new_data,index=c("cname","year"))
is.pbalanced(p2data)

view(p2data)

sum(is.na(p2data$bond_spreads))

# List of variables to check for NA values
variables <- c("bond_spreads", "vulnerability100", "readiness100", "growth",
               "inflation_cpi", "debt_gdp", "OB_gdp", "reserves", "rqe", "gee", "tt")

# Check the number of NA values for each variable and print the results
for (variable in variables) {
  na_count <- sum(is.na(p2data[[variable]]))
  cat(variable, "has", na_count, "NA values\n")
}




str(new_data)

head(new_data,10)

# Check the structure of the data frame
str(new_data)

# Convert character variables to numeric if necessary
new_data$vulnerability100 <- as.numeric(new_data$vulnerability100)
new_data$readiness100 <- as.numeric(new_data$readiness100)
new_data$growth <- as.numeric(new_data$growth)
new_data$inflation_cpi <- as.numeric(new_data$inflation_cpi)
new_data$debt_gdp <- as.numeric(new_data$debt_gdp)
new_data$OB_gdp <- as.numeric(new_data$OB_gdp)
new_data$reserves <- as.numeric(new_data$reserves)
new_data$rqe <- as.numeric(new_data$rqe)
new_data$gee <- as.numeric(new_data$gee)
new_data$tt <- as.numeric(new_data$tt)
new_data$bond_spreads <- as.numeric(new_data$bond_spreads)

# Ensure index columns are of the appropriate type
new_data$cname <- as.factor(new_data$cname)
new_data$year <- as.integer(new_data$year)

#handling NA
#pdata$bond_spreads[is.na(pdata$bond_spreads)] <- mean(pdata$bond_spreads, na.rm = TRUE)
#pdata$vulnerability100[is.na(pdata$vulnerability100)] <- mean(pdata$vulnerability100, na.rm = TRUE)
#pdata$readiness100[is.na(pdata$readiness100)] <- mean(pdata$readiness100, na.rm = TRUE)
#pdata$growth[is.na(pdata$growth)] <- mean(pdata$growth, na.rm = TRUE)
#pdata$inflation_cpi[is.na(pdata$inflation_cpi)] <- mean(pdata$inflation_cpi, na.rm = TRUE)
#pdata$debt_gdp[is.na(pdata$debt_gdp)] <- mean(pdata$debt_gdp, na.rm = TRUE)
#pdata$OB_gdp[is.na(pdata$OB_gdp)] <- mean(pdata$OB_gdp, na.rm = TRUE)
#pdata$reserves[is.na(pdata$reserves)] <- mean(pdata$reserves, na.rm = TRUE)
#pdata$rqe[is.na(pdata$rqe)] <- mean(pdata$rqe, na.rm = TRUE)
#pdata$gee[is.na(pdata$gee)] <- mean(pdata$gee, na.rm = TRUE)
#pdata$tt[is.na(pdata$tt)] <- mean(pdata$tt, na.rm = TRUE)

#1---------------------------------

# Ensure variables are numeric
p2data$vulnerability100 <- as.numeric(p2data$vulnerability100)
p2data$readiness100 <- as.numeric(p2data$readiness100)
p2data$growth <- as.numeric(p2data$growth)
p2data$inflation_cpi <- as.numeric(p2data$inflation_cpi)
p2data$debt_gdp <- as.numeric(p2data$debt_gdp)
p2data$OB_gdp <- as.numeric(p2data$OB_gdp)
p2data$reserves <- as.numeric(p2data$reserves)
p2data$rqe <- as.numeric(p2data$rqe)
p2data$gee <- as.numeric(p2data$gee)
p2data$tt <- as.numeric(p2data$tt)
p2data$bond_spreads <- as.numeric(p2data$bond_spreads)

# Remove rows with NA in bond_spreads
p2data <- p2data[!is.na(p2data$bond_spreads), ]



#1------------------------------------

# Check for multicollinearity using VIF
model <- lm(bond_spreads ~ vulnerability100 + readiness100 + growth +
              inflation_cpi + debt_gdp + OB_gdp + reserves + rqe + gee + tt, data = p2data)
vif(model)


#2-----------------------------------
p2data<-p2data %>% mutate(ln_vul=log(vulnerability100))
p2data<-p2data %>% mutate(ln_readi=log(readiness100))
p2data<-p2data %>% mutate(ln_bond=log(bond_spreads))
p2data<-p2data %>% mutate(ln_growth=log(growth))
p2data<-p2data %>% mutate(ln_cpi=log(inflation_cpi))
p2data<-p2data %>% mutate(ln_debt=log(debt_gdp))
p2data<-p2data %>% mutate(ln_OB=log(OB_gdp))
p2data<-p2data %>% mutate(ln_reserve=log(reserves))
p2data<-p2data %>% mutate(ln_rqe=log(rqe))
p2data<-p2data %>% mutate(ln_gee=log(gee))
p2data<-p2data %>% mutate(ln_tt=log(tt))

# Re-run the plm model
library(plm)
advctyplm <- plm(bond_spreads ~ ln_vul + ln_readi + ln_growth +
                   ln_cpi + ln_debt + ln_OB + ln_reserve + ln_rqe + ln_gee + ln_tt,
                 data = p2data, model = "within", index = c("cname", "year"))
summary(advctyplm)

#3---------------------------------
#adding intreacting terms 
advctyplm_interaction <- plm(bond_spreads ~ vulnerability100 * readiness100 + growth +
                               inflation_cpi + debt_gdp + OB_gdp + reserves + tt,
                             data = p2data, model = "within", index = c("cname", "year"))

# Summarize the interaction model
summary(advctyplm_interaction)


#4---------------------------------
#including lagged variable 
numeric_vars <- c("vulnerability100", "readiness100", "growth",
                  "inflation_cpi", "debt_gdp", "OB_gdp", "reserves", "rqe", "gee", "tt", "bond_spreads")

p2data[numeric_vars] <- lapply(p2data[numeric_vars], as.numeric)

# Remove rows with NA in bond_spreads
p2data <- p2data[!is.na(p2data$bond_spreads), ]

# Create lagged variables for one period lag (you can adjust the number of lags as needed)
p2data <- p2data %>%
  group_by(cname) %>%
  mutate(
    lag_vulnerability100 = lag(vulnerability100, 1),
    lag_readiness100 = lag(readiness100, 1),
    lag_growth = lag(growth, 1),
    lag_inflation_cpi = lag(inflation_cpi, 1),
    lag_debt_gdp = lag(debt_gdp, 1),
    lag_OB_gdp = lag(OB_gdp, 1),
    lag_reserves = lag(reserves, 1),
    lag_rqe = lag(rqe, 1),
    lag_gee = lag(gee, 1),
    lag_tt = lag(tt, 1)
  ) %>%
  ungroup()

# Remove rows with NA in any of the lagged variables
p2data <- na.omit(p2data)

# Fit the PLM model with lagged variables
advctyplm_lagged <- plm(bond_spreads ~ lag_vulnerability100 + lag_readiness100 + lag_growth +
                          lag_inflation_cpi + lag_debt_gdp + lag_OB_gdp + lag_reserves + lag_tt,
                        data = p2data, model = "within", index = c("cname", "year"))

# Summarize the PLM model with lagged variables
summary(advctyplm_lagged)


#____________________________________



# Re-run the plm model
library(plm)
advctyplm <- plm(bond_spreads ~ vulnerability100 + readiness100 + growth +
                   inflation_cpi + debt_gdp + OB_gdp + reserves + rqe + gee + tt,
                 data = p2data , model = "within", index = c("cname", "year"))
summary(advctyplm)



is.pbalanced(new_data)
pdata<-pdata.frame(new_data,index=c("cname","year"))
sum(is.na(p2data$bond_spreads))

# Impute NA values in the dependent variable 'bond_spreads' with the mean
pdata$bond_spreads[is.na(pdata$bond_spreads)] <- mean(pdata$bond_spreads, na.rm = TRUE)

# Run the tvPLM model
library(plm)
tvadvcoun <- tvPLM(bond_spreads ~ vulnerability100 + readiness100 + growth +
                     inflation_cpi + debt_gdp + OB_gdp + reserves + rqe + gee + tt,
                   data = p2data, index = c("cname", "year"))

summary(tvadvcoun)



#------------------------------------
#plots
install.packages("corrplot")
library(corrplot)
# Sample data



install.packages("ggplot2")
library(ggplot2)

# Sample data (replace this with your actual data)
df <- data.frame(
  bond_spreads = rnorm(100, mean = 2, sd = 0.5),
  vulnerability100 = rnorm(100, mean = 50, sd = 10),
  readiness100 = rnorm(100, mean = 50, sd = 10),
  growth = rnorm(100, mean = 3, sd = 1),
  inflation_cpi = rnorm(100, mean = 2, sd = 0.5),
  debt_gdp = rnorm(100, mean = 60, sd = 20),
  OB_gdp = rnorm(100, mean = 3, sd = 1),
  reserves = rnorm(100, mean = 100, sd = 50),
  rqe = rnorm(100, mean = 5, sd = 2),
  gee = rnorm(100, mean = 4, sd = 1.5),
  tt = rnorm(100, mean = 2, sd = 0.5)
)

# List of variables to plot against bond_spreads
variables <- c("vulnerability100", "readiness100", "growth", "inflation_cpi", 
               "debt_gdp", "OB_gdp", "reserves", "rqe", "gee", "tt")

# Create scatter plots
plots <- lapply(variables, function(var) {
  ggplot(df, aes_string(x = var, y = "bond_spreads")) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = paste("Scatter Plot of Bond Spreads vs", var),
         x = var,
         y = "Bond Spreads") +
    theme_minimal()
})

# Display the plots
for (plot in plots) {
  print(plot)
}

install.packages("ggplot2")
install.packages("reshape2")


library(ggplot2)
library(reshape2)

# Load the data
data <- readxl::read_excel("D:\\6th Sem Summer Surge IITK24\\R_Data_and_codes\\Dev and adv data.xlsx")

view(data)

# Calculate the correlations for each country
correlations <- data %>%
  group_by(cname) %>%
  summarise(
    corr_vulnerability = cor(bond_spreads, vulnerability, use = "complete.obs"),
    corr_readiness = cor(bond_spreads, readiness, use = "complete.obs"),
    corr_growth = cor(bond_spreads, growth, use = "complete.obs")
  )

# Melt the data for ggplot2
correlations_melted <- melt(correlations, id.vars = "cname")

# Define a vector for market classification
advanced_markets <- c("Australia", "Austria", "Belgium", "Canada", "Chile","Denmark","Finland", "France", "Germany", "Hong Kong","Hungary","Iceland",
                      "Ireland", "Israel", "Italy", "Japan", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal",
                      "Singapore","Spain", "Sweden", "Switzerland", "United Kingdom", "United States")

# Add market classification
correlations_melted$market <- ifelse(correlations_melted$cname %in% advanced_markets, "Advanced Countries", "Developing Countries")

# Plotting the graph
ggplot(correlations_melted, aes(x = cname, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "orange", "yellow"),
                    labels = c("corr(Bond Spreads, Vulnerability)", "corr(Bond Spreads, Readiness)", "corr(Bond Spreads, Growth)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  facet_wrap(~ market, scales = "free_x") +
  geom_vline(xintercept = 22.6, linetype = "solid", color = "black", size = 0.5)





# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)

# Load the data


# Calculate the correlations for each country
correlations <- data %>%
  group_by(cname) %>%
  summarise(
    corr_vulnerability = cor(bond_spreads, vulnerability, use = "complete.obs"),
    corr_readiness = cor(bond_spreads, readiness, use = "complete.obs"),
    corr_growth = cor(bond_spreads, growth, use = "complete.obs")
  )

# Melt the data for ggplot2
correlations_melted <- melt(correlations, id.vars = "cname")

# Define a vector for market classification
advanced_markets <- c("Australia", "Austria", "Belgium", "Canada", "Chile","Denmark","Finland", "France", "Germany", "Hong Kong","Hungary","Iceland",
                      "Ireland", "Israel", "Italy", "Japan", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal",
                      "Singapore","Spain", "Sweden", "Switzerland", "United Kingdom", "United States")

# Add market classification
correlations_melted$market <- ifelse(correlations_melted$cname %in% advanced_markets, "Advanced Markets", "Developing Markets")

# Plotting the graph
ggplot(correlations_melted, aes(x = cname, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "orange", "yellow"),
                    labels = c("corr(Bond Spreads, Vulnerability)", "corr(Bond Spreads, Readiness)", "corr(Bond Spreads, Growth)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  labs(x = NULL, y = NULL, fill = NULL) +
  facet_wrap(~ market, scales = "free_x") +
  geom_vline(xintercept = 22.6, linetype = "solid", color = "black", size = 0.5)











#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
#model for devloping country 

library(plm)
devctyplm <- plm(bond_spreads ~ vulnerability + readiness + growth +
                   inflation_cpi + debt_gdp + ob_gdp + reserves  + tt,
                 data = data2_dev , model = "within", index = c("cname", "year"))
summary(devctyplm)
`


