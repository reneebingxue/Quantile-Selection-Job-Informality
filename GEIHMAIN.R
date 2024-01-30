#File: GEIH_micrometrics.R
#Project: Microeconometrics_Quantile_Selection_IV
#Author: Renee Li, Roberto Mauricio
#Date: 2023-11-27

# Setup -------------------------------------------------------------------
### CLEAN UP MY CONSOLE/ENVIRONMENT
cat("\014")
rm(list = ls()) #clean up the environment
### PACKAGES SET UP
install.packages("sampleSelection")
install.packages("quantreg")
install.packages("np")
set.seed(42) # for consistent results
library(haven) # to read dta data file
library(data.table) # to read csv data file
library(ggplot2) # to render our graphs
library(stargazer) # to format model output to latex code
library(lmtest) # to gather our clustered standard errors - coeftest()
library(plm)  # to gather our clustered standard errors - vcovHC()
library(quantreg) # to run quantile regressions by Koenker 
library(dplyr) # to conveniently report data
library(kableExtra)
library(gridExtra)
library(sampleSelection)
library(xtable)
library(modelsummary)
library(np)

### SET WORKING DIRECTORY
setwd('/Users/renee/Desktop/Micrometrics/GEIH/') # TBM by user

# Load and Clean Data -------------------------------------------------------------------
###IMPORT DATA
df_full <- read_dta('/Users/renee/Desktop/Micrometrics/GEIH/GEIH_2022-001.dta')
df_informal <- read_dta('/Users/renee/Desktop/Micrometrics/GEIH/new_variables_informal.dta')
###MERGE DATA
merged <- merge(df_full,df_informal,by=c("DIRECTORIO","SECUENCIA_P","ORDEN"))
df <-merged
##RENAME SOME VARIABLES
names(df)[names(df)=="P6040"] <- "age"
names(df)[names(df)=="P3271"] <- "gender"
names(df)[names(df)=="PET"] <- "working_age"
names(df)[names(df)=="DSI"] <- "unemployed"
names(df)[names(df)=="DSCY"] <- "unemployed_sr"
names(df)[names(df)=="ocu"] <- "employed"
names(df)[names(df)=="FFT"] <- "out_labor"
names(df)[names(df)=="INGLABO"] <- "monthly_income"
names(df)[names(df)=="P6800"] <- "weekly_whours"
names(df)[names(df)=="CLASE"] <- "urban_rural"
names(df)[names(df)=="DPTO"] <- "province"
names(df)[names(df)=="P6080"] <- "ethnicity"
names(df)[names(df)=="P6070"] <- "marital_status"
names(df)[names(df)=="Discapacidad"] <- "disability"
names(df)[names(df)=="P6426"] <- "seniority"
names(df)[names(df)=="P3042"] <- "education"
names(df)[names(df)=="P6440"] <- "contract"
names(df)[names(df)=="P6450"] <- "verbal_written"
names(df)[names(df)=="P6430"] <- "worker_type"
names(df)[names(df)=="P6920"] <- "pension_fund"
###SELECT VARIABLES NEEDED
variables_to_select <- c("DIRECTORIO","SECUENCIA_P","ORDEN","informal","sector","nolabor_income","members",
                         "children","weight","cities", "age","gender","working_age","unemployed",
                         "employed","out_labor","unemployed_sr",
                         "monthly_income","weekly_whours","urban_rural","province",
                         "ethnicity","marital_status","disability","seniority","education", "contract",
                         "verbal_written","worker_type","pension_fund")
df <- subset(df, select = variables_to_select)
df <- df[!is.na(df$DIRECTORIO), ] #Further excludes those with NAs across the row
###COMPUTE SOME VARIABLES
df$hourly_wage <- df$monthly_income/(4*df$weekly_whours) #hourly wage
df$lwage <- log(df$hourly_wage) #log hourly wage
df$lowage <- log(df$hourly_wage+1) #log hourly wage
###SAMPLE SELECTION
df <- df[df$working_age == 1, ] #of working age
df <- df[df$urban_rural==1,] #urban workers
df <- df[df$citie !=0,] #excludes urban but in the small counties
df <- df[!is.na(df$DIRECTORIO), ] 
 #excludes agriculture workers
###CLEAN SOME VARIABLES
df$out_labor <- ifelse(df$employed == 1, 0, df$out_labor) 
df$out_labor[is.na(df$out_labor)] <- 0 #number of NAs left=number of unemployed
table(df$out_labor, useNA = "always")
table(df$employed, useNA = "always")
table(df$unemployed, useNA = "always")
count <- sum(!is.na(df$monthly_income)) # number 247048 obs. 
count1 <- sum(!is.na(df$weekly_whours)) # number 258239 obs. 
count2 <- sum(!is.na(df$lowage)) # number 247048 obs. 
countemployed <- sum(df$employed == 1, na.rm = TRUE) # number 258239 obs.
count_select1_noy <- sum(df$employed == 1 & is.na(df$monthly_income)) #number of 11191 observations
count_select0_yesy <- sum(df$employed == 0 & !is.na(df$monthly_income)) #number of 0 observations

# Some Stylized Facts About Our Sample -----------------------------------------------------
###Descriptive statistics by formal/informal
groupinformal<- df[df$employed == 1  & df$informal == 1,]
groupformal <- df[df$employed == 1  & df$informal == 0,]
###PLOT THE WAGE DISTRIBUTIONS
# Excluding outliers (e.g., values above 100) before creating the histogram
histogram_plot <- ggplot() +
  geom_histogram(data = groupinformal, aes(x = lwage), bins = 50, fill = "blue", alpha = 0.5) +
  geom_histogram(data = groupformal, aes(x = lwage), bins = 50, fill = "red", alpha = 0.5) +
  labs(x = "Log(Hourly Wage+1)", y = "Frequency") +
  theme_minimal()
density_plot <- ggplot() +
  geom_density(data = groupinformal, aes(x = lwage), fill = "blue", alpha = 0.5) +
  geom_density(data = groupformal, aes(x = lwage), fill = "red", alpha = 0.5) +
  labs(x = "Log(Hourly Wage+1)", y = "Density") +
  theme_minimal()+
  xlim(0, 15)
histogram_plot <- histogram_plot + ggtitle("Histogram of Wage Distributions")
density_plot <- density_plot + ggtitle("Density Plot of Wage Distributions")
# Arrange plots in a panel using grid.arrange
grid.arrange(histogram_plot, density_plot, ncol = 2)

### Wage Differentials by Quantile
### PLOT WAGE DIFFERENTIALS ACROSS QUANTILES
par(mfrow = c(1, 1))  # Set up a 1x2 plot layout
quantiles_informal <- quantile(groupinformal$lwage[!is.na(groupinformal$lwage)], probs = seq(0, 1, by = 0.05))
quantiles_formal <- quantile(groupformal$lwage[!is.na(groupformal$lwage)], probs = seq(0, 1, by = 0.05))
wage_differentials <- quantiles_formal - quantiles_informal
plot(seq(0, 1, by = 0.05), wage_differentials, type = "l", xlab = "Quantiles", ylab = "Wage Differentials", 
     main = "Wage Differentials By Informality Across Quantiles")

# Linear Regression Analysis -----------------------------------------------------
df_june <- read_dta('/Users/renee/Desktop/Micrometrics/GEIH/data_june.dta')
df_expanded <- read_dta('/Users/renee/Desktop/Micrometrics/GEIH/GEIH_2022_S14_expanded.dta')
df_june$sector <- factor(df_june$sector)
df_june$domin <- factor(df_june$domin)
df_june$head_hh <- factor(df_june$head_hh)
# Subset the data where sample2 == 1
sub_df <- df_june[df_june$sample2 == 1, ]
lmbase0 = lm (lhlabincome ~ informal,  data = sub_df, weights = sub_df$weight)
lmbase1 = lm (lhlabincome ~ informal + age + gender + educ_years + educ_years2_ + marital_status +head_hh ,  data = sub_df, weights = sub_df$weight)
lmbase2 = lm (lhlabincome ~ informal + age + gender + educ_years + educ_years2_ + marital_status +head_hh + seniority + sector + domin,  data = sub_df, weights = sub_df$weight)
lmbase3 = lm (lhlabincome ~ informal + inw + nwew + age + gender + educ_years + educ_years2_ + marital_status +head_hh + seniority + sector + domin,  data = sub_df, weights = sub_df$weight)
stargazer(lmbase0, lmbase1, lmbase2, title="Results", align=TRUE)

# Heckman selection model
sub_df$supdated <- ifelse(sub_df$selection == 1 & !is.na(sub_df$lhlabincome), 1, 0)
selection_eq <- supdated ~ gender + educ_years + age + couple + head_hh + children + d_nolabor_income + inter_nli + domin
selection_eq <- selection ~ gender + educ_years + age + couple + head_hh + children + d_nolabor_income + inter_nli + domin
# Define the outcome equation (OLS regression)
outcome_eq1 <- lhlabincome ~ informal + gender + educ_years + educ_years2_ + age + couple + head_hh + seniority + sector + domin
outcome_eq2 <- lhlabincome ~ informal + inw + nwew + gender + educ_years + educ_years2_ + age + couple + head_hh + seniority + sector + domin
# Fit the Heckman selection model using twostep estimation
heckman_model1 <- heckit(selection = selection_eq, outcome = outcome_eq1, data = sub_df)
heckman_model2 <- heckit(selection = selection_eq, outcome = outcome_eq2, data = sub_df)
stargazer(lmbase2,heckman_model1,heckman_model2)

# check if the first stage selection valid by normality assumption 
selecprob = glm(supdated ~ gender + educ_years + age + couple + head_hh + children + d_nolabor_income + inter_nli + domin,
             data   = sub_df,
             family = binomial(link = 'probit'))
summary(selecprob)
res <- resid(selecprob)
plot(density(res), main = "Density Plot", xlab = "Values")
# Create a Q-Q plot
qqnorm(res, col = "black", main = "Heckman Probit Selection Residual Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(res, col = "black", lwd = 2, lty = 2)

# Define your nonparametric selection equation using np package
nonparametric_selection_eq <- npregbw(selection ~ gender + educ_years + age + couple + head_hh + children + d_nolabor_income + inter_nli + domin, data = sub_df)
# Fit the Heckman model using the nonparametric selection equation
heckman_model_nonparametric <- heckit(selection = nonparametric_selection_eq, outcome = outcome_eq1, data = sub_df)

# check if the selection probit valid by normality assumption 
subset_df <- subset(df_june, employed == 1 & sample2 == 1)
subset_df$missingd <- ifelse(is.na(subset_df$lhlabincome), 1, 0)
probmissing <- glm(missingd ~ informal + gender + age + educ_years + couple + head_hh  + seniority + sector + domin, data = subset_df, family = binomial(link = "probit"))
summary_table <- summary(probmissing )
stargazer(probmissing )
latex_table <- xtable(summary_table)
print(latex_table, type = "latex")  

# Quantile Regression Analysis -----------------------------------------------------
#Baseline Quantile Regression
### CONSTRUCT ESTIMATION FORMULA OF INTEREST
groupinformal<- df_june[df_june$employed == 1  & df_june$informal == 1,]
groupformal <- df_june[df_june$employed == 1  & df_june$informal == 0,]
groupinformal1<- df_june[df_june$employed == 1  & df_june$informal == 1 & df_june$worker_type == 1,]
groupformal1 <- df_june[df_june$employed == 1  & df_june$informal == 0 & df_june$worker_type == 1,]
groupinformal2<- df_june[df_june$employed == 1  & df_june$informal == 1 & df_june$worker_type != 1,]
groupformal2 <- df_june[df_june$employed == 1  & df_june$informal == 0 & df_june$worker_type != 1,]
par(mfrow = c(1, 1))  # Set up a 1x2 plot layout
quantiles_informal <- quantile(groupinformal$lhlabincome[!is.na(groupinformal$lhlabincome)], probs = seq(from = 0.05, to = 0.95, by = 0.1))
quantiles_formal <- quantile(groupformal$lhlabincome[!is.na(groupformal$lhlabincome)], probs = seq(from = 0.05, to = 0.95, by = 0.1))
wage_differentials <- quantiles_informal - quantiles_formal
quantiles_informal1 <- quantile(groupinformal1$lhlabincome[!is.na(groupinformal1$lhlabincome)], probs = seq(from = 0.05, to = 0.95, by = 0.1))
quantiles_formal1 <- quantile(groupformal1$lhlabincome[!is.na(groupformal1$lhlabincome)], probs = seq(from = 0.05, to = 0.95, by = 0.1))
wage_differentials1 <- quantiles_informal1 - quantiles_formal1
quantiles_informal2 <- quantile(groupinformal2$lhlabincome[!is.na(groupinformal2$lhlabincome)], probs = seq(from = 0.05, to = 0.95, by = 0.1))
quantiles_formal2 <- quantile(groupformal2$lhlabincome[!is.na(groupformal2$lhlabincome)], probs = seq(from = 0.05, to = 0.95, by = 0.1))
wage_differentials2 <- quantiles_informal2 - quantiles_formal2
y_min <- min(c(wage_differentials, wage_differentials1,wage_differentials2))
y_max <- max(c(wage_differentials, wage_differentials1,wage_differentials2))
plot(seq(from = 0.05, to = 0.95, by = 0.1), wage_differentials, type = "l",lwd = 2, xlab = "Quantiles", ylab = "Wage Differentials", 
     main = "Wage Differentials By Informality Across Quantiles", col = "black", ylim = c(y_min, y_max))
lines(seq(from = 0.05, to = 0.95, by = 0.1), wage_differentials1, type = "l", col = "red",lwd = 2) # Add the second line
lines(seq(from = 0.05, to = 0.95, by = 0.1), wage_differentials2, type = "l", col = "blue", lwd = 2) # Add the third line
legend("bottomright", legend = c("All Types of Workers", "Private Sector Employees", "All Other Type Workers"), 
       col = c("black", "red", "blue"), lty = 1) 

#Baseline Quantile Regression
time_0 <- Sys.time()
df_expanded$sector <- factor(df_expanded$sector)
df_expanded$domin <- factor(df_expanded$domin)
df_expanded$head_hh <- factor(df_expanded$head_hh)
time_0 <- Sys.time()
reg_exp1 <- rq(lhlabincome ~ informal + age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = seq(0.05,0.95,by = 0.05), data = df_expanded, method = 'fn')
reg_exp2 <- rq(lhlabincome ~ informal + inw + nwew + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = seq(0.05,0.95,by = 0.05), data = df_expanded, method = 'fn')
time_1 <- Sys.time()
paste0("Computing the quantiles took ",time_1-time_0)
sum_reg1 <- summary.rqs(reg_exp1, method = 'boot')
sum_reg2 <- summary.rqs(reg_exp2, method = 'boot')
time_2 <- Sys.time()
paste0("Computing the errors took ",time_2-time_1)
plot(sum_reg1, parm = c(1,2,3,4,5,6,7,9))
plot(sum_reg2, parm = c(1,2,3,4,5,6,7,8,10))
# Combine the tables horizontally
combined_table <- cbind(sum_reg1, sum_reg2)
table_combined <- xtable(combined_table)
print(table_combined, file = "combined_table.tex") 

p10 <- rq(lhlabincome ~ informal + age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = c(0.1), data = df_expanded, method = 'fn')
p25 <- rq(lhlabincome ~ informal + age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = c(0.25), data = df_expanded, method = 'fn')
p50 <- rq(lhlabincome ~ informal + age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = c(0.5), data = df_expanded, method = 'fn')
p75 <- rq(lhlabincome ~ informal + age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = c(0.75), data = df_expanded, method = 'fn')
p90 <- rq(lhlabincome ~ informal + age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = c(0.9), data = df_expanded, method = 'fn')
stargazer(lmbase2,p10,p25,p50,p75,p90)
p10 <- rq(lhlabincome ~ informal + inw + nwew +age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = c(0.1), data = df_expanded, method = 'fn')
p25 <- rq(lhlabincome ~ informal + inw + nwew +age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = c(0.25), data = df_expanded, method = 'fn')
p50 <- rq(lhlabincome ~ informal + inw + nwew +age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = c(0.5), data = df_expanded, method = 'fn')
p75 <- rq(lhlabincome ~ informal + inw + nwew +age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = c(0.75), data = df_expanded, method = 'fn')
p90 <- rq(lhlabincome ~ informal + inw + nwew +age + gender + educ_years + educ_years2_ + couple +head_hh + seniority + sector + domin,tau = c(0.9), data = df_expanded, method = 'fn')
stargazer(lmbase3,p10,p25,p50,p75,p90)



