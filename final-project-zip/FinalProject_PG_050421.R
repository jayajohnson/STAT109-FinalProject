#SECTION 4: WHICH INDUSTRIES EXHIBITED THE HIGHEST RATE OF UNEMPLOYMENT DURING THE PANDEMIC THUS FAR AND
# DOES THEIR ABILITY TO FUNCTION LARGELY VIRTUALLY EFFECT THE RATE OF UNEMPLOYMENT?

#Libraries
library(dplyr)
library(tfplot)
library(tframe)
library(ggplot2)
library(Hmisc)
library(funModeling)
library(car)
library(knitr)
library(fastDummies)

#Upload the employment data per industry
employmentData <- read.csv("EmploymentDataPandemic.csv", header = TRUE)
allemployment_df <- data.frame(employmentData)

#Subsetting only the columns with the industries we want to look at
employment_df <- allemployment_df %>% select(1, 4:17)
employment_df$Date <- as.Date(employment_df$Date)

#Set up data frame for unemployment rates
date <- employment_df$Date
calc_unemploymentrates <- subset(employment_df, select = - Date)
unemploymentrates <- data.frame(date)

#Unemployment rate = ((-(Previous Month Employment #s - Current Month Employment #s)/Previous Month Employment #s)*100)
#Calculate all unemployment rates per industry per month
calc_UERate <- function(x) {
  c(NA, round(sapply(2:length(x), function(y) {
    (-(x[y] - x[y-1]) / x[y-1])*100
  }), 3))
}

#Store all unemployment rates in a data frame
unemploymentrates <- cbind(unemploymentrates, lapply(setNames(calc_unemploymentrates, paste0(colnames(calc_unemploymentrates))), calc_UERate))
summary(unemploymentrates)

#Remove the NA values for 2-2-20 as they are all N/A
unemploymentrates <- unemploymentrates %>% slice(-1)

#Histograms of each industry's unemployment rate per month looked at
# FIGURE 4.1
hist_cols<-unemploymentrates[,2:15]
plot_num(hist_cols)

#Create a data frame that looks at overall averages of unemployment thus far per industry
UERates_AVG <- subset(unemploymentrates, select = -date)
colMeans(UERates_AVG)

Industries <- colnames(UERates_AVG)
averages <- colMeans(UERates_AVG)

IndustryAvgs <- data.frame(Industries, averages)

#Create a bar chart to compare the averages per industry
# FIGURE 4.2
par(mar=c(15,5,5,5))

bp <- barplot(IndustryAvgs$averages, 
              ylab = "Average Unemployment Rate",
              main = "Average Unemployment Rate per Industry during the COVID 19 Pandemic",
              las = 2, 
              col = "red")

axis(side=1, pos=0, at=bp, labels=IndustryAvgs$Industries, las = 2)

#Measure the significance of industry type and its' ability to operate virtually
#Create separate data frames assigning the virtual and industry variables for each industry

#Mining and Logging
a <- data.frame(industry = "Mining and logging",virtual = "no")
n <- 13
miningandlogging <- do.call("rbind", replicate(n, a, simplify = FALSE))
unemploymentrate <- unemploymentrates$Mining.and.logging
miningandlogging <- cbind(miningandlogging, unemploymentrate)

#Construction
a <- data.frame(industry = "Construction",virtual = "no")
construction <- do.call("rbind", replicate(n, a, simplify = FALSE))
unemploymentrate <- unemploymentrates$Construction
construction <- cbind(construction, unemploymentrate)

#Wholesale trade
a <- data.frame(industry = "Wholesale trade",virtual = "no")
wholesaletrade <- do.call("rbind", replicate(n, a, simplify = FALSE))
unemploymentrate <- unemploymentrates$Wholesale.trade
wholesaletrade <- cbind(wholesaletrade, unemploymentrate)

#Retail tade
a <- data.frame(industry = "Retail trade",virtual = "yes")
retailtrade <- do.call("rbind", replicate(n, a, simplify = FALSE))
unemploymentrate <- unemploymentrates$Retail.trade
retailtrade <- cbind(retailtrade, unemploymentrate)

#Transportation and warehouse
a <- data.frame(industry = "Transportation and warehouse",virtual = "no")
transportation <- do.call("rbind", replicate(n, a, simplify = FALSE))
unemploymentrate <- unemploymentrates$Transportation.and.warehousing
transportation <- cbind(transportation, unemploymentrate)

#Utilities
a <- data.frame(industry = "Utilities",virtual = "no")
utilities2 <- do.call("rbind", replicate(n, a, simplify = FALSE))
unemploymentrate <- unemploymentrates$Utilities
utilities2 <- cbind(utilities2, unemploymentrate)

#Information
a <- data.frame(industry = "Information",virtual = "yes")
information <- do.call("rbind", replicate(n, a, simplify = FALSE))
unemploymentrate <- unemploymentrates$Information
information <- cbind(information, unemploymentrate)

#Financial activities
a <- data.frame(industry = "Financial activities",virtual = "yes")
finance <- do.call("rbind", replicate(n, a, simplify = FALSE))
unemploymentrate <- unemploymentrates$Financial.activities
finance <- cbind(finance, unemploymentrate)

#Education and health services
a <- data.frame(industry = "Education and health services",virtual = "no")
education <- do.call("rbind", replicate(n, a, simplify = FALSE))
unemploymentrate <- unemploymentrates$Education.and.health.services
education <- cbind(education, unemploymentrate)

#Leisure and hospitality
a <- data.frame(industry = "Leisure and hospitality",virtual = "no")
leisure <- do.call("rbind", replicate(n, a, simplify = FALSE))
unemploymentrate <- unemploymentrates$Leisure.and.hospitality
leisure <- cbind(leisure, unemploymentrate)

#Consolidate the data
allunemploymentdata <- rbind(miningandlogging, construction, wholesaletrade, retailtrade, transportation, utilities2, information, 
                             finance, education, leisure)

u <- allunemploymentdata$unemploymentrate
i <- allunemploymentdata$industry
v <- allunemploymentdata$virtual

#Check histogram: histogram is right skewed
# FIGURE 4.3
hist(u)

#Plot shows almost a straight line across
# FIGURE 4.4
plot(u)

#Use log for unemployment rate to hopefully correct for skewness of the data
#Produces errors and results in NA values
m <- lm (log(u) ~ i)
m1 <- lm (log(u) ~ v)
m3 <- lm(log(u) ~ i + v)

#Create generalized regression model due to skewness of the data
#No variables indicated significance
#P value is almost 1 for both models
glm <- glm(u ~ i)
summary(glm)

glm2 <- glm(u ~ v)
summary(glm2)

#Create dummy variables for the categorical variables: industries and virtual 
df5 <- data.frame(u, i, v)
results <- fastDummies::dummy_cols(df5)
results <- data.frame(results)

#Create a multinomial regression model
#No significant variables
industries_m <- lm(u ~ results$i_Construction + results$i_Education.and.health.services + results$i_Financial.activities + results$i_Information
                   + results$i_Leisure.and.hospitality + results$i_Mining.and.logging + results$i_Retail.trade + results$i_Transportation.and.warehouse
                   + results$i_Utilities + results$i_Wholesale.trade)
summary(industries_m)

#glm analysis also does not produce significant variables
industries_m2 <- glm(u ~ results$i_Construction + results$i_Education.and.health.services + results$i_Financial.activities + results$i_Information
                     + results$i_Leisure.and.hospitality + results$i_Mining.and.logging + results$i_Retail.trade + results$i_Transportation.and.warehouse
                     + results$i_Utilities + results$i_Wholesale.trade)
summary(industries_m2)

#Create a multinomial regression model to test virtual
#Virtual variables are not significantnis
virtual_m <- lm(u ~ results$v_no + results$v_yes)
summary(virtual_m)















