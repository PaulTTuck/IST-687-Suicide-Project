# Libraries (if needed)

library(sqldf)
library(ggplot2)
library(kernlab)
library(e1071)
library(gridExtra)
library(Metrics)

# Subset data from 2000-2010

SuicideRate10 <- subset(SuicideRateSel, SuicideRateSel$Year >= 2000)
SuicideRate10 <- subset(SuicideRate10, SuicideRate10$Year <= 2010)

# Group data by Country and Year(total suicides)

Sui10 <- data.frame(sqldf("SELECT Country, Year, SUM(Suicides_no), SUM(Suicides_per_100k), gdp_for_Year, gdp_per_Capita 
               FROM SuicideRate10 GROUP BY Country, Year"))

colnames(Sui10) <- c("Country", "Year", "Suicides_no", "Suicides_per_100k", "gdp_for_Year", "gdp_per_Capita")

# Recreate descriptive statistics for 10 years

DescStats10 <- data.frame(sqldf("SELECT Country, AVG(Suicides_no), STDEV(Suicides_no), AVG(Suicides_per_100k), STDEV(Suicides_per_100k), 
                                AVG(gdp_for_Year), STDEV(gdp_for_Year), AVG(gdp_per_Capita), STDEV(gdp_per_Capita)
                                FROM Sui10 GROUP BY Country"))

colnames(DescStats10) <- colnames(DescriptiveStatistics)

# Plot a histogram for SuiNo

HistSuiNo <- ggplot(DescStats10, aes(x = Country, y = Mean_no_Suicides)) + geom_col() + ggtitle("Average Suicides per Country") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
HistSuiNo

HistSui100 <- HistSuiNo <- ggplot(DescStats10, aes(x = Country, y = Mean_Suicides_per100k)) + geom_col() + ggtitle("Average Suicides/100k per Country") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
HistSui100

# Create lm's for Suicides/100k vs. total gdp and gdp/cap

TenSuinogdp <- lm(formula = Mean_no_Suicides ~ Mean_Yearly_gdp, data = DescStats10)
TenSui100gdp <- lm(formula = Mean_Suicides_per100k ~ Mean_Yearly_gdp, data = DescStats10)
TenSuinogdpcap <- lm(formula = Mean_no_Suicides ~ Mean_gdp_perCap, data = DescStats10)
TenSui100gdpcap <- lm(formula = Mean_Suicides_per100k ~ Mean_gdp_perCap, data = DescStats10)

#Plot lms

TenSui100gdpPlot <- ggplot(DescStats10, aes(x = Mean_Yearly_gdp, y = Mean_Suicides_per100k)) + geom_point() +
                   stat_smooth(method = "lm", col = "blue") + ggtitle("Effects of gdp on Suicides/100k 2000-2010")

TenSui100gdpCapPlot <- ggplot(DescStats10, aes(x = Mean_gdp_perCap, y = Mean_Suicides_per100k)) + geom_point() +
  stat_smooth(method = "lm", col = "blue") + ggtitle("Effects of gdp per Capita on Suicides/100k 2000-2010")

# Create other linear models.

# Group data by age?

SuiAge <- data.frame(sqldf("SELECT Country, Year, Age, SUM(Suicides_no), SUM(Suicides_per_100k) 
          FROM SuicideRate10 GROUP BY Age, Country, Year"))

colnames(SuiAge) <- c("Country", "Year", "Age", "Suicides_No", "Suicides_per_100k")

# Descriptive statistics by age

MeanSuiNoAge <- tapply(SuiAge$Suicides_No, SuiAge$Age, mean)
MeanSui100Age <- tapply(SuiAge$Suicides_per_100k, SuiAge$Age, mean)
SuiNoAgesd <- tapply(SuiAge$Suicides_No, SuiAge$Age, sd)
Sui100Agesd <- tapply(SuiAge$Suicides_per_100k, SuiAge$Age, sd)

SuicideAgeDesc <- data.frame(MeanSuiNoAge, SuiNoAgesd, MeanSui100Age, Sui100Agesd)
colnames(SuicideAgeDesc) <- c("Mean_Suicides", "Suicides_sd", "Mean_Suiper100k", "Suiper100k_sd")

SuiAgeCountry <- data.frame(sqldf("SELECT Country, Age, AVG(Suicides_no), STDEV(Suicides_no), AVG(Suicides_per_100k), 
               STDEV(Suicides_per_100k) FROM SuiAge GROUP BY Country, Age"))

colnames(SuiAgeCountry) <- c("Country", "Age", "Mean_Suicides", "Suicides_sd", "Mean_Suicides_per100k", "Suicides_per100k_sd")

# Which age groups are most at risk?
# Step 1: generate a distribution of sample means for world

SuicideNoSample <- replicate(10000, mean(sample(SuicideRate10$Suicides_no, size = 1000, replace = TRUE)), simplify = TRUE)
Suicide100Sample <- replicate(10000, mean(sample(SuicideRate10$Suicides_per_100k, size = 1000, replace = TRUE)), simplify = TRUE)

# Step 2: determine risk

SuiAgeCountry$Suicide_no_risk <- as.factor(ifelse(SuiAgeCountry$Mean_Suicides >= quantile(SuicideNoSample, probs = 0.95), 1, 0))
SuiAgeCountry$Suicide_100_risk <- as.factor(ifelse(SuiAgeCountry$Mean_Suicides_per100k >= quantile(Suicide100Sample, probs = 0.95), 1, 0))
SuiAgeCountry$MaxRisk <- as.factor(ifelse(SuiAgeCountry$Suicide_no_risk == SuiAgeCountry$Suicide_100_risk & SuiAgeCountry$Suicide_no_risk == 1, 1, 0))

MaxSuiRisk <- SuiAgeCountry[which(SuiAgeCountry$MaxRisk == 1),]

# Step 3: Plot results

AgeRiskPlot <- ggplot(data = SuiAgeCountry, aes(x = Mean_Suicides, y = Mean_Suicides_per100k))
AgeRiskPlot <- AgeRiskPlot + geom_point(aes(color = Suicide_no_risk, shape = Suicide_100_risk, size = MaxRisk)) + ggtitle("Risk Measures of Age Groups")
AgeRiskPlot

MaxRiskPlot <- ggplot(data = MaxSuiRisk, aes(x = Mean_Suicides, y = Mean_Suicides_per100k))
MaxRiskPlot <- MaxRiskPlot + geom_point(aes(shape = Age), size = 2.5) + ggtitle("Highest risk age groups")
MaxRiskPlot

# Which age groups most at risk?

tapply(MaxSuiRisk$MaxRisk, MaxSuiRisk$Age, length)
Sui100Risk <- SuiAgeCountry[which(SuiAgeCountry$Suicide_100_risk == 1),]
tapply(Sui100Risk$Suicide_100_risk, Sui100Risk$Age, length)

#Which age group is most at risk during economic change?
# Step 1: Determine economic change.

YeargdpAvg <- tapply(Sui10$gdp_for_Year, Sui10$Country, mean)
gdpCapAvg <- tapply(Sui10$gdp_per_Capita, Sui10$Country, mean)

SuiAgegdp <- data.frame(sqldf("SELECT Country, Year, Age, SUM(Suicides_no), SUM(Suicides_per_100k), 
          gdp_for_Year, gdp_per_Capita FROM SuicideRate10 GROUP BY Age, Country, Year"))

colnames(SuiAgegdp) <- c("Country", "Year", "Age", "Suicides_no", "Suicides_per_100k", "gdp_for_Year", "gdp_per_Capita")

SuiAgegdp$AVGgdp <- YeargdpAvg[SuiAgegdp$Country]
SuiAgegdp$AVGgdpCap <- gdpCapAvg[SuiAgegdp$Country]

SuiAgegdp$YeargdpChange <- SuiAgegdp$gdp_for_Year - SuiAgegdp$AVGgdp
SuiAgegdp$gdpCapChange <- SuiAgegdp$gdp_per_Capita - SuiAgegdp$AVGgdpCap

# Step 2: Create a linear model between yearly suicides and gdp.

lm15to24Nogdp <- lm(Suicides_no ~ YeargdpChange, data = SuiAgegdp, subset = Age == "15-24 years")
lm25to34Nogdp <- lm(Suicides_no ~ YeargdpChange, data = SuiAgegdp, subset = Age == "25-34 years")
lm35to54Nogdp <- lm(Suicides_no ~ YeargdpChange, data = SuiAgegdp, subset = Age == "35-54 years")
lm55to74Nogdp <- lm(Suicides_no ~ YeargdpChange, data = SuiAgegdp, subset = Age == "55-74 years")
lm75Nogdp <- lm(Suicides_no ~ YeargdpChange, data = SuiAgegdp, subset = Age == "75+ years")

lm15to24NogdpCap <- lm(Suicides_no ~ gdpCapChange, data = SuiAgegdp, subset = Age == "15-24 years")
lm25to34NogdpCap <- lm(Suicides_no ~ gdpCapChange, data = SuiAgegdp, subset = Age == "25-34 years")
lm35to54NogdpCap <- lm(Suicides_no ~ gdpCapChange, data = SuiAgegdp, subset = Age == "35-54 years")
lm55to74NogdpCap <- lm(Suicides_no ~ gdpCapChange, data = SuiAgegdp, subset = Age == "55-74 years")
lm75NogdpCap <- lm(Suicides_no ~ gdpCapChange, data = SuiAgegdp, subset = Age == "75+ years")

lm15to24100gdp <- lm(Suicides_per_100k ~ YeargdpChange, data = SuiAgegdp, subset = Age == "15-24 years")
lm25to34100gdp <- lm(Suicides_per_100k ~ YeargdpChange, data = SuiAgegdp, subset = Age == "25-34 years")
lm35to54100gdp <- lm(Suicides_per_100k ~ YeargdpChange, data = SuiAgegdp, subset = Age == "35-54 years")
lm55to74100gdp <- lm(Suicides_per_100k ~ YeargdpChange, data = SuiAgegdp, subset = Age == "55-74 years")
lm75100gdp <- lm(Suicides_per_100k ~ YeargdpChange, data = SuiAgegdp, subset = Age == "75+ years")

lm15to24100gdpCap <- lm(Suicides_per_100k ~ gdpCapChange, data = SuiAgegdp, subset = Age == "15-24 years")
lm25to34100gdpCap <- lm(Suicides_per_100k ~ gdpCapChange, data = SuiAgegdp, subset = Age == "25-34 years")
lm35to54100gdpCap <- lm(Suicides_per_100k ~ gdpCapChange, data = SuiAgegdp, subset = Age == "35-54 years")
lm55to74100gdpCap <- lm(Suicides_per_100k ~ gdpCapChange, data = SuiAgegdp, subset = Age == "55-74 years")
lm75100gdpCap <- lm(Suicides_per_100k ~ gdpCapChange, data = SuiAgegdp, subset = Age == "75+ years")


# Step 3: Plot any results that are significant

Sui15to24 <- subset(SuiAgegdp, Age == "15-24 years")
Sui100kgdpCap15to24plot <- ggplot(data = Sui15to24, aes(x = gdpCapChange, y = Suicides_per_100k)) + geom_point()
Sui100kgdpCap15to24plot <- Sui100kgdpCap15to24plot + stat_smooth(method = "lm", col = "blue") + ggtitle("Effects of changes in gdp per Capita on Suicides/100k: 15-24 year-olds")
Sui100kgdpCap15to24plot

# Turns out, the model for change in gdp per Cap predicting Suicides/100k is significant, lets plot that.

Sui100gdpCapplot <- ggplot(data = SuiAgegdp, aes(x = gdpCapChange, y = Suicides_per_100k)) + geom_point()
Sui100gdpCapplot <- Sui100gdpCapplot + stat_smooth(method = "lm", col = "blue") + ggtitle("Effects of Change in gdp per Capita on Suicides/100k")
Sui100gdpCapplot
  
# Create categorical models for age.
# Step 1: Randomly select data for training and test

randIndex <- sample(1:dim(SuiAge)[1])
cutPoint2_3 <- floor(2 * dim(SuiAge)[1]/3)
trainSuiAge <- SuiAge[randIndex[1:cutPoint2_3],]
testSuiAge <- SuiAge[randIndex[(cutPoint2_3+1):dim(SuiAge)[1]],]
