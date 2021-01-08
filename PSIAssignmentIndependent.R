library('plyr')
install.packages('dplyr')
library('dplyr')
install.packages('tidyverse')
library('tidyverse')
install.packages('tidyr')
library(tidyr)
install.packages("rvest")
library('rvest')
library(ggplot2)
library(lubridate)
library(ggcorrplot)
install.packages("psych")
library(psych)
install.packages("lm.beta")
library(lm.beta)
install.packages("stargazer")
library(stargazer)
library(readr)
library(readxl)
library(reshape2)
install.packages("moments")
library(moments)
install.packages("corrplot")
library(corrplot)
install.packages("factoextra")
library(factoextra)
library(car)

engStudentsDataset <- data.frame(read_excel("data_academic_performance.xlsx"))

View(engStudentsDataset)
names(engStudentsDataset)

dim(engStudentsDataset) # 45 vars

# ...10 column all missing
colSums(is.na(engStudentsDataset))[colSums(is.na(engStudentsDataset)) > 1] 
# Remove ...10 column
engStudentsDataset <- engStudentsDataset[ , -which(colnames(engStudentsDataset) == "...10")]

dim(engStudentsDataset) # 44 vars

# Get num and chr columns
str(engStudentsDataset)
numericColumns <- c("MAT_S11", "CR_S11", "CC_S11", "BIO_S11", "ENG_S11", "QR_PRO", 
                    "CR_PRO", "CC_PRO", "ENG_PRO", "WC_PRO", "FEP_PRO", "G_SC", 
                    "PERCENTILE", "X2ND_DECILE", "QUARTILE", "SEL", "SEL_IHE")

# Convert to factor???
categoricalColumns <- c("COD_S11", "GENDER", "EDU_FATHER", "EDU_MOTHER", "OCC_FATHER", 
                        "OCC_MOTHER", "STRATUM", "SISBEN", "PEOPLE_HOUSE", "INTERNET", 
                        "TV", "COMPUTER", "WASHING_MCH", "MIC_OVEN", "CAR", "DVD", "FRESH", 
                        "PHONE", "MOBILE", "REVENUE", "JOB", "SCHOOL_NAME", "SCHOOL_NAT", 
                        "SCHOOL_TYPE", "Cod_SPro", "UNIVERSITY", "ACADEMIC_PROGRAM")

########## NUMERIC VARIABLES EXPLORATION ############
engStudentsNumeric <- engStudentsDataset[, numericColumns] 

# HISTOGRAMS
length(numericColumns)

ggplot(data = melt(engStudentsNumeric[1:6]), aes(x = value)) +
  geom_histogram(color="black", fill="white", bins = 15) +
  theme(text = element_text(size = 15)) +
  facet_wrap(~variable, scales = "free")

ggplot(data = melt(engStudentsNumeric[7:12]), aes(x = value)) +
  geom_histogram(color="black", fill="white", bins = 15) +
  theme(text = element_text(size = 15)) +
  facet_wrap(~variable, scales = "free")

ggplot(data = melt(engStudentsNumeric[13:17]), aes(x = value)) +
  geom_histogram(color="black", fill="white", bins = 15) +
  theme(text = element_text(size = 15)) +
  facet_wrap(~variable, scales = "free")

data.frame(summary(engStudentsNumeric))

# Numeric Columns Sumarry
numericColsSummary <- round(data.frame(lapply(engStudentsNumeric, function(x) {
  c(min = min(x), 
    quantile(x, 0.25),
    mean = mean(x), 
    quantile(x, 0.75),
    max = max(x),
    range = max(x) - min(x),
    var = var(x),
    stddev = sd(x)
    )
})), 2)

write.csv(t(numericColsSummary), "numericColsSummary.csv", row.names = TRUE)

# BOX PLOTS
ggplot(data = melt(engStudentsNumeric[1:6]), aes(x = value)) +
  geom_boxplot() +
  theme(text = element_text(size = 15)) +
  facet_wrap(~variable, scales = "free")

boxplot(engStudentsNumeric[1:5])[c("stats", "names")]
boxplot(engStudentsNumeric[6:10])[c("stats", "names")]
boxplot(engStudentsNumeric[11], xlab = names(engStudentsNumeric[11]))[c("stats", "names")]
boxplot(engStudentsNumeric[12], xlab = names(engStudentsNumeric[12]))[c("stats", "names")]
boxplot(engStudentsNumeric[13], xlab = names(engStudentsNumeric[13]))[c("stats", "names")]
boxplot(engStudentsNumeric[14:17])[c("stats", "names")]

# Get and write out boxplot stats
boxPlotsDf <- data.frame(data = boxplot(engStudentsNumeric, plot = FALSE)[c("stats")])
names(boxPlotsDf) <- names(engStudentsNumeric)
write.csv(boxPlotsDf, "boxPlotsStats.csv", row.names = FALSE)

### QQ PLOTS and Density Curves
ggplot(data = melt(engStudentsNumeric[1:6]), aes(sample = value)) +
  geom_qq() +
  stat_qq() +
  stat_qq_line() +
  theme(text = element_text(size = 15)) +
  facet_wrap(~variable, scales = "free")

ggplot(data = melt(engStudentsNumeric[1:6]), aes(x = value)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, color="black", fill="white", bins = 15) +
  geom_density(alpha=0.6, col = "blue") +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  labs(title="Distribution",x="Variable", y = "Density") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")

ggplot(data = melt(engStudentsNumeric[7:12]), aes(sample = value)) +
  geom_qq() +
  stat_qq() +
  stat_qq_line() +
  theme(text = element_text(size = 15)) +
  facet_wrap(~variable, scales = "free")

ggplot(data = melt(engStudentsNumeric[7:12]), aes(x = value)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, color="black", fill="white", bins = 15) +
  geom_density(alpha=0.6, col = "blue") +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  labs(title="Distribution",x="Variable", y = "Density") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")

ggplot(data = melt(engStudentsNumeric[13:17]), aes(sample = value)) +
  geom_qq() +
  stat_qq() +
  stat_qq_line() +
  theme(text = element_text(size = 10)) +
  facet_wrap(~variable, scales = "free")

ggplot(data = melt(engStudentsNumeric[13:17]), aes(x = value)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, color="black", fill="white", bins = 15) +
  geom_density(alpha=0.6, col = "blue") +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  labs(title="Distribution",x="Variable", y = "Density") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")



normalityAssessment <- data.frame(lapply(engStudentsNumeric, function(x) {
                                      round(c(
                                        kurtosis = kurtosis(x), 
                                        skewness = skewness(x, 0.25),
                                        PercOutside_3.29 = FSA::perc(as.numeric(abs(scale(x))), 3.29, "gt")
                                      ), 2)
                                    }))

write.csv(t(normalityAssessment), "normalityAssessment.csv", row.names = TRUE)


### SCATTER PLOTS
# pairs(engStudentsNumeric[1:5], pch=19, lower.panel = NULL)



###### CATEGORICAL VARIABLES EXPLORATION #########
engStudentsFactors <- engStudentsDataset[, categoricalColumns] 

for (factorCol in names(engStudentsFactors)) {
  engStudentsFactors[[factorCol]] <- as.factor(engStudentsFactors[[factorCol]])
}


write.csv(data.frame(summary(engStudentsFactors)), "engStudentsFactors.csv")

# Get level counts
categoricalLevels <- data.frame(sapply(engStudentsFactors, nlevels))
names(categoricalLevels) <- c("Levels")
write.csv(categoricalLevels, "categoricalLevels.csv")

##### HISTOGRAMS
# ggplot(data = engStudentsDataset, aes(x = COD_S11)) + geom_histogram(stat = "count", color="black", fill="white", binwidth = 1)
ggplot(data = engStudentsDataset, aes(x = GENDER)) + geom_histogram(stat = "count", color="black", fill="white", binwidth = 1)
ggplot(data = engStudentsDataset, aes(x = GENDER)) + geom_histogram(stat = "count", color="black", fill="white", binwidth = 1)
ggplot(data = engStudentsDataset, aes(x = GENDER)) + geom_histogram(stat = "count", color="black", fill="white", binwidth = 1)
ggplot(data = engStudentsDataset, aes(x = GENDER)) + geom_histogram(stat = "count", color="black", fill="white", binwidth = 1)
ggplot(data = engStudentsDataset, aes(x = GENDER)) + geom_histogram(stat = "count", color="black", fill="white", binwidth = 1)


# View(engStudentsFactors)

# weighting of female and male to be 50 50 
# Remove Not Sure rows in Education mom & dad

##################### TESTS FOR HYPOTHESES AND CORRELATION


############# LINEAR REGRESSION ###############
# Check for assumptions
  # COLINEARITY
    # PCA OR FA
  #Constant var in errors
  # Outliers. try like this then remove them
    # LOOK at Cook’s distance for values greater than one (Descriptives and Frequencies)
    # Already done 3.29 thing standardised smth. Do cooks distance
  # homoscedasticity (like quantiles...etc.)
  # NORMALITY OF RESIDUALS (ERRORS)
    # eruption.lm = lm(eruptions ~ waiting, data=faithful) 
    # eruption.stdres = rstandard(eruption.lm)

# HOW TO REPORT IS IN ASSUMPTIONS LECTURE



### Step 1

##Using ggcorrplot
engStudentsNumericCorrMat <- cor(engStudentsNumeric)
p.mat <- ggcorrplot::cor_pmat(engStudentsNumericCorrMat)
ggcorrplot::ggcorrplot(engStudentsNumericCorrMat, lab=TRUE,  p.mat = p.mat, sig.level = .05,
                       title = "Correlation matrix",  type="lower")

engStudentsNumericAdj <- engStudentsNumeric[, -which(names(engStudentsNumeric) %in% 
                                                    c("PERCENTILE", "QUARTILE", "X2ND_DECILE", "G_SC"))]

engStudentsNumericCorrMat <- cor(engStudentsNumericAdj)
p.mat <- ggcorrplot::cor_pmat(engStudentsNumericCorrMat)
ggcorrplot::ggcorrplot(engStudentsNumericCorrMat, lab=TRUE,  p.mat = p.mat, sig.level = .05,
                       title = "Correlation matrix",  type="lower")

### Step 2
#Bartlett's test
psych:: cortest.bartlett(engStudentsNumericCorrMat, n = nrow(engStudentsNumeric))

#Measure of Sampling Accuracy (execute one of these):
KMO(engStudentsNumeric)

#Determinant (execute one of these from the base package):
det(engStudentsNumericCorrMat)


### Step 3
#Principal Component Analysis
#On raw data using principal components analysis, nfactors is set to the number of variables we expect to get out which is equal to the number going in.
pc1 <- principal(engStudentsNumericAdj, nfactors = length(engStudentsNumericAdj), rotate = "none")
plot(pc1$values, type = "b") #scree plot

#Factor Analysis
#Principal Axis Factoring fm=pa
factsol <- fa(engStudentsNumericCorrMat, nfactors=2, obs=NA, n.iter=1, rotate="varimax", fm="ml")
print.psych(factsol,cut=0.3, sort=TRUE)
fa.graph(factsol) 
fa.sort(factsol$loading)
fa.diagram(factsol)#create a diagram showing the factors and how the manifest variables load
plot(factsol$values, type = "b") #scree plot



pc1$values #output eigenvalues

pc1$communality
pc1$loadings
data.frame(pc1$scores)

#Another way to look at Eigen values plus variance explained
pcf <- princomp(engStudentsNumericAdj)
factoextra::get_eigenvalue(pcf)

pc1$Vaccounted#Variance accounted for
pc1$communality #Communality 
pcf$loadings #Loadings 


#Rotation PCA
pc2 <-  principal(engStudentsNumeric, nfactors = 3, rotate = "varimax")#Extracting 4 factors
print.psych(pc2, cut = 0.3, sort = TRUE)
#Get the output as you would for initial solution



#Reliability analysis
grades <- engStudentsNumeric[, which(names(engStudentsNumeric) %in% 
                                       c("BI0_S11", "CR_S11", "MAT_S11", "CR_PRO", "QR_PRO", "CC_PRO", "FEP_PRO"))]
alpha(grades)

socialClass <- engStudentsNumeric[, which(names(engStudentsNumeric) %in% 
                                      c("ENG_S11", "ENG_PRO", "SEL_IHE", "SEL"))]
alpha(socialClass)


pcaScores <- data.frame(pc1$scores)
alpha(pcaScores)

### Hypotheses
# 2.	Citizen Competencies will not have an effect on the Global Score 
cor.test(engStudentsDataset$CC_PRO, engStudentsDataset$G_SC, method = "pearson")
# 3.	The English Score in Saber Pro (University) is highly dependent on the English Score in Saber 11 (Grade 11)
cor.test(engStudentsDataset$ENG_S11, engStudentsDataset$ENG_PRO, method = "pearson")
# 4.	A student’s social class affects his/her ability to speak English positively
cor.test(engStudentsDataset$ENG_PRO, engStudentsDataset$SEL, method = "spearman")

##### LINEAR REGRESSION #####
lmModel <- lm(engStudentsDataset$G_SC~pcaScores$PC1 + pcaScores$PC2)
anova(lmModel)
summary(lmModel)
lm.beta::lm.beta(lmModel)
stargazer(lmModel, type="text") #Tidy output of all the required stats


lmModel$residuals
lmModel$effects

# Outliers
plot(lmModel, 5)
plot(lmModel, 4, id.n = 5)

stargazer(lmModel, type = "text")

#### REMOVE OUTLIERS AND CREATE ANOTHER MODEL
model2 <- lm(engStudentsDataset$G_SC[-c(1336,2477,5010,5616,7721)]~pcaScores$PC1[-c(1336,2477,5010,5616,7721)] + pcaScores$PC2[-c(1336,2477,5010,5616,7721)])
stargazer(model2, type = "text")

# Outliers
plot(model2, 5)
plot(model2, 4, id.n = 5) # Cooks Distance
# Linearity
plot(model2, 1)
# Homoscedasticity
plot(model2, 3)
# QQ Plot for normality
plot(model2, 2)


car::vif(model2)

