##################################################
### Unit 3 Assignment - Wine Sales
### MSDS 411-DL-56
### Brandi Beals

##################################################
### Load packages
library(openxlsx)

##################################################
### Set working directory & read data
setwd("C:/Users/bbeals/Dropbox (Personal)/Masters in Predictive Analytics/411-DL-56/W-unit 3 Weeks 7 - 9/Unit 3 - Wine/3 Homework")
data=read.csv("WINE_TEST.csv",header=T)

### Create indicator variables for NA values
data$ResidualSugar_IMP <- ifelse(is.na(data$ResidualSugar)==TRUE,1,0)
data$Chlorides_IMP <- ifelse(is.na(data$Chlorides)==TRUE,1,0)
data$FreeSulfurDioxide_IMP <- ifelse(is.na(data$FreeSulfurDioxide)==TRUE,1,0)
data$TotalSulfurDioxide_IMP <- ifelse(is.na(data$TotalSulfurDioxide)==TRUE,1,0)
data$pH_IMP <- ifelse(is.na(data$pH)==TRUE,1,0)
data$Sulphates_IMP <- ifelse(is.na(data$Sulphates)==TRUE,1,0)
data$Alcohol_IMP <- ifelse(is.na(data$Alcohol)==TRUE,1,0)
data$STARS_IMP <- ifelse(is.na(data$STARS)==TRUE,1,0)

### Impute with mean
data$ResidualSugar[is.na(data$ResidualSugar)==TRUE] <- 5.418733065
data$Chlorides[is.na(data$Chlorides)==TRUE] <- 0.054822489
data$FreeSulfurDioxide[is.na(data$FreeSulfurDioxide)==TRUE] <- 30.845571287
data$TotalSulfurDioxide[is.na(data$TotalSulfurDioxide)==TRUE] <- 120.714232643
data$pH[is.na(data$pH)==TRUE] <- 3.207628226
data$Sulphates[is.na(data$Sulphates)==TRUE] <- 0.527111782
data$Alcohol[is.na(data$Alcohol)==TRUE] <- 10.489236260
data$STARS[is.na(data$STARS)==TRUE] <- 2.041754981

### Model scoring
# *** Assumes fithurdle is available as a global variable ****
# *** You must run the Beals Unit3 Analysis Code script ***

data$P_TARGET <- predict(fithurdle, newdata = data, type = "response")
hist(data$P_TARGET)
summary(data$P_TARGET)

### Subset output
names(data)[1] <- "INDEX"
prediction <- data[c("INDEX","P_TARGET")]

### Prediction output 
write.xlsx(prediction, file = "Brandi Beals Unit3 Predictions.xlsx", sheetName = "Predictions", col.names = TRUE)

