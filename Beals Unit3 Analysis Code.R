##################################################
### Unit 3 Assignment - Wine Sales
### MSDS 411-DL-56
### Brandi Beals

##################################################
### Load packages
library(corrplot)
library(MASS)
library(car)
library(pscl)

##################################################
### Set working directory & read data
setwd("C:/Users/bbeals/Dropbox (Personal)/Masters in Predictive Analytics/411-DL-56/W-unit 3 Weeks 7 - 9/Unit 3 - Wine/3 Homework")
master=read.csv("WINE.csv",header=T)
wine <- master[,-1]

##################################################
### Exploratory data analysis
str(wine)
summary(wine)
apply(wine, 2, sd, na.rm=TRUE)

# Histograms
hist(wine$TARGET, breaks=10)

par(mfrow=c(2,2))
hist(wine$FixedAcidity)
hist(wine$VolatileAcidity)
hist(wine$CitricAcid)
hist(wine$ResidualSugar)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(wine$Chlorides)
hist(wine$FreeSulfurDioxide)
hist(wine$TotalSulfurDioxide)
hist(wine$Density)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(wine$pH)
hist(wine$Sulphates)
hist(wine$Alcohol)
hist(wine$AcidIndex)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(wine$LabelAppeal)
hist(wine$STARS)
par(mfrow=c(1,1))

# Scatterplots
plot(wine$TARGET, wine$FixedAcidity)
plot(wine$TARGET, wine$VolatileAcidity)
plot(wine$TARGET, wine$CitricAcid)
plot(wine$TARGET, wine$ResidualSugar)
plot(wine$TARGET, wine$Chlorides)
plot(wine$TARGET, wine$FreeSulfurDioxide)
plot(wine$TARGET, wine$TotalSulfurDioxide)
plot(wine$TARGET, wine$Density)
plot(wine$TARGET, wine$pH)
plot(wine$TARGET, wine$Sulphates)
plot(wine$TARGET, wine$Alcohol)
plot(wine$TARGET, wine$AcidIndex)

par(mfrow=c(1,2))
plot(wine$TARGET, wine$LabelAppeal)
plot(wine$TARGET, wine$STARS)
par(mfrow=c(1,1))

# Correlation matrix
corrplot(cor(wine, use="complete.obs"), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

##################################################
### Preparation and transformations

# Imputation
wineimp <- wine

wineimp$ResidualSugar_IMP <- ifelse(is.na(wineimp$ResidualSugar)==TRUE,1,0)
wineimp$Chlorides_IMP <- ifelse(is.na(wineimp$Chlorides)==TRUE,1,0)
wineimp$FreeSulfurDioxide_IMP <- ifelse(is.na(wineimp$FreeSulfurDioxide)==TRUE,1,0)
wineimp$TotalSulfurDioxide_IMP <- ifelse(is.na(wineimp$TotalSulfurDioxide)==TRUE,1,0)
wineimp$pH_IMP <- ifelse(is.na(wineimp$pH)==TRUE,1,0)
wineimp$Sulphates_IMP <- ifelse(is.na(wineimp$Sulphates)==TRUE,1,0)
wineimp$Alcohol_IMP <- ifelse(is.na(wineimp$Alcohol)==TRUE,1,0)
wineimp$STARS_IMP <- ifelse(is.na(wineimp$STARS)==TRUE,1,0)

for(i in 1:ncol(wineimp)){
  wineimp[is.na(wineimp[,i]), i] <- mean(wineimp[,i], na.rm = TRUE)
}

# Correlation matrix
corrplot(cor(wineimp[,1:15]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

##################################################
### Model creation

# Multiple Linear Regression
# Full model - baseline
fitfulllm <- lm(TARGET ~ ., data=wineimp)
summary(fitfulllm)
sort(vif(fitfulllm), decreasing=TRUE)
par(mfrow=c(2,2))
plot(fitfulllm)
par(mfrow=c(1,1))

# Stepwise regression
fitsteplm <- stepAIC(fitfulllm, direction="both")
summary(fitsteplm)
sort(vif(fitsteplm), decreasing=TRUE)
par(mfrow=c(2,2))
plot(fitsteplm)
par(mfrow=c(1,1))

# Poisson Regression
fitpoisson <- glm(TARGET ~ VolatileAcidity + Chlorides + FreeSulfurDioxide + 
                    TotalSulfurDioxide + Density + pH + Sulphates + Alcohol + 
                    LabelAppeal + AcidIndex + STARS + ResidualSugar_IMP + STARS_IMP, 
                  family="poisson"(link="log"), data=wineimp)
summary(fitpoisson)

par(mfrow=c(1,2))
plot(wineimp$TARGET,fitpoisson$residuals)
plot(wineimp$TARGET,fitpoisson$fitted.values)
par(mfrow=c(1,1))

# Zero-Inflated Poisson Regression
fitzip <- zeroinfl(TARGET ~ VolatileAcidity + FreeSulfurDioxide + 
                     TotalSulfurDioxide + pH + Sulphates + Alcohol + 
                     LabelAppeal + AcidIndex + STARS + STARS_IMP,
                   data=wineimp)
summary(fitzip)

par(mfrow=c(1,2))
plot(wineimp$TARGET,fitzip$residuals)
plot(wineimp$TARGET,fitzip$fitted.values)
par(mfrow=c(1,1))

# Negative Binomial Regression
fitnb <- glm.nb(TARGET ~ VolatileAcidity + Chlorides + FreeSulfurDioxide + 
                  TotalSulfurDioxide + Density + pH + ResidualSugar_IMP + STARS_IMP +
                  FixedAcidity + Sulphates + Alcohol + LabelAppeal,
                data=wineimp)
summary(fitnb)

par(mfrow=c(1,2))
plot(wineimp$TARGET,fitnb$residuals)
plot(wineimp$TARGET,fitnb$fitted.values)
par(mfrow=c(1,1))

fitnb2 <- glm.nb(TARGET ~ STARS_IMP + Alcohol + LabelAppeal,
                data=wineimp)
summary(fitnb2)

par(mfrow=c(1,2))
plot(wineimp$TARGET,fitnb2$residuals)
plot(wineimp$TARGET,fitnb2$fitted.values)
par(mfrow=c(1,1))

fitnb3 <- glm.nb(TARGET ~ VolatileAcidity + Chlorides + FreeSulfurDioxide + 
                   TotalSulfurDioxide + Density + pH + Sulphates + Alcohol + 
                   LabelAppeal + AcidIndex + STARS + ResidualSugar_IMP + STARS_IMP,
                 data=wineimp)
summary(fitnb3)

par(mfrow=c(1,2))
plot(wineimp$TARGET,fitnb3$residuals)
plot(wineimp$TARGET,fitnb3$fitted.values)
par(mfrow=c(1,1))

# Zero-Inflated Negative Binomial Regression
fitzipnb <- zeroinfl(TARGET ~ VolatileAcidity + FreeSulfurDioxide + 
                       TotalSulfurDioxide + Density + pH + STARS_IMP +
                       FixedAcidity + Sulphates + Alcohol + LabelAppeal, 
                     data=wineimp, dist="negbin", EM=TRUE)
summary(fitzipnb)

par(mfrow=c(1,2))
plot(wineimp$TARGET,fitzipnb$residuals)
plot(wineimp$TARGET,fitzipnb$fitted.values)
par(mfrow=c(1,1))

# Hurdle
fithurdle <- hurdle(TARGET ~ VolatileAcidity + Chlorides + FreeSulfurDioxide + 
                      TotalSulfurDioxide + pH + Sulphates + Alcohol + 
                      LabelAppeal + AcidIndex + STARS + STARS_IMP, 
                    data=wineimp)
summary(fithurdle)

par(mfrow=c(1,2))
plot(wineimp$TARGET,fithurdle$residuals)
plot(wineimp$TARGET,fithurdle$fitted.values)
par(mfrow=c(1,1))

##################################################
### Model selection
n <- nrow(wineimp)

matrix(data=c(
  AIC(fitfulllm),
  AIC(fitsteplm),
  AIC(fitpoisson),
  AIC(fitzip),
  AIC(fitnb),
  AIC(fitnb2),
  AIC(fitnb3),
  AIC(fitzipnb),
  AIC(fithurdle),
  AIC(fitfulllm, k=log(n)),
  AIC(fitsteplm, k=log(n)),
  AIC(fitpoisson, k=log(n)),
  AIC(fitzip, k=log(n)),
  AIC(fitnb, k=log(n)),
  AIC(fitnb2, k=log(n)),
  AIC(fitnb3, k=log(n)),
  AIC(fitzipnb, k=log(n)),
  AIC(fithurdle, k=log(n)),
  (-2*logLik(fitfulllm, REML = TRUE)),
  (-2*logLik(fitsteplm, REML = TRUE)),
  (-2*logLik(fitpoisson, REML = TRUE)),
  (-2*logLik(fitzip, REML = TRUE)),
  (-2*logLik(fitnb, REML = TRUE)),
  (-2*logLik(fitnb2, REML = TRUE)),
  (-2*logLik(fitnb3, REML = TRUE)),
  (-2*logLik(fitzipnb, REML = TRUE)),
  (-2*logLik(fithurdle, REML = TRUE))
), ncol=3, byrow=FALSE, 
dimnames=list(c("fitfulllm","fitsteplm","fitpoisson","fitzip","fitnb","fitnb2","fitnb3","fitzipnb","fithurdle"),
              c("AIC","BIC","Log Likelihood Deviance")))
