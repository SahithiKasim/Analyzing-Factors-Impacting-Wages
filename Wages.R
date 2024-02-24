# Read the data
data <- read.table("Wages1.csv", header=TRUE, sep=",")

# Ridge Bootstrapping
boot.ridgecoef <- function(data, indices, maxit=100) {
  data <- data[indices,]
  colnames(data) <- c("wage","school","sex","exper")
  mod <- lmridge(wage ~ school + sex + exper, data=data, K=0.02)
  return(coef(mod))
}

ridgemodel_bootcoeff <- boot(data=data.frame(data), statistic=boot.ridgecoef, R=1000, maxit=100)
ridgemodel_bootcoeff
plot(ridgemodel_bootcoeff, index=2)
boot.ci(ridgemodel_bootcoeff, type="perc", index=2)

# Robust Bootstrapping
library(MASS)
boot.huber <- function(data, indices, maxit=100){
  data <- data[indices,] 
  mod <- rlm(wage ~ school + sex + exper, data=data, maxit=maxit)
  coefficients(mod)
}

robustboot <- boot(data=data, statistic=boot.huber, R=100, maxit=100)
robustboot
plot(robustboot, index=2)
boot.ci(robustboot, type="perc", index=2)
qt(0.975, 3290)

# Ridge code
library(lmridge)
ridgemodel <- lmridge(wage ~ exper + sex + school, data=data, K=seq(0,1,0.5))
vif(ridgemodel)
summary(ridgemodel)
plot(ridgemodel)

# Robust
robust <- rlm(wage ~ sex + exper + school, data=data, psi=psi.bisquare)
summary(robust)
plot(robust)

# K-fold validation
library(caret)
ctrl <- trainControl(method="cv", number=5)
model <- train(wage ~ exper + sex + school, data, method="lm", trControl=ctrl)
print(model)
summary(model)
model$resample
model$finalModel

# Linear Regression
linear_model <- lm(wage ~ exper + sex + school, data=data)
summary(linear_model)
plot(linear_model)

# Logistic Regression
logistic_model <- glm(sex ~ wage + exper + school, data=data, family=binomial)
summary(logistic_model)

# Pearson's Correlation
correlation_matrix <- cor(data[c("wage", "exper", "school", "sex")])
print(correlation_matrix)

# Plots and Summary for Research Question 1
Project.mod <- lm(wage ~ school, data)
plot(wage ~ school, data)
abline(Project.mod)
summary(Project.mod)
anova(Project.mod)
resid <- residuals(Project.mod)
plot(data$wage, resid)
abline(h=0)
Projectid <- residuals(lm(wage ~ school, data))
shapiro.test(Projectid)
qqnorm(Projectid)
qqline(Projectid)

# Plots and Summary for Research Question 2
Project.mod2 <- lm(wage ~ sex, data)
plot(wage ~ sex, data)
abline(Project.mod2)
summary(Project.mod2)
anova(Project.mod2)
resid <- residuals(Project.mod2)
plot(data$wage, resid)
abline(h=0)
Projectid2 <- residuals(lm(wage ~ sex, data))
shapiro.test(Projectid2)
qqnorm(Projectid2)
qqline(Projectid2)

# Normal, scatter, and residual vs fitted plots for Research Question 3
Projectid3 <- residuals(lm(wage ~ exper + sex, data))
shapiro.test(Projectid3)
qqnorm(Projectid3)
qqline(Projectid3)
model13 <- lm(wage ~ exper + sex, data)
summary(model13)
anova(model13)
plot(data)

# Best Subset
bs <- BestSub(data[,1:3], data$wage, num=1)
Bs

# Remedy & research 3
library(car)
y <- lm(wage ~ sex + exper + school, data)
Anova(y, type="II")
shapiro.test(residuals(y))
qqnorm(residuals(y))
qqline(residuals(y))
bf.test(residuals(y) ~ sex, data)
avPlots(y)
influencePlot(y)
