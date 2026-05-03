# Install package (cukup sekali saja)
install.packages(c("readxl","ggplot2","biotools","car","MASS","pscl","VGAM"))

# Load library
library(readxl)
library(ggplot2)
library(biotools)
library(car)
library(MASS)
library(pscl)
library(VGAM)

# Import data
data <- read.csv(file.choose())

# Cek struktur & ringkasan
str(data)
summary(data)

# Cek missing value
sum(is.na(data))
colSums(is.na(data))

data$NObeyesdad <- factor(data$NObeyesdad,
                          levels = c("Insufficient_Weight",
                                     "Normal_Weight",
                                     "Overweight_Level_I",
                                     "Overweight_Level_II",
                                     "Obesity_Type_I",
                                     "Obesity_Type_II",
                                     "Obesity_Type_III"),
                          ordered = TRUE)

# Cek hasil
str(data$NObeyesdad)
table(data$NObeyesdad)

ggplot(data, aes(x=NObeyesdad)) +
  geom_bar(fill="skyblue") +
  theme_minimal()

ggplot(data, aes(x=NObeyesdad, y=Weight)) +
  geom_boxplot(fill="pink") +
  theme_minimal()

ggplot(data, aes(x=NObeyesdad, y=FAF)) +
  geom_boxplot(fill="yellow") +
  theme_minimal()

ggplot(data, aes(x=Weight, y=FAF, color=NObeyesdad)) +
  geom_point() +
  theme_minimal()

# Normalitas
shapiro.test(data$Age)
shapiro.test(data$Height)
shapiro.test(data$Weight)

# Homogenitas kovarians
data_num <- data[, c("Age","Height","Weight")]
boxM(data_num, data$NObeyesdad)

# Multikolinearitas
model_lm <- lm(Age ~ Height + Weight + FCVC + FAF, data=data)
vif(model_lm)

lda_model <- lda(NObeyesdad ~ Age + Height + Weight + FCVC + FAF, data=data)

# Prediksi
pred <- predict(lda_model)

# Confusion matrix
cm <- table(pred$class, data$NObeyesdad)
cm

# Akurasi
accuracy <- sum(diag(cm)) / sum(cm)
accuracy

# APER
aper <- 1 - accuracy
aper

# Standarisasi variabel
data$Weight_s <- scale(data$Weight)
data$FAF_s <- scale(data$FAF)
data$FCVC_s <- scale(data$FCVC)
data$Age_s <- scale(data$Age)

model_ord <- polr(NObeyesdad ~ Weight_s + FAF_s, data=data, Hess=TRUE)

summary(model_ord)

ctable <- coef(summary(model_ord))
p_value <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable_final <- cbind(ctable, p_value))

exp(coef(model_ord))

pR2(model_ord)

model_null <- polr(NObeyesdad ~ 1, data=data, Hess=TRUE)
anova(model_null, model_ord)

deviance(model_ord)
AIC(model_ord)

coef(model_ord)

model_ord$zeta

model_vglm <- vglm(NObeyesdad ~ Weight_s + FAF_s,
                   family = cumulative(parallel=TRUE),
                   data=data)

summary(model_vglm)