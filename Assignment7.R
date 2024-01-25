data <- read.table("ExDecay.txt", header=T)

attach(data)

linear_model <- lm(Counts ~ Time)

summary(linear_model)

plot(Time,Counts, ylab = "Counts per second")

abline(linear_model)

# R^2 = 0.767

quadratic_model <- lm(Counts ~ Time + I(Time^2))

summary(quadratic_model)

f2 <- function(data) {107.9549 -7.3543*data + 0.1513*data*data}

plot(Time, Counts, ylab = "Counts per second")
curve(f2, add=T)

# R^2 = 0.9081 --> Besser Fit des quadratischen Modells.

exponential_model <- lm(log(Counts) ~ Time)

summary(exponential_model)

f3 <- function(data) {exp(4.55-0.064785*data)}

plot(Time,Counts, ylab = "Counts per second")

curve(f3, add=T)

# linear_model$R^2 < exponential_model$R^2 < quadratic_model$R^2

# Bonusaufgabe

attendance <- read.csv("Teilnahme.csv", sep= ";")

attendance$attend_or_not <- factor(attendance$attend_or_not, levels = c("course not attended", "course attended"))

attendance$attend_or_not <- (as.integer(attendance$attend_or_not)-1)

head(attendance)

logistic_model <- glm(attendance$attend_or_not ~ attendance$stat_joy, data=attendance, family = binomial())

summary(logistic_model)

# Probabilities and not log odds
p <- predict(logistic_model, type = "response")

pred <- prediction(p, attendance$attend_or_not)

# y = true positive rate; x = false positive rate
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Calculate AUC
# auc = 0.8079052
auc <- unlist(slot(performance(pred, "auc"), "y.values"))

#Enhance the model by incorporating all columns
enhance_model <- glm(attend_or_not ~ stat_joy + temperature + sun_joy, data = attendance, family = binomial())

p_enhanced <- predict(enhance_model, type = "response")

# Erstellen des Prediction-Objekts für das erweiterte Modell
pred_enhanced <- prediction(p_enhanced, attendance$attend_or_not)

# Erstellen des Performance-Objekts für das erweiterte Modell und Plotten der ROC-Kurve
perf_enhanced <- performance(pred_enhanced, "tpr", "fpr")
plot(perf_enhanced)

# auc = 0.8099891
auc_enhanced <- unlist(slot(performance(pred_enhanced, "auc"), "y.values"))
show(auc_enhanced)

