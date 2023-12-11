
relativer_dateipfad <- "data/Daten_Fragebogen.csv"

daten <- read.csv(relativer_dateipfad, sep= ";")

head(daten)

#a)
class(daten)

v <- daten$Km

mean(v)
median(v)
sd(v)
var(v)

#b)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


v2 <- daten$Alter

mean(v2)
sd(v2)

getmode(v2)

#c)
mean(daten$Alter[daten$Geschlecht == "m"])

mean(daten$Alter[daten$Geschlecht == "f"])

#d) - f)

hist(v2)
hist(daten$Alter[daten$Geschlecht == "m"])
hist(daten$Alter[daten$Geschlecht == "f"], xlab = "Alter der Frauen")

pdf("HistogrammAlter.pdf")
hist(v2, col= "red")
dev.off()

#Aufgabe 2: Fibonacci-Funktion

fib <- function(n){
  
  if(n == 1){return (1)}
  if(n == 2){return (1)}
  
  return (fib(n-1) + fib(n-2))
  
}

save(fib, file = "fibonacci_function.R")

load("fibonacci_function.R")

fib(6)
fib(9)

#Aufgabe 4

bd <- read.csv("Blutdruck.csv", sep = ";")

mean(bd$Blutdruck)
sd(bd$Blutdruck)

hist(bd$Blutdruck, xlab = "Blutdruck")

# Zweiseitiger t-test: Signifikanzlevel α = 0.05; Konfidenzintervall = 1-α;Angenommener Mittelwert μ = 17
# H0 : Der mittlere Blutdruck weicht nach der Gabe des Medikaments nicht signifikant von Mittelwert 17 ab.
# H1 : Der mittlere Blutdruck ist nach der Gabe des Medikaments signifikant kleiner oder größer als 17.

t.test(bd$Blutdruck, alternative = c("two.sided", "less", "greater"), mu = 17, conf.level = 0.99)

#data:  bd$Blutdruck
#t = -2.2247, df = 99, p-value = 0.02837
#alternative hypothesis: true mean is not equal to 17
#95 percent confidence interval:
#  16.19972 16.95428
#sample estimates:
#  mean of x 
#16.577 
# --> H0 wird verworfen, da p < α


# Zweiseitiger t-test: Signifikanzlevel α = 0.01; Konfidenzintervall = 1-α;Angenommener Mittelwert μ = 17
t.test(bd$Blutdruck, alternative = c("two.sided", "less", "greater"), mu = 17, conf.level = 0.99)

# data:  bd$Blutdruck
# t = -2.2247, df = 99, p-value = 0.02837
# alternative hypothesis: true mean is not equal to 17
# 99 percent confidence interval:
#  16.07762 17.07638
#sample estimates:
#  mean of x 
# 16.577
# H1 wird verworfen, da p > α


# Einseitiger t-test: Signifikanzlevel α = 0.05; Konfidenzintervall = 1-α;Angenommener Mittelwert μ = 17
# H0: Der mittlere Blutdruck weicht nach der Gabe des Medikaments nicht kleiner als 17.
# H1 : Der mittlere Blutdruck ist nach der Gabe des Medikaments signifikant kleiner als 17
t.test(bd$Blutdruck, alternative = c("less"), mu = 17, conf.level = 0.95)

#data:  bd$Blutdruck
#t = -2.2247, df = 99, p-value = 0.01419
#alternative hypothesis: true mean is less than 17
#95 percent confidence interval:
#  -Inf 16.89271
#sample estimates:
#  mean of x 
#16.577
# H0 wird verworfen, da p < α

# Einseitiger t-test: Signifikanzlevel α = 0.05; Konfidenzintervall = 1-α;Angenommener Mittelwert μ = 17
# H0: Der mittlere Blutdruck weicht nach der Gabe des Medikaments nicht größer als 17.
# H1 : Der mittlere Blutdruck ist nach der Gabe des Medikaments signifikant größer als 17
t.test(bd$Blutdruck, alternative = c("greater"), mu = 17, conf.level = 0.95)

#data:  bd$Blutdruck
#t = -2.2247, df = 99, p-value = 0.9858
#alternative hypothesis: true mean is greater than 17
#95 percent confidence interval:
#  16.26129      Inf
#sample estimates:
#  mean of x 
#16.577 
# H0 wird beibehalten, da p > α

# Aufgabe 5

data <- read.csv("effect.csv", sep = ";")

attach(data)
Diff[Group == "A"]
# Das Resultat sind alle Werte in der Spalte "Diff", in deren Zeile unter "Group" "A" steht.

A <- Diff[Group == "A"]
B <- Diff[Group == "B"]

cat(B)

mean_A <- mean(A)
mean_B <- mean(B) 


# Zweiseitiger t-test: Signifikanzlevel α = 0.05; Konfidenzintervall = 1-α; Angenommener Mittelwert μ = mean(A)
# H0 : Die Mittelwerte von A und B sind gleich
# H1 : Die Mittelwerte von A und B sind nicht gleich
t.test(A, B, alternative = c("two.sided"), mu = mean_A, conf.level = 0.95)

#data:  A and B
#t = -2.9478, df = 163.62, p-value = 0.003669
#alternative hypothesis: true difference in means is not equal to 0.467
#95 percent confidence interval:
#  -0.6952132  0.2372132
#sample estimates:
#  mean of x mean of y 
#0.467     0.696 
# H0 wird verworfen, da p < α, t deutet darauf hin, dass A kleiner ist als B, daher führen wir einen weiteren einseitigen Test durch


# Einseitiger t-test: Signifikanzlevel α = 0.05; Konfidenzintervall = 1-α; Angenommener Mittelwert μ = mean(A)
# H0 : Der Mittelwert von A is nicht signifikant kleiner als der Mittelwert von B
# H1 : Der Mittelwert von A ist signifikant kleiner als der Mittelwert von B
t.test(A, B, alternative = "less", mu = mean_A, conf.level = 0.95)

# H0 wird verworfen, da p < α


