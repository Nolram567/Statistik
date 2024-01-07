#Aufgabe 1 a)
relativer_dateipfad <- 'Elektronik.csv'

data <- read.csv(relativer_dateipfad, sep = ';')

hist(data$B0T)
hist(data$B1T)

# Die Datenreihen folgenden sich nicht normalverteilt, sondern exponentialverteilt.

#Aufgabe 1b)

# Wir quantifizieren diese Einschätzung mit dem Kolmogorov Smirnov Test.
# Wir nehmen ein Konfidenzlimit von 95% an. Demenstsprechend ist unser Signifikanzlevel α 0.05, da das Konfidenzlimit als 1-α definiert ist.
# Unsere H1 ist, dass die Daten nicht normalverteilt sind. H0 ist, dass die Daten normalverteilit sind.
ks.test(data$B0T, "pnorm", mean(data$B0T), sd(data$B0T))

#	Asymptotic one-sample Kolmogorov-Smirnov test
#data:  data$B0T
#D = 0.16025, p-value = 6.915e-05
#alternative hypothesis: two-sided
# H0 wird verworfen, da p<α

ks.test(data$B1T, "pnorm", mean(data$B1T), sd(data$B1T))

#Asymptotic one-sample Kolmogorov-Smirnov test
#data:  data$B1T
#D = 0.15639, p-value = 0.0001127
#alternative hypothesis: two-sided
# H0 wird verworfen, da p<α

#Aufgabe 1 c)

t.test(data$B0T, data$B1T, alternative = "two.sided", conf.level = 0.95)

#Welch Two Sample t-test
#data:  data$B0T and data$B1T
#t = 1.1593, df = 381.99, p-value = 0.2471
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.08322647  0.32235803
#sample estimates:
#  mean of x mean of y 
#1.127797  1.008232 
#H0 wird akzeptiert, da p > α
# Dem Ergebnis ist nicht zu trauen, da unsere Datensätze nicht normalverteilt sind, wie wir in 1 b) festgestellt haben. Bei großen 
#Stichproben können auch nicht normalverteilte Daten toleriert werden, unsere dof liegen jedoch nur bei 382

#Aufgabe 2
# α = 0.05

data <- read.csv("Haar.csv", sep = ";")

A <- data$GA
B <- data$GB
C <- data$GC

# Wir prüfen, ob wir den t-test verwenden können, indem wir mit kolmogorov smirnov testen, ob die Daten normalverteilt sind.#
#H0: Die Daten sind normalverteilt
#H1: Die Daten sind nicht normalverteilt

ks.test(A, "pnorm", mean(A), sd(A))

#Asymptotic one-sample Kolmogorov-Smirnov test
#data:  A
#D = 0.04331, p-value = 0.9919
#alternative hypothesis: two-sided

# H0 wird akzeptiert, da p>α

ks.test(B, "pnorm", mean(B), sd(B))

#Asymptotic one-sample Kolmogorov-Smirnov test
#data:  B
#D = 0.076941, p-value = 0.5946
#alternative hypothesis: two-sided

#  H0 wird akzeptiert, da p>α

#  Daraus schlussfolgern wir, dass wir eine statistische signifikante Grundlage haben, um anzunehmen, dass A und B normalverteilt sind.
# Wir führen einen Welch Two Sample t-test durch, daher müssen wir die Geleichheit der Varianzen nicht überprüfen.

t.test(A, B, alternative = "two.sided", conf.level = 0.95)

#Welch Two Sample t-test
#data:  A and B
#t = -0.71381, df = 197.48, p-value = 0.4762
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.06939313  0.03250835
#sample estimates:
#  mean of x mean of y 
#1.262117  1.280559

#H0 wird akzeptiert, weil p > α

t.test(A, C, alternative = "two.sided", conf.level = 0.95)

#data:  A and C
#t = 0.46211, df = 83.477, p-value = 0.6452
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.05377256  0.08632519
#sample estimates:
#  mean of x mean of y 
#1.262117  1.245840 

#H0 wird akzeptiert, da p > α

# Bei beiden Tests zeigt sich kein statistisch signifikanter Unterschied zwischen Kontroll- und Experimentalgruppe.
# Die drei Keingütekriterien empirischer Forschung sind Validität, Reliabilität und Objektivität. Der Verleich von A mit B ist
# valider als der Vergleich von A und C, weil C beispielsweise demografisch anders strukturiert sein könnte als A. Diese Störfaktoren
# müssten ggf. kontrolliert werden. A und B hingegen besitzen dieselben demografischen Eigenschaften (ob diese repräsentativ sind, ist eine
# andere Frage).
attach(data)

for (i in 1:12) {
  subset_data <- GB[Sternzeichen == i]
  
  if (length(subset_data) > 1) {  
    result <- t.test(GA[Sternzeichen == i], subset_data)
    
    if (result$p.value < 0.05) {
      cat("H0 wird verworfen für", i, "\n")
    }
    else{
      cat("H0 wird akzeptiert für", i, "\n")
    }
  } else {
    cat("Nicht genügend Daten für Sternzeichen", i, "\n")
  }
}

# H0 wird nur für Sternzeichen 9 akzeptiert. Das heißt es gibt nur für dieses Sternzeichen eine signifikante Abweichung vom Mittelwert
# der Kontrollgruppe. Als Marketer würde ich empfehlen, dass Produkt nicht speziell ab Jungfrauen zu vermarkten, da ansonsten vielleicht
# alle anderen Sternezeichen ein anderes Produkt wählen. Als Wissenschaftler würde ich sagen, dass Korrelation nicht Kausalität impliziert.
# Und die Korrelation auf ein nicht aufgeführtes drittes Merkmal zurückführbarsein könnte oder der Effekt aufgrund der kleinen Stichprobengröße
# "zufällig entstanden ist.

#Aufgabe 3 a)

alpha <- 0.05
n <- 10
dof <- n - 1

t_krit_lower <- qt(alpha/2, dof)
t_krit_upper <- qt(1-alpha/2, dof)

#b)

wilcox.test(c(92, 96, 96, 106, 112, 114, 114, 118, 123, 124), mu = 100)

#Wilcoxon signed rank test with continuity correction
#data:  c(92, 96, 96, 106, 112, 114, 114, 118, 123, 124)
#V = 48, p-value = 0.04123
#alternative hypothesis: true location is not equal to 100

# H0 wird verworfen, p < α

größe <- c(185, 189, 196, 172, 175, 165, 199, 168, 191, 180)
gewicht <- c(75, 81, 85, 66, 68, 62, 89, 63, 80, 72)

cor(größe, gewicht)

# 0.9936803


# Wir führen einen Pearson-Korrelationstest durch.
# H0: Die wahre Korrelation zwischen Größe und Gewicht ist 0.
# H1 : Die wahre Korrelatin zwischen Größe und Gewicht ist nicht 0.

cor.test(größe, gewicht)

#Pearson's product-moment correlation
#data:  größe and gewicht
#t = 25.039, df = 8, p-value = 6.926e-09
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.9724890 0.9985602
#sample estimates:
#      cor 
# 0.9936803
# H0 wird verworfen, da p<α

plot(größe, gewicht, main="Scatterplot von Größe und Gewicht", xlab="Gewicht",  ylab="Größe", pch=19, col="blue")