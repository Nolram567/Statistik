
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
