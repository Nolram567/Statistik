import random
import pandas as pd



def generate_nd_csv(n: int, distribution: str) -> None:
    """
    Diese Funktion generiert CSV-Dateien mit zufälligen Werten entsprechend der spezifizierten
     Wahrscheinlichkeitsverteilung. Übergeben die für den Parameter "distribution" "normal", "uniform" oder "exponential".
    """
    if distribution == "normal":
        nd_data = [random.gauss(0, 10) for _ in range(n)]
        pd.Series(nd_data).to_csv("nd_data.csv", encoding="utf-8", sep=";")
    elif distribution == "uniform":
        uniform_data = [random.uniform(-10, 10) for _ in range(n)]
        pd.Series(uniform_data).to_csv("uniform_data.csv", encoding="utf-8", sep=";")
    elif distribution == "exponential":
        exponential_data = [random.expovariate(1) for _ in range(n)]
        pd.Series(exponential_data).to_csv("exponential_data.csv", encoding="utf-8", sep=";")
    else:
        print("Übergeben sie einen gültiges Arguement für die Verteilung.")


if __name__ == "__main__":

    generate_nd_csv(100, "exponential")

