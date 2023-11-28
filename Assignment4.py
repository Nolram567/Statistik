import random

if __name__ == "__main__":

    c = 0
    for i in range(1000):

        height_list = [random.gauss(180, 10) for _ in range(100)]

        if sum(height_list)/100 < 178:
            c += 1

    print(f"Realtive Häufigkeit: {c/1000}; Die Wahrscheinlichkeit berechnet mit dem Z-wert und der PHI-tabelle liegt bei 42%.")


