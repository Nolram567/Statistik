import numpy as np

if __name__ == "__main__":

    # Es werden 10 Ratten entnommen
    n = 10

    # Die Wahrscheinlichkeit A, für den Phänotyp "Krebs"
    p = float(input("Geben sie die Wahrscheinlichkeit an: "))

    # 100 Simulationen
    num_simulations = 100

    simulated_data = np.random.binomial(n, p, num_simulations)

    print(f"Nach {num_simulations} Iterationen war P(A) jeweils: {simulated_data.tolist()}\n Für den Phänotyp 'Kein Krebs' ergibt sich die Ereignisfolge: {[10-e for e in simulated_data.tolist()]}")


