import math
import random

if __name__ == "__main__":

    c = 0
    for i in range(1000000):
        x, y = random.uniform(-1, 1), random.uniform(-1, 1)
        if math.sqrt(x ** 2 + y ** 2) <= 1:
            c += 1
        if i == 10000:
            print((c / 10000) * 4)
        if i == 100000:
            print((c / 100000) * 4)

    print((c / 1000000) * 4)

    '''
    Wir werden mit steigendem n (wobei n die Zahl der Iterationen ist) nur schleppend genauer. Für jede korrekte
     (oder annährend korrekte), neue Nachkommastelle müssen wir unser n mit 10 multiplizieren. Allerdings liegen 
     viele Stichproben mit hohem n auch tendenziell häufiger daneben nach der zweiten Nachkommastelle.
    Die Komplexität steigt also exponentiell.
    
    pi = 3.14159
    10.000: 3.17
    100:000: 3.14632
    1.000.000: 3.141016
    '''
