import math
import matplotlib.pyplot as plt

def calculate_mean(l: list) -> float:
    return sum(l) / len(l)

def calculate_median(l: list) -> int or float:
    if len(l) % 2 != 0:
        return l[int((len(l)+1)/2)-1]
    else:
        i = (len(l)/2)-1
        j = (len(l)/2)
        x = l[int(i)] + l[int(j)]

        return x/2

def calculate_variance(l: list) -> float:
    sum = 0
    mean = calculate_mean(l)
    for entry in l:
        sum += (entry - mean) ** 2

    return sum/len(l)

def calculate_standard_deviaton(l: list) -> float:
    return math.sqrt(calculate_variance(l))

def calculate_coefficient_of_variation(l: list) -> float:
    return calculate_standard_deviaton(l) / calculate_mean(l)

def calculate_modalwert(l: list) -> int or list:

    max = []
    max_count = 0
    for entry in l:
        if l.count(entry) > max_count:
            max = [entry]
            max_count = l.count(entry)
        elif l.count(entry) == max_count and entry not in max:
            max.append(entry)

    return max if len(max) > 1 else max[0]


if __name__ == '__main__':

    age = []
    distance = []
    with open('Data_Exercise_1.txt', 'r') as f:
        for line in f:
            if line[:2].isnumeric():
                age.append(int(line[:2]))
                temp = line[2:]
                i = temp.find('f')
                j = temp.find('m')
                if j == -1:
                    temp = temp[:i]
                if i == -1:
                    temp = temp[:j]

                distance.append(float(temp.strip()))



    age.sort()
    distance.sort()

    print(f"Der Altersdurschnitt liegt bei {calculate_mean(age)}\n"
          f"Der Median liegt bei {calculate_median(age)}\n"
          f"Die Varianz des Alters liegt bei {calculate_variance(age)}\n"
          f"Die Standardabweichung liegt bei {calculate_standard_deviaton(age)}\n"
          f"Der Variationskoeffizient liegt bei {calculate_coefficient_of_variation(age)}\n"
          f"Der/Die Modalwerte sind {calculate_modalwert(age)}\n\n")

    print(f"Der mittlere Entfernung vom Geburtsort liegt bei {calculate_mean(distance)}\n"
          f"Der Median liegt bei {calculate_median(distance)}\n"
          f"Die Varianz der Entfernung vom Geburtsort liegt bei {calculate_variance(distance)}\n"
          f"Die Standardabweichung liegt bei {calculate_standard_deviaton(distance)}\n"
          f"Der Variationskoeffizient liegt bei {calculate_coefficient_of_variation(distance)}\n "
          f"Der/Die Modalwerte sind {calculate_modalwert(distance)}\n")
    '''
    Der Altersdurschnitt liegt bei 23.227272727272727
    Der Median liegt bei 22.5
    Die Varianz des Alters liegt bei 9.72107438016529
    Die Standardabweichung liegt bei 3.117863752662276
    Der Variationskoeffizient liegt bei 0.13423288171931522
    Der/Die Modalwerte sind [20, 21, 22, 25, 26]
    
    
    Der mittlere Entfernung vom Geburtsort liegt bei 1824.6931818181818
    Der Median liegt bei 135.0
    Die Varianz der Entfernung vom Geburtsort liegt bei 15174410.13881715
    Die Standardabweichung liegt bei 3895.434525032753
    Der Variationskoeffizient liegt bei 2.134843580196436
    Der/Die Modalwerte sind 0.0
    '''