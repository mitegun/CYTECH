#corrigé étude de cas sur l'arbitrage de change
from itertools import permutations
import pandas as pd
import math
import networkx as nx
#On suppose que les sommets du graphe sont numérotés de 
#0 à nbSommets-1
#La fonction suivante retourne tous les ensembles de 
#sommets de taille >= 2
#ne contenant pas le sommet numSommet

def sousEnsemblesSansUnElement(nbSommets, numSommet):
    p = []
    i, imax = 0, 2**nbSommets-1
    while i <= imax:
        s = []
        j, jmax = 0, nbSommets-1
        while j <= jmax:
            if (i>>j)&1 == 1:
                s.append(j)
            j += 1
        if ((len(s)>1)&(numSommet not in s)):      
          p.append(s)
        i += 1 
    return p
	
#La fonction suivante retourne toutes les permutations 
#d'une liste
    
def toutesLesPermutations(l):
   lret = []
   lTuples = list(permutations(l))
   
   for i in range(len(lTuples)):
       lret.append(list(lTuples[i]))
       
   return lret

#La fonction suivante affiche le contenu d'un cycle.
#Un cycle est défini par un sommet d+une liste d'autres sommets

def afficherCycle(numd, ld):
    cycle = [numd]+ld
    
    for nums in range(0, len(cycle)):
        numdev1 = cycle[nums]
        numdev2 = cycle[(nums+1)%len(cycle)]
        dev1 = devises[numdev1]
        dev2 = devises[numdev2]
        print(dev1, "-->", dev2, " : ", float(df.iat[numdev2,numdev1+1]))

#La fonction suivante retourne somme(log(poids)) 
#sur tous les arcs du cycle.
#Un cycle est défini par un sommet d+une liste 
#d'autres sommets

def valeurCycle(numd, ld):
    cycle = [numd]+ld
    valCycle = 0
    
    for nums in range(0, len(cycle)):
        numdev1 = cycle[nums]
        numdev2 = cycle[(nums+1)%len(cycle)]
        valCycle += math.log2(float(df.iat[numdev2,numdev1+1]))
    return valCycle    
    
#Programme principal
#lecture et affichage des données

df = pd.read_csv('5-Arbitrage/tauxchange.csv')
devises = list(df.columns)[1:]
nbDevises = len(devises)

for numd1 in range(nbDevises):
  for numd2 in range(nbDevises):
    print(numd1, numd2, devises[numd1], devises[numd2], df.iat[numd2,numd1+1])

meilleureVal = 0
meilleurCycle = []
meilleureDevise = 0
for numdev in range(nbDevises):
  se = sousEnsemblesSansUnElement(nbDevises,numdev)

  tousLesCycles = []
  for nume in range(len(se)):
    lret = toutesLesPermutations(se[nume])
    tousLesCycles +=lret

  for numc in range(0, len(tousLesCycles)):
    c = tousLesCycles[numc]
    v = valeurCycle(numdev,c)
    if (v>meilleureVal):
      print(numdev,c,"\n")
      meilleureDevise = numdev
      meilleureVal = v
      meilleurCycle = c
	  
	
print("=======================================","\n")
afficherCycle(meilleureDevise,meilleurCycle)
print("Valeur = ",2**meilleureVal,"\n")
print("=======================================","\n")

import matplotlib.pyplot as plt

def dessinerCycle(numd, ld):
    cycle = [numd] + ld
    edges = []
    labels = {}
    
    for nums in range(len(cycle)):
        numdev1 = cycle[nums]
        numdev2 = cycle[(nums + 1) % len(cycle)]
        dev1 = devises[numdev1]
        dev2 = devises[numdev2]
        edges.append((dev1, dev2))
        labels[(dev1, dev2)] = round(float(df.iat[numdev2, numdev1 + 1]), 4)
    
    G = nx.DiGraph()
    G.add_edges_from(edges)
    
    pos = nx.circular_layout(G)
    nx.draw(G, pos, with_labels=True, node_size=3000, node_color='skyblue', font_size=10, font_weight='bold', arrowsize=20)
    nx.draw_networkx_edge_labels(G, pos, edge_labels=labels, font_color='red')
    
    plt.title("Arbitrage Cycle")
    plt.show()


dessinerCycle(meilleureDevise, meilleurCycle)