import matplotlib.pyplot as plt
import numpy as np

# Liste des utilisateurs
users = [44, 318, 387, 414, 448, 599, 600, 610]

# Données fictives (en secondes) pour chaque configuration
# Remplacez ces valeurs par vos mesures réelles.
times_1_module = [1.7119262, 2.8683687, 3.4228886, 1.3394975, 3.6968618, 3.0722556, 2.5624956, 4.0110278]
times_2_modules = [1.141063, 2.0774009, 2.4187035, 1.1069038, 2.5636984, 2.3725141, 1.6856721, 2.5991162]
times_4_modules = [0.828193, 1.5451718, 1.6972813, 0.7095557, 2.0161447, 1.5671228, 1.3897856, 1.8454371]

# Position sur l'axe des x pour chaque utilisateur
x = np.arange(len(users))
width = 0.25  # largeur de chaque barre

# Création du graphique
fig, ax = plt.subplots(figsize=(10, 6))
rects1 = ax.bar(x - width, times_1_module, width, label='1 module')
rects2 = ax.bar(x, times_2_modules, width, label='2 modules')
rects3 = ax.bar(x + width, times_4_modules, width, label='4 modules')

# Ajout des labels et du titre
ax.set_xlabel('Utilisateur')
ax.set_ylabel("Temps d'exécution (secondes)")
ax.set_title("Comparaison du temps d'exécution de notre pipeline")
ax.set_xticks(x)
ax.set_xticklabels(users)
ax.legend()

# Fonction pour annoter les barres avec leur valeur
def autolabel(rects):
    for rect in rects:
        height = rect.get_height()
        ax.annotate(f'{height:.2f}',
                    xy=(rect.get_x() + rect.get_width() / 2, height),
                    xytext=(0, 3),  # décalage vertical
                    textcoords="offset points",
                    ha='center', va='bottom')

autolabel(rects1)
autolabel(rects2)
autolabel(rects3)

plt.tight_layout()
plt.savefig("execution_time_comparison.png", dpi=300)
plt.show()
