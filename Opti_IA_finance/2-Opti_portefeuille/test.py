import numpy as np
import matplotlib.pyplot as plt

# Fonction pour simuler un mouvement brownien standard avec Wn = Wn-1 + g * sqrt(t)
def simulate_brownian_formula(T, n, m):
    """
    T : Durée (en années)
    n : Nombre de périodes de simulation
    m : Nombre de trajectoires simulées
    """
    dt = T / n  # Intervalle de temps
    time_grid = np.linspace(0, T, n + 1)  # Grille de temps
    W = np.zeros((m, n + 1))  # Initialiser les trajectoires à zéro
    
    # Générer les incréments du mouvement brownien
    for i in range(1, n + 1):
        g = np.random.normal(0, 1, m)  # Variables normales centrées
        W[:, i] = W[:, i - 1] + g * np.sqrt(dt)  # Formule Wn = Wn-1 + g * sqrt(dt)

    return time_grid, W

# Fonction pour calculer espérance et variance avec Monte Carlo
def monte_carlo_expectation_variance(W):
    # Espérance (moyenne) sur toutes les trajectoires à chaque point dans le temps
    expectation = np.mean(W, axis=0)
    
    # Variance sur toutes les trajectoires à chaque point dans le temps
    variance = np.var(W, axis=0)
    
    return expectation, variance

# Fonction pour tracer les trajectoires et montrer espérance et variance
def plot_brownian_with_expectation_variance(time_grid, W, expectation, variance, num_paths_to_plot=5):
    plt.figure(figsize=(12, 6))

    # Tracer les trajectoires simulées
    for i in range(min(num_paths_to_plot, W.shape[0])):
        plt.plot(time_grid, W[i, :], lw=1.5)

    # Tracer l'espérance
    plt.plot(time_grid, expectation, color='red', lw=2, label="Espérance")
    
    # Tracer +/- 1 écart-type (racine carrée de la variance) autour de l'espérance
    plt.fill_between(time_grid, expectation - np.sqrt(variance), expectation + np.sqrt(variance),
                     color='orange', alpha=0.3, label="± 1 écart-type")
    
    plt.axhline(0, color='black', lw=1)  # Ligne de symétrie
    plt.title('Simulation de Mouvement Brownien avec $W_n = W_{n-1} + g \\sqrt{t}$')
    plt.xlabel('Temps (années)')
    plt.ylabel('Valeur du Mouvement Brownien')
    plt.legend()
    plt.grid(True)
    plt.show()

# Paramètres d'exemple
T = 2  # Durée de la simulation (1 an)
n = 100  # Nombre de pas de temps (discrétisation, jours de bourse)
m = 1000  # Nombre de trajectoires simulées

# Simulation du mouvement brownien avec Wn = Wn-1 + g * sqrt(t)
time_grid, W = simulate_brownian_formula(T, n, m)

# Calcul de l'espérance et de la variance via Monte Carlo
expectation, variance = monte_carlo_expectation_variance(W)

# Visualisation des résultats
plot_brownian_with_expectation_variance(time_grid, W, expectation, variance, num_paths_to_plot=5)
