"""Simulations module

simulation_1():
    "Preliminary observations" - There are three possible cases.

simulation_2():
    "Homogeneous case" - Calculates asymptotic eigenvalues and alignments of eigenvectors.

simulation_3():
    "Homogeneous case" - Calculates asymptotic alignment of eigenvectors for growing n.

simulation_4():
    "Homogeneous case" - Performs community detection.
"""

from model import Model
from utils import *
import numpy as np
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
import warnings
warnings.filterwarnings('ignore')  # use only after debugging


def simulation_1():
    """Shows spectrum and extremal eigenvectors for the chosen case.
    """
    
    # Input basic variables
    n = int(input('Enter number of points (n, default 1000): ') or "1000")
    k = 3
    case = int(input('Enter case desired (1, 2, or 3): '))
    
    # Build Q from case
    q = 0*np.ones(n)  # initialise

    # CASE 1
    if case == 1:
        q_0 = float(input("Enter q_0 (default 0.05): ") or "0.05")
        q = q_0*np.ones(n)

    # CASE 2
    if case == 2:
        epsilon = 0.25
        q_0 = 0.5
        q = np.random.uniform(low=q_0-epsilon, high=q_0+epsilon, size=n)

    # CASE 3
    if case == 3:
        q_1 = float(input("Enter q_1 (default 0.5): ") or "0.5") 
        q_2 = float(input("Enter q_2 (default 0.05): ") or "0.05") 
        q = np.random.choice(np.array([q_1,q_2]), size = n)

    Q = np.diag(q)
    
    # Build the model and take eigenvalues
    model = Model(n, k, Q)

    # Graphical representations
    # Spectrum
    isolated_evals = isolated_eigenvalues(model)
    plt.hist(model.B_evals, density=True, bins=100)
    plt.title(r'Empirical spectral distribution of $B/\sqrt{n}$')
    plt.xlabel(r'Spectrum of $B/\sqrt{n}$')
    plt.ylabel('Density') 
    annotation_string = f'n = {n} \nM ~ {int(model.order)} \nq_0 \
= {np.round(model.Q[0,0],2)} \nn. of spikes = {len(isolated_evals)}'
    plt.text(0.75, 0.8, annotation_string, transform=plt.gca().transAxes)
    # Indicate spikes clearly, if any (attention: only for homogeneous case)
    if case == 1:
        for _, val in enumerate(isolated_evals):
            plt.annotate('Spike', xy=(val, 0), xycoords='data',
                    xytext=(0, 10), textcoords='offset points',
                    ha='center', color='red', rotation=45
                    )

    # Eigenvectors
    plt.figure()
    plt.title(f'Extremal ({model.k} largest) eigenvectors')  # not showing
    for i in range(model.k):
        plt.subplot(model.k, 1, i+1)
        plt.plot(model.B_evecs[:,-i-1])
        plt.annotate(r'$\lambda =$ '+f'{np.round(model.B_evals[-i-1], 4)}',
                xy=(0.9,0.9), xycoords='axes points')
        for j in range(model.k):
            plt.axvline(x=np.sum(model.class_sizes[:j+1]), c='gray', linestyle='--')
        if i < model.k-1:
            plt.xticks([])
    plt.xlabel('Dimensions')
    plt.show()


def simulation_2():
    """Calculates the asymptotics of eigenvalues.
    """

    # Input basic variables
    n = int(input('Enter number of points (n, default 1000): ') or "1000")
    k = 3
    q_0 = float(input('Enter q_0 (default 0.05): ') or '0.05')
    Q = np.diag(q_0*np.ones(n))
    
    # Build model 
    model = Model(n, k, Q)

    # Eigenvalues
    lambd = isolated_eigenvalues(model)
    n_isolated_eigvals = len(lambd)
    
    # Graphical representation (Eigenvalues)
    color = iter(plt.cm.rainbow(np.linspace(0, 1, len(lambd))))
    plt.hist(model.B_evals, density=True, bins=100)
    for i in range(len(lambd)):
        plt.axvline(lambd[i], c=next(color), linestyle='dashed')
    plt.title(f"Number of isolated eigenvalues: {n_isolated_eigvals}.")
    plt.show()


def simulation_3():
    """Calculates alignements of eigenvectors for growing n.
    """
    
    # Basic variables
    min_n = int(input("Enter minimal n (default 200): ") or "200")
    max_n = int(input("Enter maximal n (default 2000): ") or "2000")+200
    step_n = int(input("Enter step for n (default 200): ") or "200")
    list_n = list(range(min_n, max_n, step_n))
    k = 3
    q_0 = float(input("Enter q_0 (default 0.05): ") or "0.05")

    # Fix diagonal M
    order = float(input("Enter an order for M (default 100): ") or "100")
    m = order*np.random.rand(k)
    M = np.diag(m)

    # For saving results
    alignments_final = []
    alignments_theoretical = []

    # Main loop
    for i, n in enumerate(list_n):
        # Create model with n points but fixed M
        q = q_0*np.ones(n)
        Q = np.diag(q)
        model_n = Model(n, k, Q, M, ask_priors=False)
        
        # Isolated eigenvectors
        alignments_n, theoretical_alignments_n = alignments(model_n)

        np.set_printoptions(precision=3, suppress=True)
        print(f'n = {n} \n')
        print('Observed alignments: ', np.array_str(alignments_n, precision=4), '\n')
        print('Theoret. alignments: ', np.array_str(theoretical_alignments_n, precision=4), '\n')

        # Register iteration
        alignments_final.append(alignments_n)
        alignments_theoretical.append(theoretical_alignments_n)
    
    # Transform into convenient format
    alignments_final = np.array(alignments_final)
    alignments_theoretical = np.array(alignments_theoretical)

    # Print M to know its influence on observations
    print('M: ', m)

    # Graphical representation
    # Observed alignments
    for i in range(k):
        plt.plot(list_n, alignments_final[:,i], label=f'Observed alignments {i+1}')
        plt.plot(list_n, alignments_theoretical[:, i], linestyle='dashed', label=f'Predicted {i+1}')
        plt.ylim([0,1])
        plt.legend()
    plt.show()


def simulation_4():
    """Performs community detection by K-means on rows of isolated eigenvectors.
    """

    # Take isolated eigenvectors
    n = int(input('Enter number of points (n, default 1000): ') or "1000")
    k = 3
    q_0 = float(input('Enter q_0 (default 0.1): ') or '0.1')
    Q = np.diag(q_0*np.ones(n))
    
    # Build model 
    model = Model(n, k, Q)

    # Perform K-means clustering on the rows of k largest eigenvectors.
    kmeans_predictions = KMeans(n_clusters=k).fit_predict(model.B_evecs[:,-k:])
    print("Accuracy: ", np.mean(kmeans_predictions == model.indicatrix))

    # For n=1000, q_0=0.1, M=150, I get 0.968 accuracy. 


