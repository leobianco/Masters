import numpy as np

class Model:
    """ (TODO): Add docstring for Model class.
    """

    def __init__(self, n, k, Q, M=None, ask_priors=True):
        # Basic attributes
        self.n = n
        self.k = k
        self.Q = Q
        self.q_0 = self.Q[0,0]
        # Class priors
        self.class_priors_string=""
        if ask_priors:
            self.class_priors_string\
                    = input("Enter class priors (sep. by space): ") 
        if not self.class_priors_string:  
            # Symmetric SBM, uniform priors
            self.class_priors = [1/self.k]*self.k
        else:
            self.class_priors\
                    = [float(i) for i in self.class_priors_string.split()]
        # Indicatrix of classes
        self.indicatrix = np.array([], dtype=int)
        for i in range(self.k):
            if (i < self.k-1):
                self.indicatrix = np.append(self.indicatrix, 
                        [i]*int(self.class_priors[i]*self.n))
            else:
                self.indicatrix = np.append(self.indicatrix,
                        [i]*(n - len(self.indicatrix)))  # ensures len==n
        # Selecting matrix J
        self.J = np.zeros((self.n, self.k))
        self.J[np.arange(self.n), self.indicatrix] = 1
        # Size of classes
        self.class_sizes = np.diag(self.J.T @ self.J)
        # Random diagonal matrix M
        if M is None:
            self.order = float(input("Enter an order for M (default 150): ")
                    or "150") 
            m = np.random.rand(k)
            self.M = self.order*np.diag(m)
        else:
            self.M = M
            self.order = np.round(M[0,0])
        # Inter-cluster affinities C
        self.C = 1 + self.M/np.sqrt(n)
        # Assignment matrix (augmented C)
        self.CA = self.J @ self.C @ self.J.T
        # Class densities c
        self.c = np.diag(self.J.T @ self.J)/n
        # Deterministic component
        self.EA = self.Q @ self.CA @ self.Q
        # Affinity matrix
        self.A = np.random.binomial(n=1, p=self.EA)
        # Degree matrix
        self.D = np.sum(self.A, 0)
        # Random component
        self.V = self.A - self.EA
        # Modularity matrix B
        self.B = self.A - self.Q @ np.ones((self.n, self.n)) @ self.Q
        # Normalized modularity
        self.B_norm = (self.A - np.outer(self.D, self.D)/np.sum(self.D))
        # Eigendata
        self.B_evals, self.B_evecs = np.linalg.eig(1/np.sqrt(self.n)*self.B)
        self.B_evals, self.B_evecs = np.real(self.B_evals), np.real(self.B_evecs)
        self.sorted_idx_evals = np.argsort(self.B_evals)
        self.B_evals = self.B_evals[self.sorted_idx_evals]
        self.B_evecs = self.B_evecs[:, self.sorted_idx_evals]

    # Methods
