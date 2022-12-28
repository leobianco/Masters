import numpy as np


def isolated_eigenvalues(model):
    """Calculates isolated eigenvalues for homogeneous case.

    Attributes of the model used:
        M = diagonal matrix
        c = vector with classes density |C_i|/n
    """
    
    q_0 = model.Q[0,0]
    lambd = []  # stores eigenvalues outside the bulk
    for i in range(model.k):
      if (model.M[i,i] * model.c[i] > np.sqrt(1-q_0**2)/q_0):
        isolated_eigval = q_0**2 * model.M[i,i] * model.c[i] \
                + (1-q_0**2)/(model.M[i,i] * model.c[i])
        lambd.append(isolated_eigval)
  
    return lambd


def alignments(model):
    """(TODO): Write function docstring.
    """

    # Take the k largest eigenvectors and see them as lines instead of columns.
    largest_eigenvectors = model.B_evecs[:,-model.k:].T

    # Observed alignments (already selects class by taking max)
    J_normalised = (model.J.T/np.sqrt(np.expand_dims(model.class_sizes, axis=1))).T
    alignments = np.max(np.power(largest_eigenvectors @ J_normalised, 2), axis=1)

    # Theoretical alignments
    theoretical_align = np.zeros(model.k)
    for i in range(model.k):
        if (model.M[i,i] * model.c[i] 
                > np.sqrt(1-model.q_0**2)/model.q_0):
            theoretical_align[i] = 1 - (model.q_0**2 * (1-model.q_0**2)\
                    /((model.q_0**2 * model.M[i,i] * model.c[i])**2))

    return np.real(alignments), np.real(theoretical_align)


