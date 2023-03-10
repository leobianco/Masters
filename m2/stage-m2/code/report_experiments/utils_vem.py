import numpy as np
import numpy.matlib
import matplotlib.pyplot as plt
import networkx as nx
import itertools


def draw_graph(tau, A, communities=None, permutation=None):
    """Wrapper for networkx drawing capabilities.
    
    Args:
        tau ((n, k) np.array): matrix of variational parameters.
        A ((n, n) np.array): adjacency matrix of the graph to be shown.
        communities (n np.array): vector with true community labels. (if known)
        permutation ((k,) np.array): permutation of labels maximizing accuracy.
        (if known)

    Returns:
        None (pyplot window with graph)
    """

    n = A.shape[0]
    G = nx.from_numpy_matrix(A)
    pos = nx.spring_layout(G)
    # MADE CHANGES HERE
    if communities is None:
        nx.draw(G, pos=pos, node_color=np.argmax(MAP(tau), axis=1), with_labels=False)
    else:
        Z_v_hat = np.argmax(MAP(tau[:, permutation]), axis=1)
        nx.draw(G, pos=pos, node_color=communities, with_labels=False)
        labels={}
        for i in range(n):
            labels[i]=f'${Z_v_hat[i]}|{communities[i]}$'
        nx.draw_networkx_labels(G, pos=pos, labels=labels, font_color='r')
    plt.show();


def extract_upper_triang(A):
    """Extracts the upper triangular part of a matrix.

    Args:
        A ((n, n) np.array): matrix to have triangular part extracted.

    Returns:
        ((n, n) np.array): upper triangular part of A.
    """

    return np.triu(A) - np.diag(np.diag(A))


def normalize_rows(A):
    """Normalizes a matrix row-wise (no inbuilt method in numpy for this !).

    Args:
        A ((n, n) np.array): matrix to be normalized row-wise.

    Returns:
        norm_A ((n, n) np.array): row-wise normalized version of A.
    """
    
    sA = np.sum(A, axis=1)  # sum along rows
    k = A.shape[1]

    norm_A = A / np.matlib.repmat(sA.T, k, 1).T
    
    return norm_A


def print_info(i, ELBO_prev, diff_ELBO, tau, Gamma, Pi):
    """Prints parameter information for the i-th iteration of EM. I
    just coded this function so that the code in variational_em.py is cleaner.
    
    Args:
        i (int): iteration index.
        var_em_object (VariationalEM object): the algorithm object.
    """

    print('----------')
    print('\n', i, ' iterations \n')
    print('Current ELBO: ', ELBO_prev, '\n')
    if i>0:
        print('Current ELBO variation: ', diff_ELBO, '\n')
    print('Current Tau: \n', tau[:4,:], '\n ... \n', tau[-5:,:], '\n')
    print('Current Gamma: \n', Gamma, '\n')
    print('Current Pi: \n', Pi, '\n') 
    print('----------')


def MAP(tau):
    """Estimates the communities given tau via maximum a posteriori estimator.

    Args:
        tau ((n, k) np.array): matrix of variational estimators.

    Returns
        Z_hat ((n, k) np.array): matrix of MAP estimated communities.
    """

    n, k = tau.shape
    max_lines_tau = np.matlib.repmat(np.max(tau, axis=1), k, 1).T
    Z_hat = np.where(tau >= max_lines_tau, 1, 0)

    return Z_hat


def accuracy(tau, Z):
    """Calculates the maximal classification accuracy of the MAP estimator under
    permutations of tau.

    Args:
        tau ((n, k) np.array): matrix of variational parameters.
        Z ((n, k) np.array): ground truth matrix of communities. 

    Returns:
        accuracy (float): maximal percentage of correct class predictions.
        saved_permutation (k np.array): ``correct'' permutation of tau for
        labels.
    """

    n, k = Z.shape
    accuracy = 0
    # Permute columns of tau
    all_permutations = list(itertools.permutations(list(range(k))))
    saved_permutation = None
    for permutation in all_permutations:
        tau_permuted = tau[:, permutation]
        Z_hat = MAP(tau_permuted)
        curr_accuracy = np.mean(np.all(Z_hat==Z, axis=1))
        if curr_accuracy > accuracy:
            accuracy = curr_accuracy
            saved_permutation = permutation

    return accuracy, saved_permutation

def save_graph(file_name, Gamma, Pi, Z, Z_v, A):
    """Saves graphs where I obtained good results."""

    with open('saved_graphs/'+file_name+'.npz', 'wb') as f:
        np.savez(f, name1=Gamma, name2=Pi, name3=Z, name4=Z_v, name5=A)


def load_graph(file_name):
    """Loads saved graphs

    Args:
        file_name (string): name of file to be loaded.

    Returns:
        Parameters and sample information.
    """

    with open('saved_graphs/'+file_name+'.npz', 'rb') as f:
        container = np.load(f)
        Gamma, Pi, Z, Z_v, A = [container[key] for key in container]

    return Gamma, Pi, Z, Z_v, A
