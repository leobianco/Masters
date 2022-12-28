import numpy as np
import numpy.matlib
import matplotlib.pyplot as plt
import networkx as nx
import itertools
import seaborn as sns
from sklearn.decomposition import PCA
from network2tikz import plot


def accuracy(labels, Z_v):
    """Calculates the classification accuracy under label permutations.

    Args:
        labels ((n,) np.array): vector with k-means estimated community labels.
        Z_v ((n, k) np.array): ground truth vector of community labels.

    Returns:
        accuracy (float): maximal percentage of correct class predictions.
        saved_permutation (k np.array): ``correct'' permutation of labels.
    """

    k = np.unique(Z_v).shape[0]
    accuracy = 0
    all_permutations = list(itertools.permutations(list(range(k))))
    saved_permutation = None
    for permutation in all_permutations:
        labels_permuted = np.select([labels==i for i in range(k)], permutation)
        curr_accuracy = np.mean(labels_permuted==Z_v)
        if curr_accuracy > accuracy:
            accuracy = curr_accuracy
            saved_permutation = permutation

    return accuracy, saved_permutation


def visualize_eigenvectors(eigvecs, centroids, labels, permutation):

    k = centroids.shape[0]  # which is also the number of centroids
    pca = PCA(n_components=2).fit_transform(np.vstack((eigvecs, centroids)))

    # Build correct labels for centroids (must permute).
    #centroid_label = np.zeros(k)
    #for i in range(k):
    #    idx = np.argwhere(labels==i)[0]
    #    representant = eigvecs[idx,:]
    #    distances =\
    #           [np.linalg.norm(representant - centroids[j,:]) for j in range(k)]
    #    centroid = np.argmin(distances)
    #    labels_permuted = np.select([labels==i for i in range(k)], permutation)
    #    centroid_label[centroid] = labels_permuted[idx]

    # pca[:-k, :] = (pca[-k:, :])[centroid_label,:]  # correct labels
    plt.scatter(pca[:-k,0], pca[:-k,1])
    plt.scatter(pca[-k:,0], pca[-k:, 1], c='orange')
    for i in range(k):
        #plt.annotate(f'{int(centroid_label[i]) + 1}', (pca[-(i+1),0], pca[-(i+1), 1]),
        #        size=15, xytext=(10, 10), textcoords='offset points')
        plt.annotate(f'{i+1}', (pca[-(i+1),0], pca[-(i+1), 1]),
                size=15, xytext=(10, 10), textcoords='offset points')
    plt.show()


def visualize_eigenvalues(eigvals):
    k = eigvals.shape[0]
    plt.bar(x=list(range(k-1)), height=np.diff(eigvals))
    #plt.scatter(list(range(eigvals.shape[0])), eigvals, c='darkblue')
    plt.show()


def draw_graph(A, Z_v):
    """Wrapper for networkx drawing capabilities.

    Args:
        A ((n, n) np.array): adjacency matrix of the graph to be shown.
        permutation ((k,) np.array): permutation of labels maximizing accuracy.
        Z_v (n np.array): vector with true community labels.
        tau ((n, k) np.array): matrix of variational parameters.

    Returns:
        None (pyplot window with graph)
    """

    # networkx part
    n = A.shape[0]
    G = nx.from_numpy_matrix(A)
    pos = nx.spring_layout(G, k = 0.5*1/np.sqrt(n))  # k smaller, farther nodes
    nx.draw(G, pos=pos, node_color=Z_v, with_labels=False)
    plt.show();


def graph_tikz(A, Z_v, layout, vertex_size, edge_width, edge_color, edge_curve, model):

    # networkx part
    n = A.shape[0]
    k = len(np.unique(Z_v))
    G = nx.from_numpy_matrix(A)

    # For ER
    isol_nodes = [node for node,adj in G._adj.items() if adj=={}]
    non_isol_nodes = [x for x in list(range(n)) if x not in isol_nodes]
    indicatrix = [0 for i in list(range(n))]  # for styling isolated nodes
    for index in isol_nodes:
        indicatrix[index] = 1

    function_dict = {
            'shell': lambda x: nx.shell_layout(
                x,
                nlist=[np.argwhere(Z_v==c).flatten() for c in
                    list(range(k))],
                rotate=0
                ),
            'bipartite': lambda x: nx.bipartite_layout(x,
                np.argwhere(Z_v==0).flatten()),
            'random': nx.random_layout,
            'circular': nx.circular_layout,
            'spring': lambda x: nx.spring_layout(
                x, k = 5*1/np.sqrt(n), scale=3),
            'graphviz': lambda x: nx.nx_agraph.graphviz_layout(x, prog='neato'),
            'graphviz-dot': lambda x: nx.nx_agraph.graphviz_layout(x, prog='dot')
            }
    pos = function_dict[layout](G)


    # Tikz part
    style = {}
    if model=='sbm':
        colors = sns.color_palette(None, len(np.unique(Z_v)))
        colors = [(255*r, 255*g, 255*b) for r,g,b in colors]
        style['node_color'] = [colors[g-1] for g in Z_v]
        style['vertex_size'] = vertex_size if vertex_size is not None else 1/np.log(n)
    if model=='er':
        colors = [(87, 204, 153), (34, 51, 105)]
        style['node_color'] = [colors[g] for g in indicatrix]
        # vertex_size_list = [1/np.log(n) for i in list(range(n))]
        vertex_size_list = [0.4 for i in list(range(n))]
        for index in isol_nodes:
            vertex_size_list[index] = 0.01
        style['vertex_size'] = vertex_size_list
    #opacity_list = [0.7 for i in list(range(n))]
    #for index in isol_nodes:
    #    opacity_list[index] = 0.25
    style['node_opacity'] = 0.7
    style['edge_width'] = edge_width
    style['edge_color'] = eval(edge_color) if edge_color is not None else (211, 101, 130)
    style['edge_curved'] = edge_curve
    style['layout'] = pos
    plot(G, 'network.tex', **style, canvas=(20,20))


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
