import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import argparse
import utils_vem
import utils_spectral_clustering
from utils import *
from spectral_clustering import SpectralClustering
from variational_em import VariationalEM


parser = argparse.ArgumentParser()
parser.add_argument('-data', help='name of network to load', type=str)
parser.add_argument('-k', help='number of communities', type=int)
# For VEM
parser.add_argument('-maxiter', help='maximum number of iterations', type=int,
        default=100)
parser.add_argument('-tolELBO', help='tolerance to consider elbo converged',
        type=float, default=10**(-6))
parser.add_argument('-verbose', help='verbose mode',
        action='store_true')
# For Spectral Clustering
parser.add_argument(
        '-unn', '--unnormalized',
        help='Whether to use unnormalized Laplacian or not',
        action='store_true'
        )
parser.add_argument(
        '-lar', '--largest',
        help='whether to use largest (Rohe) or smallest (von Luxburg) k\
        eigenvectors of the Laplacian.', action='store_true'
        )

args = parser.parse_args()


def main():
    graph = csv_to_networkx(args.data)
    A = nx.adjacency_matrix(graph).todense()

    # Variational EM
#    var_em = VariationalEM(A, args.k)
#    var_em.run(
#            max_iter=args.maxiter,
#            tol_diff_ELBO=args.tolELBO,
#            verbose=args.verbose)
#    print('tau: ', var_em.tau)
#    utils_vem.draw_graph(var_em.tau, A)

    # Spectral clustering
    scAlg = SpectralClustering(args.k)
    labels, eigvecs, eigvals, centroids = scAlg.cluster(
            A, args.unnormalized, args.largest)
    utils_spectral_clustering.draw_graph(graph, labels)
    return

if __name__=='__main__':
    main()
