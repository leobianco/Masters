import numpy as np
import argparse
from sbm import SBM
#from spectral_clustering import SpectralClustering
#from sklearn.cluster import SpectralClustering as skSpectralClustering
from utils import *
import matplotlib.pyplot as plt


parser = argparse.ArgumentParser()
parser.add_argument(
        '-n', help='Number of points in graph to be generated',
        type=int, default=100
        )
parser.add_argument(
        '-vis', '--visual',
        help='Whether to visualize generated graph or not.', action='store_true'
        )
parser.add_argument(
        '-layout', help='What layout to use (spring, cir',
        type=str, default='spring'
        )
parser.add_argument(
        '-vertex_size', help='Size of vertices',
        type=float, default=None
        )
parser.add_argument(
        '-edge_width', help='Width of edges',
        type=float, default=.5
        )
parser.add_argument(
        '-edge_color', help='Color of edges in R,G,B format',
        type=str, default=None
        )
parser.add_argument(
        '-edge_curve', help='How curved edges are',
        type=float, default=0.05
        )
parser.add_argument(
        '-model', help='SBM or ER',
        type=str, default='sbm'
        )
args, extras = parser.parse_known_args()


def main():
    # Generate SBM graph
    with open('gamma.txt', 'r') as gamma_file:
        Gamma = np.array([[float(eval(entry)) for entry in line.split(',')] for line in gamma_file])
    with open('pi.txt', 'r') as pi_file:
        Pi = np.array([[float(eval(entry)) for entry in line.split(',')] for line in pi_file])
    model = SBM(args.n, Gamma, Pi)
    Z, Z_v, A = model.sample()

    if args.visual:
        draw_graph(A, Z_v)
        
    # Always save the tikz
    graph_tikz(
            A, Z_v, args.layout,
            args.vertex_size, args.edge_width, args.edge_color,
            args.edge_curve, args.model)


if __name__=="__main__":
    main()
