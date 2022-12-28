import networkx as nx
import pandas as pd

def csv_to_networkx(name):
    path = f'./data/{name}.csv'
    pdgraph = pd.read_csv(path)
    graph = nx.from_pandas_edgelist(pdgraph, source='# source', target=' target')
    return graph
