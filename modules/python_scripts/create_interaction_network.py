

##############################################
## load the libraries
##############################################

import os
import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt


##############################################
## Load the data
##############################################

## load the interaction data
mir_interaction_data = pd.read_csv(
	'/media/linux/work/phd/projects/paget_interaction_network/data/mir_interaction_data_of_interest_filtered.csv',
	sep = ','
)

## load the interaction data
# mir_interaction_data = dt.fread(
# 	'/media/linux/work/phd/projects/paget_interaction_network/data/mir_interaction_data_of_interest_filtered.csv',
# )

print(mir_interaction_data)


##############################################
## CREATE THE METABOLIC PATHWAY NETWORK
##############################################


## initialize the graph
graph = nx.MultiGraph()


##########################
##### NODES of the graph
##########################

## extract the mirRNA into a vector
mir_vector = mir_interaction_data['miRNA'].tolist()
mir_vector = set(mir_vector)
mir_vector = list(mir_vector)

## extract the gene into a vector
gene_vector = mir_interaction_data['Target Gene'].tolist()
gene_vector = set(gene_vector)
gene_vector = list(gene_vector)


##########################
##### Add nodes and edge into the graph
##########################

## add the nodes into the graph
graph.add_nodes_from(mir_vector)
graph.add_nodes_from(gene_vector)

## add the edges into the graph
for i in range(1, len(mir_interaction_data.index), 1) :

	## extract the nodes associated with the interaction i
	node1 = (mir_interaction_data.loc[:, 'miRNA'].iloc[i,])
	node2 = (mir_interaction_data.loc[:, 'Target Gene'].iloc[i,])

	## add this interaction into the graph
	graph.add_edge(node1, node2)


#####################################################
## VISUALIZE THE KEGG NETWORK
#####################################################


print("Nodes of graph: ")
print(graph.number_of_nodes())
print("Edges of graph: ")
print(graph.number_of_edges())

nx.draw(
    graph, 
    with_labels=True,
    font_weight='bold'
)

plt.show()

#plt.savefig(
#	snakemake.output["interaction_network"],
#	format="svg"
#)


