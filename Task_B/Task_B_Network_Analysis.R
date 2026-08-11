# ============================================================
# CIS6008 - TASK B
# AVIATION STAKEHOLDER NETWORK ANALYSIS
# ============================================================


# ------------------------------------------------------------
# 1. Import Dataset
# ------------------------------------------------------------

network_data <- read.csv(file.choose())


# ------------------------------------------------------------
# 2. View Dataset
# ------------------------------------------------------------

View(network_data)


# ------------------------------------------------------------
# 3. Inspect Dataset Structure
# ------------------------------------------------------------

dim(network_data)

names(network_data)

str(network_data)

head(network_data)

summary(network_data)


# ------------------------------------------------------------
# 4. Check Missing Values
# ------------------------------------------------------------

colSums(is.na(network_data))


# ------------------------------------------------------------
# 5. Check Relationship Types
# ------------------------------------------------------------

table(network_data$Relationship)


# ------------------------------------------------------------
# 6. Check Edge Weights
# ------------------------------------------------------------

summary(network_data$Weight)


# ------------------------------------------------------------
# 7. Identify All Unique Stakeholders
# ------------------------------------------------------------

stakeholders <- sort(
  unique(
    c(
      network_data$Source,
      network_data$Target
    )
  )
)

print(stakeholders)

length(stakeholders)


# ------------------------------------------------------------
# 8. Create Output Folders
# ------------------------------------------------------------

if (!dir.exists("Task_B_Figures")) {
  dir.create("Task_B_Figures")
}

if (!dir.exists("Task_B_Results")) {
  dir.create("Task_B_Results")
}


# ------------------------------------------------------------
# 9. Save Basic Dataset Information
# ------------------------------------------------------------

capture.output(
  {
    cat("TASK B - NETWORK DATASET INSPECTION\n")
    cat("===================================\n\n")
    
    cat("Number of Relationships (Edges):\n")
    print(nrow(network_data))
    
    cat("\nNumber of Stakeholders (Nodes):\n")
    print(length(stakeholders))
    
    cat("\nStakeholders:\n")
    print(stakeholders)
    
    cat("\nRelationship Types:\n")
    print(table(network_data$Relationship))
    
    cat("\nEdge Weight Summary:\n")
    print(summary(network_data$Weight))
    
    cat("\nMissing Values:\n")
    print(colSums(is.na(network_data)))
  },
  file = "Task_B_Results/B01_Dataset_Inspection.txt"
)

cat("\nTask B dataset inspection completed successfully.\n")



# ============================================================
# STEP B2 - FINAL NETWORK CONSTRUCTION AND VISUALIZATION
# ============================================================


# ------------------------------------------------------------
# 1. Install and Load igraph
# ------------------------------------------------------------

if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
} else {
  library(igraph)
}


# ------------------------------------------------------------
# 2. Create Output Folders
# ------------------------------------------------------------

if (!dir.exists("Task_B_Figures")) {
  dir.create("Task_B_Figures")
}

if (!dir.exists("Task_B_Results")) {
  dir.create("Task_B_Results")
}


# ------------------------------------------------------------
# 3. Construct Aviation Stakeholder Network
# ------------------------------------------------------------

# Undirected network is used because the analysis focuses on
# stakeholder coordination and interdependence.

aviation_network <- graph_from_data_frame(
  d = network_data,
  directed = FALSE
)

print(aviation_network)

cat("\nNumber of Nodes:", vcount(aviation_network), "\n")
cat("Number of Edges:", ecount(aviation_network), "\n")


# ------------------------------------------------------------
# 4. Assign Relationship Type and Weight to Edges
# ------------------------------------------------------------

E(aviation_network)$relationship <- network_data$Relationship
E(aviation_network)$weight <- network_data$Weight


# ------------------------------------------------------------
# 5. Edge Colours by Relationship Type
# ------------------------------------------------------------

relationship_colours <- c(
  "Commercial"  = "steelblue",
  "Operational" = "darkorange",
  "Regulatory"  = "firebrick",
  "Support"     = "forestgreen"
)

E(aviation_network)$color <-
  relationship_colours[E(aviation_network)$relationship]


# Edge thickness represents the supplied relationship weight.
# Weight is used only for visual emphasis here.

E(aviation_network)$width <-
  0.8 + (E(aviation_network)$weight / 4)


# ------------------------------------------------------------
# 6. Categorize Stakeholders
# ------------------------------------------------------------

stakeholder_category <- c(
  
  "Bandaranaike International Airport (CMB)" =
    "Airport",
  
  "Mattala Rajapaksa International Airport (MRIA)" =
    "Airport",
  
  "Civil Aviation Authority of Sri Lanka (CAASL)" =
    "Regulator",
  
  "Airport & Aviation Services SL (AASL)" =
    "Operator",
  
  "SriLankan Airlines" =
    "Airline",
  
  "Mihin Lanka" =
    "Airline",
  
  "International Airlines" =
    "Airline",
  
  "Air Traffic Control (ATC)" =
    "ATC",
  
  "Ground Handling Unit" =
    "Ground Ops",
  
  "Maintenance & Engineering" =
    "Ground Ops",
  
  "Customs & Immigration" =
    "Security/Border",
  
  "Sri Lanka Air Force" =
    "Security/Border",
  
  "Fuel Supply Companies" =
    "Support",
  
  "Tourism Authority" =
    "Support",
  
  "Cargo Operators" =
    "Support"
)


# ------------------------------------------------------------
# 7. Stakeholder Category Colours
# ------------------------------------------------------------

category_colours <- c(
  "Airport"          = "#4C72B0",
  "Regulator"        = "#C44E52",
  "Operator"         = "#8172B2",
  "Airline"          = "#55A868",
  "ATC"              = "#CCB974",
  "Ground Ops"       = "#64B5CD",
  "Security/Border"  = "#DD8452",
  "Support"          = "#937860"
)

V(aviation_network)$category <-
  stakeholder_category[V(aviation_network)$name]

V(aviation_network)$color <-
  category_colours[V(aviation_network)$category]


# ------------------------------------------------------------
# 8. Create Short Display Labels
# ------------------------------------------------------------

# These only change how names appear on the graph.
# Original stakeholder names remain unchanged in the network.

display_labels <- c(
  
  "Bandaranaike International Airport (CMB)" =
    "CMB Airport",
  
  "Mattala Rajapaksa International Airport (MRIA)" =
    "MRIA Airport",
  
  "Civil Aviation Authority of Sri Lanka (CAASL)" =
    "CAASL",
  
  "Airport & Aviation Services SL (AASL)" =
    "AASL",
  
  "SriLankan Airlines" =
    "SriLankan Airlines",
  
  "Mihin Lanka" =
    "Mihin Lanka",
  
  "International Airlines" =
    "International Airlines",
  
  "Air Traffic Control (ATC)" =
    "ATC",
  
  "Ground Handling Unit" =
    "Ground Handling",
  
  "Maintenance & Engineering" =
    "Maintenance & Eng.",
  
  "Customs & Immigration" =
    "Customs & Immigration",
  
  "Sri Lanka Air Force" =
    "SL Air Force",
  
  "Fuel Supply Companies" =
    "Fuel Suppliers",
  
  "Tourism Authority" =
    "Tourism Authority",
  
  "Cargo Operators" =
    "Cargo Operators"
)

V(aviation_network)$label <-
  display_labels[V(aviation_network)$name]


# ------------------------------------------------------------
# 9. Calculate Degree Centrality
# ------------------------------------------------------------

degree_values <- degree(
  aviation_network,
  mode = "all"
)

degree_centrality <- degree(
  aviation_network,
  mode = "all",
  normalized = TRUE
)


# Node size represents degree centrality.
# Reduced scaling improves readability.

V(aviation_network)$size <-
  9 + (degree_centrality * 32)


# ------------------------------------------------------------
# 10. Calculate Betweenness Centrality
# ------------------------------------------------------------

# weights = NA is intentional.
# The supplied Weight variable is not defined as a path
# distance/cost, therefore unweighted shortest paths are used.

betweenness_centrality <- betweenness(
  aviation_network,
  directed = FALSE,
  weights = NA,
  normalized = TRUE
)


# ------------------------------------------------------------
# 11. Identify Exactly the Top 3 Bridge Nodes
# ------------------------------------------------------------

bridge_order <- order(
  betweenness_centrality,
  decreasing = TRUE
)

top_bridge_indices <- bridge_order[1:3]

is_bridge <- rep(
  FALSE,
  vcount(aviation_network)
)

is_bridge[top_bridge_indices] <- TRUE


# Top 3 bridge nodes receive a red border

V(aviation_network)$frame.color <- ifelse(
  is_bridge,
  "red",
  "grey25"
)

V(aviation_network)$frame.width <- ifelse(
  is_bridge,
  3,
  1
)


# ------------------------------------------------------------
# 12. Label Formatting
# ------------------------------------------------------------

V(aviation_network)$label.color <- "black"
V(aviation_network)$label.cex <- 0.65


# ------------------------------------------------------------
# 13. Generate Improved Layout
# ------------------------------------------------------------

set.seed(123)

network_layout <- layout_with_fr(
  aviation_network,
  niter = 5000,
  weights = NA
)


# Spread nodes further apart
network_layout <- network_layout * 1.55


# ------------------------------------------------------------
# 14. Save Final Professional Network Graph
# ------------------------------------------------------------

png(
  "Task_B_Figures/B02_Final_Aviation_Stakeholder_Network.png",
  width = 2200,
  height = 1700,
  res = 180
)

par(
  mar = c(3, 3, 5, 3)
)

plot(
  aviation_network,
  
  layout = network_layout,
  
  vertex.size =
    V(aviation_network)$size,
  
  vertex.color =
    V(aviation_network)$color,
  
  vertex.frame.color =
    V(aviation_network)$frame.color,
  
  vertex.frame.width =
    V(aviation_network)$frame.width,
  
  vertex.label =
    V(aviation_network)$label,
  
  vertex.label.color =
    V(aviation_network)$label.color,
  
  vertex.label.cex =
    V(aviation_network)$label.cex,
  
  vertex.label.dist = 0.65,
  
  edge.color =
    E(aviation_network)$color,
  
  edge.width =
    E(aviation_network)$width,
  
  edge.curved = 0.05,
  
  main =
    "Sri Lanka Aviation Stakeholder Network"
)


# ------------------------------------------------------------
# 15. Stakeholder Category Legend
# ------------------------------------------------------------

legend(
  "topleft",
  
  legend =
    names(category_colours),
  
  pt.bg =
    category_colours,
  
  col = "grey25",
  
  pch = 21,
  
  pt.cex = 1.5,
  
  cex = 0.72,
  
  title =
    "Stakeholder Category",
  
  bty = "n"
)


# ------------------------------------------------------------
# 16. Relationship Type Legend
# ------------------------------------------------------------

legend(
  "bottomleft",
  
  legend =
    names(relationship_colours),
  
  col =
    relationship_colours,
  
  lty = 1,
  
  lwd = 4,
  
  cex = 0.72,
  
  title =
    "Relationship Type",
  
  bty = "n"
)


# ------------------------------------------------------------
# 17. Graph Explanation
# ------------------------------------------------------------

mtext(
  "Node size = degree centrality | Red border = top 3 bridge nodes | Edge thickness = supplied relationship weight",
  
  side = 1,
  line = 1,
  cex = 0.68
)

dev.off()


# ------------------------------------------------------------
# 18. Display Final Graph in RStudio
# ------------------------------------------------------------

plot(
  aviation_network,
  
  layout = network_layout,
  
  vertex.size =
    V(aviation_network)$size,
  
  vertex.color =
    V(aviation_network)$color,
  
  vertex.frame.color =
    V(aviation_network)$frame.color,
  
  vertex.frame.width =
    V(aviation_network)$frame.width,
  
  vertex.label =
    V(aviation_network)$label,
  
  vertex.label.color = "black",
  
  vertex.label.cex = 0.65,
  
  vertex.label.dist = 0.65,
  
  edge.color =
    E(aviation_network)$color,
  
  edge.width =
    E(aviation_network)$width,
  
  edge.curved = 0.05,
  
  main =
    "Sri Lanka Aviation Stakeholder Network"
)


legend(
  "topleft",
  
  legend =
    names(category_colours),
  
  pt.bg =
    category_colours,
  
  col = "grey25",
  
  pch = 21,
  
  pt.cex = 1.5,
  
  cex = 0.72,
  
  title =
    "Stakeholder Category",
  
  bty = "n"
)


legend(
  "bottomleft",
  
  legend =
    names(relationship_colours),
  
  col =
    relationship_colours,
  
  lty = 1,
  
  lwd = 4,
  
  cex = 0.72,
  
  title =
    "Relationship Type",
  
  bty = "n"
)


# ------------------------------------------------------------
# 19. Create Centrality Results Table
# ------------------------------------------------------------

centrality_table <- data.frame(
  
  Stakeholder =
    V(aviation_network)$name,
  
  Category =
    V(aviation_network)$category,
  
  Degree =
    as.numeric(degree_values),
  
  Degree_Centrality =
    round(
      as.numeric(degree_centrality),
      3
    ),
  
  Betweenness_Centrality =
    round(
      as.numeric(betweenness_centrality),
      3
    ),
  
  Critical_Bridge =
    ifelse(
      is_bridge,
      "Yes",
      "No"
    )
)


# Sort first by degree and then by betweenness

centrality_table <- centrality_table[
  order(
    -centrality_table$Degree_Centrality,
    -centrality_table$Betweenness_Centrality
  ),
]


print(centrality_table)


# Save table

write.csv(
  centrality_table,
  "Task_B_Results/B02_Centrality_Table.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 20. Save Top 3 Critical Bridge Nodes
# ------------------------------------------------------------

top_bridges <- data.frame(
  
  Rank = 1:3,
  
  Stakeholder =
    V(aviation_network)$name[
      top_bridge_indices
    ],
  
  Betweenness_Centrality =
    round(
      betweenness_centrality[
        top_bridge_indices
      ],
      3
    )
)


print(top_bridges)


write.csv(
  top_bridges,
  "Task_B_Results/B03_Top_3_Critical_Bridges.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 21. Save Degree Centrality Ranking
# ------------------------------------------------------------

degree_ranking <- centrality_table[
  order(
    -centrality_table$Degree_Centrality
  ),
]


write.csv(
  degree_ranking,
  "Task_B_Results/B04_Degree_Centrality_Ranking.csv",
  row.names = FALSE
)


# ============================================================
# END OF STEP B2
# ============================================================

cat("\nFinal network graph successfully created.\n")

cat(
  "Graph saved as: ",
  "Task_B_Figures/B02_Final_Aviation_Stakeholder_Network.png\n"
)

cat(
  "Centrality table saved as: ",
  "Task_B_Results/B02_Centrality_Table.csv\n"
)

cat(
  "Top bridge nodes saved as: ",
  "Task_B_Results/B03_Top_3_Critical_Bridges.csv\n"
)



# ============================================================
# STEP B4 - NETWORK STRUCTURE, CONNECTIVITY,
# COMMUNITIES AND VULNERABILITY ANALYSIS
# ============================================================


# ------------------------------------------------------------
# 1. NETWORK DENSITY
# ------------------------------------------------------------

network_density <- edge_density(
  aviation_network,
  loops = FALSE
)

cat("\nNETWORK DENSITY\n")
cat("================\n")
cat("Density:", round(network_density, 4), "\n")


# ------------------------------------------------------------
# 2. CONNECTIVITY
# ------------------------------------------------------------

is_network_connected <- is_connected(
  aviation_network
)

cat("\nNETWORK CONNECTIVITY\n")
cat("====================\n")
cat("Connected Network:", is_network_connected, "\n")


# Number of connected components
network_components <- components(
  aviation_network
)

cat(
  "Number of Connected Components:",
  network_components$no,
  "\n"
)


# ------------------------------------------------------------
# 3. AVERAGE PATH LENGTH
# ------------------------------------------------------------

average_path <- mean_distance(
  aviation_network,
  directed = FALSE
)

cat("\nAVERAGE PATH LENGTH\n")
cat("===================\n")
cat(
  "Average Path Length:",
  round(average_path, 3),
  "\n"
)


# ------------------------------------------------------------
# 4. NETWORK DIAMETER
# ------------------------------------------------------------

network_diameter <- diameter(
  aviation_network,
  directed = FALSE,
  weights = NA
)

cat("\nNETWORK DIAMETER\n")
cat("================\n")
cat(
  "Network Diameter:",
  network_diameter,
  "\n"
)


# ------------------------------------------------------------
# 5. GLOBAL TRANSITIVITY / CLUSTERING COEFFICIENT
# ------------------------------------------------------------

global_clustering <- transitivity(
  aviation_network,
  type = "global"
)

cat("\nGLOBAL CLUSTERING COEFFICIENT\n")
cat("=============================\n")
cat(
  "Global Clustering Coefficient:",
  round(global_clustering, 4),
  "\n"
)


# ------------------------------------------------------------
# 6. COMMUNITY DETECTION
# ------------------------------------------------------------

# Louvain community detection
# For community detection, we use unweighted topology because
# the supplied Weight variable is not explicitly defined as
# a path distance or similarity measure.

community_result <- cluster_louvain(
  aviation_network,
  weights = NA
)

community_membership <- membership(
  community_result
)

cat("\nCOMMUNITY DETECTION\n")
cat("===================\n")

cat(
  "Number of Communities:",
  length(unique(community_membership)),
  "\n"
)

cat(
  "Modularity:",
  round(modularity(community_result), 4),
  "\n"
)


# ------------------------------------------------------------
# 7. CREATE COMMUNITY TABLE
# ------------------------------------------------------------

community_table <- data.frame(
  Stakeholder = V(aviation_network)$name,
  Community = as.numeric(community_membership)
)

community_table <- community_table[
  order(community_table$Community),
]

print(community_table)


write.csv(
  community_table,
  "Task_B_Results/B05_Community_Membership.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 8. SAVE COMMUNITY GRAPH
# ------------------------------------------------------------

community_colours <- rainbow(
  length(unique(community_membership))
)

V(aviation_network)$community_color <-
  community_colours[
    community_membership
  ]


png(
  "Task_B_Figures/B03_Aviation_Communities.png",
  width = 2000,
  height = 1600,
  res = 170
)

plot(
  aviation_network,
  
  layout = network_layout,
  
  vertex.size =
    10 + (degree_centrality * 30),
  
  vertex.color =
    V(aviation_network)$community_color,
  
  vertex.frame.color =
    "grey30",
  
  vertex.label =
    V(aviation_network)$label,
  
  vertex.label.cex =
    0.65,
  
  vertex.label.dist =
    0.6,
  
  edge.color =
    "grey60",
  
  edge.width =
    1.5,
  
  main =
    "Community Structure of the Sri Lanka Aviation Network"
)

legend(
  "topleft",
  
  legend =
    paste(
      "Community",
      sort(unique(community_membership))
    ),
  
  pt.bg =
    community_colours,
  
  pch = 21,
  
  pt.cex = 1.5,
  
  cex = 0.75,
  
  bty = "n"
)

dev.off()


# ------------------------------------------------------------
# 9. CREATE NETWORK STRUCTURE SUMMARY TABLE
# ------------------------------------------------------------

network_structure_summary <- data.frame(
  
  Metric = c(
    "Number of Nodes",
    "Number of Edges",
    "Network Density",
    "Connected",
    "Connected Components",
    "Average Path Length",
    "Network Diameter",
    "Global Clustering Coefficient",
    "Number of Communities",
    "Modularity"
  ),
  
  Value = c(
    vcount(aviation_network),
    ecount(aviation_network),
    round(network_density, 4),
    is_network_connected,
    network_components$no,
    round(average_path, 3),
    network_diameter,
    round(global_clustering, 4),
    length(unique(community_membership)),
    round(modularity(community_result), 4)
  )
)

print(network_structure_summary)


write.csv(
  network_structure_summary,
  "Task_B_Results/B06_Network_Structure_Summary.csv",
  row.names = FALSE
)


# ============================================================
# STEP B4 - NETWORK STRUCTURE, CONNECTIVITY,
# COMMUNITIES AND VULNERABILITY ANALYSIS
# ============================================================


# ------------------------------------------------------------
# 1. NETWORK DENSITY
# ------------------------------------------------------------

network_density <- edge_density(
  aviation_network,
  loops = FALSE
)

cat("\nNETWORK DENSITY\n")
cat("================\n")
cat("Density:", round(network_density, 4), "\n")


# ------------------------------------------------------------
# 2. CONNECTIVITY
# ------------------------------------------------------------

is_network_connected <- is_connected(
  aviation_network
)

cat("\nNETWORK CONNECTIVITY\n")
cat("====================\n")
cat("Connected Network:", is_network_connected, "\n")


# Number of connected components
network_components <- components(
  aviation_network
)

cat(
  "Number of Connected Components:",
  network_components$no,
  "\n"
)


# ------------------------------------------------------------
# 3. AVERAGE PATH LENGTH
# ------------------------------------------------------------

# IMPORTANT:
# weights = NA is used so path length is based on
# network structure rather than the supplied Weight column.

average_path <- mean_distance(
  aviation_network,
  directed = FALSE,
  weights = NA
)

cat("\nAVERAGE PATH LENGTH\n")
cat("===================\n")
cat(
  "Average Path Length:",
  round(average_path, 3),
  "\n"
)


# ------------------------------------------------------------
# 4. NETWORK DIAMETER
# ------------------------------------------------------------

network_diameter <- diameter(
  aviation_network,
  directed = FALSE,
  weights = NA
)

cat("\nNETWORK DIAMETER\n")
cat("================\n")
cat(
  "Network Diameter:",
  network_diameter,
  "\n"
)


# ------------------------------------------------------------
# 5. GLOBAL CLUSTERING COEFFICIENT
# ------------------------------------------------------------

global_clustering <- transitivity(
  aviation_network,
  type = "global"
)

cat("\nGLOBAL CLUSTERING COEFFICIENT\n")
cat("=============================\n")
cat(
  "Global Clustering Coefficient:",
  round(global_clustering, 4),
  "\n"
)


# ------------------------------------------------------------
# 6. COMMUNITY DETECTION
# ------------------------------------------------------------

# Unweighted Louvain community detection is used because
# the supplied Weight variable is not explicitly defined
# as a path distance or similarity measure.

community_result <- cluster_louvain(
  aviation_network,
  weights = NA
)

community_membership <- membership(
  community_result
)

cat("\nCOMMUNITY DETECTION\n")
cat("===================\n")

cat(
  "Number of Communities:",
  length(unique(community_membership)),
  "\n"
)

cat(
  "Modularity:",
  round(modularity(community_result), 4),
  "\n"
)


# ------------------------------------------------------------
# 7. CREATE COMMUNITY TABLE
# ------------------------------------------------------------

community_table <- data.frame(
  Stakeholder = V(aviation_network)$name,
  Community = as.numeric(community_membership)
)

community_table <- community_table[
  order(community_table$Community),
]

print(community_table)

write.csv(
  community_table,
  "Task_B_Results/B05_Community_Membership.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 8. SAVE COMMUNITY GRAPH
# ------------------------------------------------------------

community_colours <- rainbow(
  length(unique(community_membership))
)

V(aviation_network)$community_color <-
  community_colours[
    community_membership
  ]


png(
  "Task_B_Figures/B03_Aviation_Communities.png",
  width = 2000,
  height = 1600,
  res = 170
)

plot(
  aviation_network,
  
  layout = network_layout,
  
  vertex.size =
    10 + (degree_centrality * 30),
  
  vertex.color =
    V(aviation_network)$community_color,
  
  vertex.frame.color =
    "grey30",
  
  vertex.label =
    V(aviation_network)$label,
  
  vertex.label.cex =
    0.65,
  
  vertex.label.dist =
    0.6,
  
  edge.color =
    "grey60",
  
  edge.width =
    1.5,
  
  main =
    "Community Structure of the Sri Lanka Aviation Network"
)

legend(
  "topleft",
  
  legend =
    paste(
      "Community",
      sort(unique(community_membership))
    ),
  
  pt.bg =
    community_colours,
  
  pch = 21,
  
  pt.cex = 1.5,
  
  cex = 0.75,
  
  bty = "n"
)

dev.off()


# ------------------------------------------------------------
# 9. CREATE NETWORK STRUCTURE SUMMARY TABLE
# ------------------------------------------------------------

network_structure_summary <- data.frame(
  
  Metric = c(
    "Number of Nodes",
    "Number of Edges",
    "Network Density",
    "Connected",
    "Connected Components",
    "Average Path Length",
    "Network Diameter",
    "Global Clustering Coefficient",
    "Number of Communities",
    "Modularity"
  ),
  
  Value = c(
    vcount(aviation_network),
    ecount(aviation_network),
    round(network_density, 4),
    is_network_connected,
    network_components$no,
    round(average_path, 3),
    network_diameter,
    round(global_clustering, 4),
    length(unique(community_membership)),
    round(modularity(community_result), 4)
  )
)

print(network_structure_summary)

write.csv(
  network_structure_summary,
  "Task_B_Results/B06_Network_Structure_Summary.csv",
  row.names = FALSE
)


# ============================================================
# STEP B4B - VULNERABILITY ANALYSIS
# ============================================================


# ------------------------------------------------------------
# 10. FUNCTION TO TEST NODE FAILURE
# ------------------------------------------------------------

test_node_failure <- function(graph, node_name) {
  
  graph_after_failure <- delete_vertices(
    graph,
    node_name
  )
  
  remaining_nodes <- vcount(
    graph_after_failure
  )
  
  remaining_edges <- ecount(
    graph_after_failure
  )
  
  connected_after_failure <- is_connected(
    graph_after_failure
  )
  
  components_after_failure <- components(
    graph_after_failure
  )$no
  
  density_after_failure <- edge_density(
    graph_after_failure,
    loops = FALSE
  )
  
  avg_path_after_failure <- NA
  
  # Calculate average path length only if
  # the remaining network is still connected.
  
  if (connected_after_failure) {
    
    avg_path_after_failure <- mean_distance(
      graph_after_failure,
      directed = FALSE,
      weights = NA
    )
  }
  
  return(
    data.frame(
      
      Removed_Node = node_name,
      
      Remaining_Nodes = remaining_nodes,
      
      Remaining_Edges = remaining_edges,
      
      Connected =
        connected_after_failure,
      
      Components =
        components_after_failure,
      
      Density =
        round(
          density_after_failure,
          4
        ),
      
      Average_Path_Length =
        ifelse(
          is.na(avg_path_after_failure),
          NA,
          round(
            avg_path_after_failure,
            3
          )
        )
    )
  )
}


# ------------------------------------------------------------
# 11. TEST FAILURE OF IMPORTANT STAKEHOLDERS
# ------------------------------------------------------------

atc_failure <- test_node_failure(
  aviation_network,
  "Air Traffic Control (ATC)"
)

ground_failure <- test_node_failure(
  aviation_network,
  "Ground Handling Unit"
)

cmb_failure <- test_node_failure(
  aviation_network,
  "Bandaranaike International Airport (CMB)"
)

cargo_failure <- test_node_failure(
  aviation_network,
  "Cargo Operators"
)

fuel_failure <- test_node_failure(
  aviation_network,
  "Fuel Supply Companies"
)

customs_failure <- test_node_failure(
  aviation_network,
  "Customs & Immigration"
)


# ------------------------------------------------------------
# 12. COMBINE VULNERABILITY RESULTS
# ------------------------------------------------------------

vulnerability_results <- rbind(
  atc_failure,
  ground_failure,
  cmb_failure,
  cargo_failure,
  fuel_failure,
  customs_failure
)

cat("\nNODE FAILURE ANALYSIS\n")
cat("=====================\n")

print(vulnerability_results)

write.csv(
  vulnerability_results,
  "Task_B_Results/B07_Node_Failure_Analysis.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 13. TEST FAILURE OF TOP THREE BRIDGE NODES
# ------------------------------------------------------------

bridge_failure_results <- do.call(
  rbind,
  lapply(
    V(aviation_network)$name[
      top_bridge_indices
    ],
    function(x) {
      
      test_node_failure(
        aviation_network,
        x
      )
      
    }
  )
)

cat("\nTOP BRIDGE NODE FAILURE RESULTS\n")
cat("===============================\n")

print(
  bridge_failure_results
)

write.csv(
  bridge_failure_results,
  "Task_B_Results/B08_Top_Bridge_Failure_Analysis.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 14. SAVE COMPLETE STRUCTURAL ANALYSIS
# ------------------------------------------------------------

capture.output(
  {
    
    cat("TASK B - NETWORK STRUCTURE ANALYSIS\n")
    cat("===================================\n\n")
    
    cat("Network Density:\n")
    print(network_density)
    
    cat("\nConnected Network:\n")
    print(is_network_connected)
    
    cat("\nConnected Components:\n")
    print(network_components$no)
    
    cat("\nAverage Path Length:\n")
    print(average_path)
    
    cat("\nNetwork Diameter:\n")
    print(network_diameter)
    
    cat("\nGlobal Clustering Coefficient:\n")
    print(global_clustering)
    
    cat("\nNumber of Communities:\n")
    print(
      length(
        unique(
          community_membership
        )
      )
    )
    
    cat("\nModularity:\n")
    print(
      modularity(
        community_result
      )
    )
    
    cat("\nCommunity Membership:\n")
    print(
      community_table
    )
    
    cat("\nNode Failure Analysis:\n")
    print(
      vulnerability_results
    )
    
    cat("\nTop Bridge Failure Analysis:\n")
    print(
      bridge_failure_results
    )
    
  },
  
  file =
    "Task_B_Results/B09_Full_Network_Structure_Analysis.txt"
)


# ============================================================
# END STEP B4
# ============================================================

cat("\nStep B4 completed successfully.\n")
cat("Network structure, community and vulnerability results saved.\n")


print(network_structure_summary)
print(community_table)
print(vulnerability_results)
print(bridge_failure_results)