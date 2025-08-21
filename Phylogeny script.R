
library(phylobase)
library(ape)
library(openxlsx)
library(treeio)
library(ggtree)
library(ggplot2)
library(ggplot2)
library(dplyr)
library(khroma)
library(treeio)
library(tidyselect)



###### MUNIDOPSIDAE ######
setwd("/Users/catalinamerino/RP2/Molecular analysis/munidopsidae IQTree")
list.files()

#OUTGROUP PETROLISTHES
Munidopsis_16S <- read.iqtree("Nucleotide_alignment_Munidopsidae_16S_MAFFT4_129_seq_other_outgroup.fasta.treefile")


a <- ggtree(Munidopsis_16S) #create tree object first - this creates a hidden file called p$data, which you can extract values from 

a <-a + 
  geom_text2(aes(subset = !isTip & UFboot > 95 & SH_aLRT > 80, #this bit of code only shows significant (>95/>80%)
                 label = paste0(UFboot, "/", SH_aLRT)), #boostrap or SH_aLRT values
             hjust = -0.2, size = 1.5, angle = 45)+ #modifies node label size, angle etc
  geom_tiplab(size=2, hjust = -0.1)+ #tip lable size
  geom_treescale(x = 0, y = -2, offset = 0.6, label = "substitutions/site", fontsize = 2)+ #includes the scale bar
  xlim(NA, max(a$data$x, na.rm = TRUE) + 0.02)+ #use xlim and y lim to modify 
  theme_tree() #gives basic theme 


plot(a)
ggsave("Munidopsis_16Sa.pdf", plot = a, width = 8, height = 12, units = "in") #saves as pdf you can play with size 


#OUTGROUP ALAINUS
list.files()

Munidopsis_16Sb <- read.iqtree("Nucleotide_alignment_Munidopsidae_16S_MAFFT3_129_seq.fasta.treefile")


b <- ggtree(Munidopsis_16Sb) #create tree object first - this creates a hidden file called p$data, which you can extract values from 

Munidopsis_16Sb@phylo$tip.label <- gsub("_", " ", Munidopsis_16Sb@phylo$tip.label)

b <- ggtree(Munidopsis_16Sb) + 
  # Mixed format tip labels - create expression directly in aes()
  geom_tiplab(aes(label = paste0("italic('", gsub('^(\\S+\\s+\\S+).*', '\\1', label), "')~'", 
                                 gsub('^\\S+\\s+\\S+\\s+', '', label), "'")), 
              parse = TRUE, 
              size = 2, 
              hjust = -0.1) +
  geom_text2(aes(subset = !isTip & UFboot > 95 & SH_aLRT > 80,
                 label = paste0(UFboot, "/", SH_aLRT)),
             hjust = 1.2, nudge_y = 0.8, size = 1.5, angle = 0) +
  geom_nodepoint(aes(subset = !isTip & UFboot > 95 & SH_aLRT > 80),
                 color = "red", size = 0.5) +
  geom_treescale(x = 0, y = -2, offset = 0.6, label = "substitutions/site", fontsize = 2) +
  xlim(NA, max(b$data$x, na.rm = TRUE) + 0.15) +
  theme_tree()


plot(b)

ggsave("Munidopsis_16Sfinal2.pdf", plot = b, width = 8, height = 12, units = "in") #saves as pdf you can play with size 







####Playing to see which tree is better 

#Visualize differences: Use cophylo() to see exactly which clades differ:
install.packages("phytools")
library(phytools)
cophylo_plot <- cophylo(tree_a_pruned, tree_b_pruned)
plot(cophylo_plot)






###Check likelihoods
iqtree_file_a <- "Nucleotide_alignment_Munidopsidae_16S_MAFFT4_129_seq_other_outgroup.fasta.iqtree"
iqtree_file_b <- "Nucleotide_alignment_Munidopsidae_16S_MAFFT3_129_seq.fasta.iqtree"

lines_a <- readLines(iqtree_file_a)
lines_b <- readLines(iqtree_file_b)

# Look for log-likelihood in the file
grep("Log-likelihood", lines_a, value = TRUE)
grep("Log-likelihood", lines_b, value = TRUE)
#The Alainus outgroup tree has a higher log-likelihood (less negative), so it fits the sequence data better.







##can compare support values (like UFboot or SH-aLRT)
library(ape)
library(phytools)

# Load your trees
tree_a <- read.tree("Nucleotide_alignment_Munidopsidae_16S_MAFFT4_129_seq_other_outgroup.fasta.treefile")
tree_b <- read.tree("Nucleotide_alignment_Munidopsidae_16S_MAFFT3_129_seq.fasta.treefile")



# 2. Prune trees to common tips

common_tips <- intersect(tree_a$tip.label, tree_b$tip.label)
tree_a_pruned <- drop.tip(tree_a, setdiff(tree_a$tip.label, common_tips))
tree_b_pruned <- drop.tip(tree_b, setdiff(tree_b$tip.label, common_tips))


# 3. Extract UFboot / SH-aLRT safely

extract_support <- function(tree) {
  node_labels <- tree$node.label
  
  # If no node labels, return empty matrix
  if(is.null(node_labels)) {
    warning("Tree has no node labels")
    return(matrix(NA, nrow=tree$Nnode, ncol=2, dimnames=list(NULL, c("UFboot","SHaLRT"))))
  }
  
  # Ensure all internal nodes have a string (replace NULL with NA)
  node_labels[is.na(node_labels)] <- NA
  node_labels <- as.character(node_labels)
  
  # Split "UFboot/SH-aLRT"
  support_list <- strsplit(node_labels, "/")
  
  # Convert to numeric and pad missing
  support_matrix <- t(sapply(support_list, function(x) {
    if(length(x) == 0) x <- c(NA, NA)
    if(length(x) == 1) x <- c(x, NA)
    as.numeric(x)
  }))
  
  colnames(support_matrix) <- c("UFboot","SHaLRT")
  return(support_matrix)
}

# Use the function
support_a <- extract_support(tree_a_pruned)
support_b <- extract_support(tree_b_pruned)



# 4. Quick scatterplot comparison


# UFboot comparison
plot(support_a[,1], support_b[,1],
     xlab = "UFboot (Tree A)",
     ylab = "UFboot (Tree B)",
     pch = 16, col = "blue",
     main = "UFboot Support Comparison")
abline(0,1, col="red", lty=2)

# SH-aLRT comparison
plot(support_a[,2], support_b[,2],
     xlab = "SH-aLRT (Tree A)",
     ylab = "SH-aLRT (Tree B)",
     pch = 16, col = "darkgreen",
     main = "SH-aLRT Support Comparison")
abline(0,1, col="red", lty=2)


# 5. Summary statistics

cat("Mean UFboot Tree A:", mean(support_a[,1], na.rm = TRUE), "\n")
cat("Mean UFboot Tree B:", mean(support_b[,1], na.rm = TRUE), "\n")
cat("Mean SH-aLRT Tree A:", mean(support_a[,2], na.rm = TRUE), "\n")
cat("Mean SH-aLRT Tree B:", mean(support_b[,2], na.rm = TRUE), "\n")
#UFboot: Tree A ≈ 69.2, Tree B ≈ 68.7 → very similar overall support.

#SH-aLRT: Tree A ≈ 85.0, Tree B ≈ 83.5 → Tree A slightly higher on average







###how many nodes exceed commonly used support thresholds, e.g.:
#UFboot ≥ 95%
#SH-aLRT ≥ 80%

# Count highly supported nodes
uf_a <- sum(support_a[,1] >= 95, na.rm = TRUE)
sh_a <- sum(support_a[,2] >= 80, na.rm = TRUE)

uf_b <- sum(support_b[,1] >= 95, na.rm = TRUE)
sh_b <- sum(support_b[,2] >= 80, na.rm = TRUE)

cat("Tree A (Petrolisthes outgroup):\n")
cat("UFboot >= 95:", uf_a, "\n")
cat("SH-aLRT >= 80:", sh_a, "\n\n")

cat("Tree B (Alainus outgroup):\n")
cat("UFboot >= 95:", uf_b, "\n")
cat("SH-aLRT >= 80:", sh_b, "\n")

#Tree A (Petrolisthes outgroup): UFboot ≥ 95: 30 nodes SH-aLRT ≥ 80: 84 nodes

#Tree B (Alainus outgroup): UFboot ≥ 95: 34 nodes SH-aLRT ≥ 80: 79 nodes
#Interpretation: 
#Tree B has slightly more nodes with very high UFboot support (34 vs 30),
#Tree A has more nodes with high SH-aLRT support (84 vs 79).

#So neither tree is universally “better” in terms of support: Tree B is slightly better in UFboot, Tree A is slightly better in SH-aLRT.














a <- ggtree(Munidopsis_16S) + 
  geom_text2(aes(subset = !isTip & UFboot > 95 & SH_aLRT > 80,
                 label = paste0(UFboot, "/", SH_aLRT)),
             hjust = -0.2, size = 1.5, angle = 45) +
  geom_tiplab(size=2, hjust = -0.1) +
  geom_treescale(x = 0, y = -2, offset = 0.3, label = "substitutions/site", fontsize = 2) +
  xlim(NA, max(a$data$x, na.rm = TRUE) * 0.05) +  # reduce tree width
  theme_tree()
plot(a)




######Xylo######
list.files()
setwd("/Users/catalinamerino/RP2/Molecular analysis")



library(ggtree)
library(ape)
library(ggtree)
library(ape)


# Read tree WITH SUPPORT VALUES using treeio
Xylo18S_28S <- read.iqtree("Xylo_18S_28S_concat_noGaps.fasta.treefile")

# Replace underscores with spaces in tip labels
Xylo18S_28S@phylo$tip.label <- gsub("_", " ", Xylo18S_28S@phylo$tip.label)

# Multiply branch lengths
Xylo18S_28S@phylo$edge.length <- Xylo18S_28S@phylo$edge.length * 4

# Plot tree with mixed-format labels - SIMPLE APPROACH
p <- ggtree(Xylo18S_28S) +
  
  # Mixed format tip labels - create expression directly in aes()
  geom_tiplab(aes(label = paste0("italic('", gsub('^(\\S+\\s+\\S+).*', '\\1', label), "')~'", 
                                 gsub('^\\S+\\s+\\S+\\s+', '', label), "'")), 
              parse = TRUE, 
              size = 2, 
              hjust = -0.1) +
  
  # Bootstrap support values
  geom_text2(aes(subset = !isTip & UFboot > 95 & SH_aLRT > 80,
                 label = paste0(UFboot, "/", SH_aLRT)),
             hjust = 1.2, nudge_y = 0.8, size = 1.5, angle = 0) +
  
  # Red dots for highly supported nodes
  geom_nodepoint(aes(subset = !isTip & UFboot > 95 & SH_aLRT > 80),
                 color = "red", size = 0.5) +
  
  # Tree scale
  geom_treescale(x = 0, y = -2, offset = 0.6, 
                 label = "substitutions/site", fontsize = 2) +
  
  # Adjust x-axis limits
  xlim(NA, max(p$data$x, na.rm = TRUE) + 0.15) +
  
  # Clean theme
  theme_tree()

# Display the plot
print(p)

# Save as PDF
ggsave("Xylo18S_28S.pdf", plot = p, width = 8, height = 12, units = "in")





##list of taxa 
# Get all tip labels
all_tip_labels <- Xylo18S_28S@phylo$tip.label
print(all_tip_labels)

# Write to a file
writeLines(all_tip_labels, "all_tip_labels.txt")







######Thyasiridae ######
list.files()
setwd("/Users/catalinamerino/RP2/Molecular analysis")


Thyas18S <- read.iqtree("Thyas_18S_b_align_noGaps.fasta.treefile")

# Replace underscores with spaces in tip labels
Thyas18S@phylo$tip.label <- gsub("_", " ", Thyas18S@phylo$tip.label)

# Multiply branch lengths (adjust factor as needed)
Thyas18S@phylo$edge.length <- Thyas18S@phylo$edge.length * 3

# Plot tree with mixed-format labels for Thyasiridae
m <- ggtree(Thyas18S) +
  
  # Negative offset to make labels overlap branches
  geom_tiplab(aes(label = paste0("italic('", gsub('^(\\S+\\s+\\S+).*', '\\1', label), "')~'", 
                                 gsub('^\\S+\\s+\\S+\\s+', '', label), "'")), 
              parse = TRUE, 
              size = 2, 
              hjust = 0, 
              linesize = 0.1) +
  
  geom_text2(aes(subset = !isTip & UFboot > 95 & SH_aLRT > 80,
                 label = paste0(UFboot, "/", SH_aLRT)),
             hjust = 1.2, nudge_y = 0.4, size = 2, angle = 0) +
  
  geom_nodepoint(aes(subset = !isTip & UFboot > 95 & SH_aLRT > 80),
                 color = "red", size = 0.5) +
  
  geom_treescale(x = 0, y = -1, offset = 0.6, 
                 label = "substitutions/site", fontsize = 2) +
  
  # Tight limits
  xlim(NA, max(m$data$x, na.rm = TRUE) +0.1) +  # Negative padding
  
  theme_tree() +
  theme(plot.margin = margin(0, 0, 0, 0))

print(m)



# Save as PDF
ggsave("Thyas18S_phylogeny.pdf", plot = m, width = 8, height = 12, units = "in")











##list of taxa 
# Get all tip labels
all_tip_labels <- Thyas18S@phylo$tip.label
print(all_tip_labels)

# Write to a file
writeLines(all_tip_labels, "all_tip_labels.txt")
