## Use case for complex_heatmap.R

source("complex_heatmap.R")

heatmap <- complex_heatmap("gene_counts.tsv", reordered_rows = c(2:7,1,8:58),  reordered_cols = c(12,5,3,7,2,1,4,6,8,9,10,11),
                            color_palette = c("azure2","dodgerblue3","dodgerblue4"), head_annotation =  c(MITO = "lightblue", CYP2 = "gold", CYP3 = "green4", CYP4 = "orange2"),
                            row_annotation = TRUE, gaps_row = c(8,10), gaps_col = c(7,14,43))

ggsave (plot = heatmap, "ComplexHeatmap.svg", dpi = 600)
