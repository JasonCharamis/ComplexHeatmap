## Library for drawing heatmaps.

### Heatmap for GSTs

package_install <- function ( package ) {
  if (requireNamespace(package, quietly = TRUE)) {
    library(package, character.only = TRUE)
  }
  
  else {
    print ( (sprintf("%s %s",package, "is not installed. Installing it!")))
    BiocManager::install(package)  
  }
}

dependencies <- c("grid","dplyr","ComplexHeatmap","ggplot2")

for ( i in dependencies ) { 
  package_install(i)
}


heatmap <- function ( counts_table, top_annotation = TRUE, row_ha=TRUE ) {
    
        ## input is a tab-separated table with gene counts per ortholog group ##
        counts <-read.delim(counts_table, header=T, sep = "\t")
        rownames(counts) <- counts[,3]
        counts <- counts[,-c(1,2,3)]

        ## transpose data to convert the heatmap to horizontal ##
        counts <-t(counts)
        counts <- as.data.frame(counts)
        p <- sub("Sand fly-specific ","",colnames(counts))
        colnames(counts) <- sub("..",".", p,  fixed=TRUE)

        ## create palette for color counts ##
        col2 <-colorRampPalette(c("azure2","dodgerblue3","dodgerblue4"))(400)
        
        ## change order of species - optional ##
        counts <- counts[c(5,3,7,2,1,4,6,8,9,10,11,12),c(seq(2,4),8,6,5,7,1,9)]
        
        labels_i <- colnames(counts)
        
        ## convert df to matrix ##
        counts_m <- as.matrix.data.frame(counts)
        
        ## add species labels left and in italics ##
        species_ha = rowAnnotation("Species" = anno_text(rownames(counts),
                                                         gp=gpar(fontsize=11,fontface="italic")))
        
        
        ## add block annotation for CYP clans ##
        
        if ( !is.null(top_annotation)) {
        
            top_annotation = HeatmapAnnotation(clans = anno_block(gp = gpar(fill=c("lightblue","gold","green4","orange2")),
                                                              labels = c("MITO", "CYP2", "CYP3","CYP4"), width =unit(0.5,"mm"),
                                                              labels_gp = gpar(col = "black")))
            
        }
        
        if ( !is.null(row_ha)) {
        
        ## add barplot annotation with total number of CYP genes per species ##
            row_ha = rowAnnotation("Total"=anno_barplot(rowSums(counts), border = F,
                                                    bar_width = 0.8,
                                                    gp = gpar(fill = "azure2",fontsize=40),
                                                    add_numbers = T,  numbers_rot=0, numbers_offset=unit(1,"mm"),
                                                    height=unit(6,"mm"), ylim=c(0,25)))
        }
        
        ## draw heatmap with gene counts ##
        return ( ComplexHeatmap::pheatmap(counts_m, cluster_cols = F, cluster_rows = F, scale="none", 
                                      number_color = "black", gaps_row = c(8,10,12), 
                                      cellwidth = 17, cellheight = 17, color=col2,
                                      border_color = "white", silent = F, show_colnames = T, show_rownames = F,
                                      display_numbers = F, angle_col = c("45"),
                                      fontsize=15,fontsize_row = 17, fontsize_col = 10,legend = T, 
                                   #   heatmap_legend_param = list(color_bar = "continuous", legend_direction = "horizontal", 
                                                                #  legend_width = unit(2 ,"cm"), title_position = "lefttop"),
                                      left_annotation=species_ha,right_annotation=row_ha) ) 
}
