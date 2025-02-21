
# Library of functions for constructing multi-level heatmaps using the ComplexHeatmap package.

#' .load_packages
#' @param tools A character vector specifying the packages to load.
#'
#' @return Nothing is returned; the function is used for loading packages.
#' 
#' @importFrom BiocManager install
#' @importFrom BiocManager available
#'
#' @export

.load_packages <- function(tools) {
  tmp <- as.data.frame(installed.packages())
  max_version <- max(as.numeric(substr(tmp$Built, 1, 1)))
  tmp <- tmp[as.numeric(substr(tmp$Built, 1, 1)) == max_version, ]
  
  for (pkg in tools) {
    if (pkg %in% tmp$Package) {
      library(pkg, character.only = TRUE)
    } else {
      print(sprintf("%s %s", pkg, "is not installed. Installing it!"))
      
      if (pkg %in% BiocManager::available(pkg)) {
        BiocManager::install(pkg, dependencies = TRUE, update = TRUE)
      } else {
        install.packages(pkg, dependencies = TRUE, ask = FALSE)
      }
    }
  }
}

# Load required packages or install them if necessary
dependencies <- c(
  "ComplexHeatmap",
  "ggplot2",
  "dplyr",
  "tidyverse",
  "colorRamp2",
  "extrafont"
)

.load_packages(dependencies)

.load_file <- function(input_file = NULL) {
  if (!is.null(input_file)) {
    if (typeof(input_file) == "character") {
      if (file.exists(input_file)) {
        file <- read.delim(input_file, header = TRUE, sep = "\t")
        } 
      } else if (class(input_file) == "data.frame") {
          file <- input_file
      } else {
          stop("Input file is not provided.")
    }
  }
  return( as.data.frame (file) )
}


#' Visualize a heatmap with metadata and customizable features
#'
#' This function generates a heatmap using the `ComplexHeatmap` package, allowing for various annotations and formatting options.
#'
#' @param input_file Path to the input file containing the data.
#' @param transposed Logical, whether to transpose the input matrix. Default is TRUE.
#' @param display_numbers Logical, whether to display values inside heatmap cells. Default is FALSE.
#' @param num_breaks Numeric, specifying the number of breaks for color mapping. Default is 10.
#' @param reorder_rows Indices specifying the order of rows. Default is NULL.
#' @param reorder_cols Indices specifying the order of columns. Default is NULL.
#' @param color_palette Character vector specifying colors for heatmap gradient. Default is c("azure2", "dodgerblue3", "dodgerblue4").
#' @param zero_color Color to represent zero values in the heatmap. Default is "white".
#' @param top_annotation A vector specifying the annotation for the heatmap header. Default is NULL.
#' @param top_annotation_title Title for the top annotation. Default is NULL.
#' @param right_annotation Data for the right annotation bar. Default is NULL.
#' @param right_annotation_title Title for the right annotation. Default is "Total".
#' @param stacks_n List defining stacked bar components for right annotation. Default is NULL.
#' @param stacked_colors Colors for stacked bars in right annotation. Default is NULL.
#' @param left_annotation Data for left annotation. Default is NULL.
#' @param left_annotation_title Title for the left annotation. Default is "Category".
#' @param row_order Logical, whether to order rows automatically. Default is FALSE.
#' @param fontfamily Character, specifying font family for text. Default is "Arial".
#' @param fontsize Numeric, specifying base font size. Default is 30.
#' @param numbers_fontsize Numeric, specifying font size for displayed numbers. Default is 20.
#' @param gaps_row Numeric, specifying gaps between rows. Default is NULL.
#' @param gaps_col Numeric, specifying gaps between columns. Default is NULL.
#' @param border_color Color for cell borders. Default is "black".
#' @param scale Character, specifying whether to scale data. Options: "none", "row", "column". Default is "none".
#' @param title Character, title of the heatmap. Default is NULL.
#' @param legend Logical, whether to display the legend. Default is FALSE.
#'
#' @return A heatmap plot created using the `ComplexHeatmap` package.
#'
#' @export

complex_heatmap <- function(
    input_file = NULL,
    transposed = TRUE,
    display_numbers = F,
    num_breaks = 10,
    reorder_rows = NULL,
    reorder_cols = NULL,
    color_palette = c("azure2", "dodgerblue3", "dodgerblue4"),
    zero_color = "white",
    top_annotation = NULL,
    top_annotation_title = NULL,
    right_annotation = NULL,
    right_annotation_title = "Total",
    stacks_n = NULL,
    stacked_colors = NULL,
    left_annotation = NULL,
    left_annotation_title = "Category",
    row_order = F,
    fontfamily = "Arial",
    fontsize = 30,
    numbers_fontsize = 20,
    gaps_row = NULL,
    gaps_col = NULL,
    border_color = "black",
    scale = "none",
    title = NULL,
    legend = FALSE
) {
  
  counts <- .load_file(input_file)
  
  # Manipulate input data
  if (!is.null(counts)) {
  
    # First column as rownames - useful for horizontal_annotation
    rownames(counts) <- counts[, 1]
    counts <- as.matrix(counts[, -1])
    
    if (!is.null(reorder_rows)) {
      counts <- counts[reorder_rows, ]
    }
    
    if (!is.null(reorder_cols)) {
      counts <- counts[, reorder_cols]
    }
    
    if (transposed == TRUE) {
      counts <- t(counts)
    }
    
    counts <- as.data.frame(counts)
    
    
    # Generate color_palette
    
    # First get the range for non-zero values
    min_nonzero <- range(counts[counts != 0])[1]
    max_nonzero <- range(counts[counts != 0])[2]
    
    # Create color palette for non-zero values
    nonzero_colors <- colorRampPalette(color_palette)(399)
    
    # Combine with zero color
    col2 <- c(zero_color, nonzero_colors)
    
    # Rescale the matrix values to map to our color indices
    counts_scaled <- counts
    
    counts_scaled[counts_scaled == 0] <- 0  # First color (zero_color)
    counts_scaled[counts_scaled > 0] <- 1 + ((counts_scaled[counts_scaled > 0] - min_nonzero) / 
                                               (max_nonzero - min_nonzero)) * 398
    
    counts_m <- as.matrix.data.frame(counts)
    
    
  } else {
      stop("File was not loaded correctly.")
  }
  
# Left row annotation with rownames of tsv file
  if (!is.null(left_annotation)) {
    if (!is.null(left_annotation_title)) {
      left_annotation_title = left_annotation_title
      left_annotation = rowAnnotation(
        left_annotation_title = anno_text(
          gsub("_", " ", rownames(counts)),
          gp = gpar(
            fontfamily = fontfamily,
            fontsize = fontsize,
            fontface = "italic"
          ),
          width = unit(90, "mm")
        )
      )
    } else {
      left_annotation = NULL
    }
  }
 
  
  # Top block annotation for grouping columns by common features
  if (!is.null(top_annotation)) {
    if (!is.null(top_annotation_title)) {
      top_annotation = HeatmapAnnotation(
        top_annotation_title = anno_block(
          gp = gpar(fill = top_annotation),  # Block colors
          labels = names(top_annotation),    # Custom labels (no numbers)
          labels_gp = gpar(fontsize = 10, fontface = "bold"),  # Text formatting
          labels_just = NULL
        )
      )
    } else {
      stop("Please provide a top_annotation_title.")
    }
  }
  
  # Right annotation with total gene counts per row
  if (!is.null(right_annotation)) {
    if (!is.null(right_annotation_title)) {
      if (!is.null(stacks_n)) {
        
        percentages <-
          sapply(
            rownames(counts_m), 
            function(x) {
              sapply(stacks_n, function(y) {
                sum(counts_m[x, y])
              })
            }
          )
        
        sum_percents <- as.data.frame(
          rowsum(
            percentages,
            group = rownames(percentages)
          ) 
        )

        sum_percents <- sum_percents[c(4,1,2,3),]
        counts_to_print <- t(sum_percents)
        totals <- rowSums(counts_to_print)
        colors_to_print <- stacked_colors
        
      } else {
          counts_to_print <- rowSums(counts_m)
          totals <- counts_to_print
          colors_to_print <- "azure2"
      }
      
      right_annotation <- rowAnnotation(
        "Total" = anno_barplot(
          at = seq(0, 180, by = 20),
          counts_to_print,
          border = FALSE,                                  # Remove border
          bar_width = 0.75,                                # Adjust bar width
          gp = gpar(fill = colors_to_print, fontfamily = fontfamily, fontsize = fontsize),  # Set fill color and font size
          add_numbers = TRUE,                              # Add numbers to the bars
          numbers_gp = gpar(fontfamily = fontfamily, fontsize = numbers_fontsize),
          numbers_rot = 0,                                 # Number orientation
          #numbers_offset = unit(0.05, "mm"),                # Position of numbers
          height = unit(max(counts_to_print) / 4, "mm"),    # Width adjustment based on data
          width = unit(max(counts_to_print)/2, "mm"),    # Width adjustment based on data
          ylim = c(0, 180),               # Set y-axis limits
          axis = TRUE                                      # Ensure calibrating axis is shown
        ),
      
      # Add text annotations with the total sum relative to each bar's length
      "Total Text" = anno_text(
        totals,                                      # The total values to display
        gp = gpar(
          col = "black", 
          fontsize = fontsize, 
          fontfamily = fontfamily
        )
        )
      )
    } else {
        stop("Please provide a right_annotation_title.")
    }
  }
  
  # split in 10 breaks
  breaks <- seq(round(min(counts_m)), round(max(counts_m)+5), by = 5)
  
    plot <- ComplexHeatmap::pheatmap(
        counts_m,
        cluster_cols = F,
        cluster_rows = F,
        scale = scale,
        number_color = "black",
        gaps_row = gaps_row,
        gaps_col = gaps_col,
        cellwidth = 17,
        cellheight = 17,
        color = col2,
        border_color = border_color,
        legend_breaks = breaks,
        silent = F,
        show_colnames = T,
        show_rownames = F,
        display_numbers = display_numbers,
        angle_col = c("45"),
        fontfamily = fontfamily,
        fontsize = fontsize,
        fontsize_row = 17,
        fontsize_col = 10,
        annotation_legend = F,
        main = title,
        top_annotation = top_annotation,
        left_annotation = left_annotation,
        right_annotation = right_annotation
      )
  if ( exists ("plot")) {
    return (plot)
  } else {
    print ("Error. Plot was not generated.")
  }
}