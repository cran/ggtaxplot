#' ggtaxplot Process and Plot Taxonomic Data
#'
#' This function processes data and generates a taxonomic river plot.
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom dplyr distinct
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom dplyr n
#' @import tidyr
#' @import tidyverse
#' @import ggplot2
#' @importFrom ggalluvial geom_flow geom_stratum
#' @importFrom scales percent_format
#' @importFrom magrittr %>%
#' @importFrom rlang sym :=
#' @importFrom RColorBrewer brewer.pal
#' @importFrom cluster pam
#' @importFrom vegan vegdist
#' @param data A data frame containing two columns: ID and Taxonomy.
#' @param ID_col A column with ID values.
#' @param tax_col A column with Taxonomy.
#' @param rm_NA A logical value indicating whether to remove rows where the taxonomy column is 'Unknown' or NA. Default is FALSE.
#' @param threshold A numeric threshold for filtering low-abundance taxa (Others).
#' @param custom_colors Optional custom colors assigned to phyla.
#' @return A ggplot object of the river plot.
#' @examples
#' # Example data frame
#'data <- data.frame(
#'  ID = c("ID1", "ID2", "ID3"),
#'  Taxonomy = c("d__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;\
#'  o__Enterobacterales;f__Enterobacteriaceae;g__Escherichia",
#'             "d__Bacteria;p__Actinobacteria;c__Actinobacteria;\
#'             o__Corynebacteriales;f__Corynebacteriaceae;g__Corynebacterium",
#'             "d__Bacteria;p__Firmicutes;c__Bacilli;\
#'             o__Bacillales;f__Bacillaceae;g__Bacillus")
#'             )
#' # Generate the river plot
#' plot <- ggtaxplot(data)
#' print(plot)
#' @export
ggtaxplot <- function(data, ID_col = 'ID', tax_col = "Taxonomy", rm_NA = FALSE, threshold = 2, custom_colors = NULL) {
  # Step 1: Identify the columns
  first_column_name <- ID_col
  taxonomy_column_name <- tax_col
  
  # Option to remove rows where taxonomy_column_name is 'Unknown' or NA
  if (rm_NA) {
    data <- data %>%
      dplyr::filter(!(!!sym(taxonomy_column_name) == "Unknown" | is.na(!!sym(taxonomy_column_name))))
  }
  
  # Step 2: Process the second column
  processed_data <- data %>%
    mutate(!!sym(taxonomy_column_name) := as.character(!!sym(taxonomy_column_name))) %>%
    mutate(!!sym(taxonomy_column_name) := ifelse(
      grepl("d__|p__|c__|o__|f__|g__", !!sym(taxonomy_column_name)), 
      gsub("d__|p__|c__|o__|f__|g__", "", !!sym(taxonomy_column_name)), 
      !!sym(taxonomy_column_name)  # Leave unchanged if no prefixes found
    )) %>%
    separate(!!sym(taxonomy_column_name), into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus"), 
             sep = ";", extra = "drop", fill = "right", remove = FALSE) %>%  # Keep the original column
    mutate(
      Phylum = ifelse(is.na(Phylum) | Phylum == "", "Unknown", Phylum),
      Class = ifelse(is.na(Class) | Class == "", "Unknown", paste(Phylum, Class, sep = ";")),
      Order = ifelse(is.na(Order) | Order == "", "Unknown", paste(Class, Order, sep = ";")),
      Family = ifelse(is.na(Family) | Family == "", "Unknown", paste(Order, Family, sep = ";")),
      Genus = ifelse(is.na(Genus) | Genus == "", "Unknown", paste(Family, Genus, sep = ";"))
    ) %>%
    select(-Domain)  # Remove the Domain column
  
  # Step 3: Transform to long format
  long_data <- processed_data %>%
    pivot_longer(cols = c(Phylum, Class, Order, Family, Genus), 
                 names_to = "Rank", values_to = "Value") %>%
    mutate(Value = ifelse(Value == "", "Unknown", Value)) %>%  # Replace empty values with 'Unknown'
    select(first_column_name, Rank, Value) %>%
    mutate(Rank = factor(Rank, levels = c("Phylum", "Class", "Order", "Family", "Genus")))  # Set factor levels
  
  # Step 4: Calculate counts and percentages for each Value by Rank
  Count_data <- long_data %>%
    group_by(Rank, Value) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    group_by(Rank) %>%
    mutate(Percentage = (Count / sum(Count)) * 100)  # Calculate percentage for each Value within each Rank
  
  # Step 5: Join the percentage back to the original long_data
  long_data <- long_data %>%
    left_join(Count_data %>% select(Rank, Value, Percentage), by = c("Rank", "Value"))
  
  # Step 6: Replace low percentages with 'Others'
  long_data <- long_data %>%
    mutate(Value = ifelse(Percentage <= threshold, "Others", Value))
  
  # Step 7: Create unique taxonomic table and arrange values
  unique_taxonomic_table <- long_data %>%
    select(Rank, Value) %>%  # Select only Rank and Value columns
    distinct() %>%          # Get unique combinations of Rank and Value
    arrange(Value)          # Order by Value alphabetically
  
  # Step 8: Filter for Rank == "Phylum" and select relevant columns
  Phylum_data <- unique_taxonomic_table %>%
    dplyr::filter(Rank == 'Phylum')
  
  # Extract unique values
  unique_values <- unique(Phylum_data$Value)
  
  # Step 9: Create the phylum_colors data frame with the unique values
  phylum_colors <- data.frame(
    Value = unique_values,
    stringsAsFactors = FALSE
  )
  
  # Check if 'Others' exists and move it to the last position if it does
  if ("Others" %in% phylum_colors$Value) {
    phylum_colors <- phylum_colors %>%
      mutate(Value = factor(Value, levels = c(setdiff(unique(Value), "Others"), "Others"))) %>%
      arrange(Value)  # Arrange to ensure "Others" is at the end
  }
  
  # Step 10: Create palette for colors
  if (is.null(custom_colors)) {
    # Default color palette
    custom_colors <- c("#2777B4", "#F27E3A", "#49A02E", "#D6322D", "#9466BD", 
                       "#8B564C", "#E377C2", "#BCBD3A", "#50BFCE", "#EDCA45")
  }
  
  # If there are more unique values than colors, repeat the palette
  if (nrow(phylum_colors) > length(custom_colors)) {
    custom_colors <- rep(custom_colors, length.out = nrow(phylum_colors))
  }
  
  # Assign colors to the phylum_colors data frame
  phylum_colors$Color <- custom_colors[1:nrow(phylum_colors)]  # Ensure correct length
  
  # Set "Others" to dark gray if it exists
  if ("Others" %in% unique_values) {
    phylum_colors$Color[phylum_colors$Value == "Others"] <- "#A9A9A9"  # Dark gray for Others
  }  
  
  # Step 11: Assign colors based on Rank levels
  unique_taxonomic_table_with_colors <- unique_taxonomic_table %>%
    left_join(phylum_colors, by = "Value")  # Join on the Value column
  
  # Assign colors based on Rank levels
  for (i in 1:nrow(unique_taxonomic_table_with_colors)) {
    current_rank <- unique_taxonomic_table_with_colors$Rank[i]
    current_value <- unique_taxonomic_table_with_colors$Value[i]
    
    if (current_rank == "Phylum") {
      # Assign the Phylum color
      phylum_color <- phylum_colors$Color[phylum_colors$Value == current_value]
      unique_taxonomic_table_with_colors$Color[i] <- phylum_color
      
      # Check the next rank
      if (i < nrow(unique_taxonomic_table_with_colors)) {
        next_rank <- unique_taxonomic_table_with_colors$Rank[i + 1]
        
        if (next_rank == "Class") {
          unique_taxonomic_table_with_colors$Color[i + 1] <- phylum_color  # Same color for Class
        } else {
          # Assign a lighter color if the next rank is not Class
          lighter_color <- lighten_color(phylum_color, 0.5)  # Lighten by 50%
          unique_taxonomic_table_with_colors$Color[i + 1] <- lighter_color
        }
      }
    } else if (current_rank == "Class") {
      # Check the next rank for Class
      if (i < nrow(unique_taxonomic_table_with_colors)) {
        next_rank <- unique_taxonomic_table_with_colors$Rank[i + 1]
        
        if (next_rank == "Order") {
          unique_taxonomic_table_with_colors$Color[i + 1] <- unique_taxonomic_table_with_colors$Color[i]  # Same color for Order
        } else {
          # Assign a lighter color if the next rank is not Order
          lighter_color <- lighten_color(unique_taxonomic_table_with_colors$Color[i], 0.5)  # Lighten by 50%
          unique_taxonomic_table_with_colors$Color[i + 1] <- lighter_color
        }
      }
    } else if (current_rank == "Order") {
      # Check the next rank for Order
      if (i < nrow(unique_taxonomic_table_with_colors)) {
        next_rank <- unique_taxonomic_table_with_colors$Rank[i + 1]
        
        if (next_rank == "Family") {
          unique_taxonomic_table_with_colors$Color[i + 1] <- unique_taxonomic_table_with_colors$Color[i]  # Same color for Family
        } else {
          # Assign a lighter color if the next rank is not Family
          lighter_color <- lighten_color(unique_taxonomic_table_with_colors$Color[i], 0.5)  # Lighten by 50%
          unique_taxonomic_table_with_colors$Color[i + 1] <- lighter_color
        }
      }
    } else if (current_rank == "Family") {
      # Check the next rank for Family
      if (i < nrow(unique_taxonomic_table_with_colors)) {
        next_rank <- unique_taxonomic_table_with_colors$Rank[i + 1]
        
        if (next_rank == "Genus") {
          unique_taxonomic_table_with_colors$Color[i + 1] <- unique_taxonomic_table_with_colors$Color[i]  # Same color for Genus
        } else {
          # Assign a lighter color if the next rank is not Genus
          lighter_color <- lighten_color(unique_taxonomic_table_with_colors$Color[i], 0.5)  # Lighten by 50%
          unique_taxonomic_table_with_colors$Color[i + 1] <- lighter_color
        }
      }
    } else if (current_rank == "Genus") {
      # Check the next rank for Genus
      if (i < nrow(unique_taxonomic_table_with_colors)) {
        next_rank <- unique_taxonomic_table_with_colors$Rank[i + 1]
        
        if (next_rank != "Phylum") {
          # Assign a lighter color if the next rank is not Phylum
          lighter_color <- lighten_color(unique_taxonomic_table_with_colors$Color[i], 0.5)  # Lighten by 50%
          unique_taxonomic_table_with_colors$Color[i + 1] <- lighter_color
        }
      }
    }
  }
  
  # Assign specific colors for "Others" and "Unknown"
  unique_taxonomic_table_with_colors$Color[unique_taxonomic_table_with_colors$Value == "Others"] <- "#A9A9A9"  # Dark gray for Others
  unique_taxonomic_table_with_colors$Color[unique_taxonomic_table_with_colors$Value == "Unknown"] <- "white"  # White for Unknown
  
  # Select only the Rank, Value, and Color columns
  unique_taxonomic_table_with_colors <- unique_taxonomic_table_with_colors %>%
    select(Value, Color)
  
  # Prepare the data by joining and mutating
  filtered_vOTUs_vBins_Host_Prediction_t_color_df <- long_data %>%
    left_join(unique_taxonomic_table_with_colors, by = "Value") %>%
    mutate(Color = ifelse(is.na(Color), "#FFFFFF", Color)) %>%
    distinct()  # Remove duplicate rows
  
  # Step 12: Create a sorted list of unique values, placing "Unknown" and "Others" at the end
  rank_order <- c("Phylum", "Class", "Order", "Family", "Genus")
  
  # Create a sorted list of unique values, placing "Unknown" and "Others" at the end
  sorted_values <- filtered_vOTUs_vBins_Host_Prediction_t_color_df %>%
    mutate(Rank = factor(Rank, levels = rank_order)) %>%  # Set the order of ranks
    arrange(Rank, Value) %>%  # First sort by Rank, then by Value
    distinct(Value) %>%
    pull(Value)
  
  # Move "Unknown" and "Others" to the end of the factor levels
  sorted_values <- c(setdiff(sorted_values, c("Others", "Unknown")), "Others", "Unknown")
  
  # Set the factor levels for Value based on the sorted order
  filtered_vOTUs_vBins_Host_Prediction_t_color_df <- filtered_vOTUs_vBins_Host_Prediction_t_color_df %>%
    mutate(Value = factor(Value, levels = sorted_values))
  
  # Step 13: Plot the taxonomy
  color_legend <- setNames(filtered_vOTUs_vBins_Host_Prediction_t_color_df$Color, filtered_vOTUs_vBins_Host_Prediction_t_color_df$Value)
  
  n_id <- length(unique(filtered_vOTUs_vBins_Host_Prediction_t_color_df[[1]]))  # Use the first column for unique counts
  
  # Create the river plot using the new Color column
  figure <- ggplot(filtered_vOTUs_vBins_Host_Prediction_t_color_df, 
                   aes(x = Rank, stratum = Value, alluvium = filtered_vOTUs_vBins_Host_Prediction_t_color_df[[1]],  # Use the first column dynamically
                       fill = Value, label = Value)) +  # Use Value for fill and Value for labels
    labs(x = "Taxonomic rank", y = "Percentage", fill = "Taxonomy") +  # Change the legend title as needed
    theme_classic() +
    scale_fill_manual(values = color_legend) +  # Use the named vector for colors
    scale_y_continuous(breaks = c(0, n_id/4, n_id/2, n_id/1.33, n_id), 
                       labels = percent_format(scale = 100 / n_id),
                       expand = c(0.02, 0)) +
    geom_flow(stat = "flow", knot.pos = 1/4, aes.flow = "forward", color = "gray", width = 0.7) + 
    geom_stratum(alpha = .8, width = 0.7, size = 1) + 
    theme(axis.text.x = element_text(size = 12, face = "bold", color = "black"),
          axis.text.y = element_text(size = 12, face = "bold", color = "black"),
          axis.title.y = element_text(size = 12, face = "bold", color = "black"),
          legend.title = element_text(color = "black", size = 12, face = "bold"),
          axis.title.x = element_blank(),
          strip.text.x = element_blank(),
          legend.position = "right")
  
  # Print the river plot
  print(figure)
  return(figure)
  
  # Return the final processed data
  return(invisible(filtered_vOTUs_vBins_Host_Prediction_t_color_df))
}

# Declare global variables
#' @importFrom utils globalVariables
#' @noRd
utils::globalVariables(c("Phylum", "Class", "Order", "Family", "Genus", "Domain", "Count", "Percentage", "Value", "Color", "Rank"))