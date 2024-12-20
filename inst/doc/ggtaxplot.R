## ----setup_install------------------------------------------------------------
devtools::install_git("https://gitlab.com/ccoclet/ggtaxplot.git")

## ----setup_load---------------------------------------------------------------
library(ggtaxplot)

## ----example_data-------------------------------------------------------------
data <- data.frame(
    ID = c("ID1", "ID2", "ID3"),
    Taxonomy = c("d__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacterales;f__Enterobacteriaceae;g__Escherichia",
                 "d__Bacteria;p__Firmicutes;c__Bacilli;o__Bacillales;f__Bacillaceae;g__Bacillus",
                 "d__Bacteria;p__Actinobacteria;c__Actinobacteria;o__Corynebacteriales;f__Corynebacteriaceae;g__Corynebacterium")
)

## ----generate_plot------------------------------------------------------------
plot <- ggtaxplot(data)
print(plot)

