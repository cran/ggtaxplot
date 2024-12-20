# ggtaxplot

## Introduction

`ggtaxplot` is an R package designed to process and visualize taxonomic data through a taxonomic river plot. This package is ideal for researchers and data scientists who need to visualize taxonomic data.

## Description

The `ggtaxplot` function processes data and generates a taxonomic river plot, allowing users to visualize the distribution of taxa across different samples.

## Installation

You can install the `ggtaxplot` package from GitLab using the following command:

```R
devtools::install_git("https://gitlab.com/ccoclet/ggtaxplot.git")
```

## Usage

To use the `ggtaxplot` function, you can follow the example below:

```R
library(ggtaxplot)

# Example data frame
data <- data.frame(
  ID = c("ID1", "ID2", "ID3"),
  Taxonomy = c("d__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacterales;f__Enterobacteriaceae;g__Escherichia",
               "d__Bacteria;p__Firmicutes;c__Bacilli;o__Bacillales;f__Bacillaceae;g__Bacillus",
               "d__Bacteria;p__Actinobacteria;c__Actinobacteria;o__Corynebacteriales;f__Corynebacteriaceae;g__Corynebacterium")
)

# Generate the river plot
plot <- ggtaxplot(data)
print(plot)
```

## Arguments

- `data`: A data frame containing two columns: `ID` and `Taxonomy`.
- `ID_col`: A column with ID values (default is `"ID"`).
- `tax_col`: A column with Taxonomy (default is `"Taxonomy"`).
- `threshold`: A numeric threshold for filtering low-abundance taxa (default is `2`).
- `custom_colors`: Optional custom colors assigned to phyla.

## Value

The function returns a `ggplot2` object of the river plot, which can be further customized using ggplot2 functions.

## Support

For any issues or questions, please open an issue on the GitLab repository or contact the maintainer.

## Contributing

Contributions are welcome! Please fork the repository and submit a pull request with your changes.

## Authors and Acknowledgment

Thank you to all contributors who have helped improve `ggtaxplot`.

## License

This project is licensed under the GPL-3 License.

## Project Status

Development is ongoing. Contributions and feedback are encouraged to enhance the functionality of `ggtaxplot`.
