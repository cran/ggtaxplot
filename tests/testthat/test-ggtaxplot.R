library(testthat)
library(ggtaxplot)

test_that("ggtaxplot generates a plot", {
  data <- data.frame(
    ID = c("ID1", "ID2", "ID3"),
    Taxonomy = c("d__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;\
                 o__Enterobacterales;f__Enterobacteriaceae;g__Escherichia",
                 "d__Bacteria;p__Actinobacteria;c__Actinobacteria;\
                 o__Corynebacteriales;f__Corynebacteriaceae;g__Corynebacterium",
                 "d__Bacteria;p__Firmicutes;c__Bacilli;\
                 o__Bacillales;f__Bacillaceae;g__Bacillus")
)
  plot <- ggtaxplot(data)
  
  expect_s3_class(ggtaxplot(data), "gg")
})