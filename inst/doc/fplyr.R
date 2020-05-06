## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 7,
  fig.height = 4,
  fig.align = "center",
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(fplyr)

## ----store_path---------------------------------------------------------------
f <- system.file("extdata", "dt_iris.csv", package = "fplyr")

# Let's have a look at the first four lines of the file
fread(f, nrows = 4)

## ----flply_summary------------------------------------------------------------
species_summ <- flply(input = f, FUN = summary)

# Now `species_summ` is a list of three elements; let's show the 'versicolor' element
species_summ$versicolor

## ----flply_hclust-------------------------------------------------------------
clusters <- flply(f, FUN = function(d) {
  dm <- dist(d[, -1]) # Compute the distance matrix, excluding the first field
  hclust(dm) # Perform the clustering and return the object
})

# The `cluster` variable contains one "hclust" object for each species.
# Let's plot the 'setosa' dendrogram
plot(clusters$setosa)

## ----flply_kmeans-------------------------------------------------------------
kmeans_FUN <- function(d, my_centers) {
  kmeans(d[, -1], centers = my_centers)
}

my_centers <- 2
# We pass `my_centers` to flply(), and flply() passes it to kmeans_FUN
clusters <- flply(f, FUN = kmeans_FUN, my_centers)
# Let's display the centers of the 'setosa' clusters
clusters$setosa$centers

# Now let's do the same thing, but with three centers for each species
my_centers <- 3
clusters <- flply(f, FUN = kmeans_FUN, my_centers)
clusters$setosa$centers

## ----flply_select-------------------------------------------------------------
sepal_length <- flply(f, `[[`, 2)

# Now `sepal_length` contains all the sepal lengths, divided by species
sepal_length

## ----ftply_by-----------------------------------------------------------------
selected_flowers <- ftply(f, function(d, by) {
  if (by == "setosa")
    return(NULL)
  else
    return(d)
})

# Let's have a look at the first few lines of the output; note that it start directly with 'versicolor', because all the 'setosa' flowers have been omitted
head(selected_flowers, 4)

## ----ftply_firstfield---------------------------------------------------------
count_cols <- function(d, by) {
  ncol(d)
}
ftply(f, count_cols)

## ----ftply_head---------------------------------------------------------------
flowers_head <- ftply(f, nblocks = 1)

# Now `flowers_head` has 50 observations, while the original data set had 150. Let's have a look at the first ones.
head(flowers_head, 4)

## ----ftply_parallel-----------------------------------------------------------
result <- ftply(f, parallel = 3, FUN = function(d, by) {
  d[sample(1:nrow(d), 10), ]
})

# Let's check that the output has 30 rows (10 for each species)
nrow(result)

## ----ffply--------------------------------------------------------------------
out <- tempfile() # Create temporary output file
ffply(f, out, function(d, by) {
  # Here, `d` does not contain the subject IDs; they will be automatically added back later
  x <- prcomp(d)$x
  as.data.table(x)
})

# Let's check the result. Note in particular that the subject IDs are present
fread(out, nrows = 4)

## ----fmply_paths--------------------------------------------------------------
out <- c(pca = tempfile(), transf = tempfile())
# Note that the vector needs not be named, we use these names just for convenience

analyze_block <- function(d) {
  # Here, `d` does contain the subject IDs, so we have to remove them...
  x <- prcomp(d[, -1])$x
  # ...and add them back manually
  x <- cbind(d[, 1], x)
  # Transform each number 'z' into e^(-z)
  y <- cbind(d[, 1], exp(-d[, -1]))
  # Return a list of two "data.table"s
  list(x, y)
}

fmply(f, out, analyze_block)

## ----fmply_list---------------------------------------------------------------
analyze_block2 <- function(d) {
  pca <- prcomp(d[, -1])
  x <- cbind(d[, 1], pca$x)
  y <- cbind(d[, 1], exp(-d[, -1]))
  # 'x' and 'y' are the same as before, but now we add the 'pca' object
  list(x, y, pca)
}

iris_pca <- fmply(f, out, analyze_block2)

# Let's have a look at the screeplot of the 'versicolor' PCA
screeplot(iris_pca$versicolor)

