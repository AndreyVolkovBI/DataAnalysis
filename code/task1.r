library(ggplot2)
baseDataSetLink <- "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.digits/train.%s"

# a) Perform the PCA for these data (perhaps, via singular value decomposition)
makeBarPlot <- function(pca, digit) {
  pca.variance.percentages <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)
  
  barplot(
    pca.variance.percentages,
    main=paste("Scree Plot - Handwritten Digits", digit),
    xlab="Principal Component", 
    ylab="Percent Variation", 
    xlim=c(0, 30)
  )
}

makePCAPlot <- function(pca, digit) {
  pca.variance.percentages <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)
  pca.data <- data.frame(Sample=rownames(pca$x), X = pca$x[,1], Y = pca$x[,2])
  
  ggplot(data = pca.data, aes(X, Y, label=Sample)) + geom_point(color = "steelblue") +
    xlab(paste("PC1 - ", pca.variance.percentages[1], "%", sep="")) +
    ylab(paste("PC2 - ", pca.variance.percentages[2], "%", sep="")) +
    theme_bw() + ggtitle(paste("Handwritten Digit", digit, "PCA Graph"))
}

digits3 <- read.csv(sprintf(baseDataSetLink, 3), header = FALSE)
pca3 <- prcomp(digits3, scale = TRUE)
makeBarPlot(pca3, 3)
makePCAPlot(pca3, 3)

# b) Reproduce Figure 14.23 showing the relevant code. Discuss the plot. Take another digit (other than 3) and repeat.
digits5 <- read.csv(sprintf(baseDataSetLink, 5), header = FALSE)
pca5 <- prcomp(t(digits5), scale = TRUE)
summary(pca5)

makeBarPlot(pca5, 5)
makePCAPlot(pca5, 5)

# c) Explain in words Eq.(14.55) in detail (structure, in what basis it is written, possible use). Use the PCA terminology (e.g., “scores”, etc).
makeImage <- function(vector) {
  matrix_ <- matrix(as.numeric(vector), 16, 16)
  image(-matrix_[,ncol(matrix_):1], axes = F, col = grey(seq(0, 1, length = 256)))
}

makeImage(colMeans(digits3))

digits3_pca <- PCA(digits3)
makeImage(digits3[2,])
makeImage(digits3_pca$svd$V[,2])
makeImage(digits3_pca$svd$V[,2])

# d) Using the PC scores for some observation, reconstruct it in the original space, visualize it and compare the reconstructed image with the original one

pca3_ <- prcomp(digits3, scale = FALSE)
pca3_
makeImage(pca3_$x[4,])
makeImage(digits3[4,])
