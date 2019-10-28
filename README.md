# Multidimensional data analysis - Gross home assignment 
## Andrey Volkov, BBI 174


### 1. Principal component analysis. Consider Example: Handwritten Digits

a) Perform the PCA for these data (perhaps, via singular value decomposition)

First, let us declare the variable of base data set link and read all the observations to variable "digits3".
Let us use `prcomp()` function to perform a principal component analysis on our data set, using `t()` function to transpose data set. 
~~~r
baseDataSetLink <- "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.digits/train.%s"

digit3 <- read.csv(sprintf(baseDataSetLink, 3), header = FALSE)
pca3 <- prcomp(t(digit3), scale = TRUE)
~~~
Let us have a closer look on how `prcomp()` function works. Basically, we have to calculate the average measurement for all the observations in each row. 
With the average value we can calculate the center of the data. Now we can shift the data, so the center of the plot has the same coordinates as the calculated average measurement.
Now, when data is centered on the origin, we can try to fit a line to it. To do it, we can start to draw a rnadom line that goes through the origin. Then we rotate the line until it fits the data as well as it can.

How PCA decides, if it fit is good or not, so let's go back to the original line, that goes through the origin. To quantify how good this line fits the data, PCA projects the data onto it.
And then it finds the line that that maximizes the distances from the projection points to the origin.

So the PCA finds the best fitting line by maximizing the sum of the square distances from the projected point to the origin. So, for this line, PCA projects the data onto it and then measures the distance from this point to the origin. (d1, d2, d3, ...) -> (d1^2 + d2^2 + d3^3 + ...) = SS (distances). So, therefore we find the line with the largest sum of squares distances. This line is called the PC1 - Principal component 1. 

Because it is only 2-D graph, PC2 is simply the line through the origin that is perpendicular to PC1, without any further optimization that has to be done. 
The same way to find the singular vector and the eigen vector. To draw the final PCA plot we simply rotate everything, so the PCA1 is horizontal.
Then we use the projection points to find out where the PCA plot go in the PCA plot.  

Now we can call the `prcomp()` function to do a PCA on our data. With a goal to draw a graph that shows how the variables are related to each other.
Because by default the function expects the variables to be columns and observation to be rows we can use `t()` function to transpose our matrix. 

In order to extract all the PCAs, we have to call `x` variable on our PCA object.
Plotting the data is rather simple by just calling the `plot()` function with PCA1 and PCA2.

The `prcomp()` function returns 3 variables: x, sdev and rotation.

To have a better understanding let us see how much variation in the original data PC1 accounts for. We can calculate the square of standard deviation.
And then calculate the percentages of the variance. Finally, use `barplot()` function to plot the result.  
~~~r
pca3.variance <- pca3$sdev^2
pca3.variance.percentages <- round(pca3.variance/sum(pca3.variance)*100, 1)
barplot(pca3.var.per, main="Scree Plot - Handwritten Digits", xlab="Principal Component", ylab="Percent Variation", xlim=c(0, 30))
~~~
<div style="text-align:center">
<img src="media/task1/scree_plot_handwritten_digits.png" width="700">
</div>

We can see that first 2 components accounts for the majority of the variation of the data.


~~~r
pca3.data <- data.frame(Sample=rownames(pca3$x), X = pca3$x[,1], Y = pca3$x[,2])

ggplot(data = pca3.data, aes(X, Y, label=Sample)) + geom_point() +
  xlab(paste("PC1 - ", pca3.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca3.var.per[2], "%", sep="")) +
  theme_bw() + ggtitle("Handwritten Digit 3 PCA Graph")
~~~
<div style="text-align:center">
<img src="media/task1/handwritten_digit_3_PCA_graph.png" width="700">
</div>






 

Now let us create the gif image that shows, how all PCAs one by one makes better picture of how each of data points related to each other.
In separate folder create an image of each PCAs.
~~~
for (i in 2:256) {
  file <- paste("plot/", i, ".png", sep = "")
  png(filename=file)
  plot(pca3$x[,i-1], pca3$x[,i], xlim = c(-2, 2), ylim = c(-2, 2))
  dev.off()
}
~~~
![](media/pca_in_action.gif)
Now, let us add a few higher dimensions: PC3 and PC4 and see the difference.
~~~
plot(pca3$x[,2], pca3$x[,3])
~~~
![](media/PCA3.png)

~~~
plot(pca3$x[,3], pca3$x[,4])
~~~
![](media/PCA4.png)


### 2. Bias-variance trade-off. Let N be the total length of your first and last names. Also, choose a random number a ∈ [0.4, 0.6] and round it to one decimal place.