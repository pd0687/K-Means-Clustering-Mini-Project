# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

install.packages(c("cluster", "rattle.data","NbClust")) ######## PD
#### PD: when I try to install rattle.data, I receive a message:
######## "package 'rattle.data' is not available (for R version 3.1.2)"
#### so I get the data from: https://www.kaggle.com/yannmallegol/winedataset

library(cluster) ######## PD

# Now load the data and look at the first few rows
#### data(wine, package="rattle.data")
#### PD: I'll read the data via:
wine <- read.table("wine.data.txt", header = FALSE, sep = ",", quote = "\"") # PD
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df <- scale(wine[-1]) ######## PD

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

#### PD: the method suggests that 3 clusters is appropriate, because 3
#### clusters contains the data point where the next data point witnesses a 
#### minor dropoff in WSS relative to that witnessed in the previous one.

#### PD: K-means works in that it seeks to optimize a minimial within-group
#### SS while maximizing the betweeen-group SS.  As more and more clusters
#### are added, WSS will keep declining, yet past a certain point, the
#### decline becomes negligible.  A rule of thumb is to target a number
#### of clusters in the plot where the kink in the plot exists.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

#### PD: this method suggests 3 clusters

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# fit.km <- kmeans( ... )
fit.km <- kmeans(df, centers = 3, nstart = 25) ######## PD

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster, wine$V1) ######## PD: from
#### https://www.r-bloggers.com/k-means-clustering-from-r-in-action/,
#### the first variable is "Type."  This first variable corresponds to "V1"
#### in my imported iteration.

#### PD: the outputted table is:
####
####       1  2  3
####    1  0  3 48
####    2  0 65  0
####    3 59  3  0
####
#### It appears as if 172 out of 178 observations are similiarly clustered.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

#clusplot( ... )

clusplot(pam(df, 3)) ######## PD

#### PD: Overall it appears as if the 3 clusters are adequately identified.
#### It does appear, however, as if there appears to be some overlap between
#### the bottom-left and top clusters.
