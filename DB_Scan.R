library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
#_______________________________________________________________________________
##### DBSCAN

# Simulate the data
set.seed(2)
n <- 400
x <- cbind(x = runif(4) + rnorm(n, sd = 0.1), y = runif(4) + rnorm(n, sd = 0.1))
true_clusters <- rep(1:4, time = 100)

#_______________________________________________________________________________
##### DBSCAN: CHOICE OF HYPERPARAMETERS

# minPts numero di punti la cui vicinanza identifica i centri 
# -> Rule of thumb, minPts = num_features + 1 = 3 here
minPts <- ncol(x)+1

# eps distanza che identifica la vicinanza, come si sceglie?
# -> Plot of the distances to the minPts nearest neighbor
kNNdistplot(x, k = minPts) #eps = 0.05 seems to be a good threshold

abline(h = 0.05, col = "red", lty = 2)

#_______________________________________________________________________________
##### DBSCAN: GO

dbs <- dbscan(x, eps = 0.05, minPts = 3)
dbs

#PLOT
plot(x, col = dbs$cluster + 1L, pch=19) #0 = noise points, in graph black


#_______________________________________________________________________________
##### DBSCAN: GOODNESS BY SIHOUETTE

clustered_index <- which(dbs$cluster != 0) # Index of non noise points
clustered_points <- x[clustered_index] # only clustered points
clustered_labels <- dbs$cluster[clustered_index] # corresponding labels

#SILHOUETTE
sil <- silhouette(clustered_labels, dist(clustered_points))
summary(sil)

sil_score <- function(labels, dist) { #average silhouette of all data
  sil <- silhouette(labels, dist)
  sil_widths <- sil[,"sil_width"]
  mean(sil_widths)
}

sil_score(clustered_labels, dist(clustered_points))

#FIND THE MAX SILHOUETTE
minPts_grid <- 1:20
eps_grid <- seq(0.01, 0.2, by = 0.01)

max_share_noise <- 0.2

dbscan_perf <- function(minPts, eps) {
  # Compute the silhouette score resulting from dbscan clustering
  dbs <- dbscan(x, eps, minPts) # Run dbscan
  
  clustered_index <- which(dbs$cluster != 0) # Index of non noise points
  clustered_points <- x[clustered_index] # only clustered points
  clustered_labels <- dbs$cluster[clustered_index] # corresponding labels
  nb_clusters <- length(unique(clustered_labels))
  
  if ((nb_clusters > 1 & nb_clusters < n) & (length(which(dbs$cluster == 0))/n < max_share_noise)) { 
    # Silhouette score is defined only if 2 <= nb_clusters <= n-1
    sil_score(clustered_labels, dist(clustered_points))
  }
  
  else {
    # otherwise we return 0 which would be the approx. value of the silhouette
    # score if the clusters were completely overlapping
    0
  }
}

# Silhouette score for all combinations of minPts and eps
perf_grid <- outer(minPts_grid, eps_grid, FUN = Vectorize(dbscan_perf))
dimnames(perf_grid) <- list(minPts_grid, eps_grid)

# Best Parameters
max_score <- max(perf_grid)
argmax_score <- which(perf_grid == max_score, arr.ind = TRUE)
best_eps <- eps_grid[argmax_score[2]]
best_minPts <- minPts_grid[argmax_score[1]]
best_eps
best_minPts
max_score

# DBScan with BEST PARAMETERS
dbs <- dbscan(x, best_eps, best_minPts)
dbs

x11()
plot(x, col = dbs$cluster + 1L, pch=19)
