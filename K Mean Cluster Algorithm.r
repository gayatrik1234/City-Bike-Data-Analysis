# Implement K-means clustering algorithm
# https://en.wikipedia.org/wiki/K-means_clustering

# Function to calculate the euclidean distance between centroids and points in M and also
# create new clusters and determine new centroids
calculateEuclidean <- function(M,M1,M2,M3)  {
        
        Old_centroids = c(M1,M2,M3)
        for(j in seq(1,13,1)) {
                Result1 = dist(rbind(M[j,],M1), method = "euclidean")
                Result2 = dist(rbind(M[j,],M2), method = "euclidean")
                Result3 = dist(rbind(M[j,],M3), method = "euclidean")
                
                if(Result1 <= Result2 && Result1 <= Result3)
                {      Clustering_vector = replace(Clustering_vector, j ,1) }
                else if(Result2 <= Result1 && Result2 <= Result3)    
                {      Clustering_vector = replace(Clustering_vector, j ,2) }
                else if(Result3 <= Result1 && Result3 <= Result2)
                {      Clustering_vector = replace(Clustering_vector, j ,3) }
                else {break}
        }
        
        #Creating null clusters to put elements into it
        cluster1 = NULL
        cluster2 = NULL
        cluster3 = NULL
        
        #Using for loop assign all points to clusters defind in above step
        for(p in seq(1,13,1)) {
                if(Clustering_vector[p] == 1)
                { cluster1 <- rbind(cluster1,data.frame(M[p,]))}
                else if (Clustering_vector[p] == 2)
                { cluster2 <- rbind(cluster2,data.frame(M[p,]))}
                else if (Clustering_vector[p] == 3)
                { cluster3 <- rbind(cluster3,data.frame(M[p,]))}
                else {break}
        }
        
        #Determine/Reassign cluster centroids M1,M2,M3 after first clusters allocation  
        m = nrow(cluster1)
        n = nrow(cluster2)
        o = nrow(cluster3)
        for (a in seq(1,m,1)){
                New_centroid1 = c(sum(cluster1[,1])/m,sum(cluster1[,2])/m)
        }
        for (a in seq(1,n,1)){
                New_centroid2 = c(sum(cluster2[,1])/n,sum(cluster2[,2])/n)
        }
        for (a in seq(1,o,1)){
                New_centroid3 = c(sum(cluster3[,1])/o,sum(cluster3[,2])/o)
        }
        
        New_centroids = c(New_centroid1,New_centroid2, New_centroid3)
        
        return(list(first=Clustering_vector, second=New_centroids, third = Old_centroids))
        
}

assignClusterToPoints <- function(Xcoord, Ycoord, K) {
        
        #assigne randomly selected values using i as centroid M1,M2,M3 to start
        M1 <- c(Xcoord[i[1]],Ycoord[i[1]])
        M2 <- c(Xcoord[i[2]],Ycoord[i[2]])
        M3 <- c(Xcoord[i[3]],Ycoord[i[3]])
        
        # call the function calculateEuclidean
        Clustering_vector_updated = calculateEuclidean(M,M1,M2,M3)
        
        New_centroids = Clustering_vector_updated$second
        
        New_centroid1 <- c(New_centroids[1],New_centroids[2])
        New_centroid2 <- c(New_centroids[3],New_centroids[4])
        New_centroid3 <- c(New_centroids[5],New_centroids[6])
        
        Old_centroids = Clustering_vector_updated$third
        
        Old_centroid1 <- c(Old_centroids[1],Old_centroids[2])
        Old_centroid2 <- c(Old_centroids[3],Old_centroids[4])
        Old_centroid3 <- c(Old_centroids[5],Old_centroids[6])
        
        cat("Iteration :", Counter, "\n")
        cat("Cluster Allocation : ", Clustering_vector_updated$first, "\n")
        cat("Old Centroids      : ", Clustering_vector_updated$third, "\n")
        cat("New Centroids      : ", Clustering_vector_updated$second, "\n","\n")
        
       repeat {
                Clustering_vector_updated = calculateEuclidean(M,New_centroid1,New_centroid2,New_centroid3)
                
                #assign 2nd output of function to New_centroids
                New_centroids = Clustering_vector_updated$second
                        
                New_centroid1 <- c(New_centroids[1],New_centroids[2])
                New_centroid2 <- c(New_centroids[3],New_centroids[4])
                New_centroid3 <- c(New_centroids[5],New_centroids[6])
                
                #assign 3rd output of function to Old_centroids
                Old_centroids = Clustering_vector_updated$third
                
                Old_centroid1 <- c(Old_centroids[1],Old_centroids[2])
                Old_centroid2 <- c(Old_centroids[3],Old_centroids[4])
                Old_centroid3 <- c(Old_centroids[5],Old_centroids[6])
                Counter = Counter +1
                        
                cat("Iteration :", Counter, "\n")
                cat("Cluster Allocation : ", Clustering_vector_updated$first, "\n")
                cat("Old Centroids      : ", Clustering_vector_updated$third, "\n")
                cat("New Centroids      : ", Clustering_vector_updated$second, "\n","\n")
                
        if (Old_centroid1 == New_centroid1 && Old_centroid2 == New_centroid2 && Old_centroid3 == New_centroid3 )
        { return(Clustering_vector_updated$first)
          print(Clustering_vector_updated$first)
          break} }
}

Xcoord <- c(1,3,1,1,4,5,7,8,9,4,5,1,2)
Ycoord <- c(2,4,5,9,6,5,7,9,7,5,6,1,1)

# Combing Xcoord and Ycoord using dataframe
M <-data.frame(Xcoord,Ycoord)

K <- 3

#Variable for iteration count
Counter = 1

# Vector to store cluster allocation (same like Kmeans$cluster)
Clustering_vector = vector("numeric", length = 13)

# To select 3 random points from Xcoord and Ycoord
i <-sample(1:13,K)

#Calling function for K-mean clsutering
clusters = assignClusterToPoints(Xcoord, Ycoord, K)

# Plot the cluster
plot(Xcoord, Ycoord, col=clusters)
