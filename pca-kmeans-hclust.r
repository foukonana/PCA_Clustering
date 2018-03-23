setwd("dataset.path")            
wine.data2<- read.csv("wine2.txt", header = T)
wine.data<-wine.data2[,c(2:14)]
summary(wine.data)

#or stat.table which ier better thatn summary
library(pastects)
stat.desc(wine.data)

##correlation matrix rather than covariance matrix
R<-cor(wine.data, method="pearson")
View(round(R, digits=3))
#for the pvalues
library(Hmisc)
r<-rcorr(as.matrix(wine.data),type="pearson")
View(r$P)

#barttlets test for sphericity
library(psych)
cortest.bartlett(R,n = nrow(wine.data))
nrow(wine.data)/ncol(wine.data)

#ΚΜΟ index
KMO(R)

#pca by hand
Eig<-eigen(R) #Eigenvalues & Eigenvectors
EV<-Eig$values;EV  # Eigenvalues
EV/sum(EV)   #variance explained
cumsum(EV/sum(EV))  #cumulative variance explained
plot(EV, type='b', xlab="Principal Component", ylab="Variance Explained",
     main="Scree plot")
abline(h=0.85 , lty=1, col="red")

plot(cumsum(EV/sum(EV)), type='b',main='Cumulative "scree plot"',
     ylab='Cumul. Variance explained', xlab='Principal Component')
abline(h=0.85, lty=1, col="red")   #keep 6 components ~85% of variance explained

#plot clustering
library(ggfortify)
autoplot(princomp(wine.data,cor=TRUE), data = wine.data2, colour = 'Wine', 
         loadings = T, loadings.label=T, label = TRUE)



#κατασκευή διαγώνιου πίνακα με ιδιοτιμές
L<-Eig$values #placing the eigenvalues in L
p<-ncol(wine.data); p
Vm<-matrix(0,nrow=p,ncol=p) #creating a p x p matrix with zeroes.
#Vm is an orthogonal matrix since all correlations between variable are 0.
diag(Vm)<-L #putting the eigenvalues in the diagonals
Vm

Eig$vectors %*% Vm %*% t(Eig$vectors)
L/length(L)

#do the pca matrix
zdat<-scale(wine.data) #this is just to standardize the original data, M = 0, SD =1
pca.scores<- zdat %*% Eig$vectors #scaled values x vectors
colnames(pca.scores)<-c('pca1','pca2','pca3','pca4','pca5','pca6',
              'pc7','pc8','pc9','pc10','pc11','pc12','pc13') #just quickly naming the columns
head(pca.scores)

#scree plot
plot(L,main="Scree Plot",ylab="Eigenvalues",xlab="Component number",type='b')
abline(h=1, lty=2)

#################
# or pca with build in command

wine.pca<-princomp(wine.data,cor=T)

#variance explained by factors
part.pca <- wine.pca$sdev^2/sum(wine.pca$sdev^2)*100
print(part.pca)
print(cumsum(part.pca))

wine.pca$loadings[,1:6]
wine.pca$scolew[1:10,1:6]

par(mfrow=c(1,3))
biplot(wine.pca,choices=1:2)
biplot(wine.pca,choices=c(1,3))
biplot(wine.pca,choices=2:3)
par(mfrow=c(1,1))
biplot(wine.pca)

plot(wine.pca$scores[,1], xlab="PC1", ylab="PC1 scores")


##############################################################
#hierarchical clustring
#try every combination
d<-c("euclidean","maximum","manhattan","canberra","minkowski")
cl<-c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
for(i in d) 
{
  for (j in cl)
  {
    wd<-dist(as.matrix(wine.data),method=i);
    clusters<-hclust(wd,method = "ward.D");
    plot(clusters);
    clusterCut <- cutree(clusters, 3);
    print(table(clusterCut, wine.data2$Wine))}} 


#best performance canberra distance and ward.D linkage
#do hierarchical on those parameters
#'ward' : The nearest-neighbor chain algorithm can be used to find the same
#          clustering defined by Ward's method, in time proportional to the
#          size of the input distance matrix and space linear in the number
#          of points being clustered. At each step find the pair of clusters
#          that leads to minimum increase in total within-cluster variance
#          after merging. This increase is a weighted squared distance
#          between cluster centers. At the initial step, all clusters are
#          singletons (clusters containing a single point)

set.seed(1234)
d<-dist(as.matrix(wine.data), method="canberra")
clusters <- hclust(d, method="ward.D")
plot(clusters)
abline(h=30, lty=1, col="red")
rect.hclust(clusters, k = 3, border = 2:5)

#keep 3 clusters
clusterCut1 <- cutree(clusters,3)
#compare with known
table(clusterCut1, wine.data2$Wine)
randIndex(table(clusterCut1, wine.data2$Wine))

#some nice plotting
clusplot(wine.data,clusterCut1, color=T,shade=T,lebels=2,lines=0,
         main="clusplot on wine.data based on hierarchical clustering")

#plot on the dendrogramm
library(colorspace)
library(dendextend)
wine.labels<-wine.data2$Wine

wine.col <- rev(rainbow_hcl(3))[wine.labels]

wine.clust <- rev(levels(as.numeric(wine.data2$Wine)))
#do the dendrogramm
dend <- as.dendrogram(clusters)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:150)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=3) #, groupLabels=wine.clust)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(wine.data2[,1])[order.dendrogram(dend)]
  )]

# add the wine type to the labels:
labels(dend) <- paste(as.character(wine.data2[,1])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered Wine data
     (the labels give the true wine type)", 
     horiz =  TRUE,  nodePar = list(cex = .007))

#do hierarchical clustering on scaled wine data
wine.scale<-scale(wine.data)
dis<-dist(wine.scale, method="canberra")
clusters2<-hclust(dis, method="ward.D")
plot(clusters2)
clusterCut2<-cutree(clusters2,3)
table(clusterCut2, wine.data2$Wine)
randIndex(table(clusterCut2, wine.data2$Wine))
dend<-as.dendrogram(clusters2)


##k mean clustering
library(NbClust)
library(flexclust)
library(cluster)
library(useful)

#since we start with randomly choosen centroids, a different
# solution can be abtained every time

#also this clustering approach is sensitive to initial
#selection of centroids
#use nstart=25
#uses 25 different start points and decides on the one 
#with least variance

#also requires number of clusters specified in advance
#do the plot of total within-groups sums of squares
#agains the number of clusters in a K-mean solution
#look for a bend in the graph for the appropriate num of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#data=the numeric dataset to be analyzed
#nc=max number of clusters to consider

#scale wine.data
wine.scale<-scale(wine.data)
wssplot(wine.scale)
abline(h=1220, lty=1, col='red')
set.seed(1234)
nc<-NbClust(wine.scale,min.nc=2,max.nc=15,method="kmeans")
table(nc$Best.nc[1,])
#keep 3 centroids
#as suggested by 15 criteria

par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]),xlab="Number of Clusters",
        ylab="Number of Criteria", 
        main="Number of Clusters chosen by a total of 26 criteria")

#do the kmean clustering
set.seed(1234)
fitkm<-kmeans(wine.scale,3,nstart=25)

fitkm$size #how many data in each cluster
fitkm$centers
fitkm$withinss
fitkm$betweenss

## for the centroids in the data rather than the standarized ones
aggregate(wine.data,by=list(cluster=fitkm$cluster),mean)

#cross-tabulation
ctkm<-table(wine.data2$Wine,fitkm$cluster)
ctkm
randIndex(ctkm)

clusplot(wine.data, fitkm$cluster, color=TRUE, shade=TRUE, 
         labels=1, lines=0)

#and another one
plot(fitkm, wine.scale)

#kmeans on original data
fitkmor<-kmeans(wine.data, 3, nstart=25)
fitkmor$size
fitkmor$centers
table(wine.data2$Wine, fitkmor$cluster)
randIndex(table(wine.data2$Wine,fitkmor$cluster))
clusplot(wine.data, fitkmor$cluster,color=T,shade=T,labels=2,lines=0)
