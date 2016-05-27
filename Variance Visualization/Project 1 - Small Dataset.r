##################
#
#Author: Wallace Chan
#Date: 27/5/2016
#
#Using a specific small dataset with apparent variance
#
#####


#converting the vector into characters, from table to data
meta <- read.table("metadata.txt", header=T, row.names=1, check.names=F)

#Concatenate vectors after converting to character, putting together the three columns from the metadata
nms <- paste(meta[,2], meta[,3], meta[,1], sep=":")

#converting txt file from table to data
d <- read.table("countfinal2.txt", header=T, row.names=1, check.names=F)

#change the column names, using the metadata titles
colnames(d) <- nms 

#selecting only the first 7 replicate or columns
d.w <- d[,1:7]

#using the necessary packages(ALDEx2 and Procrustes)
library(ALDEx2)
library(vegan)

#generate 1000 monte-Carlo Dir instances and transform to clr
x <- aldex.clr(d.w, mc.samples=1000, verbose=TRUE)

#putting each Monte-Carlo Instance into a vector
cl2p <- NULL
	
for (g in getMonteCarloInstances(x))
	{
   		cl2p <- cbind(cl2p,  g)
	}

#set individual matrices for each sample
t <- NULL
u <- NULL
v <- NULL
w <- NULL
y <- NULL
z <- NULL
a <- NULL

#cbind each 1000 instances of each sample to the corresponding matrices
for (m in 1:1000) 
	{
		t <- cbind(t,cl2p[,m])
		u <- cbind(u,cl2p[,m+1000])
		v <- cbind(v,cl2p[,m+2000])
		w <- cbind(w,cl2p[,m+3000])
		y <- cbind(y,cl2p[,m+4000])
		z <- cbind(z,cl2p[,m+5000])
		a <- cbind(a,cl2p[,m+6000])
	}

#not really needed
#set matrix for variance and mean, keeping same name and parameters
#apply variance and mean function to each matrix	
var <- matrix(data=NA, nrow=nrow(t), ncol=7)
mean <- matrix(data=NA, nrow=nrow(t), ncol=7)
rownames(var) <- rownames(t)
rownames(mean) <- rownames(t)

var[,1] <- apply(t, 1, var)	
mean[,1] <- apply(t, 1, mean)	

var[,2] <- apply(u, 1, var)	
mean[,2] <- apply(u, 1, mean)

var[,3] <- apply(rv, 1, var)	
mean[,3] <- apply(v, 1, mean)

var[,4] <- apply(w, 1, var)	
mean[,4] <- apply(w, 1, mean)

var[,5] <- apply(y, 1, var)	
mean[,5] <- apply(y, 1, mean)	

var[,6] <- apply(z, 1, var)	
mean[,6] <- apply(z, 1, mean)

var[,7] <- apply(a, 1, var)	
mean[,7] <- apply(a, 1, mean)




#implementing PCA and Procrustes, comparison between 2 procrustes  (for reference)
clr.1 <- cbind(t[,1], u[,1], v[,1], w[,1], y[,1], z[,1], a[,1])

pca.1 <- prcomp(clr.1)

clr.2 <- cbind(t[,2], u[,2], v[,2], w[,2], y[,2], z[,2], a[,2])

pca.2 <- prcomp(clr.2)

pro.1 <- procrustes(pca.1$x, pca.2$x, scale = TRUE)

plot(pca.1$x[,1], pca.1$x[,2], col="red")

points(pro.1$Yrot[,1], pro.1$Yrot[,2], col="blue")


#function to compare and plot all procrustes 
#if you want to see the variables use rbind instead for the clr.?
#clr -> pca -> pro (order of functions)
clr.1 <- cbind(t[,1], u[,1], v[,1], w[,1], y[,1], z[,1], a[,1])

pca.1 <- prcomp(clr.1)

pro.total <- NULL
pro.total1 <- NULL
pro.total2 <- NULL

#loop to generate procrustes projections for the dataset, relative to the reference point (pca.1) 
for (n in 2:10)
	{
		clr.parts <- cbind(t[,n], u[,n], v[,n], w[,n], y[,n], z[,n], a[,n])
		
		pca.parts <- prcomp(clr.parts)
		
		pro.total <- procrustes(pca.1$x, pca.parts$x, scale = TRUE)
		
		pro.total1 <- cbind(pro.total1, pro.total$Yrot[,1])
		
		pro.total2 <- cbind(pro.total2, pro.total$Yrot[,2])
		
	}	

#creating empty matrices for the means
mean.1 <- NULL
mean.2 <- NULL 

#loops to place the mean of both layers of procustes into seperate matrices
for(f in 1:nrow(pro.total1))
	{
		mean.1 <- c(mean.1, mean(pro.total1[f,]))
		mean.2 <- c(mean.2, mean(pro.total2[f,])) 	
	}

#final plot
plot(pca.1$x[,1], pca.1$x[,2], col="blue")
points(mean.1, mean.2, col="red")

	
	



