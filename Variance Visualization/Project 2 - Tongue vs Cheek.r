##################
#
#Author: Wallace Chan
#Date: 27/5/2016
#
#Using tongue_vs_cheek dataset (large dataset)
#
#####


#convert txt file from table to data
d <- read.table("tongue_vs_cheek.txt", header=T, row.names=1, check.names=F)

#remove tax info in the last column 
d.w <- d[,1:366]

#using necessary packages (ALDEx2 and Procrustes)
library(ALDEx2)
library(vegan)

#generate 20 monte-Carlo Dir instances and transform to clr
x <- aldex.clr(d.w, mc.samples=20, verbose=TRUE)

#puts each Monte-Carlo Instance into a vector
cl2p <- NULL
	
for (g in getMonteCarloInstances(x))
	{
   		cl2p <- cbind(cl2p,  g)
	}

#create another vector list to place each Monte-Carlo Instance vector in
sample <- vector("list", ncol(d.w))
instance.1 <- 0

#loop that places the 20 Monte-Carlo Instances of each sample/column into a column in the sample matrix
#uses cbind function and and interates the "instance.1" variable to group 20 instances together
for (sampleIndex in 1:366) 
	{
		for (m in 1:20)
		{	 
			sample[[sampleIndex]] <- cbind(sample[[sampleIndex]], cl2p[,m + (instance.1*20)])
			
		}
		
		instance.1 <- instance.1 + 1
	}

#again, not really needs
#function to calculate mean and variance for any data set
var <- matrix(data=NA, nrow=nrow(d.w), ncol=ncol(d.w))

mean <- matrix(data=NA, nrow=nrow(d.w), ncol=ncol(d.w))

rownames(var) <- rownames(d.w)

rownames(mean) <- rownames(d.w)

sampleIndex <- NULL

for (sampleIndex in 1:366)
	{
		var[,sampleIndex] <- apply(sample[[sampleIndex]], 1, var)
		mean[,sampleIndex] <- apply(sample[[sampleIndex]], 1, mean)
	}


############
#general function to be able plot the average of the average of the procrustes projection of each instance with a reference instant
#clr -> pca -> pro (order of function)
#plotting only the first two layers of procrustes (due to the lack of information from the other layers)
#######


#need to nulify sample index from previous function
sampleIndex <- NULL

#combines all columns of the Monte-Carlo instances and places it into another matrix
clr.1 <- NULL

for (sampleIndex in 1:366)
	{
		clr.1 <- cbind(clr.1,sample[[sampleIndex]][,1])
	}

#perfoms pca function on the compiled matrix
#this is the reference point for your procrutes
pca.1 <- prcomp(clr.1)


#create empty matrices
sampleIndex <- NULL
clr.parts <- NULL
pro.total <- NULL
pro.total1 <- NULL
pro.total2 <- NULL

#loop to generate procrustes projections for the dataset, relative to the reference point (pca.1) 
for(n in 2:20)
	{
		for (sampleIndex in 1:366)
		{
			clr.parts <- cbind(clr.parts, sample[[sampleIndex]][,n])
		}
	
		pca.parts <- prcomp(clr.parts)
		
		pro.total <- procrustes(pca.1$x, pca.parts$x, scale = TRUE)
		
		pro.total1 <- cbind(pro.total1, pro.total$Yrot[,1])
		
		pro.total2 <- cbind(pro.total2, pro.total$Yrot[,2])
		
		print(n)
		
	}
#create empty matrices for the means
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








	
	

