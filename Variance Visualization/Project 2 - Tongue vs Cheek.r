
d <- read.table("tongue_vs_cheek.txt", header=T, row.names=1, check.names=F)

#remove tax info in the last column 
d.w <- d[,1:366]

library(ALDEx2)
library(vegan)

#generate 128 monte-Carlo Dir instances and transform to clr
x <- aldex.clr(d.w, mc.samples=20, verbose=TRUE)

cl2p <- NULL
	
for (g in getMonteCarloInstances(x))
	{
   		cl2p <- cbind(cl2p,  g)
	}

sample <- vector("list", ncol(d.w))
instance.1 <- 0

for (sampleIndex in 1:366) 
	{
		for (m in 1:20)
		{	 
			sample[[sampleIndex]] <- cbind(sample[[sampleIndex]], cl2p[,m + (instance.1*20)])
			
		}
		
		instance.1 <- instance.1 + 1
	}
	
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

sampleIndex <- NULL

clr.1 <- NULL

for (sampleIndex in 1:366)
	{
		clr.1 <- cbind(clr.1,sample[[sampleIndex]][,1])
	}

pca.1 <- prcomp(clr.1)

sampleIndex <- NULL
clr.parts <- NULL
pro.total <- NULL
pro.total1 <- NULL
pro.total2 <- NULL

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

mean.1 <- NULL
mean.2 <- NULL 

for(f in 1:nrow(pro.total1))
	{
		mean.1 <- c(mean.1, mean(pro.total1[f,]))
		mean.2 <- c(mean.2, mean(pro.total2[f,])) 
	}

plot(pca.1$x[,1], pca.1$x[,2], col="blue")
points(mean.1, mean.2, col="red")	








	
	

