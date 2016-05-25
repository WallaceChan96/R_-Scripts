#attempt with selex (data incorporated in aldex)

#load selex data
data(selex)

n <- selex[,1:7]
s <- selex[,8:14]

#generate 20 monte-Carlo Dir instances and transform to clr for both sets

nDir <- aldex.clr(n, mc.samples=20, verbose=TRUE)
sDir <- aldex.clr(n, mc.samples=20, verbose=TRUE)

#obtain the monte-Carlo instances and place in matrix for both sets

nMC <- NULL
sMC <- NULL

for(g.1 in getMonteCarloInstances(nDir)) 
	{
		nMC <- cbind(nMC, g.1)	
	}
	
for(g.2 in getMonteCarloInstances(sDir)) 
	{
		sMC <- cbind(sMC, g.2)		
	}

#create list and place each instance matrix into each cell

sample.n <- vector("list", ncol(n))
sample.s <- vector("list", ncol(s))

instance.n <- 0
instance.s <- 0


for(sampleIndex.n in 1:ncol(n))
	{
		for (m in 1:20)
			{
				sample.n[[sampleIndex.n]] <- cbind(sample.n[[sampleIndex.n]], nMC[,m + (instance.n*20)])
			}
		instance.n <- instance.n + 1
		
	}

for(sampleIndex.s in 1:ncol(n))
	{
		for (a in 1:20)
			{
				sample.s[[sampleIndex.s]] <- cbind(sample.s[[sampleIndex.s]], sMC[,a + (instance.s*20)])
			}
		instance.s <- instance.s + 1
		
	}
	
	
#reset index values
sampleIndex.n <- NULL
sampleIndex.s <- NULL

#create the first clr(reference for procrustes) for each of the data sets and place them in matrices
clr.1n <- NULL
clr.1s <- NULL

for(sampleIndex.n in 1:ncol(n))
	{
		clr.1n <- cbind(clr.1n,sample.n[[sampleIndex.n]][,1])
	}

for(sampleIndex.s in 1:ncol(s))
	{
		clr.1s <- cbind(clr.1s,sample.s[[sampleIndex.s]][,1])
	}
	
	pca.1n <- prcomp(clr.1n)
	pca.1s <- prcomp(clr.1s)
	

#reset index values
sampleIndex.n <- NULL
sampleIndex.s <- NULL

#create matrices for all rest of the clr parts and the first two components of procrustes
clr.parts.n <- NULL
clr.parts.s <- NULL

pro.total.n <- NULL
pro.total.s <- NULL

pro.total1.n <- NULL
pro.total1.s <- NULL

pro.total2.n <- NULL
pro.total2.s <- NULL 

#create the other clr and procrustes for all the other instances
for(p in 2:20)
	{
		for(sampleIndex.n in 1:ncol(n))
			{
				clr.parts.n <- cbind(clr.parts.n, sample.n[[sampleIndex.n]][,p])
			}
		
		pca.parts.n <- prcomp(clr.parts.n)
		
		pro.total.n <- procrustes(pca.1n$x, pca.parts.n$x, scale = TRUE)			
		
		pro.total1.n <- cbind(pro.total1.n, pro.total.n$Yrot[,1])
		
		pro.total2.n <- cbind(pro.total2.n, pro.total.n$Yrot[,2])
		
		print(p)
	
	}

#second loop for the other set
for(q in 2:20)
	{
		for(sampleIndex.s in 1:ncol(s))
			{
				clr.parts.s <- cbind(clr.parts.s, sample.s[[sampleIndex.s]][,q])
			}
		
		pca.parts.s <- prcomp(clr.parts.s)
		
		pro.total.s <- procrustes(pca.1s$x, pca.parts.s$x, scale = TRUE)			
		
		pro.total1.s <- cbind(pro.total1.s, pro.total.s$Yrot[,1])
		
		pro.total2.s <- cbind(pro.total2.s, pro.total.s$Yrot[,2])
		
		print(q)
	
	}
	
mean.1n <- NULL
mean.1s <- NULL

mean.2n <- NULL
mean.2s <- NULL

for(i in 1:nrow(pro.total1.n))
	{
		mean.1n <- c(mean.1n, mean(pro.total1.n[i,]))
		mean.2n <- c(mean.2n, mean(pro.total2.n[i,])) 
	}

for(j in 1:nrow(pro.total1.s))
	{
		mean.1s <- c(mean.1s, mean(pro.total1.s[j,]))
		mean.2s <- c(mean.2s, mean(pro.total2.s[j,])) 
	}

plot(pca.1n$x[,1], pca.1n$x[,2], col="blue")
points(mean.1n, mean.2n, col="red")	

plot(pca.1s$x[,1], pca.1s$x[,2], col="blue")
points(mean.1s, mean.2s, col="red")	

plot(mean.1n, mean.2n, col="black")
points(mean.1s, mean.2s, col="red")
		
