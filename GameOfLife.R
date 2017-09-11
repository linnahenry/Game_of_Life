#Conway's game of life

lifevec <- rbinom(10000, 1, 0.5)
lifemat <- matrix(lifevec, 100, 100)

lifemat <- matrix(0, 100, 100)
lifemat[1:5, 1:100] <- 1
lifemat[1:100, 1:5] <- 1
lifemat[1:100, 95:100] <- 1
lifemat[95:100, 1:100] <- 1

plotmat <- function(mat){
	x <- 1:100
	x <- rep(x, 100)
	y <- rep(1:100, each = 100)
	plot(x[mat == 0], y[mat==0], col = 'white', pch = 18, ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', bty = 'n')
	points(x[mat==1], y[mat==1], col = 'black', pch = 18)
}	

plotmat2 <- function(mat){
	x <- 1:100
	x <- rep(x, 100)
	y <- rep(1:100, each = 100)
	plot(x[mat == 0], y[mat==0], col = 'white', pch = 18, ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', bty = 'n')
	points(x[mat==1], y[mat==1], col = 'aquamarine2', pch = 18)
	points(x[mat==2], y[mat==2], col = 'aquamarine4', pch = 18)
}	

plotmat(lifemat)

picmat = lifemat

iter = 1000
while(iter > 0){
	for(i in 1:100){
		for(j in 1:100){
			neighb <- vector()
			if(i > 1){
				neighb <- c(neighb, lifemat[i-1,j])
				if(j > 1){
					neighb <- c(neighb, lifemat[i-1, j-1], lifemat[i, j-1])	
				}		
				if(j < 100){
					neighb <- c(neighb, lifemat[i, j+1], lifemat[i-1, j+1])	
				}	
			}		
			if(i < 100){
				neighb <- c(neighb, lifemat[i+1, j])
				if(j > 1){
					neighb <- c(neighb, lifemat[i+1, j-1])	
				}		
				if(j < 100){
					neighb <- c(neighb, lifemat[i+1, j+1])	
				}	
			}
			#now neighb is full
			if(lifemat[i,j] == 1 & sum(neighb) < 2){
				lifemat[i,j] = 0
				picmat[i,j] = 0	
			}		
			if(lifemat[i,j] == 1 & sum(neighb) > 3){
				lifemat[i,j] = 0
				picmat[i,j] = 0 	
			}	
			if(lifemat[i,j] == 0 & sum(neighb) == 3){
				lifemat[i,j] = 1	
				picmat[i,j] = 1
			}	
			if(lifemat[i,j] == 1 & sum(neighb) == 3){
				picmat[i,j] = 2
			}	
			if(lifemat[i,j] == 1 & sum(neighb) == 2){
				picmat[i,j] = 1	
			}	
		}	
	}
	#plotmat(unlist(data.frame(lifemat)))
	plotmat2(unlist(data.frame(picmat)))
	iter = iter -1 
}	

















