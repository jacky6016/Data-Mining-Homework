# 'im.train' has 7049 rows (one for each image) and 9216 columns (one for each pixel)

# Clear previous data in the environment
	rm(list = setdiff(ls(), lsf.str()))
	
# Set current & data directories
	setwd('D:/R Files/FinalProject/')
	data.dir   <- 'D:/R Files/FinalProject/data/'
	train.file <- paste0(data.dir, 'training.csv')
	test.file  <- paste0(data.dir, 'test.csv')
	data.file  <- paste0(data.dir, 'data.Rd')

# Set up libraries
	#install.packages("foreach")
	#install.packages('reshape2')
	library(foreach)
	library(reshape2)
	
# Read data from csv file
	d.train <- read.csv(train.file, stringsAsFactors=F)
	d.test  <- read.csv(test.file, stringsAsFactors=F)
	
# Turn the 'image' attribute from string into a numeric vector
	im.train <- foreach(im = d.train$Image, .combine=rbind) %do% 
	{
		as.integer(unlist(strsplit(im, " ")))
	}
	im.test  <- foreach(im = d.test$Image, .combine=rbind) %do% 
	{
		as.integer(unlist(strsplit(im, " ")))
	}
	
# Separate image attribute from original data frame
	d.train$Image <- NULL
	d.test$Image  <- NULL
	

# Using image patches 

# Parameters
	patch_size  <- 18
	search_size <- 2
# list the keypoint attributes
	coordinate.names <- gsub("_x", "", names(d.train)[grep("_x", names(d.train))])	

# for each keypoint attribute, compute the average patch
mean.patches <- foreach(coord = coordinate.names) %do% 
{
	cat(sprintf("computing mean patch for %s\n", coord))
	coord_x <- paste(coord, "x", sep="_")
	coord_y <- paste(coord, "y", sep="_")
 
	patches <- foreach (i = 1:nrow(d.train), .combine=rbind) %do% 
	{
		im  <- matrix(data = im.train[i,], nrow=96, ncol=96)
		x   <- d.train[i, coord_x]
		y   <- d.train[i, coord_y]
		x1  <- (x-patch_size)
		x2  <- (x+patch_size)
		y1  <- (y-patch_size)
		y2  <- (y+patch_size)
		if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
		{
			as.vector(im[x1:x2, y1:y2])
		}
		else
		{
			NULL
		}
	}
	matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)
}
 
# for each keypoint feature on each test image, find the position that best correlates with the average patch
p <- foreach(coord_i = 1:length(coordinate.names), .combine=cbind) %do% 
{
	# Choose one of the keypoint feature
	coord   <- coordinate.names[coord_i]
	coord_x <- paste(coord, "x", sep="_")
	coord_y <- paste(coord, "y", sep="_")
 
	# the average location of a keypoint in training data
	mean_x  <- mean(d.train[, coord_x], na.rm=T)
	mean_y  <- mean(d.train[, coord_y], na.rm=T)
 
	# search region
	x1 <- as.integer(mean_x)-search_size
	x2 <- as.integer(mean_x)+search_size
	y1 <- as.integer(mean_y)-search_size
	y2 <- as.integer(mean_y)+search_size
 
	# Make sure that patch only covers the area inside the 96 x 96 image
	x1 <- ifelse(x1-patch_size<1,  patch_size+1,  x1)
	y1 <- ifelse(y1-patch_size<1,  patch_size+1,  y1)
	x2 <- ifelse(x2+patch_size>96, 96-patch_size, x2)
	y2 <- ifelse(y2+patch_size>96, 96-patch_size, y2)
 
	
	params <- expand.grid(x = x1:x2, y = y1:y2)
 
	# for each image, find the best position for a keypoint frature based on correlation score with the average patch
	r <- foreach(i = 1:nrow(d.test), .combine=rbind) %do% 
	{
		if ((coord_i==1)&&((i %% 100)==0)) { cat(sprintf("%d/%d\n", i, nrow(d.test))) }
		im <- matrix(data = im.test[i,], nrow=96, ncol=96)
 
		r  <- foreach(j = 1:nrow(params), .combine=rbind) %do% 
		{
			x     <- params$x[j]
			y     <- params$y[j]
			p     <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
			score <- cor(as.vector(p), as.vector(mean.patches[[coord_i]]))
			score <- ifelse(is.na(score), 0, score)
			data.frame(x, y, score)
		}
 
		best <- r[which.max(r$score), c("x", "y")]
	}
	names(r) <- c(coord_x, coord_y)
	r
}
	
	
# Prediction for all keypoints on each image
predictions        <- data.frame(ImageId = 1:nrow(d.test), p)
submission         <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
example.submission <- read.csv(paste0(data.dir, 'submissionFileFormat.csv'))
sub.col.names      <- names(example.submission)
example.submission$Location <- NULL 
submission <- merge(example.submission, submission, all.x=T, sort=F)
submission <- submission[, sub.col.names] 
write.csv(submission, file="submission_search.csv", quote=F, row.names=F)

# csv file for kaggle, which only needs one (pre-assigned) keypoint location for each test image
submit <- data.frame(RowId = submission$RowId, Location = submission$Location)
write.csv(submit, file = "patch_method.csv", row.names = FALSE)
