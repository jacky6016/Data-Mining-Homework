# Retrieve Data 
	data<-read.table(url("http://www.datagarage.io/api/5488687d9cbc60e12d300ba5"))

# Data Cleaning
	o<-seq(2, 4000, by=2)
	d_c<-as.character(data[o, ])
	p_m<-sapply(strsplit(d_c[], ""), function(d_c) which(d_c == ":"))
	data<-data.frame(X=as.double(substr(d_c,p_m[2,]+1, 38)), Y=as.double(substr(d_c,p_m[1,]+1, 17)))

# Plot Data
	plot(data)

# Curve fitting using different models
	
	x <- data$X
	y <- data$Y
	
# 1. Linear & Polynomial Model (lm) 

	fit1 <- lm(y~x)		#linear

	fit2 <- lm(y~poly(x,2,raw=TRUE))	#second degree

	fit3 <- lm(y~poly(x,3,raw=TRUE))	#third degree

	fit4 <- lm(y~poly(x,4,raw=TRUE))	#fourth degree

	fit10 <- lm(y~poly(x,10,raw=TRUE))
	
	fit100 <- lm(y~poly(x,100,raw=TRUE))

	O <- order(data$X)					#X values sorted in order
	
	# Plot fitting curves
	lines(data$X[O], predict(fit1, data.frame(x=data$X[O])), col="red", lwd=3)	
	lines(data$X[O], predict(fit2, data.frame(x=data$X[O])), col="green", lwd=3)
	lines(data$X[O], predict(fit3, data.frame(x=data$X[O])), col="blue", lwd=3)
	lines(data$X[O], predict(fit4, data.frame(x=data$X[O])), col="purple", lwd=3)
	lines(data$X[O], predict(fit10, data.frame(x=data$X[O])), col="brown", lwd=3)
	lines(data$X[O], predict(fit100, data.frame(x=data$X[O])), col="orange", lwd=3)	
	
	# Generate predicted values
	Predicted_1 <- predict(fit1, data.frame(x=data$X))
	Model_1 <- data.frame(X = data$X, Y = Predicted_1)
	
	Predicted_2 <- predict(fit2, data.frame(x=data$X))
	Model_2 <- data.frame(X = data$X, Y = Predicted_2)
	
	Predicted_3 <- predict(fit3, data.frame(x=data$X))
	Model_3 <- data.frame(X = data$X, Y = Predicted_3)
	
	Predicted_4 <- predict(fit4, data.frame(x=data$X))
	Model_4 <- data.frame(X = data$X, Y = Predicted_4)
	
	Predicted_5 <- predict(fit10, data.frame(x=data$X))
	Model_5 <- data.frame(X = data$X, Y = Predicted_5)
	
	Predicted_6 <- predict(fit100, data.frame(x=data$X))
	Model_6 <- data.frame(X = data$X, Y = Predicted_6)
	
	# Evaluate prediction accuracy by checking RMSE values
	Error1 <- data$Y - Predicted_1
	RMSE1 <- sqrt(mean(Error1^2))
	
	Error2 <- data$Y - Predicted_2
	RMSE2 <- sqrt(mean(Error2^2))
	
	Error3 <- data$Y - Predicted_3
	RMSE3 <- sqrt(mean(Error3^2))
	
	Error4 <- data$Y - Predicted_4
	RMSE4 <- sqrt(mean(Error4^2))
	
	Error5 <- data$Y - Predicted_5
	RMSE5 <- sqrt(mean(Error5^2))
	
	Error6 <- data$Y - Predicted_6
	RMSE6 <- sqrt(mean(Error6^2))
	
# 2. Loess Model
	
	lw1 <- loess(y ~ x,data=data)
	
	# Plot fitting curves
	lines(data$X[O],lw1$fitted[O],col="cyan",lwd=3)
	
	# Generate predicted values
	Predicted_7 <- lw1$fitted
	Model_7 <- data.frame(X = data$X, Y = Predicted_7)
	
	# Evaluate prediction accuracy by checking RMSE values
	Error7 <- data$Y - Predicted_7
	RMSE7 <- sqrt(mean(Error7^2))

# 3. Non-linear Curve Fitting
	
	# Set sin & cos coefficients for modeling 
	p1 = 0.5
	p2 = 0.5
	fit_nl = nls(y ~ p1*cos(p2*x) + p2*sin(p1*x), start=list(p1=p1,p2=p2))
	
	# Generate predicted values
	Predicted_8 <- predict(fit_nl, data.frame(x=data$X))
	Model_8 = data.frame(X = data$X, Y = Predicted_8)
	
	# Plot fitting curves
	lines(data$X[O], predict(fit_nl, data.frame(x=data$X[O])), col="darkblue", lwd=3)
	
	# Evaluate prediction accuracy by checking RMSE values
	Error8 <- data$Y - Predicted_8
	RMSE8 <- sqrt(mean(Error8^2))

# Write predicted values to CSV files
	write.csv(Model_1, file = "LinearModel.csv", row.names = FALSE)
	write.csv(Model_2, file = "2ndOrderModel.csv", row.names = FALSE)
	write.csv(Model_3, file = "3rdOrderModel.csv", row.names = FALSE)
	write.csv(Model_4, file = "4thOrderModel.csv", row.names = FALSE)
	write.csv(Model_5, file = "10thOrderModel.csv", row.names = FALSE)
	write.csv(Model_6, file = "100thOrderModel.csv", row.names = FALSE)
	write.csv(Model_7, file = "LoessModel.csv", row.names = FALSE)
	write.csv(Model_8, file = "NonLinearModel.csv", row.names = FALSE)

# Save each fitting curve in a .png file	
	png('LinearModel.png')
	plot(data)	
	lines(data$X[O], predict(fit1, data.frame(x=data$X[O])), col="red", lwd=3)
	dev.off();

	png('2ndOrderModel.png')	
	plot(data)	
	lines(data$X[O], predict(fit2, data.frame(x=data$X[O])), col="green", lwd=3)
	dev.off();
	
	png('3rdOrderModel.png')	
	plot(data)	
	lines(data$X[O], predict(fit3, data.frame(x=data$X[O])), col="blue", lwd=3)
	dev.off();
	
	png('3rdOrderModel.png')	
	plot(data)	
	lines(data$X[O], predict(fit3, data.frame(x=data$X[O])), col="blue", lwd=3)
	dev.off();	
	
	png('4thOrderModel.png')	
	plot(data)	
	lines(data$X[O], predict(fit4, data.frame(x=data$X[O])), col="purple", lwd=3)
	dev.off();
	
	png('10thOrderModel.png')	
	plot(data)	
	lines(data$X[O], predict(fit10, data.frame(x=data$X[O])), col="brown", lwd=3)
	dev.off();
	
	png('100thOrderModel.png')	
	plot(data)	
	lines(data$X[O], predict(fit100, data.frame(x=data$X[O])), col="orange", lwd=3)	
	dev.off();
	
	png('LoessModel.png')	
	plot(data)	
	lines(data$X[O],lw1$fitted[O],col="cyan",lwd=3)
	dev.off();
	
	png('NonLinearModel.png')	
	plot(data)	
	lines(data$X[O], predict(fit_nl, data.frame(x=data$X[O])), col="darkblue", lwd=3)
	dev.off();
