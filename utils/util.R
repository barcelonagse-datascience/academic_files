make_blobs <- function(n_samples=100, n_features=2, centers=3, 
					   cluster_std=1.0, center_box=c(-10,10), 
					   shuffle=TRUE) {
	if (is.matrix(centers)) {
		if (ncol(centers) != n_features) stop("Dimensionality of centers must equal number of features.")
	} else {
		centers <- runif(n = n_features * centers, min=center_box[1], max=center_box[2])
		centers <- matrix(centers, ncol=n_features)
	}
	
	if (length(cluster_std) != 1 & length(cluster_std) != nrow(centers)) stop("Cluster_std must be 1 or the same length as the number of clusters")
	
	categories <- sample(nrow(centers), size = n_samples, replace = TRUE)
	
	starting_points <- matrix(
		rnorm(n = n_samples * n_features), 
		ncol = n_features
	)
	
	if (length(cluster_std) == 1) points <- starting_points * cluster_std
	else points <- starting_points * cluster_std[categories]
	
	points <- points + centers[categories, ]
	
	data<- as.data.frame(cbind(points,categories))
	colnames(data) <- c('X1','X2','class')
	data$class <- as.factor(data$class)
	return(data)
}



accuracy_score <- function(real, pred, positive='1'){
	correct = sum(real==pred)
	correct/length(real)
}

precision_score <- function(real, pred, positive='1'){
	pos <- real==positive
	sum(pred[pos]==positive)/sum(pred==positive)
}

recall_score <- function(real, pred, positive='1'){
	pos <- real==positive
	sum(pred[pos]==positive)/sum(real==positive)
}

F1_score <- function(real, pred, positive='1'){
	prec <- precision_score(real,pred,positive)
	rec <- recall_score(real,pred,positive)
	2*(prec*rec)/(prec+rec)
}

plot_confusion_matrix <- function(real, pred){
	confusion_matrix <- as.data.frame(table(real,pred))
	plt<-ggplot(data = confusion_matrix,
				mapping = aes(x = pred, y = real)) +
		geom_tile(aes(fill = Freq)) +
		geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
		scale_fill_gradient(low = "#fcfcfc",
							high = "#000077"
		)+
		theme_minimal()+
		theme(panel.grid.minor = element_line(colour = "white")) +
		theme(panel.grid.major = element_line(colour = "white"))
	plt
}

plot_svm <- function(model, data, plot_support=TRUE){
	x <- seq(min(data[,1]), max(data[,1]), length.out=30)
	y <- seq(min(data[,2]), max(data[,2]), length.out=30)
	Xcon <- data.frame(matrix(c(rep(x,length(y)),
								rep(y, rep(length(x), length(y)))),,2)) #Set all possible pairs of x and y  on a grid
	
	try(dec <- predict(model, data[,1:2], type = "decision"), silent=TRUE)
	try(dec <- predict(model$finalModel, data[,1:2], type = "decision"), silent=TRUE)
	
	unsure <- data[dec>=-1 & dec<=1,]
	
	colnames(Xcon) <- colnames(data)[1:2]
	try(Xcon$class <- predict(model, Xcon, type = "decision"), silent=TRUE)
	try(Xcon$class <- predict(model$finalModel, Xcon, type = "decision"), silent=TRUE)
	colnames(Xcon)<-c('X1','X2','class')
	colnames(data)<-c('X1','X2','class')
	colnames(unsure)<-c('X1','X2','class')
	
	plt<-ggplot(data)+
		geom_point(aes(x=X1, y=X2, color = as.factor(class)))+
		geom_contour(data=Xcon, aes(x=X1, y=X2, z=class), breaks=0)
	if(max(Xcon$class)>=1){
		plt<-plt+geom_contour(data=Xcon, aes(x=X1, y=X2, z=class), breaks=1, linetype='dashed')
	}
	if(min(Xcon$class)<=-1){
		plt<-plt+geom_contour(data=Xcon, aes(x=X1, y=X2, z=class), breaks=-1, linetype='dashed')
	}
	if(plot_support==TRUE){
		plt<-plt+geom_point(data=unsure, aes(x=X1 , y=X2), size=7, color='black', shape=1)
	}
	plt
}

get_nearest_neighbors <- function(data, target, k){
    distances = c()
    for(i in 1:nrow(data)){
        distances <- c(distances, dist(rbind(data[i,], target)))
    }
    print
    ids <- sort(distances, index.return=TRUE)$ix
    return(data[ids,][1:K,])
    
}
