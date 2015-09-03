# Wrap function for caret machine learning library.
#   
#    Several machine learning are training and evaluated with
#    cross-validation resampling (K-Fold).
#    A vector of models, the variable models, can be modified
#    to include/exclude any algorithm available in caret library.
#
# Authors:
#
#     João Coelho
#     David Navega
#
#     Department of Life Sciences,
#     University of Coimbra,
#     Portugal

mLearning<-function(dataset){
    require(caret)
    # Parallel backend based on OS
    require(parallel)
    require(doParallel)
    require(foreach)
    if(!Sys.info()['sysname']=="Windows"){
        require(multicore)
    }else{require(snow)}
    
    ncores <- detectCores() # Number of cores available [parallel]
    minions <- makeCluster(ncores)
    models <- c("C5.0Rules", "C5.0Tree", "svmLinear", "svmPoly", 
		"svmRadial","nnet", "rbf","JRip","LMT","knn")
    
    ml.model<-list(model=list(), name=NA)
    for(i in 1:length(models)){
        #[caret]
        trainParam <- trainControl(method = "cv",allowParallel = TRUE,
                                 returnResamp = "all",savePredictions = TRUE)
        ml.model$model[[i]] <- train(dataset[,1:2],dataset[, 3],method = models[i],
                                 trControl = trainParam)
        ml.model$name[i]<-models[i]
    }
    stopCluster(minions)
    return(ml.model)  
}
