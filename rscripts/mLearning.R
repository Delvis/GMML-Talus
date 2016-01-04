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
#     Laboratory of Forensic Anthropology
#     Department of Life Sciences,
#     University of Coimbra,
#     Portugal

mLearning <- function(dataset){
   # load packages
   pkglist <- c("caret", "foreach", "parallel", "doParallel", "snow")
   loadPackages(pkglist)
   
   ncores <- detectCores() # Number of cores available [parallel]
   minions <- makeCluster(ncores)
   models <- c("C5.0Rules", "C5.0Tree", "svmLinear", "svmPoly",
               "svmRadial", "nnet", "rda", "JRip", "LMT", "knn")
   #"lda", "xgbLinear" goes 89
   ml.model <- list(model = list(), name = NA)
   for(i in 1:length(models)){
      #[caret]
      trainParam <- trainControl(method = "cv",
                                 allowParallel = TRUE,
                                 returnResamp = "all",
                                 savePredictions = TRUE)
      ml.model$model[[i]] <- train(dataset[ ,1:2], dataset[ ,3],
                                   method = models[i],
                                   trControl = trainParam)
      ml.model$name[i] <- models[i]
   }
   stopCluster(minions)
   return(ml.model)  
}