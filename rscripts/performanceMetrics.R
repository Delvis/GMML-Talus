# performanceMetrics computes simple metrics for binary classifiers evaluation.
#
#  Metrics:
#     Overall accuracy
#     Kappa
#     Sensitivity
#     Predictive value 
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

performanceMetrics <- function(ml.model){
   library(caret)
   metrics <- data.frame(Model = NA,
                         OverallAccuracy = NA,
                         Kappa = NA,
                         SensitivityFemale = NA,
                         SensitivityMale = NA,
                         PredValFemale = NA,
                         PredValMale = NA)
   for(i in 1:length(ml.model$model)){
      cfMat <- confusionMatrix.train(ml.model$model[[i]])$table
      mtr1 <- sum(diag(cfMat))/sum(cfMat)   # Overall Accuracy
      mtr2 <- ml.model$model[[i]]['results'][[1]]$Kappa #Kappa value
      mtr3 <- diag(cfMat)[1]/sum(cfMat[,1]) # Sensitivity Female
      mtr4 <- diag(cfMat)[2]/sum(cfMat[,2]) # Sensitivity Male
      mtr5 <- diag(cfMat)[1]/sum(cfMat[1,]) # Predictive Value Female
      mtr6 <- diag(cfMat)[2]/sum(cfMat[2,]) # Predictive Value Male
      aux <- rbind(mtr1, mtr2, mtr3, mtr4, mtr5, mtr6)
      metrics[i,1] <- ml.model$model[[i]]$method
      metrics[i,2:7] <- aux
   }
   return(metrics)
}
