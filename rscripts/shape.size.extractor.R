# Automatic feature extraction function
#
#   The function extracts shape and size information from landmark
#   configurations, acquired by auto3dgm.
#   Two ordination techniques, PCA and LDA, were applied during the
#   process. Shape is extracted with LOO sampling.
#
# Authors:
#
#     João Coelho
#     David Navega
#
#     Department of Life Sciences,
#     University of Coimbra,
#     Portugal

shape.size.extractor <- function(output.dir, pca.var.cutoff = 95, scale=TRUE){
    start <- date()
    tic <- proc.time()[3] # Stopwatch START
    # Load require libraries
    require(shapes)
    require(geomorph)
    # Load parallel computing libraries for fast processing in multicore CPUs
    require(foreach)
    require(parallel)
    require(doParallel)
    # Parallel backend based on OS
    if(!Sys.info()['sysname']=="Windows"){
        require(multicore)
    }else{require(snow)}
    ncores<-detectCores() # Number of cores available [parallel]
    
    #*Modified two.d.array
    two.d.array.one<-function(A){
        pxk <- dim(A)[1] * dim(A)[2]
        tmp <- aperm(A)
        dim(tmp) <- c(1, pxk)
        rownames(tmp) <- dimnames(A)[[2]]
        return(tmp)
    }
    #*
    
    # Set working directory to auto.alignment output directory
    tmp.dir <- getwd()
    setwd(output.dir)

    # Load high resolution sampling unscaled pseudolanmarks coordinates
    landmark.data<-read.morphologika(file="./morphologika_2_unscaled.txt")
    # Load grouping variable
    group.var<-factor(x=readLines("group.txt"),levels = c("F","M"),
                      labels=c("Female","Male"))
    
    # Shape Extraction
    minions <- makeCluster(ncores) # Backend registration [doParallel]
    registerDoParallel(minions)  # Backend registration [doParallel]
    ############################################################################
    shape.var<-foreach(i = 1:dim(landmark.data)[3], .combine = rbind,
                   .packages = c("geomorph","shapes","stats"))%dopar%{
    #Perform GPA and OPA to obtain coordinates in Kendall Space
    
    # GPA
    gpa.lds<-procGPA(x=landmark.data[,,-i],scale = scale,
                             proc.output = FALSE,pcaoutput = FALSE,
                             distances = FALSE)$rotated
    # Mean shape of GPA
    meanConfig<-mshape(gpa.lds) # Mean configuration of Kendall Space
    # OPA
    opa.lds<-procOPA(A=meanConfig,B=landmark.data[,,i],scale = scale)$Bhat
    
    # Ordination I - PCA
    # Array to matrix
    gpa.coords<-two.d.array(A = gpa.lds)
    opa.coords<-two.d.array.one(A=opa.lds)
    # PCA model estimation
    pca.model<-prcomp(x=gpa.coords)
    # Explained variance of PCA
    pca.var<-(pca.model$sdev^2/sum(pca.model$sdev^2))*100
    # Cumulative variance
    pca.cvar<-cumsum(pca.var)
    # Retained PCAs
    ret.pca<-which(pca.cvar<=pca.var.cutoff)
    # PCA Scores
    pca.gpa<-predict(pca.model,gpa.coords)[,ret.pca]
    pca.opa<-predict(pca.model,opa.coords)[,ret.pca]
    
    # Ordination II - CVA
    # CVA model estimation
    cva.model<-lda(pca.gpa,grouping = group.var[-i],prior=c(0.5,0.5))
    shape<-predict(cva.model,pca.opa)$x
    shape
    }
    ############################################################################
    toc<-proc.time()[3]-tic # Stopwatch STOP
    end<-date()
    stopCluster(minions)
    # Extract size and merge shape, size and grouping variable in a data.frame
    colnames(shape.var)<-"Shape"
    size.var<-centroid.size(x = landmark.data) #[shapes]
    setwd(tmp.dir)
    return(data.frame(Shape=shape.var,Size=size.var,Sex=group.var))
}
