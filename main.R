# Main script
#
# Authors:
#
#     João Coelho
#     David Navega
#
#     Department of Life Sciences,
#     University of Coimbra,
#     Portugal

## Number of landmarks to extract
#   Change the two numeric values  in the nlandmark variable 
#   to match the number of landmarks to extract. A pair of 
#   consecutive powers of two are a good choice for this 
#   parameters. c(64,128) is a good starting choice.

nlandmarks = c(128, 256) 

## Directory
#   Modify main.dir variable to the full path of your auto3dgm-project
#   folder location.

main.dir <- "C:/auto3dgm-project" # Modify the path if necessary.

input.dir <- paste(main.dir, '/input', sep = "")
output.dir <- paste(main.dir, '/output', sep = "")
source(paste(main.dir,'/rscripts' ,'/auto.alignment.R', sep = ""))
source(paste(main.dir, '/rscripts', '/shape.size.extractor.R', sep = ""))
source(paste(main.dir, '/rscripts', '/mLearning.R', sep = ""))
source(paste(main.dir, '/rscripts', '/performanceMetrics.R', sep = ""))

## Alignment step
align <- auto.alignment(input.dir = input.dir, output.dir = output.dir, 
                        nlandmarks = nlandmarks)

## Feature extraction
dataset <- shape.size.extractor(output.dir = output.dir, pca.var.cutoff = 95,
                                scale=TRUE)

## Model Learning
set.seed(01234567)
ml.model <- mLearning(dataset = dataset)

## Model Evaluation
metrics <- performanceMetrics(ml.model = ml.model)
print(metrics)
