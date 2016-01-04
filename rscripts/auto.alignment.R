# Wrap function for auto3dgm' align_shapes function
#
# Authors:
#
#     João Coelho
#     David Navega
#
#     Department of Life Sciences,
#     University of Coimbra,
#     Portugal

auto.alignment <- function(input.dir, output.dir, nlandmarks=c(32, 64)){
   
   # Load required libraries
   library(auto3dgm)
   
   # Get and write to file the names of 3D models used for global alignment
   tmp.dir <- getwd()
   setwd(input.dir)
   model.names <- gsub(pattern = ".off",
                       replacement = "",
                       x = dir(pattern = ".off"))
   setwd(output.dir)
   conn <- file(description = "model.names.txt",
                open = "wt")
   write(model.names, file = conn)
   close(conn)
   
   # Read grouping variable from last character in 3D models name
   group <- as.factor(substr(model.names,
                             start = nchar(model.names),
                             stop = nchar(model.names)))
   setwd(output.dir)
   conn <- file(description = "group.txt", open = "wt")
   write(as.character(group), file = conn)
   close(conn)
   
   # Alignment main function
   cat(sprintf("\n Initializing alignment...\n"))
   cat(sprintf("\n Performing global alignment with %i - %i pseudolandmarks.",
               nlandmarks[1],nlandmarks[2]))
   cat(sprintf("\n\n Analysis started at %s.\n\n",date()))
   tic<-proc.time()[3] # Stopwatch START
   alignment.output <- align_shapes(Data_dir = input.dir,
                                    Output_dir = output.dir,
                                    Levels = nlandmarks,
                                    Ids = model.names,
                                    Names = model.names)
   toc<-proc.time()[3]-tic # Stopwatch STOP
   cat(sprintf("\n\n Analysis finished at %s.\n",date()))
   cat(sprintf("\n Global alignment completed in %.2g seconds.\n",toc))
   setwd(tmp.dir)
   return(alignment.output)
}
