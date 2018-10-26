# TODO: import these datasets from the corresponding libraries
dsName <- c("Ionosphere", "iris", "Glass", "Arthritis", "BreastCancer", "Sonar", "Wine") # 
packName <- c("mlbench", "datasets", "mlbench", "vcd", "mlbench", "mlbench", "candisc")

# The following are vectors, so that multilabel datasets are allowed!
className <- c(c("Class"),c("Species"), c("Type"), c("Improved"), 
               c("Class"),c("Class"),c("Cultivar"))  # Name of class attribute
classVar <- #c(35, 5, 10, 5, 11, 61, 1)   # ordinal of the class attribute
    c(c(35),c(5),c(10),c(5),c(11),c(61),c(1))# ordinals of the label attributes
idNumber <- c(NaN, NaN, NaN, 1, 1, NaN, NaN) # Other attributes to dispose of: mainly identifiers.

K <- c(2, 3, 7, 3, 2, 2, 3)  
# No. of classes in the class variable, this can actually be obtained from the dataset
# by something like unique(ds)

#To find out the number of instances and features we over the datanames
nds <- length(dsName)# number of datasets being analyzed
m <- vector(mode = "numeric", length = nds)
n <- vector(mode = "numeric", length = nds)
Kp <- vector(mode = "numeric", length = nds)
for (i in 1:nds){
    ds <- loadDataset(dsName[i], packName[i])
    m[i] <- nrow(ds)
    n[i] <- ncol(ds)- 1 - as.numeric(!is.nan(idNumber[i])) 
    # no. of valid features in the dataset.
    Kp[i] <- length(unique(ds[,classVar[i]]))
}
#m <- sapply(dsNames, function(n){nrow(evalDataset(n))}) # no. of instances in the dataset
#n <- sapply(dsNames, function(n){ncol(evalDataset(n))}) - 1 - as.numeric(!is.nan(idNumber)) # no. of features in the dataset.
datasets <- tibble(name=dsName, 
                   packName,
                   className, 
                   classVar=as.integer(classVar),
                   idNumber=as.integer(idNumber), 
                   K=as.integer(K), #Announced no. classes
                   Kp=as.integer(Kp), #Inspected no. classes
                   n=as.integer(n), 
                   m=as.integer(m))
# run
devtools::use_data(datasets,overwrite = TRUE)#Regenerate the tibble
