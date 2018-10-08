#!/usr/local/bin/Rscript
# https://www.quora.com/Can-you-use-shell-scripting-with-R. Consulted 8.10.18
#This is supposedly done prior to sending the package for publication
# on CRAN whenever you change the REFERENCES.bib
#
# It is idempotent on that file.
#
#base <- system.file(package = "entropies")
base <- getwd()#To be run in the inst dir of entropies
citFile <- paste(base, "CITATION", sep="/")
#citFile <- system.file("inst", "CITATION", package = "entropies")

sink(citFile, type="output")
#ss <- file(citFile, open="w")
#ss <- sink(file(file, open="w"))  
#cat("Hello world")

# Begin writing output to file
cat('citHeader("To cite the entropies package in publications use:")')
cat("\n")
#
# Our own recipe to do this
#
library(bibtex)
# #"inst/REFERENCES.bib"
bibs <- bibtex::read.bib("REFERENCES.bib")
# bibs <- bibtex::read.bib(
#     system.file("REFERENCES.bib", package="entropies")
#     )
# #bibs#these are bibentry q.v. 
#do.call(function(x) print(x,type="R"), as.list(bibs))
for(i in 1:4){
    print(bibs[i],style="R")
}
# ENding
cat('citFooter("As entropies is still evolving, you may want to cite its version number. Find it with help(package=entropies).")')
cat("\n")
# citation(<packagename>) is a dataframe. 
# To extract citations by their tag use, for example: 
# citation("entropies")["val:pel:18c",]
# THis is itself a citation to be printed, queried, etc.
#return the flow to normal
sink() 
