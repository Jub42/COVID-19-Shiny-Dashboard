args <- commandArgs(trailingOnly = TRUE)

path <- gsub("\\\\", "/", as.character(args[1]))
path <- substr(path, 1, nchar(path)-1)

setwd(path)
cat("Current working dir: ", getwd(), "\n")