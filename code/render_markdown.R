library(rmarkdown)


args<- commandArgs(trailingOnly = TRUE)
input_file<-args[1]


render(input_file)
