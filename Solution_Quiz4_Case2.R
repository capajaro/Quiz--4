rm(list=ls()); graphics.off()

### Quiz 4 - Solution ###

#1. Load the data
Data_Yelp = readxl::read_xlsx("Data_yelp.xlsx", col_names=FALSE)
colnames(Data_Yelp) = c("text", "target")
id = 1:nrow(Data_Yelp)
Data_Yelp = cbind(id,Data_Yelp[,1:(ncol(Data_Yelp))])

