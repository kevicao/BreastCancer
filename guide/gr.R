load("test.RData")
 ## Missing value code is NA
 ## Change file name if needed
 ## Predicting class
 predicted <- function(){
 if(is.na(Size) | Size <=    2.50000000000000      ){
 nodeid <-            2
 predict <- "0"
 } else {
 if(is.na(Nuclei) | Nuclei <=    2.50000000000000      ){
 if(!is.na(Size) & Size <=    3.50000000000000      ){
 nodeid <-           12
 predict <- "0"
 } else {
 nodeid <-           13
 predict <- "1"
 }
 } else {
 nodeid <-            7
 predict <- "1"
 }
 }
 return(c(nodeid,predict))
 }
 ###z <- read.table("gfit.txt",header=TRUE)
 ###noerr <- TRUE
 for(i in 1:dim(newdata)[1]){
 Size <- as.numeric(newdata$Size[i])
 Nuclei <- as.numeric(newdata$Nuclei[i])
 tmp <- predicted()
 node <- tmp[1]
 rpred <- tmp[2]
 ###gnode <- z$node[i]
 ###gpred <- z$predicted[i]
 ###if(rpred != gpred){
 ###print(c("Case ",i,node,gnode,rpred,gpred))
 ###noerr <- FALSE}
 }
 ###if(noerr == TRUE) print("No errors")
