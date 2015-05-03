#install.packages('neuralnet')
library("neuralnet")
#?neuralnet

#read train and test data
train = read.csv("C:/Users/Jianghui/Desktop/BreastCancer/train.csv", header = TRUE)
data = train[!is.na(train$Nuclei),2:11]
dim(data)
write.table(train, "C:/Users/Jianghui/Desktop/BreastCancer/guide/train.txt", sep=",", row.names = FALSE,col.names = FALSE)

# Basic Scatterplot Matrix
pairs(~Thickness+Size+Shape+ Adhesion+ Epithelial + Nuclei+ Bland +Nucleoli+ Mitoses+Class,data=train, 
   main="Simple Scatterplot Matrix")

test = read.csv("C:/Users/Jianghui/Desktop/BreastCancer/test.csv", header = TRUE)
valid = test[!is.na(test$Nuclei),2:10]
dim(valid)
write.table(test, "C:/Users/Jianghui/Desktop/BreastCancer/guide/test.txt", sep=",", row.names = FALSE,col.names = FALSE)
save(test, file = "C:/Users/Jianghui/Desktop/BreastCancer/guide/test.RData")

#fit the neural network model
fit = neuralnet(Class ~ Thickness+Size+Shape+ Adhesion+ Epithelial + Nuclei+ Bland +Nucleoli+ Mitoses,data, hidden=10, threshold=0.01)
print(fit)   
 
#Plot the neural network
plot(fit)

# apply model to test data
predict = compute(fit, valid)
ls(predict)
test_results = cbind(test[!is.na(test$Nuclei),11], as.data.frame(predict$net.result))
colnames(test_results) <- c("class","predict")

#apply model to train data
predict = compute(fit, data[,1:9])
train_results = cbind(data[,10], as.data.frame(predict$net.result))
colnames(train_results) <- c("class","predict")

#create liftchart
train_results$dec = as.numeric(with(train_results, cut(predict, breaks=quantile(train_results$predict,seq(0,1,0.2)),include.lowest=TRUE)))
lift = aggregate(train_results$class, by=list(decile=train_results$dec), FUN = "mean")
bad = aggregate(train_results$class, by=list(decile=train_results$dec), FUN = "sum")
good = c(table(train_results$dec)) - bad[,2]
goodbad = data.frame(decile=1:5, bad = bad[,2], good)

#create function to calculate Somers'D
SD = function(goodbad) {
   A = abs(0.5*sum((c(0,cumsum(goodbad$bad)[1:4])/sum(goodbad$bad) + (cumsum(goodbad$bad)/sum(goodbad$bad)))*
      (cumsum(goodbad$good)/sum(goodbad$good) - c(0,cumsum(goodbad$good)[1:4])/sum(goodbad$good)))-0.5)/0.5
   return(A)
}
SD(goodbad)
plot(lift,type="b",pch=16,ylim=c(0,max(lift$x)+0.01),main=paste("Liftchart \nSomers'D = ",round(SD(goodbad),4)),xlab = "Qintiles",ylab = 'Malignant Rate')

#tabulate results
train_results$c_hat = as.numeric(with(train_results, cut(predict, breaks=c(-1,0.1,1.2),include.lowest=TRUE)))
xtabs(~c_hat+class,train_results)

test_results$c_hat = as.numeric(with(test_results, cut(predict, breaks=c(-1,0.1,1.2),include.lowest=TRUE)))
xtabs(~c_hat+class,test_results)

save.image()