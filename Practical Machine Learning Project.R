nms <- vector()
for(n in colnames(trainData))
{
    if(sum(is.na(trainData[,n]))/length(trainData[,n]) > 0.20) {
        print(paste("Omit:",n))
        
        nms <- c(nms,n)
        
    }
}

x <- colnames(trainData) %in% nms
trainData <- trainData[,!x]
trainData$colname

str(trainData)



?colnames

#plot(XtrainMod$finalModel,uniform = TRUE)
#text(XtrainMod$finalModel,use.n = TRUE)

#fancyRpartPlot(XtrainMod$finalModel)

#print(XtrainMod$finalModel)
#print(XtrainMod2$finalModel)
#print(XtrainMod3$finalModel)
#print(XtrainMod4$finalModel)
