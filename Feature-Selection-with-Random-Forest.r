###Feature selection with a random forest algorithm(Boruta)
newdata1 <- read.table(file.choose(), sep =",", header = TRUE)

install.packages("Boruta")
library(Boruta)
set.seed(111)
feature_extract <- Boruta(Vital.Status.Dead~., data = newdata1, doTrace = 2)
print(feature_extract)

#take a call on tentative features
feature_extract_total <- TentativeRoughFix(feature_extract)
print(feature_extract_total)
plot(feature_extract_total)

plot(feature_extract_total, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(feature_extract_total$ImpHistory),function(i)
feature_extract_total$ImpHistory[is.finite(feature_extract_total$ImpHistory[,i]),i])
names(lz) <- colnames(feature_extract_total$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
at = 1:ncol(feature_extract_total$ImpHistory), cex.axis = 0.7)


getSelectedAttributes(feature_extract_total, withTentative = F)

bank_df <- attStats(feature_extract_total)
print(bank_df)
