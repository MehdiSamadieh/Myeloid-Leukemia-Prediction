Data <- read.table(file.choose(), sep =",", header = TRUE)
fix(Data)
summary(Data)

categoricaldata <- Data[c(1:135)]
continuousdata<-Data[c(136:145)]
output<-Data[146]

continuousdata<-scale(continuousdata, center = TRUE, scale =TRUE)
fix(continuousdata)

newdata<-cbind(continuousdata,categoricaldata)
newdata1<-cbind(continuousdata,categoricaldata,output)
summary(newdata1)

fix(newdata)
ncol(newdata)

write.csv(newdata1,"data.shrinked.csv")

###### PCA

pca<-prcomp(newdata, scale = TRUE)

#install.packages("factoextra")
library(factoextra)

fviz_eig(pca)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )

proportionvar<-summary(pca)
proportionvar

dataframe<-predict(pca, 
        newdata=tail(newdata, 993))

dataframe<-as.data.frame(dataframe)
write.csv(dataframe, "pcaresult.csv")

write.csv(proportionvar, "ss.csv")

#####################
newdf<-newdata[c(1:6)]
fix(newdf)
pca<-prcomp(newdf, scale = TRUE)
pca


#install.packages("factoextra")
library(factoextra)

fviz_eig(pca)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )

proportionvar<-summary(pca)
proportionvar

###Feature selection with a random forest algorithm(Boruta)
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
at = 1:ncol(feature_extract_total$ImpHistory), cex.axis = 0.55)


getSelectedAttributes(feature_extract_total, withTentative = F)

bank_df <- attStats(feature_extract_total)
print(bank_df)
