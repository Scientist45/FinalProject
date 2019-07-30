library(tidyverse)
library(factoextra)
library(DT)
library(caret)
#dataset <- read_csv("movie_metadata.csv")
#dataset <- dataset[complete.cases(dataset), ]
#write.csv(dataset, file = "movie_metadataupdated.csv")
dataset <- read_csv("movie_metadataupdated.csv")
dataset <- dataset[complete.cases(dataset), ]
dataset <- dataset %>% subset(genres == "Action" | genres == "Adventure" | genres == "Animation" | genres == "Comedy" | genres == "Drama" | genres == "Fantasy" | genres == "Horror")
head(dataset)
Data1 <- dataset %>% select(duration, gross, cast_total_facebook_likes, budget, title_year, imdb_score)
head(Data1)
pcadata <- prcomp(Data1, center = TRUE, scale=TRUE)
summary(pcadata)

screeplot(pcadata, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(pcadata$sdev^2 / sum(pcadata$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

plot(pcadata$x[,1],pcadata$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot", aes(fillcol = dataset$genres))

library("factoextra")
fviz_pca_biplot(pcadata, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = dataset$genres, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Genre") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


#Process data for top 500 Grossing Movie Table
Data2 <- dataset %>% arrange(desc(gross))
Data2 <- Data2[1:500, 2:18]
Data2 <- as.data.frame(Data2)
datatable(Data2)

#Histogram of Rating and Genres
ggplot(data = Data2, aes(x = gross)) +
  geom_histogram(bins = 20) + facet_wrap(~genres) + ggtitle("Histogram of Gross Income by Genre") + 
  xlab("Rating") + ylab("Count")


#Modeling
#Data Pre-Processing
ModData <- dataset %>% select("gross", "cast_total_facebook_likes","budget", "imdb_score")
#split into training and test
Train <- sample(1:nrow(ModData), size = nrow(ModData)*0.8)
Test <- dplyr::setdiff(1:nrow(ModData), Train)
ModTrain <- ModData[Train, ]
ModTest <- ModData[Test, ]#split into training and test
Train <- sample(1:nrow(ModData), size = nrow(ModData)*0.8)
Test <- dplyr::setdiff(1:nrow(ModData), Train)
ModTrain <- ModData[Train, ]
ModTest <- ModData[Test, ]

#baggedtree model
baggedTree <- train(gross ~ ., data = ModTrain, method = "treebag",
trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
preProcess = c("center", "scale"))

#Baged tree model - predict and make a table
baggedTbl <- table(data.frame(pred = predict(baggedTree, ModTest), true = ModTest$gross))
1-sum(diag(baggedTbl)/sum(baggedTbl))

#Linear regression model - predict and make a table
lmfit <- lm(gross~., data= ModTrain)
lmTable <- table(data.frame(pred = predict(lmfit, ModTest), true = ModTest$gross))
1-sum(diag(lmTable)/sum(lmTable))