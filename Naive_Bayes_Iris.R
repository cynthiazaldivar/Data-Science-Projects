# Cynthia Zaldivar
# Naive Bayes using iris data (iris.arff)

library(foreign)
library(pROC)
library(e1071)
library(klaR)

seeds.data     <- read.arff("seeds.arff")
seeds.training <- seeds.data[c(1:56, 71:126, 141:196), ] # 80% from each class
seeds.test     <- seeds.data[c(57:70, 127:140, 197:210), ] # 20% from each class

seeds.model <- naiveBayes(as.factor(class) ~ ., data = seeds.training)
seeds.pred  <- predict(seeds.model, seeds.test[, 1:7])

cat("\nAccuracy of the Naive Bayes model: ", round(100 * sum(seeds.pred == seeds.test$class) / 42, 2), "%\n\n", sep = "")

seeds.roc <- multiclass.roc(seeds.test$class, as.numeric(seeds.pred))
plot.roc(seeds.roc$rocs[[1]], col = "magenta")
lines(seeds.roc$rocs[[2]], col = "purple")
lines(seeds.roc$rocs[[3]], col = "cyan")
legend("bottomright",
  legend = c("Class 1", "Class 2", "Class 3"),
  col = c("magenta", "purple", "cyan"), lwd = 3
)

cat("AUC for class 1: ", auc(seeds.roc$rocs[[1]]), "\n")
cat("AUC for class 2: ", auc(seeds.roc$rocs[[2]]), "\n")
cat("AUC for class 3: ", auc(seeds.roc$rocs[[3]]))
