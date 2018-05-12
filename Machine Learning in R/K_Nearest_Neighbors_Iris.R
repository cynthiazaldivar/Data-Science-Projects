# Cynthia Zaldivar
# K Nearest Neighbors using iris data (iris.arff)

library(foreign) # To open .arff file

# Data frame from iris.arff
iris.data <- read.arff("iris.arff")

# Data Frames for each class
iris.setosa     <- iris.data[c(1:50), c(1:4)]
iris.versicolor <- iris.data[c(51:100), c(1:4)]
iris.virginica  <- iris.data[c(101:150), c(1:4)]

# Training sets
iris.setosa.train     <- iris.setosa[c(1:40), ]
iris.versicolor.train <- iris.versicolor[c(1:40), ]
iris.virginica.train  <- iris.virginica[c(1:40), ]
iris.train   <- rbind(iris.setosa.train, iris.versicolor.train, iris.virginica.train)
iris.classes <- c(rep("Setosa", 40), rep("Versicolor", 40), rep("Virginica", 40))

# Testing sets
iris.setosa.test     <- iris.setosa[c(41:50), ]
iris.versicolor.test <- iris.versicolor[c(41:50), ]
iris.virginica.test  <- iris.virginica[c(41:50), ]
iris.test         <- rbind(iris.setosa.test, iris.versicolor.test, iris.virginica.test)
iris.test.classes <- c(rep("Setosa", 10), rep("Versicolor", 10), rep("Virginica", 10))

# Calculates the euclidean distance between two points
euclid.distance <- function(x, y) {
  sqrt(sum((x - y)^2))
}

distance.matrix <- function(test, train) {

  # Within the three classes in the training data find the distances
  # between each "point" (row) and all other points in the other classes.
  # Matrix of distances
  dist <- matrix(c(rep(0, nrow(test) * nrow(train))), nrow = nrow(test), ncol = nrow(train))

  for (i in 1:nrow(test)) {
    for (j in 1:nrow(train)) {
      dist[i, j] <- euclid.distance(test[i, ], train[j, ])
    }
  }
  return(dist)
}


k.nearest.neighbors <- function(training.data, test.data, k) {

  # training.data: The training data used in KNN
  # test.data: The test data used in KNN
  # k: integer value indicating how many near neighbors in a given
  # point to use for majority vote.

  # Matrix of distances. Rows correspond to test values
  # columns correspond to training values
  KNN.distance <- distance.matrix(test.data, training.data)

  predicted.classes <- c() # Filled with majority vote winner

  for (i in 1:30) {
    # Counters for score keeping in majority vote
    setosa     <- 0
    versicolor <- 0
    virginica  <- 0

    temp.distance <- KNN.distance[i, ] # Row i of KNN.distance
    temp.df       <- data.frame(temp.distance, iris.classes) # Data frame of row and class

    sorted.temp.df <- temp.df[order(temp.distance), ] # Sorts data frame by distance
    sorted.classes <- sorted.temp.df$iris.classes # Vector of classes after sort
    knn <- sorted.classes[1:k] # k nearest neighbors

    # Score counter for row
    for (j in 1:k) {
      if (knn[j] == "Setosa") {
        setosa <- setosa + 1
      }
      if (knn[j] == "Versicolor") {
        versicolor <- versicolor + 1
      }
      if (knn[j] == "Virginica") {
        virginica <- virginica + 1
      }
    }

    # Majority vote
    if ((setosa > versicolor) && (setosa > virginica)) {
      winner <- "Setosa"
    }
    if ((versicolor > setosa) && (versicolor > virginica)) {
      winner <- "Versicolor"
    }
    if ((virginica > versicolor) && (virginica > setosa)) {
      winner <- "Virginica"
    }

    predicted.classes <- c(predicted.classes, winner) # Adds winner to predicted classes
  }
  return(predicted.classes)
}

predictions.k1 <- k.nearest.neighbors(iris.train, iris.test, 1)
predictions.k3 <- k.nearest.neighbors(iris.train, iris.test, 3)
predictions.k5 <- k.nearest.neighbors(iris.train, iris.test, 5)

accuracy.k1 <- 0
accuracy.k3 <- 0
accuracy.k5 <- 0

for (i in 1:30) {
  if (predictions.k1[i] == iris.test.classes[i]) {
    accuracy.k1 <- accuracy.k1 + 1
  }
  if (predictions.k3[i] == iris.test.classes[i]) {
    accuracy.k3 <- accuracy.k3 + 1
  }
  if (predictions.k5[i] == iris.test.classes[i]) {
    accuracy.k5 <- accuracy.k5 + 1
  }
}

cat("The accuracy for the KNN algorithm with Iris data for k=1 is ", (accuracy.k1 / 30) * 100, "% \n", sep = "")
cat("The accuracy for the KNN algorithm with Iris data for k=3 is ", (accuracy.k3 / 30) * 100, "% \n", sep = "")
cat("The accuracy for the KNN algorithm with Iris data for k=5 is ", (accuracy.k5 / 30) * 100, "% \n", sep = "")
