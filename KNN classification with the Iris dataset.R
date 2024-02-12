suppressMessages(library("tidyverse"))
library("class")
suppressMessages(library("caret"))

# Load in Iris data
data(iris)

head(iris)

# Creating the plots
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point()
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() 

# Set seed for reproducibility
set.seed(23)

# Duplicate the dataset
iris_numeric <- iris

# Making the data Numeric
iris_numeric$Species <- as.numeric(iris$Species)

# Scaling the data mean and SD
iris_numeric[ , c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")] <- scale(iris_numeric[,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")])

# Creating subset of 30% of the data
indices <- sample(1:nrow(iris), round(0.3 * nrow(iris)))

# Making train and test data
train_labels <- iris_numeric[-indices, ]
train_data <- iris_numeric[-indices, 1:4]
test_data <- iris_numeric[indices, 1:4]

# KNN prediction
test_prediction <- knn(
  train = train_data, 
  test = test_data,
  cl = train_labels$Species, 
  k= 5
)

# Creating dataframe with values
plot_predictions <- data.frame(
  test_data$Sepal.Length,
  test_data$Sepal.Width,
  test_data$Petal.Length,
  test_data$Petal.Width,
  predicted = test_prediction)


# Column names
colnames(plot_predictions) <- c("Sepal.Length",
                                "Sepal.Width",
                                "Petal.Length",
                                "Petal.Width",
                                'predicted')

# Actual labels of test data
test_labels <- iris[indices, 5:5]

# Creating plots
ggplot(plot_predictions, aes(Petal.Length, Petal.Width, color = predicted, fill = predicted)) + 
  geom_point() +
  geom_text(aes(label = test_labels), vjust=2,  size = 2.5) +
  ggtitle("Predicted relationship between Petal Length and Petal Width") +
  theme(legend.position = "none")

ggplot(plot_predictions, aes(Sepal.Length, Sepal.Width, color = predicted, fill = predicted)) + 
  geom_point() +
  geom_text(aes(label = test_labels), vjust=2, size = 2.5) +
  ggtitle("Predicted relationship between Sepal Length and Sepal Width") +
  theme(legend.position = "none")

# Create a vector of species names
species_names <- c("setosa", "versicolor", "virginica")

# Change numeric values into factors
test_prediction_factor <- factor(test_prediction, levels = 1:3, labels = species_names)

# Confusion matrix using caret
confusion_matrix <- confusionMatrix(test_prediction_factor, test_labels)
confusion_matrix

