library(tidyverse)
library(dplyr)
library(caret)
library(tidyselect)
library(tidymodels)
library(kknn)
library(randomForest)
booksRaw <- read.csv("Goodreads_books_with_genres.csv")
#summary(books)
head(booksRaw)
# This next segment is to separate the genres into individual columns with 1 hot encoding
# Create a list of each genre
books <- booksRaw
separate_genres <- books |>
                   separate_rows(genres, sep=';|,')
genreDF <- separate_genres$genres |> unique() |> c()
# Creating a column for each genre and populating it with zeros
new_columns <- data.frame(matrix(0, nrow = nrow(books),
                                 ncol = length(genreDF)))
colnames(new_columns) <- genreDF
# Combining the two dataframes
books <- cbind(books, new_columns)
for (g in genreDF) {
  # Create a binary column for each genre
  books[[g]] <- sapply(books$genre, function(x) as.integer(grepl(g, x)))
}
# Drop original genre column
books$genres <- NULL

# Give column titles same naming convention
names(books) <- tolower(gsub(" ", "_", names(books)))

# Removes duplicate column names
books <- books[, !duplicated(names(books))]

# Convert types (Claudia)
books <- books |>
  mutate(
    num_pages = as.integer(num_pages),
    ratings_count = as.integer(ratings_count),
    text_reviews_count = as.integer(text_reviews_count),
    average_rating = as.numeric(average_rating),
    publication_date = as.Date(publication_date, format = "%m/%d/%Y")
  )

# Remove unwanted columns
books <- subset(books, select = -c(isbn,isbn13) )

#Remove any books with no pages / no rating
books <- filter(books, num_pages > 0 & average_rating > 0)

print(ncol(books))
# Remove Genres that make up less than 5% of the dataset
for (name in genreDF){
    count = sum(books[[name]])

    if (count/nrow(books) <= 0.01){
      books[[name]] <- NULL
    }
}
print(ncol(books))

#EDA: Visualize distributions of key numeric variables
#Histogram of average ratings
ggplot(books, aes(x = average_rating)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Average Ratings", x = "Average Rating", y = "Frequency")


# Histogram of number of pages
ggplot(books, aes(x = num_pages)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  labs(title = "Distribution of Number of Pages", x = "Number of Pages", y = "Frequency")

# Scatter plot of ratings count vs average rating
ggplot(books, aes(x = ratings_count, y = average_rating)) +
  geom_point(alpha = 0.5) +
  labs(title = "Ratings Count vs Average Rating", x = "Ratings Count", y = "Average Rating")

# Correlation Matrix to see what genres might be beneficial to drop
cor_matrix <- cor(books)

# Print the correlation matrix
print(cor_matrix)

#Modeling number of pages against average rating (with regression line)
p <- ggplot(books, aes(x = num_pages, y = average_rating)) +
+     geom_point(alpha = 0.5) +                       # Scatter plot of points
+     geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
+     labs(title = "Page Count vs Average Rating",
+          x = "Page Count",
+          y = "Average Rating") +
+     scale_x_continuous(limits = c(0, 5000))
>
> print(p)

# Nol
# Setting a seed for reproducibility. Current seed 3220
set.seed(3220)

# Selecting the indices to split.
train_indices <- sample(1:nrow(books), size = 0.8 * nrow(books))

# Split the data
train_data <- books[train_indices, ]
test_data <- books[-train_indices, ]

# think we need to trim dataset or setaside a large chunk of time before we run this - Nol
linRegModel <- lm(average_rating ~ ., data=train_data)

# Claud
#select all genre columns
genres <- books[, 11:905]

genres<- lapply(genres, function(x) {
# Convert with warnings suppressed
  x <- suppressWarnings(as.numeric(as.character(x)))
# Replace NA values with 0
  x[is.na(x)] <- 0
  x
})

# Creates Data Frame
genres <- data.frame(genres)

# Sums each columns
genre_totals <- colSums(genres)

# Create a data frame with two columns genre names and total count of books
genre_summary <- data.frame(Genre = names(genre_totals), Total_Books = genre_totals)

# Sort the data frame in descending order
genre_summary <- genre_summary[order(genre_summary$Total_Books, decreasing = TRUE), ]

# Print without row names
print(genre_summary, row.names = FALSE)

# Filters Genres < 25 (about like half we can edit)
low_genres <- genre_summary |>
  filter(Total_Books < 25) |>
  select(Genre)

# Selects name of Genres to remove
remove <- low_genres$Genre

# Some had had errors in the tiltle so taking those out
remove <- remove[remove %in% names(books)]

# Removes them from books
books <- books |>
  select(-all_of(remove))

#Claud
# Set a seed
set.seed(42)

# Splitting
data_split <- initial_split(books, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

# Model with k=5
booksKNN <- nearest_neighbor(neighbors = 5) |>
  set_engine("kknn") |>
  set_mode("regression")  # Set mode to regression

# Recipe
books_Recipe <- recipe(average_rating ~ ratings_count + num_pages + text_reviews_count + publication_date, data = train_data) |>
  step_impute_mean(all_numeric_predictors()) |>  # Handle missing numeric values
  step_impute_mode(all_nominal_predictors()) |>  # Handle missing categorical values
  step_zv(all_predictors()) |>                   # Remove zero-variance predictors
  step_normalize(all_numeric_predictors())       # Normalize numeric predictors
# Workflow
books_workflow <- workflow() |>
  add_model(booksKNN) |>
  add_recipe(books_Recipe)

# Fit the model on training data
books_Fit <- books_workflow |> fit(data = train_data)

# Predict values
test_pred <- books_Fit |>
    predict(new_data = test_data)

# Align test pred with test_data.... this may be messing things up but wont work without
test_Pred <- test_data |>
  mutate(.pred = testPred$.pred[match(rownames(test_data), rownames(testPred))])

# Fill missing predictions
testPred <- testPred |>
  mutate(.pred = ifelse(is.na(.pred), mean(.pred, na.rm = TRUE), .pred))

# Print metrics
test_metrics <- test_pred |>
  metrics(truth = average_rating, estimate = .pred)

# Visualization
ggplot(testPred, aes(x = .pred, y = average_rating)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Books Predicted vs Actual Ratings",
    x = "Predicted Average Rating",
    y = "Actual Average Rating"
  )

  print(test_metrics)
books_Recipe <- recipe(average_rating ~ . , data = train_data) |>
  step_impute_mean(all_numeric_predictors()) |> # Handle missing numeric values
  step_impute_mode(all_nominal_predictors()) |> # Handle missing categorical values
  step_zv(all_predictors()) |>                  # Remove zero-variance predictors
  step_normalize(all_numeric_predictors())      # Normalize numeric predictors

# Define the Random Forest model
books_rf <- rand_forest(mtry = 5, trees = 500, min_n = 10) |>
  set_engine("randomForest") |>
  set_mode("regression")

# workflow
rf_workflow <- workflow() |>
  add_model(books_rf) |>
  add_recipe(books_Recipe)

# Fit
rf_fit <- rf_workflow |> fit(data = train_data)

# Predict and evaluate
rf_predictions <- rf_fit |> predict(new_data = test_data) |> bind_cols(test_data)
rf_metrics <- rf_predictions |>
  metrics(truth = average_rating, estimate = .pred)

# Residual calculation
rf_predictions <- rf_predictions |>
  mutate(residual = average_rating - .pred)

# Residual plot
ggplot(rf_predictions, aes(x = .pred, y = residual)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residual Plot: Random Forest",
    x = "Predicted Values",
    y = "Residuals"
  ) +
  theme_minimal()
                       
# predicted vs actual
ggplot(rf_predictions, aes(x = average_rating, y = .pred)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Predicted vs Actual Values: Random Forest",
    x = "Actual Values (average_rating)",
    y = "Predicted Values"
  ) +
  theme_minimal()


print(rf_metrics)
