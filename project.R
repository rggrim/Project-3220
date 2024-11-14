# Load libraries
library(tidyverse)
library(dplyr)

# Load the dataset
booksRaw <- read.csv("Goodreads_books_with_genres.csv")

# Preview the raw data
head(booksRaw)

# Data Cleaning and Transformation
books <- booksRaw

# Separate genres into individual rows and remove any empty genre entries
books <- books %>%
  separate_rows(genres, sep = ';|,') %>%
  mutate(genres = trimws(genres)) %>%
  filter(genres != "")

# Create unique genre list
genreDF <- unique(books$genres)

# One-hot encoding for each genre
for (g in genreDF) {
  # Ensure column name is valid and doesn’t contain invalid characters
  col_name <- make.names(g)
  books[[col_name]] <- ifelse(grepl(g, books$genres), 1, 0)
}

# Drop the original genres column
books <- books %>% select(-genres)

# Standardize column names
names(books) <- tolower(gsub(" ", "_", names(books)))

# Remove duplicate column names
books <- books %>% select(unique(names(books)))

# Convert types
books <- books %>%
  mutate(
    num_pages = as.integer(num_pages),
    ratings_count = as.integer(ratings_count),
    text_reviews_count = as.integer(text_reviews_count),
    average_rating = as.numeric(average_rating),
    publication_date = as.Date(publication_date, format = "%m/%d/%Y")
  )

# Remove unwanted columns
books <- books %>% select(-isbn, -isbn13)

# Remove any books with no pages / no rating
books <- books %>% filter(num_pages > 0 & average_rating > 0)

# EDA: Visualize distributions of key numeric variables

# Histogram of average ratings
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