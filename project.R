library(tidyverse)
library(dplyr)
booksRaw <- read.csv("Goodreads_books_with_genres.csv")
#summary(books)
head(booksRaw)
# This next segment is to separate the genres into individual columns with 1 hot encoding
# Create a list of each genre
separate_genres <- books %>% separate_rows(genres, sep=';|,')
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

# Convert types
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


#EDA: Visualize distributions of key numeric variables
#Histogram of average ratings
ggplot(books, aes(x = average_rating)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Average Ratings", x = "Average Rating", y = "Frequency")


# Histogram of number of pages
ggplot(books, aes(x = num_pages)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  labs(title = "Distribution of Number of Pages", x = "Number of Pages", y = "Frequency")

# Correlation between ratings count and average rating
ggplot(books, aes(x = ratings_count, y = average_rating)) +
  geom_point(alpha = 0.5)
