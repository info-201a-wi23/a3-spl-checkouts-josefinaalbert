library("dplyr")
library("ggplot2")
library("stringr")
checkout_df <- read.csv("~/Desktop/2017-2023-10-Checkouts-SPL-Data.csv")


# Graph 3
## Comparing the number of checkouts of the most checkout book for each year from 2017 to 2023

# Filter all material types for just books, and select checkout year, number of checkouts, title, and creator for each book.
books <- checkout_df %>% filter(str_detect(MaterialType, "\\bBOOK\\b")) %>% select(CheckoutYear, Checkouts, Title, Creator, Subjects)

# Group books by year, then filter for the most checked out book in each year
Most_checked_out <- books %>% group_by(CheckoutYear) %>% filter(Checkouts == max(Checkouts)) %>% select(CheckoutYear, Title, Checkouts)

# Create a bar graph presenting the checkout Year on the x-axis, the number of checkouts on the y-axis, a legend for the book title, and a title explaining the graph
library(ggplot2)

ggplot(Most_checked_out, aes(x = CheckoutYear, y = Checkouts, fill = Title)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Most Checked Out Books per Year",
       x = "Checkout Year",
       y = "Number of Checkouts",
       fill = "Title")
