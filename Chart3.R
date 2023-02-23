library("dplyr")
library("ggplot2")
library("stringr")
checkout_df <- read.csv("~/Desktop/2017-2023-10-Checkouts-SPL-Data.csv")

books <- checkout_df %>% filter(str_detect(MaterialType, "\\bBOOK\\b")) %>% select(CheckoutYear, Checkouts, Title, Creator, Subjects)

Most_checked_out <- books %>% group_by(CheckoutYear) %>% filter(Checkouts == max(Checkouts)) %>% select(CheckoutYear, Title, Checkouts)

library(ggplot2)

ggplot(Most_checked_out, aes(x = CheckoutYear, y = Checkouts, fill = Title)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Most Checked Out Books per Year",
       x = "Checkout Year",
       y = "Number of Checkouts",
       fill = "Title")
