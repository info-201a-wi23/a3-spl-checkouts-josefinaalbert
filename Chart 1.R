library("dplyr")
library("ggplot2")
library("stringr")
library("scales")
checkout_df <- read.csv("~/Desktop/2017-2023-10-Checkouts-SPL-Data.csv")

## Graph 1
# Compare how each year's book checkouts per month vary

# Isolate books as the material type
books <- checkout_df %>% filter(str_detect(MaterialType, "\\bBOOK\\b")) %>% select(CheckoutYear, CheckoutMonth, Checkouts)

books2017 <- books %>% filter(str_detect(CheckoutYear, "2017")) %>% group_by(CheckoutMonth) %>% summarise(checkouts = sum(Checkouts))

books2018 <-  books %>% filter(str_detect(CheckoutYear, "2018")) %>% group_by(CheckoutMonth) %>% summarise(checkouts = sum(Checkouts))

books2019 <-  books %>% filter(str_detect(CheckoutYear, "2019")) %>% group_by(CheckoutMonth) %>% summarise(checkouts = sum(Checkouts))

books2020 <-  books %>% filter(str_detect(CheckoutYear, "2020")) %>% group_by(CheckoutMonth) %>% summarise(checkouts = sum(Checkouts)) 

books2021 <-  books %>% filter(str_detect(CheckoutYear, "2021")) %>% group_by(CheckoutMonth) %>% summarise(checkouts = sum(Checkouts))

ggplot() +
  # Add books2017 line plot
  geom_line(data = books2017, aes(x = CheckoutMonth, y = checkouts, color = "books2017"), linewidth = 1) +
  
  # Add books2018 line plot
  geom_line(data = books2018, aes(x = CheckoutMonth, y = checkouts, color = "books2018"), linewidth = 1) +
  # Add books2019 line plot
  geom_line(data = books2019, aes(x = CheckoutMonth, y = checkouts, color = "books2019"), linewidth = 1) +
  
  # Add books2020 line plot
  geom_line(data = books2020, aes(x = CheckoutMonth, y = checkouts, color = "books2020"), linewidth = 1) +
  
  # Add books2021 line plot
  geom_line(data = books2021, aes(x = CheckoutMonth, y = checkouts, color = "books2021"), linewidth = 1) +
  
  # Add axis labels and title
  labs(x = "Year", y = "Number of Checkouts", title = "Number of Books Checked Out Each Month for years 2017-2021") +
  
  # Add legend
  scale_color_manual(values = c("books2017" = "lightyellow", "books2018" = "pink", "books2019" = "lightblue", "books2020" = "hotpink2", "books2021" = "lightsteelblue2"),
                     labels = c("2017","2018", "2019", "2020", "2021"),
                     name = "Month") +
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, 1)) +
  scale_y_continuous(labels = label_number_si(), breaks = seq(0, 200000, by = 20000))

