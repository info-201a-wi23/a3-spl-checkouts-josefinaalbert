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
  

## Graph 2

# Filter 5 different material types (books, ebooks, audiobooks, magazines, and videodiscs) and calculate their total number of checkouts per year

# Filter books
books <- checkout_df %>% filter(str_detect(MaterialType, "\\bBOOK\\b")) %>% select(CheckoutYear, Checkouts)
books_per_year <- books %>% group_by(CheckoutYear) %>% summarise(Checkouts = sum(Checkouts))

# Filter ebooks
ebooks <- checkout_df %>% filter(str_detect(MaterialType, "\\bEBOOK\\b")) %>% select(CheckoutYear, Checkouts)
ebooks_per_year <- ebooks %>% group_by(CheckoutYear) %>% summarise(Checkouts = sum(Checkouts))

# Filter audiobooks
audiobooks <- checkout_df %>% filter(str_detect(MaterialType, "\\bAUDIOBOOK\\b")) %>% select(CheckoutYear, Checkouts)
audiobooks_per_year <- audiobooks %>% group_by(CheckoutYear) %>% summarise(Checkouts = sum(Checkouts))

# Filter magazines
magazines <- checkout_df %>% filter(str_detect(MaterialType, "\\bMAGAZINE\\b")) %>% select(CheckoutYear, Checkouts)
magazines_per_year <- magazines %>% group_by(CheckoutYear) %>% summarise(Checkouts = sum(Checkouts))

# Filter videodiscs
videodiscs <- checkout_df %>% filter(str_detect(MaterialType, "\\bVIDEODISC\\b")) %>% select(CheckoutYear, Checkouts)
videodiscs_per_year <- videodiscs %>% group_by(CheckoutYear) %>% summarise(Checkouts = sum(Checkouts))


# Combine these different mediums together in order to use them in a ggplot
ggplot() +
  
  # Add books line plot
  geom_line(data = books_per_year, aes(x = CheckoutYear, y = Checkouts, color = "Books"), linewidth = 1) +
  
  # Add ebooks line plot
  geom_line(data = ebooks_per_year, aes(x = CheckoutYear, y = Checkouts, color = "Ebooks"), linewidth = 1) +
  
  # Add audiobooks line plot
  geom_line(data = audiobooks_per_year, aes(x = CheckoutYear, y = Checkouts, color = "Audiobooks"), linewidth = 1) +
  
  # Add magazines line plot
  geom_line(data = magazines_per_year, aes(x = CheckoutYear, y = Checkouts, color = "Magazines"), linewidth = 1) +
  
  # Add videodiscs line plot
  geom_line(data = videodiscs_per_year, aes(x = CheckoutYear, y = Checkouts, color = "Videodiscs"), linewidth = 1) +
  
  # Add axis labels
  labs(x = "Year", y = "Number of Checkouts", title = "Number of Checkouts Per Material Type from 2017-2022") +
  
  # Add legend
  scale_color_manual(values = c("Books" = "lightblue", "Ebooks" = "pink", "Audiobooks" = "lightyellow", "Magazines" = "maroon", "Videodiscs" = "lightcyan3"),
                     labels = c("Books", "Ebooks", "Audiobooks", "Magazines", "Videodiscs"),
                     name = "Material Type") +
  
  # Set x-axis and y-axis limits and breaks
  scale_x_continuous(limits = c(2017, 2022), breaks = seq(2017, 2022, 1))


## Graph 3

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


