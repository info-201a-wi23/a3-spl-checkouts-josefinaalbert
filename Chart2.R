library("dplyr")
library("ggplot2")
library("stringr")
checkout_df <- read.csv("~/Desktop/2017-2023-10-Checkouts-SPL-Data.csv")

books <- checkout_df %>% filter(str_detect(MaterialType, "\\bBOOK\\b")) %>% select(CheckoutYear, Checkouts)
books_per_year <- books %>% group_by(CheckoutYear) %>% summarise(Checkouts = sum(Checkouts))

ebooks <- checkout_df %>% filter(str_detect(MaterialType, "\\bEBOOK\\b")) %>% select(CheckoutYear, Checkouts)
ebooks_per_year <- ebooks %>% group_by(CheckoutYear) %>% summarise(Checkouts = sum(Checkouts))

audiobooks <- checkout_df %>% filter(str_detect(MaterialType, "\\bAUDIOBOOK\\b")) %>% select(CheckoutYear, Checkouts)
audiobooks_per_year <- audiobooks %>% group_by(CheckoutYear) %>% summarise(Checkouts = sum(Checkouts))

magazines <- checkout_df %>% filter(str_detect(MaterialType, "\\bMAGAZINE\\b")) %>% select(CheckoutYear, Checkouts)
magazines_per_year <- magazines %>% group_by(CheckoutYear) %>% summarise(Checkouts = sum(Checkouts))

videodiscs <- checkout_df %>% filter(str_detect(MaterialType, "\\bVIDEODISC\\b")) %>% select(CheckoutYear, Checkouts)
videodiscs_per_year <- videodiscs %>% group_by(CheckoutYear) %>% summarise(Checkouts = sum(Checkouts))

ggplot() +
  geom_line(data = books_per_year, aes(x = CheckoutYear, y = Checkouts, color = "Books"), linewidth = 1) +
  geom_line(data = ebooks_per_year, aes(x = CheckoutYear, y = Checkouts, color = "Ebooks"), linewidth = 1) +
  geom_line(data = audiobooks_per_year, aes(x = CheckoutYear, y = Checkouts, color = "Audiobooks"), linewidth = 1) +
  geom_line(data = magazines_per_year, aes(x = CheckoutYear, y = Checkouts, color = "Magazines"), linewidth = 1) +

  geom_line(data = videodiscs_per_year, aes(x = CheckoutYear, y = Checkouts, color = "Videodiscs"), linewidth = 1) +

  labs(x = "Year", y = "Number of Checkouts", title = "Number of Checkouts Per Material Type from 2017-2022") +
  scale_color_manual(values = c("Books" = "lightblue", "Ebooks" = "pink", "Audiobooks" = "lightyellow", "Magazines" = "maroon", "Videodiscs" = "lightcyan3"), name = "Material Type") +
  scale_x_continuous(limits = c(2017, 2022), breaks = seq(2017, 2022, 1))

