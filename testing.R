library(rvest)
library(readr)

male_100 <- read_html("http://www.alltime-athletics.com/m_100ok.htm")
x <- male_100 %>%
  html_nodes(xpath = "//pre")
class(x)
x2 <- x %>%
  html_text()
length(x2)
x3 <- x2[[1]]
nchar(x3)
y <- substr(x3, 2, 399)
file_conn <- file("test.txt")
writeLines(y, file_conn)
writeLines(x3, file_conn)
close(file_conn)
d <- readr::read_fwf("test.txt", skip = 1, n_max = 3178,
                     col_types = cols(.default = col_character()),
                     col_positions = fwf_positions(
                       c(1, 16, 27, 35, 66, 74, 86, 93, 123),
                       c(15, 26, 34, 65, 73, 85, 92, 122, 132)
                     ))
d2 <- readr::read_fwf(x3, skip = 1, n_max = 3178,
                      col_types = cols(.default = col_character()),
                      col_positions = fwf_positions(
                        c(1, 16, 27, 35, 66, 74, 86, 93, 123),
                        c(15, 26, 34, 65, 73, 85, 92, 122, 132)
                      ))