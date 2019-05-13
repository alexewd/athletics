library(rvest)

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
close(file_conn)
