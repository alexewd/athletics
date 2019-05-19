library(rvest)
library(readr)

male_100_html <- read_html("http://www.alltime-athletics.com/m_100ok.htm")
male_100_pres <- male_100_html %>%
  html_nodes(xpath = "//pre")
male_100_htext <- male_100_pres %>%
  html_text()
male_100_htext <- male_100_htext[[1]]

male_100 <- readr::read_fwf(male_100_htext, skip = 1, n_max = 3178,
                            col_types = cols(.default = col_character()),
                            col_positions = fwf_positions(
                              c(1, 16, 27, 35, 66, 74, 86, 93, 123),
                              c(15, 26, 34, 65, 73, 85, 92, 122, 132)
                            ))

library(dplyr)
male_100 <- male_100 %>%
  select(X2, X4)
colnames(male_100) <- c("timing", "runner")

male_100 <- male_100 %>%
  mutate(timing = gsub("A", "", timing),
         timing = as.numeric(timing))
male_100

n_distinct(male_100$runner)
runner_cnt <- male_100 %>%
  group_by(runner) %>%
  summarise(n_rec = n()) %>%
  arrange(desc(n_rec))
runner_cnt

library(ggplot2)
ggplot(runner_cnt, aes(n_rec)) +
  geom_histogram(binwidth = 5, fill = "lightblue", colour = "black") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Distribution of number of records by runner") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 0.5)
  )

male_100_2 <- male_100 %>%
  inner_join(
    select(filter(runner_cnt, n_rec >= 50), runner),
    by = c("runner" = "runner")
  )

ggplot(male_100_2, aes(timing)) +
  geom_density() +
  facet_wrap(~runner, nrow = 4, ncol = 4) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Distribution of race timings by runner") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 0.5)
  )

male_100_means <- male_100_2 %>%
  group_by(runner) %>%
  summarise(mean_timing = mean(timing)) %>%
  arrange(mean_timing)
male_100_means

male_100_medians <- male_100_2 %>%
  group_by(runner) %>%
  summarise(median_timing = median(timing)) %>%
  arrange(median_timing)
male_100_medians

male_100_3 <- male_100_2 %>%
  filter(runner %in% c("Usain Bolt", "Asafa Powell", "Yohan Blake",
                       "Justin Gatlin", "Maurice Greene", "Tyson Gay"))

kt <- kruskal.test(timing ~ runner, data = male_100_3)
kt
library(pgirmess)
ktph <- kruskalmc(male_100_3$timing, male_100_3$runner)
View(ktph$dif.com)

library(infer)
set.seed(2154)
compare_runners <- function(runner1, runner2, visualize = FALSE) {
  
  diff_med <- male_100_3 %>%
    filter(runner %in% c(runner1, runner2)) %>%
    arrange(runner) %>%
    specify(timing ~ runner) %>%
    calculate("diff in medians",
              order = c(runner1, runner2))
  
  diff_med_null <- male_100_3 %>%
    filter(runner %in% c(runner1, runner2)) %>%
    arrange(runner) %>%
    specify(timing ~ runner) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10000, type = "permute") %>%
    calculate("diff in medians",
              order = c(runner1, runner2))
  
  if (visualize) {
    diff_med_plot <- diff_med_null %>%
      visualize(method = "simulation") +
      shade_p_value(obs_stat = diff_med, direction = "less") +
      xlab("Difference in median run timings") +
      ylab(NULL) +
      ggtitle(paste0("Comparing ", runner1, " and ", runner2)) +
      scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
      theme_bw() +
      theme(
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 0.5)
      )
    print(diff_med_plot)
  }
  
  diff_med_null %>%
    get_pvalue(obs_stat = diff_med, direction = "less")
    
}

compare_runners("Usain Bolt", "Asafa Powell", TRUE)
compare_runners("Asafa Powell", "Yohan Blake", TRUE)
compare_runners("Yohan Blake", "Justin Gatlin", TRUE)
compare_runners("Justin Gatlin", "Maurice Greene", TRUE)
compare_runners("Maurice Greene", "Tyson Gay", TRUE)

compare_runners("Usain Bolt", "Yohan Blake", TRUE)

compare_runners2 <- function(runner1, runner2) {
  
  ds <- male_100_3 %>%
    filter(runner %in% c(runner1, runner2)) %>%
    mutate(
      runner = case_when(
        runner == runner1 ~ paste0("01.", runner1),
        runner == runner2 ~ paste0("02.", runner2)
      )
    ) %>%
    arrange(runner)
  t.test(timing ~ runner, data = ds,
         alternative = "less", var.equal = FALSE)
  
}
compare_runners2("Usain Bolt", "Asafa Powell")
compare_runners2("Asafa Powell", "Yohan Blake")
compare_runners2("Yohan Blake", "Justin Gatlin")
compare_runners2("Justin Gatlin", "Maurice Greene")
compare_runners2("Maurice Greene", "Tyson Gay")

compare_runners2("Usain Bolt", "Yohan Blake")
