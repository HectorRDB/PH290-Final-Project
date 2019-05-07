library(ggplot2)
library(DailyHRB)
library(purrr)
library(here)

rk1 <- 1:27
rk2 <- sample(rk1, length(rk1), replace = F)

con <- map_dbl(0:length(rk1), function(i) {
  sum(rk1[1:i] %in% rk2[1:i])}
)
p <- ggplot(data.frame(j = 0:27, common = con), aes(x = j, y = common)) +
  geom_line(color = "grey") +
  geom_point(y = 0:27, col = "red") +
  geom_point() +
  my_theme() +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  annotate("text", label = "best possible\ncurve", x = 28 / 2, y = 28 / 2 + 4,
           size = 5) +
  geom_area(col = "grey", alpha = .1, lwd = 0) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 25))
p

ggsave(filename = here("Figures", "concordance.pdf"), plot = p)
