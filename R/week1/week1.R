library(tidyverse)
library(rethinking)

set.seed(1234)

globe_toss <- function(N, W, flat_prior = rep(1, 1000)) {
  p_grid <- seq(0, 1, length.out = 1000)
  prob_data <- dbinom(W, prob = p_grid, size = N)
  posterior <- prob_data * flat_prior
  posterior <- posterior/sum(posterior)
  samples <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE)
  return(list(posterior = posterior, samples = samples))
}

## Q1. 

q1 <- tibble(
  samples = globe_toss(15, 8)$samples
)

q1 %>% 
  ggplot(aes(samples)) +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1))

# Q2

q2 <- tibble(
  original = q1$samples,
  modified = globe_toss(15, 8, flat_prior = c(rep(0, 500), rep(1, 500)))$samples
) 

q2 %>%
  gather(prior, samples) %>%
  ggplot(aes(samples, linetype = prior)) +
  geom_density() +
  geom_vline(xintercept = 0.7, color = "red") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1))

# Q3

interval_width <- function(N, p_true = 0.5, prior = rep(1, 1000)) {
  W <- rbinom(1, N, p_true)
  p_grid <- seq(0, 1, length.out = 1000)
  prob_data <- dbinom(W, prob = p_grid, size = N)
  posterior <- prob_data * prior
  posterior <- posterior/sum(posterior)
  samples <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE)
  predictive_interval <- PI(samples, prob = 0.99)
  return(as.numeric(predictive_interval[2] - predictive_interval[1]))
}

sample_sizes <- c(20, 50, 100, 200, 500, 1000, 2000, 3000)

experiment <- tibble(
  N = rep(sample_sizes, each = 100)
) %>%
  mutate(
    width = map_dbl(N, interval_width, p_true = 0.7)
  )

experiment %>%
  ggplot(aes(N, width)) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0.05, color = "red")

#need 2000+ to have a width less than 0.05.