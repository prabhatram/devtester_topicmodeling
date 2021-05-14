library(widyr)
library(dplyr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(igraph)
library(ggraph)
library(topicmodels)


mantis16ISS  <- read.csv(file.choose(), header = T)
mantis16FB  <- read.csv(file.choose(), header = T)

mantis16ISS$issueDescription <- as.character(mantis16ISS$issueDescription)
mantis16FB$testFeedback <- as.character(mantis16FB$testFeedback)

issue_descend <- tibble(id = mantis16ISS$issueId, desc = mantis16ISS$issueDescription)
feedback_descend <- tibble(id = mantis16FB$issueId, feedback = mantis16FB$testFeedback)

issue_descend <- issue_descend %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words)

feedback_descend <- feedback_descend %>% 
  unnest_tokens(word, feedback) %>% 
  anti_join(stop_words)

my_stopwords <- tibble(word = c(as.character(1:10), 
                                "modelio", "build", "x86", "win32", "version", "system", 
                                "architecture"))

feedback_descend <- feedback_descend %>% 
  anti_join(my_stopwords)

word_counts <- issue_descend %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)

# be aware that running this model is time intensive
# for issue description
desc_lda <- LDA(desc_dtm, k = 6, control = list(seed = 1234))

tidy_lda <- tidy(desc_lda)

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")


lda_gamma <- tidy(desc_lda, matrix = "gamma")


ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))


ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))

feedback_descend$id <- as.character(feedback_descend$id)

lda_gamma <- full_join(lda_gamma, feedback_descend, by = c("document" = "id"))

top_keywords <- lda_gamma %>% 
  filter(gamma > 0.9) %>% 
  count(topic, word, sort = TRUE)


top_keywords %>%
  group_by(topic) %>%
  top_n(10, n) %>%
  ungroup %>%
  mutate(word = reorder_within(word, n, topic)) %>%
  ggplot(aes(word, n, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top keywords for each LDA topic",
       x = NULL, y = "Number of documents") +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ topic, ncol = 4, scales = "free")
