# Ordering categories within ggplot2 facets

# Required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(janeaustenr)

# From section 5.1: Tokenizing by n-gram
austen_bigrams <- austen_books() %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)

# From section 5.1.1: Counting and filtering n-grams
bigrams_separated <- austen_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")


# From section 5.1.3: Using bigrams to provide context in sentiment analysis
AFINN <- get_sentiments("afinn")
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
    filter(word1 %in% negation_words) %>%
    inner_join(AFINN, by = c(word2 = "word")) %>%
    count(word1, word2, score, sort = TRUE) %>%
    ungroup()

# Create plot
negated_words %>%
    mutate(contribution = n * score) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    group_by(word1) %>%
    top_n(10, abs(contribution)) %>%
    ggplot(aes(word2, contribution, fill = n * score > 0)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(~ word1, scales = "free") +
    xlab("Words preceded by negation") +
    ylab("Sentiment score * # of occurrences") +
    theme_bw() +
    coord_flip()


negated_words %>%
    mutate(contribution = n * score) %>%
    # ----- >
    # This line does some reordering
    # but not perfect
    mutate(word2 = reorder(word2, contribution)) %>%
    # < -----
group_by(word1) %>%
    top_n(10, abs(contribution))

# Plot Data Frame
pd <- negated_words %>%
    mutate(contribution = n * score) %>%
    group_by(word1) %>%
    top_n(10, abs(contribution)) %>% 
    # 1. Remove grouping
    ungroup() %>%
    # 2. Arrange by
    #   i.  facet group
    #   ii. bar height
    arrange(word1, contribution) %>%
    # 3. Add order column of row numbers
    mutate(order = row_number())

pd

ggplot(pd, aes(order, contribution, fill = n * score > 0)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(~ word1) +
    xlab("Words preceded by negation") +
    ylab("Sentiment score * # of occurrences") +
    theme_bw() +
    coord_flip()

ggplot(pd, aes(order, contribution, fill = n * score > 0)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    # Free the scales here
    facet_wrap(~ word1, scales = "free") +
    xlab("Words preceded by negation") +
    ylab("Sentiment score * # of occurrences") +
    theme_bw() +
    coord_flip()


#Adding categories to the axis
#The last piece of the puzzle is to replace the numeric values on each x-axis with the appropriate word. We can do this using scale_x_continuous to replace the order number with the corresponding category (word2) in the row. Aside, we’re controlling the x-axis, but the words appear on the y-axis because we’re using coord_flip.

ggplot(pd, aes(order, contribution, fill = n * score > 0)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(~ word1, scales = "free") +
    xlab("Words preceded by negation") +
    ylab("Sentiment score * # of occurrences") +
    theme_bw() +
    # Add categories to axis
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$word2,
        expand = c(0,0)
    ) +
    coord_flip()
