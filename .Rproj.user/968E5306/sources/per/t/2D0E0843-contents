# Real Emojis in ggplot2

# source - https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/


# type - learning daily


library(tidyverse)
library(rtweet)
library(rvest)
devtools::install_github("clauswilke/ggtext")
library(ggtext)
devtools::install_github("hadley/emo")
library(emo)


emo::ji_completion('poop')


happy <- search_tweets("happy", include_rts = FALSE, n = 1000)


happy_emojis <- happy %>%
  mutate(emoji = emo::ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE)


emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}


link_to_img <- function(x, size = 25) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

top_happy <- happy_emojis %>%
  slice(1:10) %>%
  mutate(url = map_chr(emoji, slowly(~emoji_to_link(.x), rate_delay(1))),
         label = link_to_img(url))

top_happy %>%
  ggplot(aes(emoji, n, label = label)) +
  geom_richtext(aes(y = n), fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  theme_minimal()