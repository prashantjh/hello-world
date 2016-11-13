# Libraries
library(dplyr)
library(stringr)
library(tidyr)


# Reading movie script - Raw text file
raw <- readLines("./Input/love_actually.txt")

lines <- data_frame(raw = raw) %>%
  filter(raw != "", !str_detect(raw, "(song)")) %>%
  mutate(is_scene = str_detect(raw, " Scene "),
         scene = cumsum(is_scene)) %>%
  filter(!is_scene) %>%
  separate(raw, c("speaker", "dialogue"), sep = ":", fill = "left") %>%
  group_by(scene, line = cumsum(!is.na(speaker))) %>%
  summarize(speaker = speaker[1], dialogue = str_c(dialogue, collapse = " "))

cast <- read.csv(url("http://varianceexplained.org/files/love_actually_cast.csv"))

lines <- lines %>%
  inner_join(cast) %>%
  mutate(character = paste0(speaker, " (", actor, ")"))

# No. of lines-per-scene-per-character
by_speaker_scene <- lines %>%
  count(scene, character)

library(reshape2)
speaker_scene_matrix <- by_speaker_scene %>% 
  acast(character ~ scene, fun.aggregate = length)

# Analysis -----------------------------------------------

# Hierarchical clustering

norm <- speaker_scene_matrix/rowSums(speaker_scene_matrix)
heirach_clust <- hclust(dist(norm, method = "manhattan"))
plot(heirach_clust)
