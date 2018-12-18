x <- "iris %>% mutate(is_virgninica = Species == 'virginica') %>% group_by(species) %>% summarize(n = n())"
tokenize(x)

tokenize("x <- 1")

