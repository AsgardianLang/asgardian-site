library(magrittr)

x <- "ase kepa komi ton sen aukiho siun piki katan pau (rose/flower), katan pau sipate, sonen epan (mixing), sen (look) manen rena"
y <- x

punct <- c(".", ",", "?", "!", "(", ")")
redir <- c("FS", "CO", "QM", "EP", "LP", "RP")

for (i in 1:length(punct)) {
  old <- punct[i]
  new <- paste0("%%%[[", punct[i], "|", redir[i], "]]%%%")
  y %<>% gsub(old, new, ., fixed=T)
}
y %>% 
  gsub(" ", "___", ., fixed=T) %>%
  strsplit("%%%", fixed=T) %>%
  lapply(function(x) { gsub("___", "]]___[[", x, fixed=T) } ) %>%
  lapply(function(x) { sprintf("[[%s]]", x) } ) %>%
  lapply(function(x) { gsub("[[[[", "[[", x, fixed=T) } ) %>%
  lapply(function(x) { gsub("]]]]", "]]", x, fixed=T) } ) %>%
  lapply(function(x) { gsub("[[]]", "", x, fixed=T) } ) %>%
  unlist %>% 
  paste(collapse="") %>%
  gsub("___", " ", ., fixed=T) %>%
  gsub("n]]", "]][[n]]", ., fixed=T) -> y2
  
