### Load dependencies ##########################################################

library(magrittr)
library(stringi)

options(stringsAsFactors = F)

### Get list of all Asgardian word tiddlers ####################################

# Define tiddler path.
tiddler.path <- "../tiddlers/"

# Get list of all .tid files
tid.files <- list.files(
  path = tiddler.path,
  pattern = "*.tid")

# Throw out shadow tiddlers and other stuff.
# Basically, only operate on tiddlers whose first character is a lower-case
# letter.
tid.files %>%
  substring(1, 1) %>%
  outer(c(letters), FUN = `%in%`) %>%
  apply(1, any) -> keep
tid.files <- tid.files[keep]
bad <- grepl("_", tid.files, fixed=T)
tid.files <- tid.files[!bad]
rm(bad, keep)

# Convert filenames to list of Asgardian words
tid.files %>%
  gsub(".tid", "", ., fixed=T) -> 
  Asgardian

# Append path back on.
tid.files %<>% paste0(tiddler.path, .)

### Read files and pull definitions ############################################

# For each file, pull all the bullet-pointed definitions and join with |.
index.in <- function(ind, x) { x[ind] }
custom.pull <- function(f) {
  temp <- readLines(f)
  temp %>%
    substr(1, 1) %>%
    `==`("*") %>%
    index.in(temp) %>%
    gsub("*", "", ., fixed=T) %>%
    stri_trim_both %>%
    paste(collapse=" | ") ->
    def
  return(def)
}
tid.files %>% sapply(custom.pull) %>% as.character -> English

### Summarize results ##########################################################

# Combine into a data frame
result <- data.frame(
  Asgardian = Asgardian,
  English = English
)

# Get current date / time
Sys.time() %>% 
  format("Lexicon (%Y_%m_%d_%H%M).csv") -> out.file

# Save results
write.csv(result, out.file, row.names=F)
result$Asgardian %>%
  paste0(collapse="\n") %>%
  writeLines("taken.txt")

### Make a lexicon card for English ############################################

t <- Sys.time()
lines <- c(
  format.Date(t, "created: %Y%m%d%H%M%S00000"),
  format.Date(t, "modified: %Y%m%d%H%M%S00000"),
  "tags: ",
  "title: Lexicon (En)",
  "type: text/vnd.tiddlywiki",
  "",
  "Below is a table representing the current lexicon of Asgardian.",
  "",
  "| Asgardian | English |h"
)

for (i in 1:nrow(result)) {
  result$Asgardian[i] %>%
    sprintf("|[[%s]]|", .) -> temp.asg
  result$English[i] %>% 
    strsplit("|", fixed=T) %>%
    unlist %>%
    stri_trim %>%
    sprintf("%s|", .) -> temp.eng
  
  temp.asg %<>% rep(length(temp.eng))
  if (length(temp.asg) > 1) {
    temp.asg[-1] <- "|~|"
  }
  
  lines %<>% c(paste0(temp.asg, temp.eng))
}

lines %>%
  writeLines(paste0(tiddler.path, "Lexicon (En).tid"))

  