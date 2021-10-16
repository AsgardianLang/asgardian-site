### Load dependencies ##########################################################

library(magrittr)
library(stringi)

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
tid.files
rm(keep)

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