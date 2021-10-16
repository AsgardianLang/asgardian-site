### Load dependencies ##########################################################

library(magrittr)
library(stringi)

### Define main function #######################################################

replace.root <- function(old.root, new.root, file.names) {
  
  # Construct all possible wiki link forms for the old root.
  old.root %<>% 
    tolower()
  old.root %>% 
    sprintf("[[%s]]", .) ->
    old.lower
  old.root %>%
    stri_trans_totitle %>% 
    sprintf("[[%s|%s]]", ., old.root) ->
    old.upper
  
  # Construct all possible wiki link forms for the new root.
  new.root %<>% 
    tolower()
  new.root %>% 
    sprintf("[[%s]]", .) ->
    new.lower
  new.root %>%
    stri_trans_totitle %>% 
    sprintf("[[%s|%s]]", ., new.root) ->
    new.upper
  
  n.files <- length(file.names)
  for (i in 1:n.files) {
    f <- file.names[i]
    cat(sprintf("(%03d/%03d) Reading %s...\n", i, n.files, f))
    temp <- readLines(f)
    temp %<>% gsub(old.lower, new.lower, ., fixed=T)
    temp %<>% gsub(old.upper, new.upper, ., fixed=T)
    writeLines(temp, f)
  }
}

replace.root.all <- function(old.root, new.root) {
  
  # Define tiddler path.
  tiddler.path <- "../tiddlers/"
  
  # Get list of all .tid files
  tid.files <- list.files(
    path = tiddler.path,
   pattern = "*.tid")
  
  # Throw out shadow tiddlers and other stuff.
  # Basically, only operate on tiddlers whose first character is alphabetic.
  tid.files %>%
    substring(1, 1) %>%
    outer(c(letters, LETTERS), FUN = `%in%`) %>%
    apply(1, any) -> keep
  tid.files <- tid.files[keep]
  tid.files
  
  # Add in the path.
  tid.files %<>% 
    paste0(tiddler.path, .)
  
  # Now replace the root in each selected tiddler file.
  replace.root(old.root, new.root, tid.files)
  
  # Finally update the actual tiddler file.
  old.root %>% 
    tolower %>%
    paste0(tiddler.path, ., ".tid") -> old.file
  new.root %>% 
    tolower %>%
    paste0(tiddler.path, ., ".tid") -> new.file
  
  old.file %>%
   readLines() -> old.lines
  old.lines[3] <- sprintf("title: %s", new.root)
  old.lines %>%
   writeLines(new.file)
  unlink(old.file)
}

swap.roots <- function(root1, root2) {
  replace.root.all(root1, "placeholder_root")
  replace.root.all(root2, root1)
  replace.root.all("placeholder_root", root2)
}

#replace.root.all("mi", "i")
#replace.root.all("u", "su")

# Fix pronouns.
swap.roots("nima", "i")
swap.roots("tota", "su")
swap.roots("sapa", "ta")
