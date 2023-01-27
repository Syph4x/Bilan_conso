cleanNames <- function(x) {
  if (is.data.frame(x)) {
    cn <- colnames(x)
    cn_new <- cn %>% str_trim() %>% str_to_lower() %>%
      str_replace_all(pattern = "\\.", replacement = "_") %>%
      str_replace_all(pattern = '[éèê]+', replacement = "e") %>%
      str_replace_all(pattern = '[î]+', replacement = "i") %>%
      str_replace_all(pattern = '[à]+', replacement = "a") %>%
      str_replace_all(pattern = '[ûù]+', replacement = "u") %>%
      str_replace_all(pattern = "[:blank:]+", replacement = "_") 
    colnames(x) <- cn_new
    x
  } else if (is.character(x)) {
    x %>% str_trim() %>% str_to_lower() %>%
      str_replace_all(pattern = "\\.", replacement = "_") %>%
      str_replace_all(pattern = '[éèê]+', replacement = "e") %>%
      str_replace_all(pattern = '[î]+', replacement = "i") %>%
      str_replace_all(pattern = '[à]+', replacement = "a") %>%
      str_replace_all(pattern = '[ûù]+', replacement = "u") %>%
      str_replace_all(pattern = "[:blank:]+", replacement = "_") 
  } else {
    stop("L'argument doit être un jeu de données ou une chaîne de caractères.")
  }
}
