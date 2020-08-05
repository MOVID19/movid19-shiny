message("R/helpers.R")

RMD_to_HTML <- function(file) {
  
  HTML(
    markdown::markdownToHTML(
      text = knitr::knit(
        text = readLines(file, encoding = "UTF-8"),
        quiet = TRUE
      ),
      fragment.only = TRUE
    )  
  )
  
}

bs4CardCustom <- purrr::partial(
  bs4Card,
  status = "primary",
  solidHeader = TRUE, 
  collapsible = FALSE,
  closable = FALSE,
  elevation = 4,
  width = 12
)

bs4CardHC <- purrr::partial(
  bs4Card,
  elevation = 1,
  closable = FALSE,
  width = 6,
  collapsible = FALSE
)