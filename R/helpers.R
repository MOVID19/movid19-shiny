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

bs4CardMovid <- purrr::partial(
  bs4Card,
  width = 6,
  collapsible = FALSE,
  ... =
)


valueBox <- function(value, subtitle, icon = NULL, elevation = 3, status = NULL, 
                     width = 3, footer = NULL, href = NULL) {
  
  
  value <- ifelse(is.numeric(value), scales::comma(value, big.mark = ".", decimal.mark = ","), value)
  value <- tags$h1(value)
  
  bs4Dash::valueBox(
    value, subtitle, icon = icon, elevation = elevation, status = status, 
    width = width, footer = footer, href = href
  )
  
}

bs4Card <- function(
  ...,
  inputId = NULL, title = NULL, footer = NULL, status = NULL,
  elevation = 3, solidHeader = FALSE, headerBorder = TRUE,
  gradientColor = NULL, width = 6, height = NULL, collapsible = FALSE,
  collapsed = FALSE, closable = FALSE, maximizable = FALSE, 
  cardLabel = NULL, dropdownMenu = NULL, overflow = FALSE,
  sidebar = NULL) {
  
  # hack necesario para poder cambiar los defaults
  # creo que se debe al doble ellipsis
  
  bs4Dash::bs4Card(
    ..., 
    inputId = inputId, title = title, footer = footer, status = status, 
    elevation = elevation, solidHeader = solidHeader, headerBorder = headerBorder, 
    gradientColor = gradientColor, width = width, height = height, collapsible = collapsible, 
    collapsed = collapsed, closable = closable, maximizable = maximizable, 
    cardLabel = cardLabel, dropdownMenu = dropdownMenu, overflow = overflow, 
    sidebar = sidebar
  )
}

