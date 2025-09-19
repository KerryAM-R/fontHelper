#' Register System Fonts for R and Shiny
#'
#' Automatically registers system fonts for use in R plots and Shiny apps.
#' Users can choose to register all installed fonts or a set of 20 common fonts.
#'
#' @param which Character: either "common" to register only common fonts
#'   or "all" to register all installed fonts. Default is "common".
#'
#' @return Character vector of registered font families.
#' @importFrom showtext  showtext_auto
#' @importFrom sysfonts font_add
#' @import systemfonts
#' @examples
#' \dontrun{
#' # Register common fonts
#' fonts <- register_fonts("common")
#' print(fonts)
#'
#' # Use in a plot
#' plot(1:10, main = "Hello World", family = fonts[1])
#'
#' # Use in a Shiny app
#' library(shiny)
#' ui <- fluidPage(
#'   selectizeInput("font", "Choose font:", choices = fonts, server = TRUE),
#'   plotOutput("plot")
#' )
#' server <- function(input, output) {
#'   output$plot <- renderPlot({
#'     plot(1:10, main = "Hello World", family = input$font)
#'   })
#' }
#' shinyApp(ui, server)
#' }
#' @export
register_fonts <- function(which = c("common", "all")) {
  which <- match.arg(which)

  common_fonts_list <- c(
    "Arial", "Verdana", "Helvetica", "Times New Roman", "Georgia",
    "Courier New", "Trebuchet MS", "Palatino", "Garamond", "Bookman",
    "Comic Sans MS", "Impact", "Tahoma", "Lucida Console", "Rockwell",
    "Franklin Gothic Medium", "Candara", "Optima", "Calibri", "Cambria"
  )

  installed_fonts <- unique(systemfonts::system_fonts()$family)

  if (which == "common") {
    fonts_to_register <- common_fonts_list[common_fonts_list %in% installed_fonts]
  } else {
    fonts_to_register <- installed_fonts
  }

  for (f in fonts_to_register) {
    font_path <- systemfonts::match_fonts(f)$path[1]
    if (!is.na(font_path)) {
      try({
        sysfonts::font_add(family = f, regular = font_path)

      }, silent = TRUE)
    }
  }

  showtext::showtext_auto()
  message(length(fonts_to_register), " fonts registered with showtext.")

  invisible(fonts_to_register)
}
