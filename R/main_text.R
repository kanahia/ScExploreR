main_text <-
  shiny::mainPanel(
    shiny::p("p creates a paragraph of text."),
    shiny::p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.",
      style = "font-family: 'times'; font-si16pt"),
    shiny::strong("strong() makes bold text."),
    shiny::em("em() creates italicized (i.e, emphasized) text."),
    shiny::br(),
    shiny::code("code displays your text similar to computer code"),
    shiny::div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div",
      style = "color:blue"),
    shiny::br(),
    shiny::p("span does the same thing as div, but it works with",
    shiny::span("groups of words", 
           style = "color:blue"),
      "that appear inside a paragraph.")
  )