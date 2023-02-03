# name to display in title bar
title <- "ScExploreR"

# logo to display in header bar
header_title <- 
  shiny::tags$a(
    href = 'https://zdglab.iimcb.gov.pl/',
    shiny::tags$img(
      src = "logo-zdg-fish-white.png",
      height = '100',
      width = '200',
      align = "center",
      style = "padding-top: 20px; padding-bottom: 20px; 
               inline-size: -webkit-fill-available; width: unset; height: 88px; 
               margin-top: 10px;"
      ),
    target = "_blank"
    )

#skin color

skin_colors <-
  shiny::tags$head(shiny::tags$style(
    shiny::HTML(
      "
      .skin-blue .main-header .logo {background-color: #222d32;}
      .skin-blue .main-header .navbar {background-color: #222d32;}
      .skin-blue .main-sidebar {background-color: #222d32;}
      "
    )
    ))

experiment <-
  shiny::tags$img(
    src = "10x_workflow3.png",
    #style = "margin-left: 22.5%;",
    #height = 'auto',
    width = '75%',
    align = "center"
  )

