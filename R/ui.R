#edit
library(leaflet)
library(ggplot2)
library(markdown)
library(shinyjs)

# Choices for drop-downs
vars <- c(
  "Stations" = "Stations",
  "Overstap centraliteit (weekend)" = "TR Centrality Weekend",
  "Reistijd centraliteit (weekend)" = "TT Centrality Weekend",
  "Overstap centraliteit (dinsdag)" = "TR Centrality Tuesday",
  "Reistijd centraliteit (dinsdag)" = "TT Centrality Tuesday"
)

plaatsvars <- c(
  "Geen selectie" = "Geen selectie",
  "Mix Landgebruik" = "landuse",
  "Basisvoorzieningen" = "basic",
  "Regionale Voorzieningen" = "regional",
  "Metropolitane Voorzieningen" = "metropol",
  "Job Dichtheid" = "job"
)

kaartvars <- c(
  "Geen selectie" = "Geen selectie",
  "Fietssnelwegen netwerk" = "Fietssnelwegen netwerk",
  "Vervoerregios" = "Vervoerregios",
  "Wandelbuffer" = "Wandelbuffer",
  "Cambio plaatsen" = "Cambio plaatsen",
  "Busroutes Aarschot" = "Aarschot",
  "Busroutes Haacht" = "Haacht",
  "Busroutes Testelt" = "Testelt",
  "Busroutes Wespelaar" = "Wespelaar",
  "Busroutes Wezemaal" = "Wezemaal",
  "Busroutes Wijgmaal" = "Wijgmaal"
  )

knoopvars <- c(
  "Geen selectie" = "Geen selectie",
  "Selectie haltes De Lijn" = "Selectie haltes De Lijn",
  "Selectie haltes TEC" = "Selectie haltes TEC",
  "Selectie haltes MIVB" = "Selectie haltes MIVB",
  "Alle haltes De Lijn" = "Alle haltes De Lijn",
  "Alle haltes TEC" = "Alle haltes TEC",
  "Alle haltes MIVB" = "Alle haltes MIVB"
)

#1 graphvars <- c(
#1  "Algemeen" = "Dimensions",
#1  "Detail" = "Indicators"
#1 )

#1 filtervars <- c(
#1  "Alle Stations" = "All",
#1  "26 = Vervoerregio" = "VR_26_Leuven"
#1 )

flowsvars <- c(
  "Geen selectie",
  "Bestemmingen",
  "Herkomsten"
)

#navbarPage("StationsRadar", id="nav",
#  tabPanel("Tool",
          
fluidPage(
           useShinyjs(),
    div(class="outer", style = "margin-top:-50px;",
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeHTML("google-analytics.html")
      ),
#1      tags$script(HTML("var header = $('.navbar > .container-fluid');
#1  header.append('<div style=\"float:right\"><a href=\"URL\"><img src=\"ugent.png\" alt=\"alt\" style=\"float:right; width:41px; height:41px;padding-top:10px;\"> </a></div><div style=\"float:right\"><a href=\"URL\"><img src=\"vub.png\" alt=\"alt\" style=\"float:right; width:92px; height:41px;padding-top:10px;padding-right:20px;\"> </a></div><div style=\"float:right\"><a href=\"URL\"><img src=\"fwo.png\" alt=\"alt\" style=\"float:right; width:105px; height:41px;padding-top:10px;padding-right:20px;\"> </a></div>');
#1    console.log(header)")
#1      ), 
      div(id="loading", "Loading&#8230;"),
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 420, height = "auto",

        h4("Kaartlagen"),

        div(style="display: inline-block;vertical-align:top; width: 220px;",
            selectInput("knoop", "Knoop", knoopvars)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            selectInput("raster", "Plaats", plaatsvars)),
        div(style="display: inline-block;vertical-align:top; width: 220px;",
            selectInput("centrality", "Trein", vars)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
        selectInput("overige", "Overige", kaartvars)),
      # div(align="center", "Het laden van sommige lagen kan enkele seconden duren."),
      # hr(),
        
      
      
      
#1        h4("Stationsprofiel"),
#1        div(style="display: inline-block;vertical-align:top; width: 250px;
#1            height: 65px;",
#1            selectInput("filter", "Filter", filtervars, selected = "Alle Stations")),
#        div(style="display: inline-block;vertical-align:top; width: 150px;",
#            selectInput("flows", "Flows", flowsvars)),
#1        div(style="display: inline-block;vertical-align:top; width: 150px;",
#1            selectInput("graph", "Type", graphvars)),
#1        div(style="display: inline-block;vertical-align:top; width: 220px;",
#1            uiOutput("stationselection")),
        br()#,
  #      div(align="center",
  #      imageOutput("image", height = 300))
  #    ),

    #  tags$div(id="cite",
    #    'Data compiled for ', tags$em('UGent')
    #  )
    )
  )#,
  
#  tabPanel("Roosdiagram", includeMarkdown("docs/tool_info.Rmd")),
#  tabPanel("Tabel", tableOutput("overzicht")),
#  tabPanel("Metadata", tableOutput("metadata"))
)


