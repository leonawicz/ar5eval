#faq <- source("faq.R", local=TRUE)[[1]]

dashboardPage(
  dashboardHeader(
    title="AR5 GCM Evaluation",
    tags$li(class="dropdown",
            tags$a(href="http://snap.uaf.edu", target="_blank",
                   tags$img(src="SNAP_acronym_100px.png", width="100%", alt="SNAP"), style="margin: 10px; padding: 0px;")
    )
    #tags$head(includeScript("ga-nwtapp.js"), includeScript("ga-allapps.js")),
  ),
  dashboardSidebar(
    #useToastr(),
    #introjsUI(),
    sidebarMenu(
      id="tabs",
      menuItem("Spatial bootstrap", icon=icon("sliders"),
        menuSubItem("Overview", tabName="overview"),
        menuSubItem("Alaska", tabName="sbAK"),
        menuSubItem("Alaska (land)", tabName="sbAK_land"),
        menuSubItem("Alaska (ocean)", tabName="sbAK_water"),
        menuSubItem("Canada", tabName="sbCAN"),
        menuSubItem("Alaska-Canada", tabName="sbAKCAN"),
        menuSubItem("60-90 North", tabName="sb6090N"),
        menuSubItem("20-90 North", tabName="sb2090N"),
        menuSubItem("Lower 48 states", tabName="sblow48"),
        menuSubItem("Pacific islands", tabName="sbpacif")
      ),
      menuItem("Sequential ensembles", icon=icon("fire", lib="glyphicon"),
        menuSubItem("Alaska", tabName="AK"),
        menuSubItem("Alaska (land)", tabName="AK_land"),
        menuSubItem("Alaska (ocean)", tabName="AK_water"),
        menuSubItem("Canada", tabName="CAN"),
        menuSubItem("Alaska-Canada", tabName="AKCAN"),
        menuSubItem("60-90 North", tabName="6090N"),
        menuSubItem("20-90 North", tabName="2090N"),
        menuSubItem("Lower 48 states", tabName="low48"),
        menuSubItem("Pacific islands", tabName="pacif")
      ),
      menuItem("Information", icon=icon("info-circle"), tabName="info")
    )
  ),
  dashboardBody(
    #includeCSS("www/styles.css"),
    #bsModal("staticmap", "Fire Management Zones", "btn_staticmap", size="large",
    #        img(src='Fire_Mgmt_Areas.png', align="center", style="width: 100%")
    #),
    tabItems(
      tabItem(tabName="overview",
        fluidRow(
          box(title="Variable selection", 
            fluidRow(
              column(4, selectInput("spdom", "Spatial domains", domains, "AK", multiple=TRUE, width="100%")),
              column(4, selectInput("stat", "Error statistic", err_stats, "RMSE", multiple=TRUE, width="100%")),
              column(4, selectInput("vars", "Climate variable", variables, "Integrated", multiple=TRUE, width="100%"))
              ), status="primary", width=12
          )
        ),
        fluidRow(column(12, plotOutput("rankPlot", height=275))),
        fluidRow(column(12, plotOutput("top5Plot"))),
        fluidRow(
          box(title="Plot settings", 
            fluidRow(
              column(4, selectInput("clrby", "Color by", grp_vars, width="100%")),
              column(4, selectInput("fctby", "Facet by", grp_vars, width="100%")),
              column(4, sliderInput("n_gcms", "Number of GCMs shown", 1, 21, 21, 1, width="100%"))
            ), status="primary", width=12
          )
        ), br()
      ),
      spbootModUI(id="sbAK"),
      spbootModUI(id="sbAK_land"),
      spbootModUI(id="sbAK_water"),
      spbootModUI(id="sbCAN"),
      spbootModUI(id="sbAKCAN"),
      spbootModUI(id="sb6090N"),
      spbootModUI(id="sb2090N"),
      spbootModUI(id="sblow48"),
      spbootModUI(id="sbpacif"),
      compositeModUI(id="AK"),
      compositeModUI(id="AK_land"),
      compositeModUI(id="AK_water"),
      compositeModUI(id="CAN"),
      compositeModUI(id="AKCAN"),
      compositeModUI(id="6090N"),
      compositeModUI(id="2090N"),
      compositeModUI(id="low48"),
      compositeModUI(id="pacif"),
      tabItem(tabName="info",
              h2("Frequently asked questions"),
              #faq,
              h2("Contact information"),
              p("For questions about this application, please email mfleonawicz@alaska.edu")
      )
    )
  ),
  title="AR5 GCM Evaluation"
)
