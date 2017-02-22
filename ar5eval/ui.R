faq <- source("faq.R", local=TRUE)[[1]]

dashboardPage(
  dashboardHeader(
    title="AR5 GCM Evaluation",
    tags$li(class="dropdown",
            tags$a(href="http://snap.uaf.edu", target="_blank",
                   tags$img(src="SNAP_acronym_100px.png", width="100%", alt="SNAP"), style="padding: 10px; margin: 0px;")
    )
    #tags$head(includeScript("ga-nwtapp.js"), includeScript("ga-allapps.js")),
  ),
  dashboardSidebar(
    #useToastr(),
    introjsUI(),
    sidebarMenu(
      id="tabs",
      menuItem("Spatial bootstrap", icon=icon("bar-chart"),
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
      menuItem("Sequential ensembles", icon=icon("signal"),
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
    ),
    actionButton("help", "Take tour", style="margin: 10px 15px 10px 15px; width: 200px",
                 class="btn-flat action-button btn-block", icon=icon("question-circle"))
  ),
  dashboardBody(
    #includeCSS("www/styles.css"),
    #bsModal("staticmap", "Fire Management Zones", "btn_staticmap", size="large",
    #        img(src='Fire_Mgmt_Areas.png', align="center", style="width: 100%")
    #),
    tabItems(
      tabItem(tabName="overview",
        fluidRow(column(12, h4("Variable selection"))),
        fluidRow(
          column(3, selectInput("stat", "Error statistic", err_stats, "RMSE", multiple=TRUE, width="100%")),
          column(3, selectInput("vars", "Climate variable", variables, "integrated", multiple=TRUE, width="100%")),
          column(3, selectInput("spdom", "Spatial domains", domains, "AK", multiple=TRUE, width="100%")),
          column(3, selectInput("time", "Time", c("Annual", month.abb), "Annual", width="100%"))
        ),
        fluidRow(box(title="", status="primary", width=12, height=0)),
        fluidRow(column(12, h4("GCM rankings and selection probability"))),
        fluidRow(column(12, plotOutput("rankPlot", height=275))),
        fluidRow(column(12, plotOutput("top5Plot"))),
        fluidRow(
          column(3, selectInput("clrby", "Color by", grp_vars, width="100%")),
          column(3, selectInput("fctby", "Facet by", grp_vars, width="100%")),
          column(3, selectInput("order", "GCM plot order", gcm_order, width="100%")),
          column(3, sliderInput("n_gcms", "Number of GCMs shown", 1, 21, 21, 1, width="100%"))
        )
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
        p("A bit about this application..."),
        h3("Frequently asked questions"),
        faq,
        h3("Contact information"),
        HTML('
          <div style="clear: left;"><img src="http://www.gravatar.com/avatar/52c27b8719a7543b4b343775183122ea.png"
            alt="" style="float: left; margin-right:5px" /></div>
          <p>Matthew Leonawicz<br/>
          Statistician | useR<br/>
          <a href="http://leonawicz.github.io" target="_blank">Github.io</a> |
          <a href="http://blog.snap.uaf.edu" target="_blank">Blog</a> |
          <a href="https://twitter.com/leonawicz" target="_blank">Twitter</a> |
          <a href="http://www.linkedin.com/in/leonawicz" target="_blank">Linkedin</a> <br/>
          <a href="http://www.snap.uaf.edu/", target="_blank">Scenarios Network for Alaska and Arctic Planning</a>
          </p>'
        ),
        p("For questions about this application, please email mfleonawicz@alaska.edu")
      )
    )
  ),
  title="AR5 GCM Evaluation"
)
