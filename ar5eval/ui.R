addResourcePath("res", snap_res())
faqs <- c("gcm", "era40", "eval_domains", "eval_composite", "apps")

intro_css_args <- list(
  container = list(width = '800px', height = '400px'), 
  toast = list(top = '100px', 'background-size' = '50% 400px'), 
  rgba = c(60, 141, 188, 1), hover.rgba = c(60, 141, 188, 1), radius = '10px')

dashboardPage(
  dashboardHeader(
    title = "AR5 GCM Evaluation",
    tags$li(class = "dropdown",
            tags$a(href = "http://snap.uaf.edu", target = "_blank",
                   tags$img(src = "res/snap_acronym_white.png", width = "100%", alt = "SNAP"), style = "padding: 10px; margin: 0px;")
    )
  ),
  dashboardSidebar(
    use_apputils(TRUE, TRUE),
    useShinyjs(),
    do.call(update_toastr_css, intro_css_args),
    tags$head(tags$html(app_overlay(NULL, "res/snap_white.svg", "loading.png"))),
    tags$script("$(document).ready(function(){ $('#fade-wrapper').fadeIn(); });"),
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
                 class="btn-flat action-button btn-block", icon=icon("question-circle")),
    dashboard_footer("http://snap.uaf.edu/", "res/snap_white.svg", "SNAP Dashboards")
  ),
  dashboardBody(
    includeCSS("www/styles.css"),
    tabItems(
      tabItem(tabName="overview",
        fluidRow(
          column(9, h4("Variable selection")),
          column(3, actionButton("staticmap", "Domain map", class="btn-block", icon("globe")))
        ),
        fluidRow(
          column(3, selectInput("stat", "Error statistic", err_stats, "RMSE", multiple=TRUE, width="100%")),
          column(3, selectInput("vars", "Climate variable", variables, "integrated", multiple=TRUE, width="100%")),
          column(3, selectInput("spdom", "Spatial domains", domains, "AK", multiple=TRUE, width="100%")),
          column(3, selectInput("time", "Time", c("Annual", month.abb), "Annual", width="100%"))
        ),
        fluidRow(box(title="", status="primary", width=12, height=0)),
        fluidRow(column(12, h4("GCM rankings and selection probability"))),
        fluidRow(column(12, plotOutput("rankPlot", height=400))),
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
        about_app,
        h2("Frequently asked questions"),
        faq(faqs, bscollapse_args = list(id = "faq", open = "apps"), showcase_args = list(drop = "ar5eval")),
        contactinfo(snap = "res/snap_color.svg", iarc = "res/iarc.jpg", uaf = "res/uaf.png"), br()
      )
    )
  ),
  title="AR5 GCM Evaluation"
)
