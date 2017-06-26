bsCollapse(id="faq",
  bsCollapsePanel("What is a GCM?",
    p("General Circulation Models (GCMs) are used to depict how climate processes respond to the composition of various gases in the atmosphere.
      Using future projections of the composition of gases in the atmosphere, projections of future climate can be made.
      The climate models used in this comparative model evaluation are based on the 
      Coupled Model Intercomparison Project Phase 5 (CMIP5) / fifth assessment report (AR5).", style="text-align: justify;"),
    a("Intergovernmental Panel on Climate Change GCM guide",
      href="http://www.ipcc-data.org/guidelines/pages/gcm_guide.html", target="_blank"),
    style="info"),
  bsCollapsePanel("What is ERA-40?",
    p("ERA 40 is a climate reanalysis data set.", style="text-align: justify;"),
    a("ERA 40 summary and data access",
      href="https://climatedataguide.ucar.edu/climate-data/era40", target="_blank"),
      style="info"),
  bsCollapsePanel("The domain map shows only one Alaska domain; what are the 'land' and 'ocean' domains?",
    p("For land- and ocean-specific domains, the rectangle domain over Alaska is the same,
      but the GCM and ERA-40 grid cells within that domain are subset to those exclusively over land or ocean, respectively.
      This was done to investigate the potential influence on Alaska domain model selection
      of land vs. ocean grid cells in the analysis.",
      style="text-align: justify;"), 
    style="info"),
  bsCollapsePanel("How are individual GCMs added to a composite?",
    gcm_inclusion,
    style="info"),
  bsCollapsePanel("How did you make this app?",
    p("This app is written in the", 
      a("R programming language", href="https://www.r-project.org/", target="_blank"), "and built with the", 
      a("Shiny", href="https://shiny.rstudio.com/", target="_blank"), "web application framework for R.
      Is your organization looking for similar web applications or dashboards for your data and analytics needs?
      SNAP designs R Shiny apps and other web-based tools and software ranging from simple to complex
      and suitable for a variety of stakeholders whose purposes include
      public outreach and scientific communication, directly supporting scientific research,
      and organizational data access and analytics.", 
      a("Contact us", href="https://www.snap.uaf.edu/about/contact/", target="_blank"), "for details.",
      style="text-align: justify;"), 
    style="info")
)
