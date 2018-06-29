app_intro_title <- "Climate model evaluation results"
app_intro_logo <- "res/snap_white.svg"
app_intro_message <- "<h4>Explore SNAP's AR5/CMIP5 climate model evaluation.</h4>
<p style='text-align:justify;'>This application summarizes key results of SNAP's general circulation model (GCM) statistical evaluation and model selection.</p>
<p style='text-align:justify;'>The evaluation is of historical climate model runs over several geographic domains with an Alaska and Arctic focus.
GCM performance is ranked based on minimum error with respect to a European Re-Analysis (ERA-40) baseline data set using several error metrics.</p>
<p style='text-align:justify;'>A detailed tour is available in the sidebar. Additional information can be found on the <strong>Information</strong> tab. Click to dismiss this message at any time.</p>
<p style='text-align:justify;'>This is an application supplement to the publication:</p>
<p style='text-align:justify;padding-left:20px;padding-right:20px;'>Walsh, J. E., Bhatt, U. S., Littell, J. S., Leonawicz et al. 2018. Downscaling of climate model output for Alaskan stakeholders. Environmental Modelling & Software.</p>
<p style='text-align:justify;'>See the publication for additional context and details.</p>"

about_app <- tagList(
  h2("About this application"),
  HTML(read_md_paragraphs("about.txt", ptag = TRUE, collapse = TRUE)),
  app_citation("Matthew Leonawicz", 2018,
               title = "Web application for Coupled Model Intercomparison Project Phase 5 general circulation model evaluation",
               publisher = "Scenarios Network for Alaska and Arctic Planning, University of Alaska Fairbanks",
               url = "http://shiny.snap.uaf.edu/ar5eval")
)
