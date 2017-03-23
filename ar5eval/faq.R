bsCollapse(id="faq",
  bsCollapsePanel("What is a GCM?",
    p("General Circulation Models (GMCs) are used to depict how climate processes respond to the composition of various gases in the atmosphere.
      Using future projections of the composition of gases in the atmosphere, projections of future climate can be made.
      For this work, the GCMs provide different projections of future climate that are used to inform projections of future fire activity.
      If you are interested in exploring the range of burned area with these GCMs,
      the GFDL-CM3 and NCAR-CCSM4 models tend to correspond to the projections with the most area burned
      and the MRI-CGCM3 tends to have the least area burned. For more information, please see the following link.", style="text-align: justify;"),
    a("Intergovernmental Panel on Climate Change GCM guide",
      href="http://www.ipcc-data.org/guidelines/pages/gcm_guide.html", target="_blank"),
    style="info"),
  bsCollapsePanel("What is ERA-40?",
    p("Add text here, include link...", style="text-align: justify;"), 
      style="info"),
  bsCollapsePanel("The domain map shows only one Alaska domain; what are the 'land' and 'ocean' domains?",
    p("For land- and ocean-specific domains, the rectangle domain over Alaska is the same,
      but the GCM and ERA-40 grid cells within that domain are subset to those exclusively over land or ocean, respectively.
      This was done to investigate the potential influence on Alaska domain model selection
      of land vs. ocean grid cells in the analysis.",
      style="text-align: justify;"), 
    style="info"),
  bsCollapsePanel("Why are composite GCMs not always ordered by lowest error?",
    gcm_inclusion,
    style="info"),
  bsCollapsePanel("What is...?", 
    p("Blah blah...", style="text-align: justify;"),
    style="info")
)
