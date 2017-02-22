# calback convenience functions
stepEquals <- function(i) paste0("this._currentStep==", i-1, collapse=" || ")

dv <- function(x, quote=TRUE){
  x <- paste0("a[data-value=\"", x, "\"]")
  if(quote) x <- paste0("'", x, "'")
  x
}

rmClass <- function(x) paste0(paste0(
  "$(", dv(x), ").removeClass('active');", collapse="\n"), "\n")

goClass <- function(x){
  if(length(x) > 1) stop("Only add and trigger one class at a time.")
  paste0("$(", dv(x), ").addClass('active');\n$(", dv(x), ").trigger('click');\n")
}

stepcb <- function(condition, action){
  paste0("if (", condition, ") {", paste0(action, collapse="\n"), "}")
}

# tour steps
tour.text <- c(
  "00"="GCM evaluation is a two-stage process. It begins with ranking individual GCMs based on their historical similarity
  to an ERA-40 reanalysis baseline. The second stage evaluates composite GCMs with different
  ensemble membership to assess the optimal number of GCMs to include in a composite model.
  Stage two also incorporates statistical tests to validate model selection in the context of all available ensembles of a given size.",
  "01"="Spatial bootstrapping of estimated error for ranking individual GCM performance is the first stage in GCM evaluation.
  The spatial bootstrap overview tab is shown when the app launches.
  It displays a summary of estimated error between 1958 - 2001 climatologies for each historical GCM
  and the corresponding ERA-40 reanalysis baseline climatology.",
  "02"="Four different error metrics are used to estimate deviation between historical GCMs and ERA-40 data
  for purposes of ranking GCM performance and informing model selection for use over various spatial domains
  as both individual GCMs and in composite models.",
  "03"="The statistics used are root mean squared error (RMSE), RMSE with bias correction, mean absolute error (MAE),
  and MAE with bias correction.",
  "04"="Evaluation of GCMs is done for each of three climate variables, precipitation, sea level pressure and temperature,
  as well as an integration of all three by combining after standardizing the variables.",
  "05"="Nine spatial domains are used for the analysis, which focuses on Alaska but spreads out to reach Canada, the pan-Arctic, 
  the lower 48 states, and other lower northern lattitudes including a Pacific islands region.",
  "06"="Estimated error using each error statistic, climate variable, and spatial domain is calculated monthly.
  While GCMs can be ranked based on monthly or seasonal performance, mean annual estimated error is typically used to rank the GCMs.",
  "07"="The first step in the model evaluation involves a spatial bootstrap resampling on each geographic domain to estimate
  the sampling distribution of each error statistic.",
  "08"="The bootstrap samples offer a distribution of estimates. The first plot shows the bootstrapped range of GCM performance rankings
  as well as mean ranks.",
  "09"="The second plot shows another bootstrapped statistic: the probability of a GCM being among the top performers in terms of
  lowest estimated error. Since other work suggests the use of five models is optimal for constructing a composite model,
  this is also the probability of selection, as five GCMs are often used in various subsequent work at SNAP.",
  "10"="Below the plots are optional plot settings. If multiple selections are made from one of the variables above, they can be grouped by color.",
  "11"="Multiple selections can be similarly used for facetting the plot.", 
  "12"="Note that splitting the data in the plot into multiple colors on multiple panels allows for displaying multiple selecitons for
  up to two variables at a time. For example, you can compare four error metrics between two spatial domains but cannot then also
  compare between two climate variables simultaneously. It is also recommended that you not compare too many variables and their levels at once
  or the plots will become too difficult to read or will simply not fit on the screen.",
  "13"="By default the plot order of the rank ranges and performance bars is fixed alphabetically so that it is consistent when changing
  selection of error statistic, climate variable, spatial domain and time period. It can be changed to reorder from left to right based
  on best performance in terms of mean performance ranking. This is helpful for visualizing a specific selection of data though it can
  make comparisons between selections more difficult.",
  "14"="The number of GCMs displayed can be reduced. Displaying fewer GCMs removes the worst perfoming GCMs from the plots.
  This is helpful when coloring and/or faceting by the variables above 
  since coloring and faceting have the ability to significant crowd the visualiztion.",
  "15"="Next we will look at results pertaining specicically to Alaska. After pressing 'Next' you will leave this overview tab
  and be taken to the Alaska tab as the next part of the tour.
  Outside of this tour you can freely explore the menu tree in the sidebar. For now, let's jump to the Alaska results.",
  
  "16"="Continuing with the first stage of the GCM evaluation, which compares performance of individual GCMs,
  now we look more closely at domain-specific results using the Alaska spatial domain as an example.",
  "17"="Compared to the general overview tab, there is only choice of error metric available.
  Other choices are simplifed or not applicable.",
  "18"= "Example sampling distributions of mean annual estimated error are shown.
  The top five GCMs based on sampling distribution means are colored and lower-performing models remain gray.
  These annual distributions are marginal distributions based on similar monthly sampling distributions.",
  "19"="The heat map shows monthly relative performance among the different GCMs. Map cells are colored based on
  mean estimated error from monthly bootstrap sampling distributions like those shown in the mean annual error density plot.",
  "20"="Bootstrapped ranks are available in addition to raw estimated error.",
  "21"="All climate variables are available in the heat map.",
  "22"="Estimated error values corresponding directly to map cell colors can be added as an overlay.
  Alternatively, the overlay can be the probability that a GCM is among the top five performers in a given month or the monthly GCM
  performance ranking.",
  "23"="The second stage in GCM evaluation involves an exploration of composite model building and selection.
  Press 'Next' to continue to the final section. We will continue to use the Alaska spatial domain as an example.",
  
  
  "24"="Results for composite GCMs are domain-specific.
  Here we have results for Alaska for each error statistic and climate variable. Two heat maps are shown.",
  "25"="A common  and sensible hypothesis is that the best-performing (lowest estimated error) composite model
  is the ensemble composed of the best-performing individual GCMs.",
  "26"="The first heat map shows monthly estimated error for composite GCMs of increasing size.
  The ensembles are chosen based on this hypothesis such that the first \"composite\" or \"ensemble\" is actually
  the single GCM with the lowest estimated mean annual error over Alaska for the given error metric and climate variable.
  The size-2 composite is an average of the best two GCMs, the size-3 ensemble an average of the best three,
  and so on until the final ensemble composed of all 21 GCMs is reached.",
  "27"="The second heat map shows monthly mean estimated error derived from random \"ensembles of opportunity\" for each
  size composite model. The error associated with a composite model of size n selected under our hypothesis,
  when viewed in the context of the error distribution estimated across size-n composite models with randomly assigned GCM membership,
  represents a permutation test. The associated p-value of the selected composite can be used to say whether the composite
  tends to outperform a random ensemble.",
  "28"="Notice in the case of Alaska integrated mean annual RMSE that in both heat maps,
  error reduction tends to attenuate after about five models in a composite. This is one of the simple visual indications
  for favoring a restricted ensemble membership of about five GCMs.",
  "29"="A closer look at the permutation test brings us to the final two plots.",
  "30"="The first shows the estimated error distribution among random ensembles of opportunity for a given composite size.
  The estimated error and associated p-value for the specific composite model selected under the hypothesis is shown.",
  "31"="The second plot shows estimated error information for each size ensemble.
  The first plot can be thought of as a cross-section of the second at a given composite size.
  In this second plot we can see the volatility of p-values at different ensemble sizes 
  but that five GCMs remains a good choice for membership size.
  Across all sizes, membership based on the hypothesis that the best performing individuals make up the best performing composite tends
  to yield composites that outperform others, remaining in the lower tail of the error distribution at most composite sizes.",
  "32"="While a specific membership is unlikely to be the best among multiple climate variables and error metrics,
  let alone different spatial domains and time periods, standardizing and integrating can lead to a robust model selection.",
  "33"="It is also important to note that whereas the stated hypothesis is sensible, artificially selecting the composite with the
  absolute lowest estimated error is not. The latter is not based on any meaningful or robust decision criterion and could
  lead to spurious results in terms of model selection. It is advised to stick with the hypothesis and it is reassuring to see
  that it tends to hold true, leading to choice of composite model membership which outperforms other possible ensembles on average.",
  "34"="This completes the tour."
)

tour.pos <- c("right", "left", rep("bottom", 5), rep("left", 3), rep("top", 5), "left", 
              rep("left", 4), rep("top", 3), "left",
              rep("left", 10),
              "right")

tour.element <- c(
  "#tabs",
  "#shiny-tab-overview",
  "#stat + .selectize-control", "#stat + .selectize-control", "#vars + .selectize-control", 
  "#spdom + .selectize-control", "#time + .selectize-control",
  "#shiny-tab-overview",
  "#rankPlot", "#top5Plot",
  "#clrby + .selectize-control", "#fctby + .selectize-control", "#fctby + .selectize-control",
  "#order + .selectize-control", ".js-irs-0",
  "#shiny-tab-overview",
  
  "#shiny-tab-sbAK",
  "#sbAK-stat + .selectize-control", "#sbAK-distPlot", "#sbAK-hmap_gcms",
  "#sbAK-vals + .selectize-control", "#sbAK-var + .selectize-control", "#sbAK-overlay + .selectize-control",
  "#shiny-tab-sbAK",
  
  "#shiny-tab-AK", "#AK-hmap_sel", "#AK-hmap_sel", "#AK-hmap_ran", "#shiny-tab-AK",
  "#shiny-tab-AK", "#AK-perm_one", "#AK-perm_all", "#shiny-tab-AK", "#shiny-tab-AK",
  
  "#help"
)

steps <- reactive({
  data.frame(element=tour.element, intro=tour.text, position=tour.pos)
})



# begin tour on button click
observeEvent(input$help, {
  not.db.overview <- c(paste(domains), paste0("sb", domains))
  not.db.sbAK <- c("overview", paste(domains), paste0("sb", domains[-1]))
  not.db.AK <- c("overview", paste(domains)[-1], paste0("sb", domains))
  tb.AK <- c("Monthly error maps", "Permutation tests")
  tour.options <- list(steps=steps(), 
                       "showBullets"="false", "showProgress"="true", "showStepNumbers"="false")
  tour.events <- list(
    "onchange"=I(paste0(
      stepcb(stepEquals(c(1:16)), c(rmClass(not.db.overview), goClass("overview"))),
      stepcb(stepEquals(c(17:24)), c(rmClass(not.db.sbAK), goClass("sbAK"))),
      stepcb(stepEquals(c(25:34)), c(rmClass(not.db.AK), goClass("AK"))),
      stepcb(stepEquals(c(35)), c(rmClass(not.db.overview), goClass("overview"))),
      stepcb(stepEquals(c(25:29)), c(rmClass(tb.AK[2]), goClass(tb.AK[1]))),
      stepcb(stepEquals(c(30:34)), c(rmClass(tb.AK[1]), goClass(tb.AK[2]))),
      collapse="\n"))
  )
  introjs(session, options=tour.options, events=tour.events)
})
