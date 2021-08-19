#' MetsizeR Launcher
#'
#' Launches the Shiny application for the MetsizeR functionality. The
#' interactive application allows estimation of the sample size required for a
#' metabolomic experiment to achieve a desired statistical power.
#'
#' @importFrom dplyr %>%
#' @import shiny
#'
#' @references G. Nyamundanda, I. C. Gormley, Y. Fan, W. M. Gallagher and L. Brennan, BMC Bioinformatics, 2013, 14, 338.
#'
#' @export

MetSizeR <- function() {
  ui <- shiny::navbarPage(
    "MetSizeR",
    theme = shinythemes::shinytheme("cerulean"),
    shiny::tabPanel(
      "About",
      fluidRow(
        column(width = 2
        ),
        column(width = 8,
               h4("The", tags$b("MetSizeR"), " application was developed for metabolomic scientists to estimate the optimal sample size required for a study to achieve a desired false discovery rate. ", tags$b("MetSizeR"), "can be used with or without pilot data to estimate the sample size required.",
                  align = "center"
               )
        ),
        column(width = 2),
      ),
      tags$br(),
      fluidRow(
        column(width = 2),
        column( width = 8,
                h4(tags$em("If you are using this application, please cite:")),
                h4(
                  tags$ul(
                    tags$li( tags$b("MetSizeR Paper:"),
                             tags$a(href="https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-338",
                                    "\"MetSizeR: Selecting the optimal sample size for metabolomic studies using an analysis based approach\" G. Nyamundanda, I. C. Gormley, Y. Fan, W. M. Gallagher and L. Brennan, BMC Bioinformatics, 2013, 14, 338.",
                                    style = "color:black")
                    ),
                    tags$li(tags$b("R package:"),  tags$a(href="https://CRAN.R-project.org/package=MetSizeR",
                                                          "MetSizeR: A Tool for Estimating Sample Sizes for Metabolomic Experiments (2021).",
                                                          style = "color:black")),
                    tags$li( tags$b("Paper on analysis methods:"),
                             tags$a(href="http://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-11-571",
                                    "\"Probabilistic principal component analysis for metabolomic data\" G. Nyamundanda, L. Brennan and I. C. Gormley, BMC Bioinformatics, 2010, 11, 571.",
                                    style = "color:black")
                    )
                  )
                )
        ),
        column(width = 2),
      ),
      tags$hr(),
      fluidRow(
        column(width = 2),
        column(
          width = 8,
          align = "center",
          checkboxInput(
            "more_info",
            shiny::tags$b("Would you like to read further information about the methodology?", style = "color:#317eac"),
            value = FALSE),
          conditionalPanel(
            condition = 'input.more_info == true',
            h4("The", tags$b("MetSizeR"), " algorithm can be broken into two stages; pilot data simulation, and sample size estimation.",
               align = "center"),
            h4(tags$b("1. Pilot data simulation:"), "The algorithm works by using simulated data to take the place of pilot study data in sample size estimation. The simulated data are generated based on the planned method of analysis, thus providing an analysis informed approach. At present the algorithm is designed to operate for two different analysis methods; probabilistic principal components analysis (PPCA) and probabilistic principal components and covariates analysis (PPCCA) (Nyamundanda et al., 2010).",
               align = "center"),
            h4(tags$b("2. Sample size estimation:"),"Following the simulation of pilot data, the group labels of the data are randomly permuted and the test statistics of each spectral bin or metabolite are calculated. A specified proportion of the bins or metabolites are designated as truly significant and their test statistics updated to reflect this. The FDR is then calculated. This process is repeated several times and for several sample sizes, with a simple linear regression procedure then implemented on the resulting FDR values in order to determine the sample size at which the desired FDR is achieved. An in-depth explanation of the method can be found in Nyamundanda et al. (2013), referenced above.",
               align = "center"),
            tags$hr()
          )
        ),
        column(width = 2)
      )
    ),
    shiny::tabPanel(
      "Sample Size Estimation",
      shiny::fluidPage(shiny::sidebarLayout(
        shiny::sidebarPanel(
          h3(tags$b("Estimate optimal sample size")),
          shiny::radioButtons("targ", h5(tags$b("Type of experiment"), tags$em("Specify type of intended analysis.")),
                              choices = list("Untargeted", "Targeted"),
                              selected = "Untargeted"
          ),
          shiny::checkboxInput("pilot", shiny::tags$b("Are experimental pilot data available?", style = "color:#317eac"),
                               value = FALSE
          ),
          shiny::conditionalPanel(
            condition = "input.pilot == true",
            h5(tags$em(
              "Please upload data in .csv format. Covariate columns should be included first,
                with spectral data following in subsequent columns. If categorical covariates
                are included, these should be included before any numerical covariates, if
                present. The method assumes no missing data. Maximum filesize is 5MB."
            )),
            shiny::checkboxInput("header", tags$b("Does data contain a header?", style = "color:#317eac"),
                                 value = FALSE
            ),
            shiny::fileInput("upload", h5(tags$b("Upload pilot data as .csv file")), accept = ".csv")
          ),
          h4(tags$b("Please choose specifications")),
          shiny::conditionalPanel(
            condition = "input.pilot == false && input.targ == 'Untargeted'",
            shiny::numericInput("spect_bins", h5(tags$b("Spectral bins"), tags$em("Specify expected number of spectral bins.")),
                                min = 50,
                                max = 3000,
                                value = 200
            )
          ),
          shiny::conditionalPanel(
            condition = "input.pilot == false && input.targ == 'Targeted'",
            shiny::numericInput("spect_bins", h5(tags$b("Metabolites"), tags$em("Specify expected number of metabolites.")),
                                min = 10,
                                max = 1000,
                                value = 200
            )
          ),
          shiny::conditionalPanel(
            condition = "input.targ == 'Untargeted'",
            shiny::numericInput("prop_signif", h5(tags$b("Proportion of significant bins"), tags$em("Specify proportion of bins expected to be significant.")),
                                value = 0.2,
                                min = 0,
                                max = 1,
                                step = 0.05
            )
          ),
          shiny::conditionalPanel(
            condition = "input.targ == 'Targeted'",
            shiny::numericInput("prop_signif", h5(tags$b("Proportion of significant metabolites"), tags$em("Specify proportion of metabolites expected to be significant.")),
                                value = 0.2,
                                min = 0,
                                max = 1,
                                step = 0.05
            )
          ),
          shiny::radioButtons("model", h5(tags$b("Model"), tags$em("Specify model to be used in analysis.")),
                              choices = list("PPCA", "PPCCA"),
                              selected = "PPCA"
          ),
          shiny::conditionalPanel(
            condition = "input.model == 'PPCCA'",
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # need to restrict these to integers only!!!!!!!!!
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            shiny::numericInput("num_numeric_covs", h5(tags$b("Specify number of numeric covariates")),
                                value = 0,
                                min = 0,
                                max = 5,
                                step = 1
            ),
            shiny::numericInput("num_cat_covs", h5(tags$b("Specify number of categorical covariates")),
                                value = 0,
                                min = 0,
                                max = 5,
                                step = 1
            ),
            shiny::conditionalPanel(
              condition = "input.num_cat_covs > 0 && input.pilot == false",
              shiny::numericInput("num_levels",
                                  h5(tags$b("Specify total number of levels of all categorical covariates")),
                                  value = 2,
                                  min = 2,
                                  max = 50,
                                  step = 1
              )
            )
          ),
          shiny::numericInput("FDR", h5(tags$b("Target FDR"), tags$em("Specify desired FDR.")),
                              value = 0.05,
                              min = 0.01,
                              max = 1,
                              step = 0.01
          ),
          h4(tags$b("Sample size per group")),
          h5(tags$em("Specify minimum sample size to consider for each sample group. Note: this will fix the ratio of samples per group for each sample size tested.")),
          shiny::numericInput("n1", h5(tags$b("Group 1: n1")),
                              value = 4,
                              min = 3
          ),
          shiny::numericInput("n2", h5(tags$b("Group 2: n2")),
                              value = 4,
                              min = 3
          ),
          shiny::actionButton("go", "Estimate Optimal Sample Size!", class = "btn btn-lg btn-primary", width = "100%")
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(1),
            shiny::column(
              10,
              shiny::plotOutput("samp_size_plot")
            ),
            shiny::column(1)
          ),
          tags$hr(),
          conditionalPanel(
            condition = "input.go > 0",
            shiny::fluidRow(
              shiny::column(1),
              shiny::column(10,
                            align = "center",
                            h3(shiny::textOutput("results")),
                            tags$em(shiny::textOutput("samp_statement")),
                            h4(shiny::textOutput("with_breakdown")),
                            tags$em(shiny::textOutput("g1_statement")),
                            tags$em(shiny::textOutput("g2_statement"))
              ),
              shiny::column(1)
            ),
            tags$br(),
            tags$hr(),
            shiny::fluidRow(
              shiny::column(1),
              shiny::column(10,
                            align = "center",
                            shiny::downloadButton(
                              "plot_download",
                              "Download Plot",
                              class = "btn btn-primary"
                            ),
                            shiny::checkboxInput(
                              "show_df",
                              tags$b("Show values from plot?",
                                     style = "color:#317eac"),
                              value = FALSE
                            ),
                            shiny::tableOutput("test_tab"),
                            shiny::fluidRow(
                              shiny::downloadButton(
                                "df_download",
                                "Download Plot Data as .csv",
                                class = "btn btn-primary"
                              ))
              ),
              shiny::column(1)
            )
          )
        )
      ))
    ),
    shiny::tabPanel(
      "Vary Proportion of Significant Spectral Bins",
      shiny::fluidPage(shiny::sidebarLayout(
        shiny::sidebarPanel(
          h3(tags$b("Vary proportion of significant spectral bins or metabolites")),
          h5(tags$em("If the expected proportion of significant bins or metabolites in the experiment is unknown, this tab can be used to see the effect of changing this proportion on the sample size needed to achieve a specified FDR.")),
          shiny::radioButtons("targ_prop", h5(tags$b("Type of experiment"), tags$em("Specify type of intended analysis.")),
                              choices = list("Untargeted", "Targeted"),
                              selected = "Untargeted"
          ),
          h4(tags$b("Choose proportions to test")),
          h5(tags$em("Choose up to four proportions of significant bins or metabolites with which to run the method. If you wish to test less than four proportions, enter a value of zero in any remaining options.")),
          shiny::numericInput("prop1", label = NULL,
                              value = 0,
                              min = 0,
                              max = 1,
                              step = 0.05
          ),
          shiny::numericInput("prop2", label = NULL,
                              value = 0,
                              min = 0,
                              max = 1,
                              step = 0.05
          ),
          shiny::numericInput("prop3", label = NULL,
                              value = 0,
                              min = 0,
                              max = 1,
                              step = 0.05
          ),
          shiny::numericInput("prop4", label = NULL,
                              value = 0,
                              min = 0,
                              max = 1,
                              step = 0.05
          ),
          h4(tags$b("Please choose specifications")),
          shiny::conditionalPanel(
            condition = "input.targ_prop == 'Untargeted'",
            shiny::numericInput("spect_bins_prop", h5(tags$b("Spectral bins"), tags$em("Specify expected number of spectral bins.")),
                                min = 50,
                                max = 3000,
                                value = 200
            )
          ),
          shiny::conditionalPanel(
            condition = "input.targ_prop == 'Targeted'",
            shiny::numericInput("spect_bins_prop", h5(tags$b("Metabolites"), tags$em("Specify expected number of metabolites.")),
                                min = 10,
                                max = 1000,
                                value = 200
            )
          ),
          shiny::radioButtons("model_prop", h5(tags$b("Model"), tags$em("Specify model to be used in analysis.")),
                              choices = list("PPCA", "PPCCA"),
                              selected = "PPCA"
          ),
          shiny::conditionalPanel(
            condition = "input.model_prop == 'PPCCA'",
            shiny::numericInput("num_numeric_covs_prop", h5(tags$b("Specify number of numeric covariates")),
                                value = 0,
                                min = 0,
                                max = 5,
                                step = 1
            ),
            shiny::numericInput("num_cat_covs_prop", h5(tags$b("Specify number of categorical covariates")),
                                value = 0,
                                min = 0,
                                max = 5,
                                step = 1
            ),
            shiny::conditionalPanel(
              condition = "input.num_cat_covs_prop > 0",
              shiny::numericInput("num_levels_prop",
                                  h5(tags$b("Specify total number of levels of all categorical covariates")),
                                  value = 2,
                                  min = 2,
                                  max = 50,
                                  step = 1
              )
            )
          ),
          shiny::numericInput("FDR_prop", h5(tags$b("Target FDR"), tags$em("Specify desired FDR.")),
                              value = 0.05,
                              min = 0.01,
                              max = 1,
                              step = 0.01
          ),
          shiny::actionButton("go_prop", "Run MetsizeR for Varied Proportions!", class = "btn btn-lg btn-primary", width = "100%")
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(1),
            shiny::column(
              5,
              align = "center",
              h4(shiny::textOutput("prop1_res")),
              shiny::plotOutput("prop1_plot"),
              conditionalPanel(
                condition = "input.go_prop > 0",
                shiny::downloadButton(
                  "plot_download1",
                  "Download Plot",
                  class = "btn btn-primary"
                )
              )
            ),
            shiny::column(
              5,
              align = "center",
              h4(shiny::textOutput("prop2_res")),
              shiny::plotOutput("prop2_plot"),
              tags$br(),
              conditionalPanel(
                condition = "input.go_prop > 0",
                shiny::downloadButton(
                  "plot_download2",
                  "Download Plot",
                  class = "btn btn-primary"
                )
              )
            ),
            shiny::column(1)
          ),
          tags$br(),
          tags$hr(),
          shiny::fluidRow(
            shiny::column(1),
            shiny::column(
              5,
              align = "center",
              h4(shiny::textOutput("prop3_res")),
              shiny::plotOutput("prop3_plot"),
              conditionalPanel(
                condition = "input.go_prop > 0",
                shiny::downloadButton(
                  "plot_download3",
                  "Download Plot",
                  class = "btn btn-primary"
                )
              )
            ),
            shiny::column(
              5,
              align = "center",
              h4(shiny::textOutput("prop4_res")),
              shiny::plotOutput("prop4_plot"),
              conditionalPanel(
                condition = "input.go_prop > 0",
                shiny::downloadButton(
                  "plot_download4",
                  "Download Plot",
                  class = "btn btn-primary"
                )
              )
            ),
            shiny::column(1)
          )
        )
      )
      )
    )
  )



  server <- function(input, output, session) {

    # uploaded user data
    # in tibble format
    user_data <- shiny::reactive({
      # make sure data was uploaded before proceeding
      shiny::req(input$upload)

      # get file extension for uploaded file
      check_ext <- tools::file_ext(input$upload$name)

      # validate if extension is .csv as required
      switch(check_ext,
             csv = vroom::vroom(input$upload$datapath, delim = ",", col_names = input$header),
             shiny::validate("File type invalid: Please upload a .csv file.")
      ) %>%
        dplyr::mutate_at(0:input$num_cat_covs, as.factor) # mutate categorical covs to factors
    })

    cats <- shiny::reactive({
      if (input$pilot == TRUE & input$num_cat_covs != 0) {
        stats::model.matrix(~., data = user_data()[, 1:input$num_cat_covs])[, -1]
      }
    })

    covs <- shiny::reactive({
      if (input$pilot == TRUE & input$num_cat_covs != 0 & input$num_numeric_covs != 0) {
        as.matrix(cbind(cats(), user_data()[, (input$num_cat_covs + 1):(input$num_cat_covs + input$num_numeric_covs)]))
      } else if (input$pilot == TRUE & input$num_cat_covs == 0 & input$num_numeric_covs != 0) {
        as.matrix(user_data()[, (input$num_cat_covs + 1):(input$num_cat_covs + input$num_numeric_covs)])
      } else if (input$pilot == TRUE & input$num_cat_covs != 0 & input$num_numeric_covs == 0) {
        cats()
      }
    })

    pilot_spect <- shiny::reactive({
      if (input$model == "PPCCA") {
        as.data.frame(user_data()[, -(1:(input$num_cat_covs + input$num_numeric_covs))])
      } else {
        as.data.frame(user_data())
      }
    })

    plot_obj <- shiny::reactiveValues()

    metsizer <- shiny::eventReactive(input$go, {

      # notification of status
      # to show that the algorithm is calculating
      id <- shiny::showNotification("Estimating sample size. This may take several minutes for larger data...", duration = NULL, closeButton = FALSE)
      on.exit(shiny::removeNotification(id), add = TRUE)

      if (input$pilot) {
        # p = input$spect_bins is ok for pilot data as this is overwritten by ncol(pilot) in metsize function
        if (input$model == "PPCA") {
          metsize(
            pilot = pilot_spect(), n1 = input$n1, n2 = input$n2,
            p = input$spect_bins, prop = input$prop_signif,
            covars = NULL, ncovar = 0, model = input$model,
            plot.prop = FALSE, target.fdr = input$FDR,
            Targeted = FALSE
          )
        } else {
          metsize(
            pilot = pilot_spect(), n1 = input$n1, n2 = input$n2,
            p = input$spect_bins, prop = input$prop_signif,
            covars = covs(), ncovar = input$num_numeric_covs + input$num_levels - input$num_cat_covs,
            model = input$model,
            plot.prop = FALSE, target.fdr = input$FDR,
            Targeted = FALSE
          )
        }
      } else {
        if (input$model == "PPCA") {
          metsize(
            pilot = NULL, n1 = input$n1, n2 = input$n2,
            p = input$spect_bins, prop = input$prop_signif,
            covars = NULL, ncovar = 0, model = input$model,
            plot.prop = FALSE, target.fdr = input$FDR,
            Targeted = FALSE
          )
        } else {
          metsize(
            pilot = NULL, n1 = input$n1, n2 = input$n2,
            p = input$spect_bins, prop = input$prop_signif,
            covars = NULL, ncovar = input$num_numeric_covs + input$num_levels - input$num_cat_covs,
            model = input$model,
            plot.prop = FALSE, target.fdr = input$FDR,
            Targeted = FALSE
          )
        }
      }
    })

    plot_data <- shiny::reactive(data.frame(metsizer()$results_sim))

    output$test_tab <- shiny::renderTable({
      if (input$show_df) {
        plot_data()
      }
    })

    output$df_download <- shiny::downloadHandler(
      filename = function() {
        paste0("metsizer_table.csv")
      },
      content = function(file) {
        utils::write.csv(plot_data(), file, row.names = FALSE)
      }
    )

    output$results <- shiny::renderText("Results")

    output$samp_statement <- shiny::renderText(paste0(
      "Estimated optimal sample size: ",
      metsizer()$nhat[1]
    ))

    output$with_breakdown <- shiny::renderText("Per-Group Breakdown")

    output$g1_statement <- shiny::renderText(paste0(
      "Group 1 sample size: ",
      metsizer()$nhat[2]
    ))

    output$g2_statement <- shiny::renderText(paste0(
      "Group 2 sample size: ",
      metsizer()$nhat[3]
    ))

    plot_title <- shiny::reactive({
      variable_title <- ifelse(input$targ == "Untargeted", "Bins", "Metabolites")
      pilot_numbers <- ifelse(input$pilot, ncol(pilot_spect()), input$spect_bins)
      paste("Sample Size Estimation for", input$prop_signif, "of", pilot_numbers, variable_title, "Significant")
    })

    plot_obj$fdr_plot <- eventReactive(input$go, {
      plot_fun(plot_data(), input$FDR, metsizer()$nhat) +
        ggplot2::labs(title = plot_title())
    })

    output$samp_size_plot <- shiny::renderPlot({
      plot_obj$fdr_plot()
    })

    output$plot_download <- shiny::downloadHandler(
      filename = function() {
        paste0("metsizer_plot.png")
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot_obj$fdr_plot(), device = "png", width = 10,
                        height = 8,
                        units = "in"
        )
      }
    )

    in_props <- shiny::eventReactive(input$go_prop, {
      c(input$prop1, input$prop2, input$prop3, input$prop4)
    })

    props <- shiny::eventReactive(input$go_prop, {
      as.vector(stats::na.omit(in_props())[in_props() > 0])
    })

    prop_data <- shiny::eventReactive(input$go_prop, {
      # notification of status
      # to show that the algorithm is calculating
      id <- shiny::showNotification("Estimating sample sizes. This may take several minutes for larger data...", duration = NULL, closeButton = FALSE)
      on.exit(shiny::removeNotification(id), add = TRUE)

      metsize_props(props() , input$spect_bins_prop, input$FDR_prop, input$model_prop, input$num_numeric_covs + input$num_levels - input$num_cat_covs)

    })

    # indexes for plot generation

    ind_2 <- eventReactive(input$go_prop, {
      ifelse(input$prop1 == 0, 1, 2)
    })

    ind_3 <- eventReactive(input$go_prop, {
      if (sum(c(input$prop1, input$prop2) == 0) == 0){
        3
      } else if (sum(c(input$prop1, input$prop2) == 0) == 1){
        2
      } else {
        1
      }
    })

    ind_4 <- eventReactive(input$go_prop, {
      if (sum(c(input$prop1, input$prop2, input$prop3) == 0) == 0){
        4
      } else if (sum(c(input$prop1, input$prop2,input$prop3) == 0) == 1){
        3
      } else if (sum(c(input$prop1, input$prop2,input$prop3) == 0) == 2){
        2
      } else {
        1
      }
    })

    variable_title_prop <- shiny::reactive({
      ifelse(input$targ_prop == "Untargeted", "Bins", "Metabolites")
    })

    # make plots render only when action button pressed

    plot_1 <-  shiny::eventReactive(input$go_prop, {
      if (input$prop1 != 0) {
        plot_fun(data.frame(prop_data()[[1]]$results_sim), input$FDR_prop, prop_data()[[1]]$nhat) +
          ggplot2::labs(title = paste("Sample Size Estimation for", input$prop1, "of", input$spect_bins_prop, variable_title_prop(), "Significant"))
      } else {
        NULL
      }
    })

    plot_2 <- shiny::eventReactive(input$go_prop, {
      if (input$prop2 != 0) {
        plot_fun(data.frame(prop_data()[[ind_2()]]$results_sim), input$FDR_prop, prop_data()[[ind_2()]]$nhat) +
          ggplot2::labs(title = paste("Sample Size Estimation for", input$prop2, "of", input$spect_bins_prop, variable_title_prop(), "Significant"))
      } else {
        NULL
      }
    })

    plot_3 <- shiny::eventReactive(input$go_prop, {
      if (input$prop3 != 0) {
        plot_fun(data.frame(prop_data()[[ind_3()]]$results_sim), input$FDR_prop, prop_data()[[ind_3()]]$nhat) +
          ggplot2::labs(title = paste("Sample Size Estimation for", input$prop3, "of", input$spect_bins_prop, variable_title_prop(), "Significant"))
      } else {
        NULL
      }
    })

    plot_4 <- shiny::eventReactive(input$go_prop, {
      if (input$prop4 != 0) {
        plot_fun(data.frame(prop_data()[[ind_4()]]$results_sim), input$FDR_prop, prop_data()[[ind_4()]]$nhat) +
          ggplot2::labs(title = paste("Sample Size Estimation for", input$prop4, "of", input$spect_bins_prop, variable_title_prop(), "Significant"))
      } else {
        NULL
      }
    })

    # plotting

    output$prop1_plot <- shiny::renderPlot({
      plot_1()
    })

    output$prop2_plot <- shiny::renderPlot({
      plot_2()
    })

    output$prop3_plot <- shiny::renderPlot({
      plot_3()

    })

    output$prop4_plot <- shiny::renderPlot({
      plot_4()

    })

    text_1 <- shiny::eventReactive(input$go_prop, {
      if (input$prop1 != 0) {
        paste0(
          "For ", input$prop1, " of bins expected to be significant and an FDR of ", input$FDR_prop, ", the estimated sample size is: ", prop_data()[[1]]$nhat[1]
        )
      }
    })

    text_2 <- shiny::eventReactive(input$go_prop, {
      if (input$prop2 != 0) {
        paste0(
          "For ", input$prop2, " of bins expected to be significant and an FDR of ", input$FDR_prop, ", the estimated sample size is: ", prop_data()[[ind_2()]]$nhat[1]
        )
      }
    })

    text_3 <- shiny::eventReactive(input$go_prop, {
      if (input$prop3 != 0) {
        paste0(
          "For ", input$prop3, " of bins expected to be significant and an FDR of ", input$FDR_prop, ", the estimated sample size is: ", prop_data()[[ind_3()]]$nhat[1]
        )
      }
    })

    text_4 <- shiny::eventReactive(input$go_prop, {
      if (input$prop4 != 0) {
        paste0(
          "For ", input$prop4, " of bins expected to be significant and an FDR of ", input$FDR_prop, ", the estimated sample size is: ", prop_data()[[ind_4()]]$nhat[1]
        )
      }
    })

    output$prop1_res <- shiny::renderText({
      text_1()
    })

    output$prop2_res <- shiny::renderText({
      text_2()
    })

    output$prop3_res <- shiny::renderText({
      text_3()
    })

    output$prop4_res <- shiny::renderText({
      text_4()
    })

    output$plot_download1 <- shiny::downloadHandler(
      filename = function() {
        paste("metsizer_prop_plot_", in_props()[1] , ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot_1(),
                        device = "png",
                        width = 10,
                        height = 8,
                        units = "in"
        )
      }
    )

    output$plot_download2 <- shiny::downloadHandler(
      filename = function() {
        paste("metsizer_prop_plot_", in_props()[2] , ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot_2(),
                        device = "png",
                        width = 10,
                        height = 8,
                        units = "in"
        )
      }
    )

    output$plot_download3 <- shiny::downloadHandler(
      filename = function() {
        paste("metsizer_prop_plot_", in_props()[3] , ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot_3(),
                        device = "png",
                        width = 10,
                        height = 8,
                        units = "in"
        )
      }
    )

    output$plot_download4 <- shiny::downloadHandler(
      filename = function() {
        paste("metsizer_prop_plot_", in_props()[4] , ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot_4(),
                        device = "png",
                        width = 10,
                        height = 8,
                        units = "in"
        )
      }
    )


  }

  shiny::shinyApp(ui, server)
} # MetsizeR
