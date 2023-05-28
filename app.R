# Function to rename colname
rename_colname <- function(colname) {
  colname |> 
    stringr::str_replace_all("_", " ") |> 
    stringr::str_to_title()
}

# Function to compute information value
compute_iv <- function(data, breaks, outcome, predictor) {

  df <- data |> 
    dplyr::mutate(
      predictor := cut(
        data[[predictor]],
        breaks = breaks
      )
    )

  KAscore::iv(
    data = df, 
    outcome = outcome, 
    predictors = predictor, 
    verbose = FALSE, 
    labels = TRUE
  )

}

# Function to create density chart
plot_density <- function(data, continuous_variable, outcome_variable, breaks) {
  data |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[continuous_variable]],
        color = .data[[outcome_variable]]
      )
    ) + 
    ggplot2::geom_density(linewidth = 1) + 
    ggplot2::scale_x_continuous(labels = scales::label_dollar()) + 
    ggplot2::scale_y_continuous(labels = scales::label_comma()) + 
    ggplot2::labs(
      x = continuous_variable,
      y = "Density",
    ) + 
    ggplot2::theme_minimal(base_size = 22) + 
    # Add bin breaks
    ggplot2::geom_vline(
      xintercept = breaks, 
      linetype = "dashed"
    )
}

ui <- bs4Dash::dashboardPage(
  header = bs4Dash::dashboardHeader(
    title = "{KA Score} Demo"
  ),
  sidebar = bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(
      bs4Dash::menuItem(
        text = "Data",
        tabName = "data_page"
      ),
      bs4Dash::menuItem(
        text = "IV Simulator",
        tabName = "simulator_page"
      )
    )
  ),
  body = bs4Dash::dashboardBody(
    shinyjs::useShinyjs(),
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "data_page",
        bs4Dash::box(
          title = "Select Data Source",
          width = 12,
          height = "100px",
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = "data_source",
                label = NULL,
                choices = c("Demo data", "Upload file")
              )
            ),
            shiny::column(
              width = 6,
              shiny::conditionalPanel(
                condition = "input.data_source === 'Upload file'",
                shiny::fileInput(
                  inputId = "user_data",
                  label = NULL
                )
              )
            )
          )
        ),
        bs4Dash::box(
          title = "Preview Selected Data",
          width = 12,
          DT::DTOutput(outputId = "preview_data")
        )
      ),
      bs4Dash::tabItem(
        tabName = "simulator_page",
        bs4Dash::box(
          title = "Set Options",
          width = 12,
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = "continuous_variable",
                label = "Select Continuous Variable",
                choices = NULL
              )
            ),
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = "outcome_variable",
                label = "Select Outcome Variable",
                choices = NULL
              )
            ),
            shiny::column(
              width = 3,
              shiny::numericInput(
                inputId = "n_bins",
                label = "# of Bins",
                value = 5,
                min = 2,
                step = 1
              )
            ),
            shiny::column(
              width = 3,
              shiny::numericInput(
                inputId = "n_decimals",
                label = "# of Decimals",
                value = 0,
                min = 0,
                step = 1
              )
            )
          ),
          shiny::actionButton(inputId = "set", label = "Set", width = "100%")
        ),
        shiny::fluidRow(
          shiny::column(
            width = 3,
            bs4Dash::box(
              title = "Cutpoints",
              width = 12,
              shiny::uiOutput(outputId = "cutpoints_ui"),
              shiny::actionButton(inputId = "apply", label = "Apply", width = "100%")
            )
          ),
          shiny::column(
            width = 9,
            bs4Dash::box(
              title = "Distribution",
              width = 12,
              shiny::plotOutput(outputId = "density_plot")
            ),
            bs4Dash::bs4InfoBoxOutput(outputId = "information_value_box", width = 12)
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    if (input$data_source == "Demo data") {
      KAscore::loans |> 
        dplyr::mutate(
          default_status = factor(
            default_status,
            levels = c("good", "bad")
          )
        ) |> 
        dplyr::rename_with(.fn = rename_colname)
    } else {
      shiny::req(input$user_data)
      file_extension <- tools::file_ext(input$user_data$name)
      switch(
        file_extension,
        csv = vroom::vroom(input$user_data$datapath, delim = ","),
        tsv = vroom::vroom(input$user_data$datapath, delim = "\t"),
        shiny::validate("Invalid file; Please upload a .csv or .tsv file")
      )
    }
  })

  output$preview_data <- DT::renderDT({
    data() |> 
      DT::datatable(
        rownames = FALSE,
        selection = "none",
        options = list(dom = "tip",
                       pageLength = 5)
      )
  })
  
  shiny::observeEvent(data(), {
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "continuous_variable",
      choices = colnames(data())
    )
    
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "outcome_variable",
      choices = colnames(data())
    )
  })
  
  # This renderUI generates the set of numericInputs that we will use to define
  # the breaks that will be passed to compute_iv()
  # This implementation creates (n_bins - 1) numericInputs because that information
  # is enough to set the breaks
  # The disabled textInputs are just to improve UX
  output$cutpoints_ui <- shiny::renderUI({
    shiny::validate(
      shiny::need(input$set, "Set Options")
    )

    shiny::tagList(
      shinyjs::disabled(
        shiny::textInput(
          inputId = "lower_limit",
          label = NULL,
          value = "-Inf"
        )
      ),
      
      lapply(1:(shiny::isolate(input$n_bins) - 1), function(i) {
        shiny::numericInput(
          inputId = paste0("cutpoint", i),
          label = NULL,
          value = NULL
        )
      }),
      
      shinyjs::disabled(
        shiny::textInput(
          inputId = "upper_limit",
          label = NULL,
          value = "Inf"
        )
      )
    )
    
  })
  
  # Update cutpoints values
  shiny::observeEvent(input$set, {
    # To compute cutpoints I use KAscore::bin_quantile() and then extract the 
    # endpoint of each interval by processing the character vector.
    # The extraction is omitted for the last interval (which always ends in Inf)
    cutpoints <- KAscore::bin_quantile(
      x = data()[[input$continuous_variable]], 
      n_bins = input$n_bins,
      decimals = input$n_decimals
    ) |>
      levels() |> 
      head(-1) |> # Remove last interval
      stringr::str_extract(pattern = ",.*") |> 
      stringr::str_sub(start = 2, end = -2) |> 
      as.numeric()
    
    # Update numericInputs with values from bin_quantile() by default
    lapply(seq_along(cutpoints), function(i) {
      shiny::updateNumericInput(
        session = session,
        inputId = paste0("cutpoint", i),
        value = cutpoints[[i]]
      )
    })
  })
  
  density_plot <- shiny::eventReactive(input$apply, {
    cutpoints <- sapply(1:(input$n_bins - 1), function(i) {
      input[[paste0("cutpoint", i)]]
    })

    plot_density(
      data(),
      input$continuous_variable,
      input$outcome_variable,
      c(-Inf, cutpoints, Inf)
    )
  })
  
  output$density_plot <- shiny::renderPlot({
    density_plot()
  })
  
  # This object will store the outcome of the information value computation
  outcome <- shiny::reactiveVal(NULL)
  
  # Compute information value
  shiny::observeEvent(input$apply, {
    # TODO: Extract to function
    cutpoints <- sapply(1:(input$n_bins - 1), function(i) {
      input[[paste0("cutpoint", i)]]
    })

    outcome(
      compute_iv(
        data = data(),
        breaks = c(-Inf, cutpoints, Inf),
        outcome = input$outcome_variable,
        predictor = input$continuous_variable
      )
    )
  })
  
  # Show the resulting value
  output$information_value_box <- bs4Dash::renderbs4InfoBox({
    shiny::req(outcome())
    bs4Dash::bs4InfoBox(
      title = "Information Value",
      value = outcome()$label,
      subtitle = outcome()$iv |> round(digits = 4),
      color = NULL,
      width = 12
    )
  })
}

shiny::shinyApp(ui, server)
