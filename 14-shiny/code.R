## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
# GB add - need to load the 'png' package to use 'readPNG'
library(png)
library(knitr)


## ----eval=knitr::is_html_output()---------------------------------------------------------------------------------------------------------
#| child: _building-shiny-app.qmd

## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
# parameters <- list(
#   # ~ Model settings ------------------------------------------------------
#   settings = list(
#     state_names = state_names,        #state names
#     Dr_C = input$Dr_C,                #Discount rate: costs
#     Dr_Ly = input$Dr_Ly,              #Discount rate: life years
#     THorizon = input$THorizon,        #Time horizon
#     state_BL_dist = c(1, 0, 0, 0),    #starting distribution of states
#     combo_dur = 2                     #number of states with combination therapy
#   ),
# 
#   # ~ Transition probability matrices (TPMs) ------------------------------
#   transitions = list(
#     TPM_mono = TPM$mono,   #Control arm transition probability matrix
#     TPM_combo = TPM$combo  #Intervention arm transition probability matrix
#     ),
# 
#   # ~ Costs ---------------------------------------------------------------
#   Costs = list(
#     DM_CC_costs = data.frame(
#       unlist(DM_CC_costs$state_A),
#       unlist(DM_CC_costs$state_B),
#       unlist(DM_CC_costs$state_C),
#       unlist(DM_CC_costs$state_D)
#     ),
#     drug_costs  = drug_costs,
#     state_costs = state_costs
#   )
# )
# 


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
# dashboardBody(
#   tabItems(
#     tabItem(
#       # ~~ Model settings --------------------------------------------------------
#       tabName = "tab_Settings",
#       tags$h2("Model settings"),
#       fluidRow(          # `R` Shiny normally renders vertically. fluidrow allows horizontal layering
#         column(          # Columns within fluidrow. the width of the whole page is always 12
#           4,
#           box(
#             title = "General settings",
#             width = 12,                  # This '12' refers to the whole space within the column
#             solidHeader = TRUE,          # Whether you want a solid colour border
#             collapsible = TRUE,          # Collapsible box
#             status = "info",             # This is the reference to the colour of the box
#             numericInput(
#               inputId = "Dr_C",
#               label = "Drug costs discount",
#               value = 0.06,
#               max = 1,
#               min = 0,
#               step = 0.01
#             ),
#             numericInput(
#               inputId = "Dr_Ly",
#               label = "Drug LY discount",
#               value = 0,
#               max = 1,
#               min = 0,
#               step = 0.01
#             ),
#             numericInput(
#               inputId = "THorizon",
#               label = "Time horizon (years)",
#               value = 20,
#               max = 100,
#               min = 10,
#               step = 1
#             )
#           )
#         ),
#         column(
#           4,
#           box(
#             title = "Other settings",
#             width = 12,
#             solidHeader = TRUE,
#             collapsible = FALSE,
#             status = "primary",
#             numericInput(
#               inputId = "combo_HR",
#               label = "Hazard Ratio (HR) for Combo",
#               value = 0.509,
#               max = 2,
#               min = 0.001,
#               step = 0.001
#             )
#           )
#         ),
# 
#         ## Code continues


## ----shiny-model-struc, echo = FALSE, fig.align="center", fig.cap = "`shiny` application structure"---------------------------------------
# All defaults
img1_path <- "figs/Structure.png"
img1 <- readPNG(img1_path, native = TRUE, info = TRUE)
include_graphics(img1_path)


## ----shiny-model-obsreac, echo = FALSE, fig.align="center", fig.cap = "`observe` and `reactive`"------------------------------------------
# All defaults
img2_path <- "figs/observeandReactive.png"
img2 <- readPNG(img2_path, native = TRUE, info = TRUE)
include_graphics(img2_path)


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
#    drug_costs <- reactive({
#     list(
#       AZT_mono = rep(Drug_unit_costs$AZT_mono,input$THorizon),
#       lamivudine = c(rep(Drug_unit_costs$lamivudine,2),rep(0,input$THorizon - 2)),
#       combo = rep(Drug_unit_costs$AZT_mono,input$THorizon) +                                                            c(rep(Drug_unit_costs$lamivudine,2),rep(0,input$THorizon - 2))
#     )
#   })
# 
# 


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
# parameters <- reactive({
#     list(
#       # ~~ Model settings --------------------------------------------------------
#       #These are laid out as an embedded list to show off what you can do with a list
#       settings = list(
#         #state names
#         state_names = state_names,
#         #Discount rate: costs
#         Dr_C = input$Dr_C,
#         #Discount rate: life years
#         Dr_Ly = input$Dr_Ly,
#         #Time horizon
#         THorizon = input$THorizon,
#         #starting distribution of states
#         state_BL_dist = c(1, 0, 0, 0)
#       ),
# # ~ Transition probability matrices (TPMs) ----------------
# …, Transition probability matrices not presented for this illustrative example
# # ~ Costs --------------------------
#       Costs = list(
#         DM_CC_costs = data.frame(
#           unlist(DM_CC_costs$state_A),
#           unlist(DM_CC_costs$state_B),
#           unlist(DM_CC_costs$state_C),
#           unlist(DM_CC_costs$state_D)
#         ),
#         drug_costs  = drug_costs(),
#         state_costs = state_costs()
#       ))
#   })


## ----shiny-model-strtup, echo = FALSE, fig.align="center", fig.cap = "Model start-up object availability"---------------------------------
# All defaults
img1_path <- "figs/StartUpAvail.PNG"
img1 <- readPNG(img1_path, native = TRUE, info = TRUE)
include_graphics(img1_path)


## ----shiny-model-reacflow, echo = FALSE, fig.align="center", fig.cap = "Example reactivity flow diagram"----------------------------------
# All defaults
img1_path <- "figs/Reacflow.PNG"
img1 <- readPNG(img1_path, native = TRUE, info = TRUE)
include_graphics(img1_path)


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
# dashboardBody(
#     tabItems( #This included the list of all the tabs
#       tabItem( #This is where a single tab is defined (model settings)
#         # ~~ Model settings --------------------------------------------------------
#         tabName = "tab_Settings", # Tab ID
#         tags$h2("Model settings"),
#         fluidRow(    # fluidrow allows horizontal layering
# …, General settings box not presented for this illustrative example
#           column(
#             4,
#             box(
#               title = "Other settings",
#               width = 12,
#               solidHeader = TRUE,
#               collapsible = FALSE,
#               status = "primary",
#               numericInput(
#                 inputId = "combo_HR",
#                 label = "Hazard Ratio (HR) for Combo",
#                 value = 0.509,
#                 max = 2,
#                 min = 0.001,
#                 step = 0.001
#               )
#             ),
#             column(        # Not all outputs need to be in boxes
#               8,
#               offset = 3,
#               actionButton(inputId = "Run_model", label = "Run the model", style = "color: #000000; background-color: #26b3bd; border-color: #2e6da4")
#             )
#           ),
# … Test outputs not presented for this illustrative example
#       ))
# 


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
# patient_flow$disc$cost <- eventReactive(input$Run_model, {
#     sapply(1:parameters()$settings$THorizon, function(n)
#            {1 / ((1 + parameters()$settings$Dr_C) ^ n)})
#   })


## ----shiny-model-rhandsreacflow, echo = FALSE, fig.align="center", fig.cap = "Reactivity flow diagram with rhandsontable"-----------------
# All defaults
img1_path <- "figs/RhandsReacflow.PNG"
img1 <- readPNG(img1_path, native = TRUE, info = TRUE)
include_graphics(img1_path)


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
# tabItem(
#         tabName = "tab_Model_res",
#         h2("Model results"),
#         conditionalPanel("input.Run_model > 0",
#                         # Conditional panels are used to render the UI depending on
#                          # particular values in the inputs, including action buttons
#                          # This "input.Run_model > 0" is only true if input$Run_model
#                          # has been pressed
#                          fluidRow(
#                            box(
#                              title = "Model summary",
#                              width = 12,
#                              solidHeader = TRUE,
#                              status = "primary",
#                              tags$u("Discounted results:"),
#                              dataTableOutput(outputId = "disc_Results_table"),
#                              br(),
#                              tags$u("Undiscounted results:"),
#                              dataTableOutput(outputId = "undisc_Results_table")
#                            ),
#                            box(
#                              title = "Model graphs",
#                              width = 12,
#                              solidHeader = TRUE,
#                              status = "primary",
#                              tags$u("Monotherapy trace:"),
#                              plotOutput(outputId = "mono_trace"),
#                              br(),
#                              tags$u("Combination trace:"),
#                              plotOutput(outputId = "combo_trace"),
#                              br(),
#                              tags$u("Comparative trace:"),
#                              plotOutput(outputId = "comparative_markov")
#                            )
#                          )),
#         conditionalPanel(
#           "input.Run_model == 0",
#           # If Run_model has not been selected
#           tags$h3("Select 'Run the model' button in Model settings to run the model")
#         )
#       )


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
# tabItems(
#       tabItem(
#         # ~~ Model settings --------------------------------------------------------
#         tabName = "tab_Settings",
#         tags$h2("Model settings"),
#         actionButton(
#           "Model_settings_reset_button",
#           "Reset settings to base case",
#           style = "color: #fff; background-color: #bd3598; border-color: #2e6da4"
#         ),
#         br(), br(),
#         uiOutput("Model_settings_UI")
#       )


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
#       # ~~ Model results --------------------------------------------------------
#       tabItem(
#         tabName = "tab_Model_res",
#         h2("Model results"),
#         uiOutput("Model_results_UI")
#       )


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
#   output$Model_results_UI <- renderUI({
#     tagList(
#       conditionalPanel("input.Run_model > 0",   # Conditional panels are used to render the UI depending on
#                        # particular values in the inputs, including action buttons
#                        # This "input.Run_model > 0" is only true if "input.Run_model"
#                        # has been pressed
#                        # Refer to the module UI here, the id of the UI is put inside the brackets. The name can be anything
#                        Results_UI("ResultsUI")),
#       conditionalPanel(
#         "input.Run_model == 0",
#         # If Run_model has not been selected
#         tags$h3("Select 'Run the model' button in Model settings to run the model")
#       )
#     )
#   })


## ----DRLdiagram, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.cap="Example model of controlling data flow in Shiny cost-effectiveness model",eval=TRUE----
DiagrammeR::grViz(
  '
  digraph {
    graph [layout = dot, rankdir = LR] {
      
      SERVER [label = "Server", shape=oval]
      
      subgraph cluster_0 {
            label="Data handling";
            node [shape = rectangle];
            D [label = D];
            `R` [label = R];
            L [label = L];
            # D -> R
            D -> L
        }

        subgraph cluster_2 {
          label="Save/load";
          S [label = S];
          L -> S -> L;
        }
          
        
        subgraph cluster_1 {
            node [shape = diamond]
            label="User interface";
            UI [label = UI];
        }
        
        L -> UI ;
        UI -> SERVER[label="Trigger",color=blue,penwidth=1.0];    
        SERVER -> R[label="Process",color=green,penwidth=1.0];
        `R` -> L[label="Confirm",color=red,penwidth=1.0,style=dashed];
        
        
    }
  }
  '
)


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
# 
# # Function: Generates a user interface for very basic drug costing inputs, for
# #           a particular drug. Does not take titration, weaning, or dose-loading into account.
# #           Does not take drug discounts or RDI into account. Patient characteristic inputs
# #           come from somewhere else (i.e. the "basic" element within L)
# #           Outputs (inputs) can be used to feed into more complex drug costing
# #           calculation steps, thus improving transparency, readability, and the ability to
# #           present intermediate calculations within-app.
# #
# #           Author: Darren Burns
# #           Edited: Darren Burns
# #           Date  : XX/XX/XXXX
# #
# #           Drug_n: The number for this drug (e.g. drug #6 would be 6). this determines the
# #                   'inputId' argument in all of the individual elements, so that they can
# #                   be referenced from the shiny server. e.g. i_drug_drug_basic_n_XXX where
# #                   XXX is the drug number entered in this argument.
# #
# #           drug_name: A label for the drug for the UI, can be pulled from another input set
# #                      earlier on, with its own commit button (probably for a UI for names and
# #                      drug types, e.g. pills, IV, banded dosing etc)
# #
# #           hots     : the hands on tables, which must be rendered separately and passed into this
# #                      function. This is because handsontables must be directly added to "output"
# #                      in shiny.
# #
# #           I     : The input set to populate the UI elements. I should come directly from L
# #                   within the server to avoid infinite looping or other issues. Updates to L
# #                   for this sub-element of L should be linked to a button to implement the
# #                   changes. Those changes will then feed into R, be processed into the appropriate
# #                   form for L, and be passed onto L, triggering a re-rendering of the UI element
# #                   generated by this function.
# #
# #           Rules for I:
# #
# #           1. n has a maximum value, default of 10. Not many drugs have more than 2 or 3 of these
# #           2. The nrow of table is preserved at max_n, irrespective of n. n is used to subset table by row
# #           3. unit is one of a set of strings, default is c("mg/kg", "mg/m2", "flat")
# #           4. Dose must be a positive number
# #
# #           Structure of I MUST be:
# #
# #           List of 5
# #           $ n    : num 10
# #           $ table: logi [1:10, 1:2] 0 0 0 0 0 0 ...
# #            ..- attr(*, "dimnames")=List of 2
# #            .. ..$ : NULL
# #            .. ..$ : chr [1:2] "sizes" "costs"
# #           $ max_n: num 10
# #           $ unit : chr "flat"
# #           $ dose : num(0)
# #
# #
# 
# func_ui_drug_dosing <- function(drug_n, drug_name, hots, I) {
# 
#   # This function has a couple of obvious requirements:
#   require(shiny)
#   require(rhandsontable)
# 
#   # I contains the inputs following the structure defined in the header.
#   # As we know the structure already, we can proceed assuming those values
#   # exist.
# 
#   # all of the other inputs are quite simple. If the user has selected flat dosing,
#   # then the dosing numeric input is then just the dose at 100% RDI, and isn't per some
#   # measure of patient characteristic.
# 
#   if (I$unit == "flat") {
#     dosing_label <- "This drug is flat-dosed. The dose is not sensitive to patient characteristics"
#   } else if (I$unit == "mg/m2") {
#     dosing_label <- "This drug is dosed per meter squared of body surface area (BSA)"
#   } else {
#     dosing_label <- "This drug is dosed per kilogram of body weight"
#   }
# 
# 
#   # UI elements:
#   #
#   # Firstly, a slider to choose how many different unit sizes are available. note the
#   # way of naming the input uses the argument to this function, which means easy
#   # programmatic generation and reference to it using the same paste mechanism!
#   #
#   # Note that the max and value arguments come from I, so that L is linked to the UI
# 
#   n_slider <- sliderInput(
#     inputId = paste0("i_drug_basic_n_", drug_n),
#     label   = paste0("Number of different unit sizes avaialble for ", drug_name),
#     min     = 1,
#     max     = I$max_n,
#     value   = I$n,
#     step    = 1,
#     ticks   = TRUE,
#     width   = "100%"
#   )
# 
#   # Next, an rhandsontable for the table. By preventing the context menu this
#   # prevents the user from adding rows to the table. As the slider above has already
#   # looped round and changed I$n, this means that the number of rows in this excel-like
#   # table should not be editable!
# 
#   hot <- rHandsontableOutput(hots)
# 
#   # Now a dropdown menu for the basis of dosing. Includes an option for flat
#   # dosing, meaning that the denominator for dose_num below is either kg, m2 or
#   # nothing.
# 
#   unit_picker <- selectInput(
#     inputId = paste0("i_drug_basic_unit_", drug_n),
#     label   = paste0("Basis of dosing for ", drug_name),
#     choices = c("mg/kg", "mg/m2", "flat"),
#     selected = I$unit,
#     multiple = FALSE,
#     width = "100%"
#   )
# 
# 
#   # next, a numeric input for dose. This is per characteristic unit, or just flat
#   # depending on option in the dropdown menu above, either way its stored in
#   # the same place. The dropdown option determines which function the data
#   # generated here feeds into (i.e. directs to the appropriate way of costing
#   # up the drug). note that the label is determined by I$unit, allowing the user
#   # to easily see in the UI what information they are entering into the model.
# 
#   if (I$unit == "flat") {
#     dose_numeric_label <- paste0("Dosing for ", drug_name, " (fixed)")
#   } else if (I$unit == "mg/kg") {
#     dose_numeric_label <- paste0("Dosing for ", drug_name, " (per kilo)")
#   } else {
#     dose_numeric_label <- paste0("Dosing for ", drug_name, " (per m2 of BSA)")
#   }
# 
#   dose_numeric <- numericInput(
#     inputId = paste0("i_drug_basic_dose_", drug_n),
#     label   = dose_numeric_label,
#     value   = I$dose,
#     min     = 0,
#     max     = Inf,
#     width   = "100%"
#   )
# 
# 
#   # Finally, a button to confirm the user selections for this drug for this drug type
#   # for this arrangement of inputs.
# 
#   confirm <- actionButton(
#     inputId = paste0("i_drug_basic_confirm_", drug_n),
#     label   = paste0("Confirm inputs for ", drug_name),
#     icon    = icon("calculator"),
#     width   = "100%"
#   )
# 
#   # put all these bits together inside of a panel so that the panel can be positioned
#   # higher up in the stack. A function called tagList() makes this easy
# 
#   ui_output <- shiny::inputPanel(
#     fluidRow(
#       width = 12,
#       column(
#         12,
#         tagList(
#           "The below inputs control the dosing for ", drug_name, ". Please confirm the ",
#           "number of drugs first using the slider and button, then enter the other inputs.",
#           br(),
#           confirm,
#           n_slider,
#           hot,
#           dose_numeric
#         )
#       )
#     )
#   )
# 
# 
#   # return the final UI
#   return(ui_output)
# 
# }
# 
# 
# func_ui_drug_dose_table <- function(I, drug_n) {
# 
#   # limit the table to go to the UI to only the rows 1:n so that if user has chosen
#   # value n < max_n then it won't show the rest of the table. Note that changing
#   # the user input for n without saving changes first will overwrite the UI inputs.
# 
#   tab_d_c <- I$table[1:I$n,]
# 
#   # now generate a hands on table
#   hot <- rhandsontable(
#     data       = tab_d_c,
#     colHeaders = colnames(tab_d_c),
#     search     = FALSE,
#     width      = "100%"
#   )
#   hot <-
#     hot_table(
#       hot = hot,
#       highlightCol = TRUE,
#       highlightRow = TRUE,
#       stretchH = "all",
#       contextMenu = FALSE
#     )
#   return(hot)
# }
# 


## ----eval=FALSE, echo=FALSE---------------------------------------------------------------------------------------------------------------
# 
# # from within the server, to generate L$drug$basic$n different UIs, one for each drug with a full
# # set of dosing inputs:
# 
# ui_drug_dosing <- renderUI({
# 
#   # L must exist to render this UI:
# 
#   req(!is.null(L))
# 
#   # pull out the relevant inputs for dosing
# 
#   LD <- L$drug
# 
#   # the inputs I are contained within L$drug$dosing for each drug 1:L$drug$basic$n
# 
#   tagList(lapply(
#     1:LD$basic$n,
#     function(drug_n) {
#       func_ui_drug_dosing(
#         drug_n    = drug_n,
#         drug_name = LD$basic$names[drug_n],
#         hots      = paste0("ui_drug_dosetable_", drug_n),
#         I         = LD$dosing[[drug_n]]
#       )
#     }
#   ))
# 
# })
# 


## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------
# # Also in the server, define observers which watch all of the inputs, and update the values
# # inside of R. Also define the update buttons and the event to move data from `R` to L
# 
# for (drug_n in 1:L$drug$basic$max_n) {
# 
#   # the hands on tables - render the table for this arm so it can feed into the renderUI:
#   output[[paste0("ui_drug_dosetable_", drug_n)]] <- func_ui_drug_dose_table(I = L$drug$dosing[[drug_n]], drug_n = drug_n)
# 
#   # sliders. for this drug, watch the slider for unit sizes, and whenever the user
#   # changes the slider, update the value in R, but not L. Value is stored but not
#   # causing the UI to repeatedly refresh itself because it only depends on L. give
#   # this a high priority to make sure it happens before other responses.
# 
#   observeEvent(input[[paste0("i_drug_basic_n_", drug_n)]], {
#     if (!is.null(input[[paste0("i_drug_basic_n_", drug_n)]])) {
#       R$drug$dosing[[drug_n]]$n <- input[[paste0("i_drug_basic_n_", drug_n)]]
#     }
#   }, priority = 100)
# 
#   # tables - convert them back to standard `R` objects so they can feed into `R` properly:
#   observeEvent(input[[paste0("ui_drug_dosetable_", drug_n)]], {
#     if (!is.null(input[[paste0("ui_drug_dosetable_", drug_n)]])) {
#       R$drug$dosing[[drug_n]]$table <- hot_to_r(input[[paste0("ui_drug_dosetable_", drug_n)]])
#     }
#   }, priority = 100)
# 
# 
#   # units:
#   observeEvent(input[[paste0("i_drug_basic_unit_", drug_n)]], {
#     if (!is.null(input[[paste0("i_drug_basic_unit_", drug_n)]])) {
#       R$drug$dosing[[drug_n]]$unit <- input[[paste0("i_drug_basic_unit_", drug_n)]]
#     }
#   }, priority = 100)
# 
#   # Dosing
#   observeEvent(input[[paste0("i_drug_basic_dose_", drug_n)]], {
#     if (!is.null(input[[paste0("i_drug_basic_dose_", drug_n)]])) {
#       R$drug$dosing[[drug_n]]$dose <- input[[paste0("i_drug_basic_dose_", drug_n)]]
#     }
#   }, priority = 100)
# 
#   # Confirmation buttons: the action in this one is simply to pass along the inputs
#   # of interest to L from R. this gets highest priority to make sure it happens
#   # right away and before any rendering triggers for L into the UI.
#   observeEvent(input[[paste0("i_drug_basic_confirm_", drug_n)]], {
#     L$drug$dosing[[drug_n]] <- R$drug$dosing[[drug_n]]
#   }, priority = 150)
# 
# }



## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| eval: false
# # This can be used to do some more formatting on the table...
# readxl::read_xlsx("tables/hta-bodies-acceptability-shiny.xlsx") |>
#   knitr::kable()


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: true
# # Example application
# 
# # load the UI and server:
# ui     <- source("../app_files/UI/ui_master.R")
# server <- source("../app_files/server/server_master.R")
# 
# # run the shiny application
# shiny::shinyApp(ui = ui, server = server, onStart = source("../data/global.R"))


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: true
# # get dose per characteristic measure for all drugs:
# dosePerChar <- unlist(R$dcost$dosePerChar)
# 
# # Now use dosePerChar in subsequent calculations:


## ----fig-ExampleMarkovModel, fig.cap="A simple Markov trace in `R`", fig.id = "ExampleMarkovModel", echo=TRUE-----------------------------

library(ggplot2)

# A population starting in state 1 is exposed to matrix 1 for 5 cycles
#   and matrix 2 for the rest of the model until the time horizon
Starting_states <- c(1, 0, 0)

# first matrix is applied for N cycles (i.e. cycles 1-N)
#  Note that cycle 0 doesn't count
Switch_TPM <- 5

# time horizon
Time_Horizon <- 49 # Cycle 0 is described by Starting_states so 49 for 50 total

# Transition probability matrices (TPMs)
TPM_1 <- matrix(
  c(
    0.85, 0.10, 0.05,
    0.05, 0.80, 0.15,
    0.00, 0.00, 1.00
  ),
  ncol = 3,
  byrow = TRUE
)
TPM_2 <- matrix(
  c(
    0.80, 0.10, 0.10,
    0.01, 0.80, 0.19,
    0.00, 0.00, 1.00
  ),
  ncol = 3,
  byrow = TRUE
)

# use Reduce to repeatedly apply the matrix to the result of the
#   previous calculation, with the condition of changing the TPM
#   after applying it N times
StatePopList <- Reduce(
  x           = 1:Time_Horizon,
  init        = Starting_states,
  accumulate  = TRUE,
  f = function(PrevCycle, ThisCycle) {
	# Now, we are inside of the reduce environment. 
    # The cycle number is ThisCycle
    # The results of calculation from the previous cycle are PrevCycle
    if(ThisCycle < Switch_TPM) {
      # use matrix multiplication: vector of pops %*% TPM
      PrevCycle %*% TPM_1
    } else {
      PrevCycle %*% TPM_2
    }
  }
)

# The result from Reduce is a list, each containing the result
#  of PrevCycle after being processed within the function environment
#  (i.e. the curly braces {}). Sticking them together row-wise 
#  produces a Markov trace:

TRACE <- do.call(rbind, StatePopList)

# final touch, make a plot by making the data long
Trace_Plot <- rbind(
  data.frame(State = 1, cycle = 1:nrow(TRACE), Pop = TRACE[,1]),
  data.frame(State = 2, cycle = 1:nrow(TRACE), Pop = TRACE[,2]),
  data.frame(State = 3, cycle = 1:nrow(TRACE), Pop = TRACE[,3])
)

# put the data into a plot, separating by State
ggplot(Trace_Plot,aes(x = cycle, y = Pop, colour = as.factor(State))) + 
  geom_line() + 
  theme_classic() +
  scale_x_continuous(expand = expansion(mult = c(0,0))) + 
  scale_y_continuous(expand = expansion(mult = c(0,0))) + 
  theme(legend.position = "bottom")




## ----fig-FigDashBoardStructure, echo = FALSE, fig.align="center", fig.cap = "Typical example of dashboard links for a health economic model"----
# All defaults
img1_path <- "figs/dashboardStructure.png"
img1 <- readPNG(img1_path, native = TRUE, info = TRUE)
include_graphics(img1_path)


## ----fig-FigDashBoardBox, echo = FALSE, fig.align="center", fig.cap = "A box containing a set of general modelling inputs"----------------
# All defaults
img1_path <- "figs/dashboardBox.png"
img1 <- readPNG(img1_path, native = TRUE, info = TRUE)
include_graphics(img1_path)


## ----fig-FigDashBoardTabBox, echo = FALSE, fig.align="center", fig.cap = "A box containing a set of general modelling inputs", fig.width=6----
# All defaults
img1_path <- "figs/dashboardTabBox.png"
img1 <- readPNG(img1_path, native = TRUE, info = TRUE)
include_graphics(img1_path)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
#| eval: false

# # in the UI script, add the following 2 elements: the table and a confirm button
# 
# rhandsontable::rHandsontableOutput("my_hot")
# shiny::actionButton("ui_commit_changes", "commit the changes to the table")
# 
# # In the global settings file, usually called global.R, generate the
# # defaults inputs. This can also go in the main app.R file too, as long as it
# # runs when the app runs to define the global environment beforehand.
# # Alternatively, add it to the existing # D list.
# 
# D <- list(my_category = list(my_hot = mtcars))
# 
# # in the server, insert these elements. if `R` and L are already defined,
# # put these elements in the desired location and amend the server code
# # accordingly. Populating them here initially with some example data from core
# # R. These values would normally come from object D, a list of default values
# # for when the app first loads. To run this without shiny, simply run the
# # following lines:
# #
# # R <- D
# # L <- D
# #
# # and run through the lines inside of the shiny events
# 
# L <- shiny::reactiveValues(my_category = D$my_category)
# R <- shiny::reactiveValues(my_category = D$my_category)
# 
# # Taking the "live" values L to populate the table, generate a hands on table:
# 
# output$my_hot <- rhandsontable::renderRHandsontable({
#   dat <- L$my_category$my_hot
#   rhandsontable::rhandsontable(dat)
# })
# 
# # place the table in the "reactive" reactiveValues list R, only if it exists,
# # immediately to keep a live record of the user inputs. Do this with a very
# # high priority so that it happens before anything else. this avoids and
# # flickering or looping.
# 
# shiny::observeEvent(input$my_hot, {
#   req(!is.null(input$my_hot))
#   dat <- rhandsontable::hot_to_r(input$my_hot)
#   R$my_category$my_hot <- dat
# }, priority = 100)
# 
# # when the user commits the changes feed them from `R` to L. As the hot
# # values have already been converted back to `R` in the above, this is simply
# # passing it along. Note how this links back to output$my_hot because
# # L$my_category$my_hot has changed. This means that pressing the button may
# # cause output$my_hot to refresh. Usually it does not because `R` = L once
# # the button is pressed. Note that the priority of this event is even higher
# # than the immediate watcher to force the update to be in line with R.
# 
# shiny::observeEvent(input$ui_commit_changes, {
#   L$my_category$my_hot <- R$my_category$my_hot
# }, priority = 101)
# 
# # That's it. Now whenever the user changes the table "my_hot", the server
# # records those changes inside of `R` immediately. When the user presses the
# # commit button, this moves the changes from `R` to L, meaning that if the
# # table goes in and out of existence, its values will be preserved, similar to
# # using isolate(). This simple system can be expanded to almost any level of
# # complication.
# #
# # Finally, if an object S exists for a separately saved file, it can be
# # loaded in to replace L and that will update the UI which will then update R.
# # Loading from previous file would simply be L <- S, instead of thousands of
# # lines of individual input updates!


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: true
# # For the UI, there are 3 inputs, how many numerics, the numeric inputs
# # themselves, and the confirm button for L <- R. They are contained in 2
# # objects, one containing all the inputs and one containing the button
# 
# shiny::uiOutput("ui_input_set")
# shiny::uiOutput("ui_confirm_inputs")
# 
# # similar to the above in global, define the elements to enter into D
# 
# D <- list(my_category = list(
#   n_inputs_max = 10,
#   n_inputs     = 5,
#   values       = 1:10
# ))
# 
# # pass these into `R` and L and set up the system, similarly to the above:
# 
# R <- shiny::reactiveValues(my_category = D$my_category)
# L <- shiny::reactiveValues(my_category = D$my_category)
# 
# # generate a dynamic UI which responds to the amount of inputs the user wants.
# # Note that this relies completely on L and not on `R` to avoid infinite
# # looping!
# 
# output$ui_input_set <- shiny::renderUI({
#   req(!is.null(L))
# 
#   # pull out the inputs for this category:
# 
#   LMC <- L$my_category
# 
#   # generate the n inputs input
# 
#   n_inputs <- numericInput(
#     inputId = "ui_n_inputs",
#     label   = "Select number of inputs required",
#     max     = LMC$n_inputs_max,
#     value   = LMC$n_inputs
#   )
# 
#   # use those inputs to generate a list of numeric inputs. note that
#   # L is being used to populate all values, and L is always there, so these
#   # inputs have "memory"!
# 
#   input_list <- tagList(lapply(1:LMC$n_inputs, function(i_input) {
#     shiny::numericInput(
#       inputId = paste0("ui_my_inputs",i_input),
#       label = paste0("ui_my_inputs",i_input),
#       value = LMC$values[i_input],
#       min = 0,
#       width = "100%"
#     )
#   }))
# 
#   # put the individual bits above together into one thing:
# 
#   fluidRow(
#     width = 12,
#     column(
#       12,
#       n_inputs,
#       hr(),
#       input_list
#     )
#   )
# 
# })
# 
# # next we need to update `R` accordingly following changes to n, or the inputs:
# 
# shiny::observeEvent(input$ui_n_inputs, {
#   if(!is.null(input$ui_n_inputs)) {
#     R$my_category$n_inputs <- input$ui_n_inputs
#   }
# }, priority = 100)
# 
# # now for all possible inputs, trigger an event for that input which if that
# # input exists passes its value to the appropriate place in R:
# 
# lapply(1:D$my_category$n_inputs_max, function(i_input) {
# 
#   # so, for this input paste0("ui_my_inputs",i_input) e.g. input$ui_my_inputs1,
#   # if the value is not null (i.e. it exists, then get that value and pass it
#   # to the reactive object R)
# 
#   observeEvent(input[[paste0("ui_my_inputs",i_input)]], {
#     # require that the value exists (is not null), and then pass the value to R
#     req(!is.null(input[[paste0("ui_my_inputs",i_input)]]))
#     R$my_category$values[i_input] <- input[[paste0("ui_my_inputs",i_input)]]
#   }, priority = 100)
# })
# 
# # now we are updating `R` live we need a trigger to pass it to L when the
# # button is pressed.
# 
# observeEvent(input$ui_confirm_inputs, {
#   L$my_category <- R$my_category
# }, priority = 101)
# 
# # Now, the user changes the number of inputs ui_n_inputs and this initially does
# # nothing in the UI. when the user presses ui_confirm_inputs the number of
# # inputs in the UI will then change. changing the individual inputs whilst
# # the number of them is "out of date" will still work because they are always
# # recorded in `R` immediately, so the app will remember everything (except for
# # empty values as they will be ignored, which gets around a major issue with
# # numericInput more generally in Shiny). Even those inputs that fall out of
# # existence will be remembered (e.g.reduce ui_n_inputs, confirm, increase
# # again, confirm. inputs should still "remember" their values)
# 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: true

# # one event to respond to changes in number of inputs, and then pass those
# # changes to L immediately, triggering a refresh.
# 
# observeEvent(input$ui_n_inputs, {
#   req(!is.null(input$ui_n_inputs))
# 
#   R$my_category$n_inputs <- input$ui_n_inputs
#   L$my_category <- R$my_category
# 
# }, priority = 102)
# 

