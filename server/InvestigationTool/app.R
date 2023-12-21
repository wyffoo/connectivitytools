library(dplyr)
library(shiny)
library(gmp)
library(Rmpfr)
library(shiny.i18n)
library(leaflet)
library(stringr)
# library(mapview)
# library(webshot)

source ("method1.R")
source ("method2.R")
source ("method3.R")
source ("method4.R")

# variables <- read.csv("variables_csv.csv")
profiles <- c ()
profiles <- vroom::vroom("profiles.csv", altrep = FALSE)

variables <- c ()
#variables <- vroom::vroom("variables1.csv", altrep = FALSE)
names <- c()
names <- vroom::vroom("names.csv", altrep = FALSE)

mylog <- matrix("Detailed Calculation Log:")
myvariables <- c()
vnames <- c ()

library(shiny)

i18n <- Translator$new(translation_json_path = "translation.json")


i18n$set_translation_language("en")

ui <- fluidPage(

  titlePanel("Broadband Diagnostic Toolkit: Broadband Diagnostic Tool"),

  sidebarLayout(

    sidebarPanel(
      selectInput("profile", "Profile", choices = unique(profiles$NAME)),
      selectInput("method", "Methodology", choices = unique(variables$METHODOLOGY)),
      selectInput("algorithm", "Algorithm", choices = NULL),
      selectInput("formula", "Formula", choices = NULL),
      actionButton("calculate", "Calculate"),
      actionButton("investigate", "Investigate"),
      uiOutput("sliders")

    ),

    mainPanel(
      textOutput("title"),
	  tabsetPanel(
	  		id = "primary_tabs",
			tabPanel("Main Data","Overal Stats", tableOutput("data")),
			#tabPanel("Master Table","#OBJECTNAME       LON      LAT       RB       DTF       COV      REGION       DTBN       HopsToBN       NextNode     FRB     NPV     Tech     isBN    isLeaf   NPVC", tableOutput("data2")),
			#tabPanel("CON2BN Table", "DTBN   RB   FOCL_NPV   RTS_NPV   SAT_NPV    CELL_NPV    BEST_TECH   BEST_NPV", tableOutput("data3")),
			tabPanel("Master Table", tableOutput("data2")),
			tabPanel("CON2BN Table", tableOutput("data3")),
			tabPanel("Map",  leafletOutput("mymap",height = 700)),
			tabPanel("Log",  tableOutput("log")),
			# tabPanel("Adhoc data", "tab 5", tableOutput("c_names")),
			tabPanel("Plot",  plotOutput ("myplot"))
			# tabPanel("Params",  DTOutput("mydatatable"))
	  )

      # tableOutput("data"),
      # tableOutput("log"),
      # leafletOutput("mymap",height = 700),
      # plotOutput ("myplot")

    )
  )
)

server <- function(input, output, session) {


  #Recalculate everything
  recalculate <- function()
  {
    req (input)
    req (input$method)
    req (output)

    if (input$method == "METHOD1")
    {
      method1(input, output)
    }

    if (input$method == "METHOD2")
    {
      method2(input, output)
    }

    if (input$method == "METHOD3")
    {
      method3(input, output)
    }

    if (input$method == "METHOD4")
    {
      method4(input, output)
    }
  }

  investigate <- function ()
  {
    req (input)
    req (input$method)
    req (output)

    if (input$method == "METHOD1")
    {
      method1_inv(input, output)
    }

    if (input$method == "METHOD2")
    {
      method2_inv(input, output)
    }

    #Not supported
    if (input$method == "METHOD3")
    {
      method3_inv(input, output)
    }

    #Not supported
    if (input$method == "METHOD4")
    {
      method4_inv(input, output)
    }
  }


  createnewcontrols <- function (selected, sortlevel=1)
  {
    req (selected)
    if (is.null(selected))
    {
      return ()
    }


    ndx <- c ()


    if (sortlevel == 1)
      ndx = order(selected$SORTA)
    else
      ndx = order(selected$SORTM)


    selected2 = selected [ndx,]

    if (is.null(selected2))
    {
      return ()
    }


    if (nrow(selected2) > 0)
    {
      #.GlobalEnv$myvariables <- matrix(nrow = nrow(selected), ncol = 2)
      .GlobalEnv$myvariables <- rep ("", nrow(selected2) )
      .GlobalEnv$vnames <- rep ("", nrow(selected2))


      output$sliders <- renderUI({
        NR <- nrow(selected)


        sliders <- lapply (1:NR, function (i) {


          if (!is.na (selected2 [i,3]) && !is.na (selected2 [i,4]) && !is.na (selected2 [i,6]) &&
                          !is.na (selected2 [i,7]) && !is.na (selected2 [i,8]) && !is.na (selected2 [i,9]) &&
                                                                                      !is.na (selected2 [i,5]))
          {


            inputName <-   toString (selected2 [i,3])

            sel_pdesc <- toString (selected2 [i,4])

            sel_unit <- toString (selected2 [i,5])




            sel_pdesc <- paste (sel_pdesc, sel_unit, sep = ", ")

            .GlobalEnv$myvariables [i] <- sel_pdesc
            .GlobalEnv$vnames [i] <- inputName


            typec <- toString (selected2 [i,14])

            if (typec == "INPUT")
            {
              sel_dvalue <- toString ( selected2 [i,9])

              textInput(inputName, sel_pdesc, sel_dvalue)
            }
            else
            {
              sel_rstart <- as.numeric (selected2 [i,6])

              sel_rend <- as.numeric ( selected2 [i,7])

              sel_step <- as.numeric ( selected2 [i,8])

              sel_dvalue <- as.numeric ( selected2 [i,9])

              req (inputName, sel_pdesc, sel_unit, sel_rstart, sel_rend, sel_step, sel_dvalue)

              sliderInput(inputName, sel_pdesc, value = sel_dvalue, min = sel_rstart, max = sel_rend, step = sel_step, animate = TRUE)
            }

          }


        })


        names (.GlobalEnv$myvariables) <- .GlobalEnv$vnames


       # print (do.call (tagList, sliders))
        do.call (tagList, sliders) # ?!

        #write.table(sometext, file = 'my_data.txt', sep = '')

       # print (1)

      })
    }

  }

  profile <- reactive({

    req (input$profile)


    output$c_names <- NULL
    output$data <- NULL
    output$log <- NULL

    filtered_var <- filter(profiles, NAME == input$profile)
    return (filtered_var)

  })

  method <- reactive({

    req (input$method)

    req (.GlobalEnv$variables)

    var <- .GlobalEnv$variables

    output$data <- NULL
    output$log <- NULL

    #print (input$method)
    method_name <- filter (names, OBJECT == input$method)
    m_name <- toString(distinct(method_name, NAME))
    output$title <- renderText(m_name)


    algorithm_names <- filter (names,PARENT == input$method)
    a_names <- distinct(algorithm_names, OBJECT, NAME)
    output$c_names <- renderTable(a_names, colnames = FALSE)

    filtered_var <- c ()
    if (!is.null(var))
      filtered_var <- filter(var, METHODOLOGY == input$method)

    return (filtered_var)

  })

  algorithm <- reactive({

    req(input$algorithm)
    req (input$method)

    output$data <- NULL
    output$log <- NULL

    sortlvl <- 1

    if (input$algorithm != "ALL")
    {
      algo_name <- filter (names, OBJECT == input$algorithm)
      a_name <- toString(distinct(algo_name, NAME))
      output$title <- renderText(a_name)


      formulas_names <- filter (names,PARENT == input$algorithm)
      f_names <- distinct(formulas_names, OBJECT, NAME)
      output$c_names <- renderTable(f_names, colnames = FALSE)
    }
    else
    {
      method_name <- filter (names, OBJECT == input$method)
      m_name <- toString(distinct(method_name, NAME))
      output$title <- renderText(m_name)


      algorithm_names <- filter (names,PARENT == input$method)
      a_names <- distinct(algorithm_names, OBJECT, NAME)
      output$c_names <- renderTable(a_names, colnames = FALSE)

      sortlvl <- 2
    }

    all_methods <- method()

    var <- filter(all_methods, if (input$algorithm != "ALL")
      {
        ALGORITHM == input$algorithm
    }
    else
    {
      TYPE2 == "ID"
    }
    )
      contr <- distinct(var, ALGORITHM,	FORMULA,	PNAME,	PDESC,	UNIT,	RSTART,	REND,	STEP,	DVALUE,TYPE,TYPE2, SORTA, SORTM, CONT)
      createnewcontrols (contr, sortlvl) # Try to omit this
      return (contr)

  })

   formula <- reactive({

     req(input$formula)
     req (input$algorithm)

     if (input$algorithm == "ALL")
     {
       return ()
     }


     output$data <- NULL
     output$log <- NULL

     if (input$formula != "ALL")
     {

       form_name <- filter (names, OBJECT == input$formula)
       ff_name <- toString(distinct(form_name, NAME))
       output$title <- renderText(ff_name)
       output$c_names <- NULL
     }
     else
     {
       algo_name <- filter (names, OBJECT == input$algorithm)
       a_name <- toString(distinct(algo_name, NAME))
       output$title <- renderText(a_name)


       formulas_names <- filter (names,PARENT == input$algorithm)
       f_names <- distinct(formulas_names, OBJECT, NAME)
       output$c_names <- renderTable(f_names, colnames = FALSE)

     }

     #all_algorithms <- reactiveVal ()

       form <- filter(algorithm(), if (input$formula != "ALL")
       {
         FORMULA == input$formula
       }
        else {
          TYPE == "ID"
          }
         )
       fcont <- distinct(form, ALGORITHM,	FORMULA,	PNAME,	PDESC,	UNIT,	RSTART,	REND,	STEP,	DVALUE,TYPE,TYPE2, SORTA, SORTM, CONT)

       createnewcontrols (fcont)
       return (fcont)
   })



  #print (formula())
  #formula()

#   output$data <- renderTable(algorithm())
   #output$data <- renderTable(formula())

   observeEvent(profile(), {

     .GlobalEnv$variables <- c ()
     .GlobalEnv$variables <- vroom::vroom(profile()$V_FILE, altrep = FALSE)
   #  names <- c()
    # names <- vroom::vroom(profile()$N_FILE, altrep = FALSE)
     var <- .GlobalEnv$variables

     x <- character(0)

     updateSelectInput(session, "method", choices = unique(var$METHODOLOGY), selected=character(0))
     updateSelectInput(session, "algorithm", choices = x)
     updateSelectInput(session, "formula", choices = x)

     output$sliders <- renderUI({})
   })


  observeEvent(method(), {

    x <- character(0)

    selected <- method()

    updateSelectInput(session, "algorithm", choices = unique(selected$ALGORITHM), selected = "ALL")
    updateSelectInput(session, "formula", choices = x)
  })

    observeEvent(algorithm(), {

      req (input$algorithm)

      if (input$algorithm != "ALL")
        updateSelectInput(session, "formula", choices = unique(algorithm()$FORMULA), selected = "ALL")

    })

    observeEvent(formula(), {

    })

  observeEvent(input$calculate, {
    recalculate()
  })

  observeEvent(input$investigate, {
    investigate ()
    })
}

#sl_var.csv


shinyApp(ui, server)
