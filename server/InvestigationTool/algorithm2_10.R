library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Algorithm for determining the total cost of the installation and configuration of the cellular communication channel

#Required number of cellular  sets
formula_2_10_1 <- function (input, intermediate = NULL)
{
  req (input)

  RequiredCapacity <- 0
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth

  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }



  result =  round (RequiredCapacity/input$InitialDataCellular.ChannelCapacity, digits = 0) + 1

  return (result)
}

#Total cost of cellular equipment and installation materials
formula_2_10_2 <- function (input, intermediate = NULL)
{
  req (input)

  NumberCellularsets <- 0
  NumberCellularsets <- input$Intermediate.NumberCellularsets

  if (!is.null(intermediate))
  {
    NumberCellularsets <- as.numeric (intermediate$NumberCellularsets)
  }

  result =  NumberCellularsets*(input$InitialDataCellular.CostOfOneCellular +
                                  input$InitialDataCellular.CostOfOneCellularmat)

  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of installation and configuration of all Cellular sets
formula_2_10_3 <- function (input, intermediate = NULL)
{
  req (input)

  NumberCellularsets <- 0
  NumberCellularsets <- input$Intermediate.NumberCellularsets

  if (!is.null(intermediate))
  {
    NumberCellularsets <- as.numeric (intermediate$NumberCellularsets)
  }

  result =  NumberCellularsets*input$InitialDataCellular.CostOfInstallCellular*
    input$InitialDataCellular.LaborOfInstallCellular
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Design cost
formula_2_10_4 <- function (input, intermediate = NULL)
{
  req (input)

  CostOfCellularEqAndMat <- 0
  CostOfCellularEqAndMat <- input$Intermediate.CostOfCellularEqAndMat

  CostOfCellularInstallProcess <- 0
  CostOfCellularInstallProcess <- input$Intermediate.CostOfCellularInstallProcess

  if (!is.null(intermediate))
  {
    CostOfCellularEqAndMat <- as.numeric (intermediate$CostOfCellularEqAndMat)
    CostOfCellularInstallProcess <- as.numeric (intermediate$CostOfCellularInstallProcess)
  }

  CostOfEqCoeff <- as.numeric (input$InitialDataCellular.CostOfEqCoeff) #0.4
  DesignCoeff <- as.numeric (input$InitialDataCellular.DesignCoeff) #0.05


  result =  (CostOfEqCoeff*CostOfCellularEqAndMat + CostOfCellularInstallProcess)*DesignCoeff
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of establishing a cellular communication channel
formula_2_10_5 <- function (input, intermediate = NULL)
{
  req (input)


  DesignCellularCost <- 0
  DesignCellularCost <- input$Intermediate.DesignCellularCost

  CostOfCellularEqAndMat <- 0
  CostOfCellularEqAndMat <- input$Intermediate.CostOfCellularEqAndMat

  CostOfCellularInstallProcess <- 0
  CostOfCellularInstallProcess <- input$Intermediate.CostOfCellularInstallProcess

  if (!is.null(intermediate))
  {
    DesignCellularCost <- as.numeric (intermediate$DesignCellularCost)
    CostOfCellularEqAndMat <- as.numeric (intermediate$CostOfCellularEqAndMat)
    CostOfCellularInstallProcess <- as.numeric (intermediate$CostOfCellularInstallProcess)
  }

  result =  DesignCellularCost + CostOfCellularEqAndMat  + CostOfCellularInstallProcess
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

algorithm2_10_impl <- function(input, intermediate = NULL)
{
  # Required number of Cellular sets

  NumberCellularsets =  formula_2_10_1 (input, intermediate)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Required number of Cellular sets, units"), NumberCellularsets, sep = ": "))

  # Total cost of Cellular equipment and installation materials
  intermediate2 <- list (NumberCellularsets = 0)
  intermediate2$NumberCellularsets <- NumberCellularsets

  CostOfCellularEqAndMat =  formula_2_10_2 (input, intermediate2)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of Cellular equipment and installation materials, currency units"), CostOfCellularEqAndMat, sep = ": "))

  # Total cost of installation and configuration of all Cellular sets

  CostOfCellularInstallProcess =  formula_2_10_3 (input, intermediate2)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of installation and configuration of all Cellular sets, currency units"), CostOfCellularInstallProcess, sep = ": "))

  # Design cost
  intermediate3 <- list (CostOfCellularEqAndMat = 0.0, CostOfCellularInstallProcess = 0.0, DesignCellularCost = 0.0 )
  intermediate3$CostOfCellularEqAndMat <- CostOfCellularEqAndMat
  intermediate3$CostOfCellularInstallProcess <- CostOfCellularInstallProcess

  DesignCellularCost =  formula_2_10_4 (input, intermediate3)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Design cost, currency units"), DesignCellularCost, sep = ": "))

  intermediate3$DesignCellularCost <- DesignCellularCost

  # Total cost of establishing a cellular communication channel

  result <- matrix (nrow = 3, ncol = 2)
  result [1,1] = i18n$t("Total cost of establishing a cellular communication channel, currency units")


  result [1,2] = formula_2_10_5 (input, intermediate3)

  result [1,2] <- round (as.numeric (result [1,2]), digits = 2)

  result [2,1] = i18n$t("Required number of Cellular sets, units")
  result [2,2] = NumberCellularsets

  result [3,1] = i18n$t("Total cost of Cellular equipment and installation materials, currency units")
  result [3,2] = CostOfCellularEqAndMat

  result [3,2] <- round (as.numeric (result [3,2]), digits = 2)

  return (result)
}


algorithm2_10 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)



  switch (input$formula,
          ALL = {

            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataCellular.ChannelCapacity)
            req (input$InitialDataCellular.CostOfOneCellular)
            req (input$InitialDataCellular.CostOfOneCellularmat)
            req (input$InitialDataCellular.CostOfInstallCellular)
            req (input$InitialDataCellular.LaborOfInstallCellular)

            req (input$InitialDataCellular.CostOfEqCoeff) #0.4
            req (input$InitialDataCellular.DesignCoeff) #0.05


            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

            result <- algorithm2_10_impl (input)

            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

          },
          FORMULA_2_10_1 = {#Required number of Cellular sets

            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataCellular.ChannelCapacity)

            result <- formula_2_10_1 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_10_2 = {#Total cost of Cellular equipment and installation materials

            req (input$Intermediate.NumberCellularsets)
            req (input$InitialDataCellular.CostOfOneCellular)
            req (input$InitialDataCellular.CostOfOneCellularmat)

            result <- formula_2_10_2 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_10_3 = {#Total cost of installation and configuration of all Cellular sets

            req (input$Intermediate.NumberCellularsets)
            req (input$InitialDataCellular.CostOfInstallCellular)
            req (input$InitialDataCellular.LaborOfInstallCellular)

            result <- formula_2_10_3 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_10_4 = {#Design cost

            req (input$Intermediate.CostOfCellularEqAndMat)
            req (input$Intermediate.CostOfCellularInstallProcess)

            req (input$InitialDataCellular.CostOfEqCoeff) #0.4
            req (input$InitialDataCellular.DesignCoeff) #0.05

            result <- formula_2_10_4 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_10_5 = {#Total cost of establishing a cellular communication channel

            req (input$Intermediate.DesignCellularCost)
            req (input$Intermediate.CostOfCellularEqAndMat)
            req (input$Intermediate.CostOfCellularInstallProcess)

            result <- formula_2_10_5 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          stop ("No!")

  )
}
