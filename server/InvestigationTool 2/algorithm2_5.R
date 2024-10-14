library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")


#Algorithm for determining the total cost of the installation and configuration of the satellite communication channel

#Required number of VSAT sets
formula_2_5_1 <- function (input, intermediate = NULL)
{
  req (input)

  RequiredCapacity <- 0
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth

  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }



  result =  round (RequiredCapacity/input$InitialDataSatelite.ChannelCapacity, digits = 0) + 1

  return (result)
}

#Total cost of VSAT equipment and installation materials
formula_2_5_2 <- function (input, intermediate = NULL)
{
  req (input)

  NumberVSATsets <- 0
  NumberVSATsets <- input$Intermediate.NumberVSATsets

  if (!is.null(intermediate))
  {
    NumberVSATsets <- as.numeric (intermediate$NumberVSATsets)
  }

  result =  NumberVSATsets*(input$InitialDataSatelite.CostOfOneVSAT+input$InitialDataSatelite.CostOfOneVSATmat)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of installation and configuration of all VSAT sets
formula_2_5_3 <- function (input, intermediate = NULL)
{
  req (input)

  NumberVSATsets <- 0
  NumberVSATsets <- input$Intermediate.NumberVSATsets

  if (!is.null(intermediate))
  {
    NumberVSATsets <- as.numeric (intermediate$NumberVSATsets)
  }

  result =  NumberVSATsets*input$InitialDataSatelite.CostOfInstallVSAT*input$InitialDataSatelite.LaborOfInstallVSAT
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Design cost
formula_2_5_4 <- function (input, intermediate = NULL)
{
  req (input)

  CostOfVSATEqAndMat <- 0
  CostOfVSATEqAndMat <- input$Intermediate.CostOfVSATEqAndMat

  CostOfVSATInstallProcess <- 0
  CostOfVSATInstallProcess <- input$Intermediate.CostOfVSATInstallProcess

  if (!is.null(intermediate))
  {
    CostOfVSATEqAndMat <- as.numeric (intermediate$CostOfVSATEqAndMat)
    CostOfVSATInstallProcess <- as.numeric (intermediate$CostOfVSATInstallProcess)
  }

  CostOfEqCoeff <- as.numeric (input$InitialDataSatelite.CostOfEqCoeff) #0.4
  DesignCoeff <- as.numeric (input$InitialDataSatelite.DesignCoeff) #0.05


  result =  (CostOfEqCoeff*CostOfVSATEqAndMat + CostOfVSATInstallProcess)*DesignCoeff
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of establishing a satellite communication channel
formula_2_5_5 <- function (input, intermediate = NULL)
{
  req (input)

  DesignVSATCost <- 0
  DesignVSATCost <- input$Intermediate.DesignVSATCost

  CostOfVSATEqAndMat <- 0
  CostOfVSATEqAndMat <- input$Intermediate.CostOfVSATEqAndMat

  CostOfVSATInstallProcess <- 0
  CostOfVSATInstallProcess <- input$Intermediate.CostOfVSATInstallProcess

  if (!is.null(intermediate))
  {
    DesignVSATCost <- as.numeric (intermediate$DesignVSATCost)
    CostOfVSATEqAndMat <- as.numeric (intermediate$CostOfVSATEqAndMat)
    CostOfVSATInstallProcess <- as.numeric (intermediate$CostOfVSATInstallProcess)
  }

  result =  DesignVSATCost + CostOfVSATEqAndMat  + CostOfVSATInstallProcess
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

algorithm2_5_impl <- function(input, intermediate = NULL)
{
  # Required number of VSAT sets

  NumberVSATsets =  formula_2_5_1 (input, intermediate)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Required number of VSAT sets, units"), NumberVSATsets, sep = ": "))

  # Total cost of VSAT equipment and installation materials
  intermediate2 <- list (NumberVSATsets = 0)
  intermediate2$NumberVSATsets <- NumberVSATsets

  CostOfVSATEqAndMat =  formula_2_5_2 (input, intermediate2)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of VSAT equipment and installation materials, currency units"), CostOfVSATEqAndMat, sep = ": "))

  # Total cost of installation and configuration of all VSAT sets

  CostOfVSATInstallProcess =  formula_2_5_3 (input, intermediate2)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of installation and configuration of all VSAT sets, currency units"), CostOfVSATInstallProcess, sep = ": "))

  # Design cost
  intermediate3 <- list (CostOfVSATEqAndMat = 0.0, CostOfVSATInstallProcess = 0.0, DesignVSATCost = 0.0 )
  intermediate3$CostOfVSATEqAndMat <- CostOfVSATEqAndMat
  intermediate3$CostOfVSATInstallProcess <- CostOfVSATInstallProcess

  DesignVSATCost =  formula_2_5_4 (input, intermediate3)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Design cost, currency units"), DesignVSATCost, sep = ": "))

  intermediate3$DesignVSATCost <- DesignVSATCost

  # Total cost of establishing a satellite communication channel

  result <- matrix (nrow = 3, ncol = 2)
  result [1,1] = i18n$t("Total cost of establishing a satellite communication channel, currency units")
 # print (.GlobalEnv$mylog)

  result [1,2] = formula_2_5_5 (input, intermediate3)

  result [2,1] = i18n$t("Required number of VSAT sets, units")
  result [2,2] = NumberVSATsets

  result [3,1] = i18n$t("Total cost of VSAT equipment and installation materials, currency units")
  result [3,2] = CostOfVSATEqAndMat


  return (result)
}


algorithm2_5 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)



  switch (input$formula,
          ALL = {

            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataSatelite.ChannelCapacity)
            req (input$InitialDataSatelite.CostOfOneVSAT)
            req (input$InitialDataSatelite.CostOfOneVSATmat)
            req (input$InitialDataSatelite.CostOfInstallVSAT)
            req (input$InitialDataSatelite.LaborOfInstallVSAT)

            req (input$InitialDataSatelite.CostOfEqCoeff) #0.4
            req (input$InitialDataSatelite.DesignCoeff) #0.05


            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

            result <- algorithm2_5_impl (input)

            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

          },
          FORMULA_2_5_1 = {#Required number of VSAT sets

            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataSatelite.ChannelCapacity)

            result <- formula_2_5_1 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_5_2 = {#Total cost of VSAT equipment and installation materials

            req (input$Intermediate.NumberVSATsets)
            req (input$InitialDataSatelite.CostOfOneVSAT)
            req (input$InitialDataSatelite.CostOfOneVSATmat)

            result <- formula_2_5_2 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_5_3 = {#Total cost of installation and configuration of all VSAT sets

            req (input$Intermediate.NumberVSATsets)
            req (input$InitialDataSatelite.CostOfInstallVSAT)
            req (input$InitialDataSatelite.LaborOfInstallVSAT)

            result <- formula_2_5_3 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_5_4 = {#Design cost

            req (input$Intermediate.CostOfVSATEqAndMat)
            req (input$Intermediate.CostOfVSATInstallProcess)

            req (input$InitialDataSatelite.CostOfEqCoeff) #0.4
            req (input$InitialDataSatelite.DesignCoeff) #0.05

            result <- formula_2_5_4 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_5_5 = {#Total cost of establishing a satellite communication channel

            req (input$Intermediate.DesignVSATCost)
            req (input$Intermediate.CostOfVSATEqAndMat)
            req (input$Intermediate.CostOfVSATInstallProcess)

            result <- formula_2_5_5 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          stop ("No!")

  )
}
