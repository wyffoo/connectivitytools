library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Algorithm for overall FOCL maintenance cost evaluation

#FOCL maintenance cost along the route
formula_2_2_1 <- function (input, intermediate = NULL)
{
  req (input)

  FOCLLenghtTotal <- 0
  FOCLLenghtTotal <- input$Intermediate.FOCLLenghtTotal


  if (!is.null(intermediate))
  {
    FOCLLenghtTotal <- as.numeric (intermediate$FOCLLenghtTotal)
  }

  result =  FOCLLenghtTotal*input$InitialDataFOCL.AnnualLaborNormFOCLm*input$InitialDataFOCL.CostNormFOCLm
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cable duct maintenance cost
formula_2_2_2 <- function (input, intermediate = NULL)
{
  req (input)

  FOCLSectionLengthCD <- 0
  FOCLSectionLengthCD <- input$Intermediate.FOCLSectionLengthCD


  if (!is.null(intermediate))
  {
    FOCLSectionLengthCD <- as.numeric (intermediate$FOCLSectionLengthCD)
  }

  result =  FOCLSectionLengthCD*input$InitialDataFOCL.AnnualLaborNormCDm*input$InitialDataFOCL.CostNormCDm
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


#Rent cost for communication channels
formula_2_2_3 <- function (input, intermediate = NULL)
{
  req (input)

  RequiredCapacity <- 0
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth

  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }



  result =  RequiredCapacity*input$InitialDataFOCL.AnnualRentBand
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost for FOCL maintenance for the entire period of operation
formula_2_2_4 <- function (input, intermediate = NULL)
{
  req (input)

  AnnualCostFOCLmaintenance <- 0
  AnnualCostFOCLmaintenance <- input$Intermediate.AnnualCostFOCLmaintenance

  AnnualCostCDmaintenance <- 0
  AnnualCostCDmaintenance <- input$Intermediate.AnnualCostCDmaintenance



  CostOfInternetr <- 0
  CostOfInternetr <- input$Intermediate.CostOfInternetr


  if (!is.null(intermediate))
  {
    AnnualCostFOCLmaintenance <- as.numeric (intermediate$AnnualCostFOCLmaintenance)

    AnnualCostCDmaintenance <- as.numeric (intermediate$AnnualCostCDmaintenance)


    CostOfInternetr <- as.numeric (intermediate$CostOfInternetr)
  }

  result =  AnnualCostFOCLmaintenance + AnnualCostCDmaintenance + CostOfInternetr
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

algorithm2_2_impl <- function(input, intermediate = NULL)
{

  #FOCL maintenance cost along the route
  AnnualCostFOCLmaintenance =  formula_2_2_1 (input, intermediate)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("FOCL maintenance cost along the route, currency units/year"), AnnualCostFOCLmaintenance, sep = ": "))

  AnnualCostCDmaintenance <- 0

  if (AnnualCostFOCLmaintenance > 0)
  {
    #Cable duct maintenance cost
    AnnualCostCDmaintenance =  formula_2_2_2 (input, intermediate)

  }

  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cable duct maintenance cost, currency units/year"), AnnualCostCDmaintenance, sep = ": "))

  #Rent cost for communication channels
  CostOfInternetr <- 0
  if (!exists("bIgnoreInternetCost", input))
	CostOfInternetr = formula_2_2_3(input, intermediate)
  else
	if (input$bIgnoreInternetCost == F)
	  CostOfInternetr = formula_2_2_3(input, intermediate)

  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Rent cost for communication channels, currency units/year"), CostOfInternetr, sep = ": "))

  #Total cost for FOCL maintenance for the entire period of operation

  intermediate2 <- list (AnnualCostFOCLmaintenance = 0.0,
                         AnnualCostCDmaintenance = 0.0,
                         CostOfInternetr = 0)
  intermediate2$AnnualCostFOCLmaintenance <- AnnualCostFOCLmaintenance
  intermediate2$AnnualCostCDmaintenance <- AnnualCostCDmaintenance
  intermediate2$CostOfInternetr <- CostOfInternetr

 #print (.GlobalEnv$mylog)

  result <- matrix (nrow = 3, ncol = 2)
  result [1,1] = i18n$t("Anual Cost for FOCL maintenance, currency units/year")
  result [1,2] = formula_2_2_4 (input, intermediate2)

  result [1,2] <- round (as.numeric (result [1,2]), digits = 2)
  
  result [2,1] = i18n$t("Is required capacity out of maximum possible link capacity for FOCL link, boolean")
  result [2,2] = 0 #FOCL is unlimited now 
  
  
  result [3,1] = i18n$t("Rent cost for communication channels, currency units/year")
  if (CostOfInternetr == 0)
    CostOfInternetr <- formula_2_2_3(input, intermediate)
  
  result [3,2] = CostOfInternetr
  
  

  return (result)
}

algorithm2_2 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)

  switch (input$formula,
          ALL = {

            req (input$Intermediate.FOCLLenghtTotal)
            req (input$InitialDataFOCL.AnnualLaborNormFOCLm)
            req (input$InitialDataFOCL.CostNormFOCLm)
            req (input$Intermediate.FOCLSectionLengthCD)
            req (input$InitialDataFOCL.AnnualLaborNormCDm)
            req (input$InitialDataFOCL.CostNormCDm)
            req (input$InitialDataFOCL.AnnualRentBand)
            req (input$SchoolSpecificData.RequiredBandwidth)


            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

            result <- algorithm2_2_impl (input)

            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

          },
          FORMULA_2_2_1 = {#FOCL maintenance cost along the route

            req (input$Intermediate.FOCLLenghtTotal)
            req (input$InitialDataFOCL.AnnualLaborNormFOCLm)
            req (input$InitialDataFOCL.CostNormFOCLm)

            result <- formula_2_2_1 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_2_2 = {#Cable duct maintenance cost

            req (input$Intermediate.FOCLSectionLengthCD)
            req (input$InitialDataFOCL.AnnualLaborNormCDm)
            req (input$InitialDataFOCL.CostNormCDm)

            result <- formula_2_2_2 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_2_3 = {#Rent cost for communication channels (currency units/year)

            req (input$InitialDataFOCL.AnnualRentBand)
            req (input$SchoolSpecificData.RequiredBandwidth)

            result <- formula_2_2_3 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_2_4 = {#Total cost for FOCL maintenance for the entire period of operation

            req (input$Intermediate.AnnualCostFOCLmaintenance)
            req (input$Intermediate.AnnualCostCDmaintenance)
            req (input$Intermediate.CostOfInternetr)

            result <- formula_2_2_4 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          stop ("No!")

  )
}
