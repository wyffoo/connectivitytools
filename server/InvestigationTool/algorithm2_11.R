
library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")


#Algorithm for determining the total cost of the maintenance of the cellular communication channel 

#The total cost of service of  Cellular
formula_2_11_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberCellularsets <- 0 
  NumberCellularsets <- input$Intermediate.NumberCellularsets
  
  if (!is.null(intermediate))
  {
    NumberCellularsets <- as.numeric (intermediate$NumberCellularsets)
  }
  
  result =  NumberCellularsets*input$InitialDataCellular.AnnualLaborOfServiceCellular*input$InitialDataCellular.CostOfServiceCellular
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total rent cost for communication channels
formula_2_11_2 <- function (input, intermediate = NULL)
{
  req (input)

  RequiredCapacity <- 0 
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth
  
  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }
  
  if (RequiredCapacity > input$InitialDataCellular.MaximumLinkCapacity)
    RequiredCapacity <- input$InitialDataCellular.MaximumLinkCapacity
  
  
  result =  RequiredCapacity*input$InitialDataCellular.AnnualRentBand
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#The total cost of maintenance of the cellular channel 
formula_2_11_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  CostOfCellularm <- 0 
  CostOfCellularm <- input$Intermediate.CostOfCellularm
  
  CostOfCellularr <- 0 
  CostOfCellularr <- input$Intermediate.CostOfCellularr
  
  if (!is.null(intermediate))
  {
    CostOfCellularm <- as.numeric (intermediate$CostOfCellularm)
    CostOfCellularr <- as.numeric (intermediate$CostOfCellularr)
  }
  
  result =  CostOfCellularm + CostOfCellularr
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

algorithm2_11_impl <- function(input, intermediate = NULL)
{
  
  # The total cost of service of  Cellular
  CostOfCellularm =  formula_2_11_1 (input, intermediate)
  if (!exists ("input$bWriteLog"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The total cost of service of  Cellular, currency units/year"), CostOfCellularm, sep = ": "))  
  
  # Total rent cost for communication channels
  CostOfCellularr =  formula_2_11_2 (input, intermediate)
  if (!exists ("input$bWriteLog"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total rent cost for communication channels, currency units/year"), CostOfCellularr, sep = ": "))  
  
  # The total cost of maintenance of the cellular channel 
  
  intermediate2 <- list (CostOfCellularm = 0.0, CostOfCellularr = 0.0)
  intermediate2$CostOfCellularm <- CostOfCellularm
  intermediate2$CostOfCellularr <- CostOfCellularr
  
  
  result <- matrix (nrow = 2, ncol = 2)
  result [1,1] = i18n$t("Annual cost of maintenance of the cellular channel , currency units/year")
  result [1,2] = formula_2_11_3 (input, intermediate2)
  
  RequiredCapacity <- 0 
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth
  
  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }
  
  result [2,1] = i18n$t("Is required capacity out of maximum possible link capacity for cellular basestation, boolean")
  result [2,2] = 0
  
  if (RequiredCapacity > input$InitialDataCellular.MaximumLinkCapacity)
  {
    result [2,2] = 1
  }
  
  
  
  return (result) 
}

algorithm2_11 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            req (input$InitialDataCellular.MaximumLinkCapacity)
            
            req (input$Intermediate.NumberCellularsets)
            req (input$InitialDataCellular.AnnualLaborOfServiceCellular)
            req (input$InitialDataCellular.CostOfServiceCellular)
            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataCellular.AnnualRentBand)
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result <- algorithm2_11_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)     
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_2_11_1 = {#The total cost of service of  Cellular
            
            req (input$Intermediate.NumberCellularsets)
            req (input$InitialDataCellular.AnnualLaborOfServiceCellular)
            req (input$InitialDataCellular.CostOfServiceCellular)
            
            
            result <- formula_2_11_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_11_2 = {#Total rent cost for communication channels
            
            req (input$Intermediate.NumberCellularsets)
            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataCellular.AnnualRentBand)
            

            result <- formula_2_11_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_11_3 = {#The total cost of maintenance of the cellular channel 
            
            req (input$Intermediate.CostOfCellularm)
            req (input$Intermediate.CostOfCellularr)
            
            
            result <- formula_2_11_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
         stop ("No!")
          
  )
}