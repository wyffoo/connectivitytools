library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Algorithm for determining the total cost of the maintenance of the satellite communication channel 

#The total cost of service of  VSAT
formula_2_6_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberVSATsets <- 0 
  NumberVSATsets <- input$Intermediate.NumberVSATsets
  
  if (!is.null(intermediate))
  {
    NumberVSATsets <- as.numeric (intermediate$NumberVSATsets)
  }
  
  result =  NumberVSATsets*input$InitialDataSatelite.AnnualLaborOfServiceVSAT*input$InitialDataSatelite.CostOfServiceVSAT
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total rent cost for communication channels
formula_2_6_2 <- function (input, intermediate = NULL)
{
  req (input)

  RequiredCapacity <- 0 
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth
  
  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }
  
  if (RequiredCapacity > as.numeric(input$InitialDataSatellite.MaximumLinkCapacity))
    RequiredCapacity <- as.numeric(input$InitialDataSatellite.MaximumLinkCapacity)
  
  
  result =  RequiredCapacity*input$InitialDataSatelite.AnnualRentBand
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#The total cost of maintenance of the satellite channel 
formula_2_6_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  CostOfVSATm <- 0 
  CostOfVSATm <- input$Intermediate.CostOfVSATm
  
  CostOfVSATr <- 0 
  CostOfVSATr <- input$Intermediate.CostOfVSATr
  
  if (!is.null(intermediate))
  {
    CostOfVSATm <- as.numeric (intermediate$CostOfVSATm)
    CostOfVSATr <- as.numeric (intermediate$CostOfVSATr)
  }
  
  result =  CostOfVSATm + CostOfVSATr
  result <- round (as.numeric (result), digits = 2)
  return (result)
  
}

algorithm2_6_impl <- function(input, intermediate = NULL)
{
  
  # The total cost of service of  VSAT
  CostOfVSATm =  formula_2_6_1 (input, intermediate)
  if (!exists ("input$bWriteLog"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The total cost of service of  VSAT, currency units/year"), CostOfVSATm, sep = ": "))  
  
  # Total rent cost for communication channels
  CostOfVSATr =  formula_2_6_2 (input, intermediate)
  if (!exists ("input$bWriteLog"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total rent cost for communication channels, currency units/year"), CostOfVSATr, sep = ": "))  
  
  # The total cost of maintenance of the satellite channel 
  
  intermediate2 <- list (CostOfVSATm = 0.0, CostOfVSATr = 0.0)
  intermediate2$CostOfVSATm <- CostOfVSATm
  intermediate2$CostOfVSATr <- CostOfVSATr
  
  
  result <- matrix (nrow = 2, ncol = 2)
  result [1,1] = i18n$t("Annual cost of maintenance of the satellite channel , currency units/year")
  result [1,2] = formula_2_6_3 (input, intermediate2)
  
  RequiredCapacity <- 0 
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth
  
  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }
  
  result [2,1] = i18n$t("Is required capacity out of maximum possible link capacity for satellite channel, boolean")
  result [2,2] = 0
  
  if (RequiredCapacity > input$InitialDataSatelite.MaximumLinkCapacity)
  {
    result [2,2] = 1
  }
  
  
  return (result) 
}

algorithm2_6 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
 
            req (input$InitialDataSatelite.MaximumLinkCapacity)
            
            req (input$Intermediate.NumberVSATsets)
            req (input$InitialDataSatelite.AnnualLaborOfServiceVSAT)
            req (input$InitialDataSatelite.CostOfServiceVSAT)
            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataSatelite.AnnualRentBand)
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result <- algorithm2_6_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)     
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_2_6_1 = {#The total cost of service of  VSAT
            
            req (input$Intermediate.NumberVSATsets)
            req (input$InitialDataSatelite.AnnualLaborOfServiceVSAT)
            req (input$InitialDataSatelite.CostOfServiceVSAT)
            
            result <- formula_2_6_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_6_2 = {#Total rent cost for communication channels
            
            req (input$Intermediate.NumberVSATsets)
            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataSatelite.AnnualRentBand)
            
            result <- formula_2_6_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_6_3 = {#The total cost of maintenance of the satellite channel 
            
            req (input$Intermediate.CostOfVSATm)
            req (input$Intermediate.CostOfVSATr)
            
            result <- formula_2_6_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
         stop ("No!")
          
  )
}