library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Algorithm for total RTS maintenance cost evaluation between object and SN in locality

#Total cost for all RTS pylon maintenance
formula_2_4_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfRepeaters <- 0 
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters
  
  NumberOfTerminals <- 0
  NumberOfTerminals <- input$InitialDataRadio.NumberOfTerminals
  
  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
    NumberOfTerminals <- as.numeric (intermediate$NumberOfTerminals)
  }
  
  result =  input$InitialDataRadio.AnnualLaborPylon*input$InitialDataRadio.CostLaborPylonm*
    (NumberOfRepeaters+ NumberOfTerminals)
  
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

#Total cost for all RTS antenna feeder devices maintenance
formula_2_4_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfRepeaters <- 0 
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters

  NumberOfTerminals <- 0
  NumberOfTerminals <- input$InitialDataRadio.NumberOfTerminals
  
  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
    NumberOfTerminals <- as.numeric (intermediate$NumberOfTerminals)
  }


  result =  input$InitialDataRadio.AnnualLaborAFD*input$InitialDataRadio.CostLaborAFDm*(NumberOfRepeaters + NumberOfTerminals)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost for RTS internal devices maintenance
formula_2_4_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfRepeaters <- 0 
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters
  
  NumberOfTerminals <- 0
  NumberOfTerminals <- input$InitialDataRadio.NumberOfTerminals
  
  
  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
    NumberOfTerminals <- as.numeric (intermediate$NumberOfTerminals)
  }
  
  result =  input$InitialDataRadio.AnnualLaborRTS*input$InitialDataRadio.CostLaborRTSm*(NumberOfRepeaters+NumberOfTerminals)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Rent cost for communication channels
formula_2_4_4 <- function (input, intermediate = NULL)
{
  req (input)
  
  RequiredCapacity <- 0 
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth
  
  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }
  

  if (RequiredCapacity > input$InitialDataRadio.MaximumLinkCapacity)
    RequiredCapacity <- input$InitialDataRadio.MaximumLinkCapacity
  

  result =  RequiredCapacity*input$InitialDataRadio.AnnualRentBand
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Annual spectrum licensing cost per transmitter (end-terminals and repeaters)
formula_2_4_5 <- function (input, intermediate = NULL)
{
  req (input)
  

  WidthFrequencyChannel <- 0
  WidthFrequencyChannel <- input$Intermediate.WidthFrequencyChannel

  if (!is.null(intermediate))
  {
    WidthFrequencyChannel <- as.numeric (intermediate$WidthFrequencyChannel)
  }
  
  result =  as.numeric (input$InitialDataRadio.AnnualSpectrumFee) * WidthFrequencyChannel
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Annual spectrum licensing cost for whole channel
formula_2_4_6 <- function (input, intermediate = NULL)
{
  req (input)
  
  AnnualSpectrumCostPerTransmitter <- 0
  AnnualSpectrumCostPerTransmitter <- input$Intermediate.AnnualSpectrumCostPerTransmitter

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters
  
  if (!is.null(intermediate))
  {
    AnnualSpectrumCostPerTransmitter <- as.numeric (intermediate$AnnualSpectrumCostPerTransmitter)
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }

  result =  (as.numeric (input$InitialDataRadio.NumberOfTerminals) + NumberOfRepeaters)*AnnualSpectrumCostPerTransmitter
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


#Total cost of RTS maintenance
formula_2_4_7 <- function (input, intermediate = NULL)
{
  req (input)
  
  TotalCostAllRTSm <- 0 
  TotalCostAllRTSm <- input$Intermediate.TotalCostAllRTSm
  
  TotalCostAllAFDm <- 0 
  TotalCostAllAFDm <- input$Intermediate.TotalCostAllAFDm
  
  TotalCostAllInternalDm <- 0 
  TotalCostAllInternalDm <- input$Intermediate.TotalCostAllInternalDm
  
  CostOfInternetr <- 0 
  CostOfInternetr <- input$Intermediate.CostOfInternetr
  
  AnnualSpectrumLicenseCost <- 0 
  AnnualSpectrumLicenseCost <- input$Intermediate.AnnualSpectrumLicenseCost

  
  if (!is.null(intermediate))
  {
    TotalCostAllRTSm <- as.numeric (intermediate$TotalCostAllRTSm)
    TotalCostAllAFDm <- as.numeric (intermediate$TotalCostAllAFDm)
    TotalCostAllInternalDm <- as.numeric (intermediate$TotalCostAllInternalDm)
    CostOfInternetr <- as.numeric (intermediate$CostOfInternetr)
    AnnualSpectrumLicenseCost <- as.numeric (intermediate$AnnualSpectrumLicenseCost)
  }
  
  
  result =  TotalCostAllRTSm+TotalCostAllAFDm+TotalCostAllInternalDm+CostOfInternetr+AnnualSpectrumLicenseCost
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


algorithm2_4_impl <- function(input, intermediate = NULL)
{
  #Total cost for all RTS pylon maintenance
  TotalCostAllRTSm =  formula_2_4_1 (input, intermediate)
  if (!exists ("input$bWriteLog"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost for all RTS pylon maintenance, currency units/year"), TotalCostAllRTSm, sep = ": "))  
  
  #Total cost for all RTS antenna feeder devices maintenance
  TotalCostAllAFDm =  formula_2_4_2 (input, intermediate)
  if (!exists ("input$bWriteLog"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost for all RTS antenna feeder devices maintenance, currency units/year"), TotalCostAllAFDm, sep = ": "))  
  
  #Total cost for RTS internal devices maintenance
  TotalCostAllInternalDm =  formula_2_4_3 (input, intermediate)
  if (!exists ("input$bWriteLog"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost for RTS internal devices maintenance, currency units/year"), TotalCostAllInternalDm, sep = ": "))  
  
  #Rent cost for communication channels
  CostOfInternetr <- 0
  if (!exists("bIgnoreInternetCost", input))
    CostOfInternetr = formula_2_4_4(input, intermediate)
  else
    if (input$bIgnoreInternetCost == F)
      CostOfInternetr = formula_2_4_4(input, intermediate)
  
  if (!exists ("input$bWriteLog"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Rent cost for communication channels, currency units/year"), CostOfInternetr, sep = ": "))  
  
  # Annual spectrum licensing cost per transmitter (end-terminals and repeaters)
  AnnualSpectrumCostPerTransmitter = formula_2_4_5 (input, intermediate)
  if (!exists ("input$bWriteLog"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Annual spectrum licensing cost per transmitter (end-terminals and repeaters), currency units/year"), AnnualSpectrumCostPerTransmitter, sep = ": "))  

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters
  
  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }
  
  intermediate2 <- list (TotalCostAllRTSm = 0.0, TotalCostAllAFDm = 0.0, TotalCostAllInternalDm = 0.0, CostOfInternetr = 0, AnnualSpectrumCostPerTransmitter = 0.0, AnnualSpectrumLicenseCost = 0.0)
  intermediate2$TotalCostAllRTSm <- TotalCostAllRTSm
  intermediate2$TotalCostAllAFDm <- TotalCostAllAFDm
  intermediate2$TotalCostAllInternalDm  <- TotalCostAllInternalDm            
  intermediate2$CostOfInternetr <- CostOfInternetr
  intermediate2$AnnualSpectrumCostPerTransmitter <- AnnualSpectrumCostPerTransmitter
  intermediate2$NumberOfRepeaters <- NumberOfRepeaters
    
  # Annual spectrum licensing cost for whole channel
  AnnualSpectrumLicenseCost = formula_2_4_6 (input, intermediate2)
  if (!exists ("input$bWriteLog"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Annual spectrum licensing cost for whole channel, currency units/year"), AnnualSpectrumLicenseCost, sep = ": "))  

  intermediate2$AnnualSpectrumLicenseCost <- AnnualSpectrumLicenseCost
  #Total cost of RTS maintenance
  
  
  
  result <- matrix (nrow = 3, ncol = 2)
  result [1,1] = i18n$t("Annual cost of RTS maintenance, currency units/year")
  result [1,2] = formula_2_4_7 (input, intermediate2)
  
  RequiredCapacity <- 0 
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth
  
  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }
  
  result [2,1] = i18n$t("Is required capacity out of maximum possible link capacity for RTS link, boolean")
  result [2,2] = 0
  
  if (RequiredCapacity > input$InitialDataRadio.MaximumLinkCapacity)
  {
    result [2,2] = 1
  }

  result [3,1] = i18n$t("Rent cost for communication channels, currency units/year")
  
  if (CostOfInternetr == 0)
    CostOfInternetr <- formula_2_4_4(input, intermediate)
  result [3,2] = CostOfInternetr
  
  
  return (result)
}

algorithm2_4 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            req (input$InitialDataRadio.MaximumLinkCapacity)
            
            req (input$InitialDataRadio.AnnualLaborPylon)
            req (input$InitialDataRadio.CostLaborPylonm)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)
            req (input$InitialDataRadio.AnnualLaborAFD)
            req (input$InitialDataRadio.CostLaborAFDm)
            req (input$InitialDataRadio.AnnualLaborRTS)
            req (input$InitialDataRadio.CostLaborRTSm)
            req (input$InitialDataRadio.AnnualRentBand)
            req (input$SchoolSpecificData.RequiredBandwidth)
            
            req (input$Intermediate.WidthFrequencyChannel)
            req (input$InitialDataRadio.AnnualSpectrumFee)
            
            
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result <- algorithm2_4_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)       
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_2_4_1 = {#Total cost for all RTS pylon maintenance
            
            req (input$InitialDataRadio.AnnualLaborPylon)
            req (input$InitialDataRadio.CostLaborPylonm)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)
            
            
            result <- formula_2_4_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_4_2 = {#Total cost for all RTS antenna feeder devices maintenance
            
            req (input$InitialDataRadio.AnnualLaborAFD)
            req (input$InitialDataRadio.CostLaborAFDm)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)
            
            result <- formula_2_4_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_4_3 = {#Total cost for RTS internal devices maintenance
            req (input$InitialDataRadio.AnnualLaborRTS)
            req (input$InitialDataRadio.CostLaborRTSm)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)
            
            result <- formula_2_4_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_4_4 = {#Rent cost for communication channels (currency units/year)
            
            req (input$InitialDataRadio.AnnualRentBand)
            req (input$SchoolSpecificData.RequiredBandwidth)
            
            result <- formula_2_4_4 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_4_5 = {#Annual spectrum licensing cost per transmitter (end-terminals and repeaters)
            req (input$Intermediate.WidthFrequencyChannel)
            req (input$InitialDataRadio.AnnualSpectrumFee)
            

            result <- formula_2_4_5 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_4_6 = {#Annual spectrum licensing cost for whole channel
            req (input$Intermediate.AnnualSpectrumCostPerTransmitter)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)
            
            
            result <- formula_2_4_6 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },          
          FORMULA_2_4_7 = {#Total cost of RTS maintenance
            req (input$Intermediate.TotalCostAllRTSm)
            req (input$Intermediate.TotalCostAllAFDm)
            req (input$Intermediate.TotalCostAllInternalDm)
            req (input$Intermediate.CostOfInternetr)
            req (input$Intermediate.AnnualSpectrumLicenseCost)
            
            result <- formula_2_4_7 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          stop ("No!")
          
  )
}