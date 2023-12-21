library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Cost of operating an access network

#Determining the volume of annual labor costs for the operation of AOEs
formula_1_7_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfAOEs <- 0 
  NumberOfAOEs <- input$Intermediate.NumberOfAOEs
  
  
  if (!is.null(intermediate))
  {
    NumberOfAOEs <- as.numeric (intermediate$NumberOfAOEs)
  }
  
  result = NumberOfAOEs*input$AccessTechnologyOptionsSet.LaborCostsActiveEquipment 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determination of electricity consumption by access organization equipment
formula_1_7_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfAOEs <- 0 
  NumberOfAOEs <- input$Intermediate.NumberOfAOEs
  
  
  if (!is.null(intermediate))
  {
    NumberOfAOEs <- as.numeric (intermediate$NumberOfAOEs)
  }
  
  result = NumberOfAOEs*input$AccessTechnologyOptionsSet.PowerConsumptionEOA 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the cost of operating access organization equipment
formula_1_7_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  AnnualLaborCostAOEsOperation <- 0 
  AnnualLaborCostAOEsOperation <- input$Intermediate.AnnualLaborCostAOEsOperation


  ElectricityConsuptionsbyAOEs   <- 0 
  ElectricityConsuptionsbyAOEs   <- input$Intermediate.ElectricityConsuptionsbyAOEs  
  
  
  
  if (!is.null(intermediate))
  {
    AnnualLaborCostAOEsOperation <- as.numeric (intermediate$AnnualLaborCostAOEsOperation)
    ElectricityConsuptionsbyAOEs   <- as.numeric (intermediate$ElectricityConsuptionsbyAOEs  )
  }
  
  result = AnnualLaborCostAOEsOperation*input$AccessTechnologyOptionsSet.CostServiceEequipment + ElectricityConsuptionsbyAOEs*input$AccessTechnologyOptionsSet.CostElectricity
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the volume of annual labor costs for the operation of the TAOEs
formula_1_7_4 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfTAOEs <- 0 
  NumberOfTAOEs <- input$Intermediate.NumberOfTAOEs
  
  
  if (!is.null(intermediate))
  {
    NumberOfTAOEs <- as.numeric (intermediate$NumberOfTAOEs)
  }
  
  result = NumberOfTAOEs*input$AccessTechnologyOptionsSet.LaborCostsActiveEquipment 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determination of electricity consumption by technical access organization equipment
formula_1_7_5 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfTAOEs <- 0 
  NumberOfTAOEs <- input$Intermediate.NumberOfTAOEs
  
  
  if (!is.null(intermediate))
  {
    NumberOfTAOEs <- as.numeric (intermediate$NumberOfTAOEs)
  }
  
  result = NumberOfTAOEs*input$AccessTechnologyOptionsSet.PowerConsumptionTEOA 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the cost of operating technical access organization equipment
formula_1_7_6 <- function (input, intermediate = NULL)
{
  req (input)
  
  AnnualLaborCostTAOEsOperation <- 0 
  AnnualLaborCostTAOEsOperation <- input$Intermediate.AnnualLaborCostTAOEsOperation
  
  ElectricityConsuptionsbyTAOEs <- 0 
  ElectricityConsuptionsbyTAOEs <- input$Intermediate.ElectricityConsuptionsbyTAOEs
  
    
  if (!is.null(intermediate))
  {
    AnnualLaborCostTAOEsOperation <- as.numeric (intermediate$AnnualLaborCostTAOEsOperation)
    ElectricityConsuptionsbyTAOEs <- as.numeric (intermediate$ElectricityConsuptionsbyTAOEs)
  }
  
  result = AnnualLaborCostTAOEsOperation*input$AccessTechnologyOptionsSet.CostServiceEequipment + ElectricityConsuptionsbyTAOEs*input$AccessTechnologyOptionsSet.CostElectricity
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the volume of annual labor costs for servicing subscriber lines inside buildings
formula_1_7_7 <- function (input, intermediate = NULL)
{
  req (input)
  
  LinesInBuildingsLength <- 0 
  LinesInBuildingsLength <- input$Intermediate.LinesInBuildingsLength
  
  
  if (!is.null(intermediate))
  {
    LinesInBuildingsLength <- as.numeric (intermediate$LinesInBuildingsLength)
  }
  
  result = LinesInBuildingsLength*input$AccessTechnologyOptionsSet.LaborCostsLengthInhouseSubscriberLines 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the cost of servicing subscriber lines inside buildings
formula_1_7_8 <- function (input, intermediate = NULL)
{
  req (input)
  
  AnnualLaborCostSLOperation <- 0 
  AnnualLaborCostSLOperation <- as.numeric (input$Intermediate.AnnualLaborCostSLOperation)
  

  if (!is.null(intermediate))
  {
    AnnualLaborCostSLOperation <- as.numeric (intermediate$AnnualLaborCostSLOperation)
  }
  
  
  result = AnnualLaborCostSLOperation * as.numeric (input$AccessTechnologyOptionsSet.CostServiceCableInfrastructure) 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the volume of annual labor costs for servicing hanged-up lines
formula_1_7_9 <- function (input, intermediate = NULL)
{
  req (input)
  
  LenghtHangedupCalbesSL <- 0 
  LenghtHangedupCalbesSL <- input$Intermediate.LenghtHangedupCalbesSL
  
  LenghtHangedupCalbesCL <- 0 
  LenghtHangedupCalbesCL <- input$Intermediate.LenghtHangedupCalbesCL
  
  
  if (!is.null(intermediate))
  {
    LenghtHangedupCalbesSL <- as.numeric (intermediate$LenghtHangedupCalbesSL)
    LenghtHangedupCalbesCL <- as.numeric (intermediate$LenghtHangedupCalbesCL)
  }
  
  result = (LenghtHangedupCalbesSL + LenghtHangedupCalbesCL)*input$AccessTechnologyOptionsSet.LaborCostsServiceSuspensionCable
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the cost of servicing hanged-up lines
formula_1_7_10 <- function (input, intermediate = NULL)
{
  req (input)
  
  AnnualLaborCostHangedCableOperation <- 0 
  AnnualLaborCostHangedCableOperation <- input$Intermediate.AnnualLaborCostHangedCableOperation
  
  
  if (!is.null(intermediate))
  {
    AnnualLaborCostHangedCableOperation <- as.numeric (intermediate$AnnualLaborCostHangedCableOperation)
  }
  
  result = AnnualLaborCostHangedCableOperation*input$AccessTechnologyOptionsSet.LaborCostsServiceCableSewerage 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the volume of annual labor costs for servicing lines in cable duct
formula_1_7_11 <- function (input, intermediate = NULL)
{
  req (input)
  
  LenghtSLInExternalDuct <- 0 
  LenghtSLInExternalDuct <- input$Intermediate.LenghtSLInExternalDuct
  
  LenghtCLInExternalDuct <- 0 
  LenghtCLInExternalDuct <- input$Intermediate.LenghtCLInExternalDuct
  
    
  if (!is.null(intermediate))
  {
    LenghtSLInExternalDuct <- as.numeric (intermediate$LenghtSLInExternalDuct)
    LenghtCLInExternalDuct <- as.numeric (intermediate$LenghtCLInExternalDuct)
  }
  
  result = (LenghtSLInExternalDuct + LenghtCLInExternalDuct)*input$DevelopmentParametersSet.NormativeAnnualRentOfCableDuct
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the cost of servicing lines in cable duct
formula_1_7_12 <- function (input, intermediate = NULL)
{
  req (input)
  
  AnnualLaborCostCableInDuctOperation <- 0 
  AnnualLaborCostCableInDuctOperation <- input$Intermediate.AnnualLaborCostCableInDuctOperation
  
  
  if (!is.null(intermediate))
  {
    AnnualLaborCostCableInDuctOperation <- as.numeric (intermediate$AnnualLaborCostCableInDuctOperation)
  }
  
  result = AnnualLaborCostCableInDuctOperation*input$AccessTechnologyOptionsSet.CostServiceCableInfrastructure 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the volume of annual labor costs for servicing new infrastructure for hanging-up cables (pillars)
formula_1_7_13 <- function (input, intermediate = NULL)
{
  req (input)
  
  LengthOfNewInfrastractureForCL <- 0 
  LengthOfNewInfrastractureForCL <- input$Intermediate.LengthOfNewInfrastractureForCL
  
  LengthOfNewInfrastractureForSL <- 0 
  LengthOfNewInfrastractureForSL <- input$Intermediate.LengthOfNewInfrastractureForSL
  
    
  if (!is.null(intermediate))
  {
    LengthOfNewInfrastractureForCL <- as.numeric (intermediate$LengthOfNewInfrastractureForCL)
    LengthOfNewInfrastractureForSL <- as.numeric (intermediate$LengthOfNewInfrastractureForSL)
  }
  
  result = (LengthOfNewInfrastractureForCL + LengthOfNewInfrastractureForSL)*input$AccessTechnologyOptionsSet.LaborCostsServicePillars
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the cost of servicing new infrastructure for hanging-up cables (pillars)
formula_1_7_14 <- function (input, intermediate = NULL)
{
  req (input)
  
  AnnualLaborCostNewInfrastructure <- 0 
  AnnualLaborCostNewInfrastructure <- input$Intermediate.AnnualLaborCostNewInfrastructure
  
  
  if (!is.null(intermediate))
  {
    AnnualLaborCostNewInfrastructure <- as.numeric (intermediate$AnnualLaborCostNewInfrastructure)
  }
  
  result = AnnualLaborCostNewInfrastructure * input$DevelopmentParametersSet.CostOfOperationNewInfrastructure 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the cost of rent of place in the external cable duct for the cable
formula_1_7_15 <- function (input, intermediate = NULL)
{
  req (input)
  
  LenghtSLInExternalDuct <- 0 
  LenghtSLInExternalDuct <- input$Intermediate.LenghtSLInExternalDuct
  
  LenghtCLInExternalDuct <- 0 
  LenghtCLInExternalDuct <- input$Intermediate.LenghtCLInExternalDuct
  
    
  if (!is.null(intermediate))
  {
    LenghtSLInExternalDuct <- as.numeric (intermediate$LenghtSLInExternalDuct)
    LenghtCLInExternalDuct <- as.numeric (intermediate$LenghtCLInExternalDuct)
  }
  
  result = (LenghtSLInExternalDuct + LenghtCLInExternalDuct)*input$DevelopmentParametersSet.RentCostForCableInDuct
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the cost of the rent of place on the pillars for the hanged-up cable
formula_1_7_16 <- function (input, intermediate = NULL)
{
  req (input)
  
  LenghtHangedupCalbesSL <- 0 
  LenghtHangedupCalbesSL <- input$Intermediate.LenghtHangedupCalbesSL
  
  LenghtHangedupCalbesCL <- 0 
  LenghtHangedupCalbesCL <- input$Intermediate.LenghtHangedupCalbesCL
  
  LengthOfNewInfrastractureForSL <- 0 
  LengthOfNewInfrastractureForSL <- input$Intermediate.LengthOfNewInfrastractureForSL
  
    
  if (!is.null(intermediate))
  {
    LenghtHangedupCalbesSL <- as.numeric (intermediate$LenghtHangedupCalbesSL)
    LenghtHangedupCalbesCL <- as.numeric (intermediate$LenghtHangedupCalbesCL)
    LengthOfNewInfrastractureForSL <- as.numeric (intermediate$LengthOfNewInfrastractureForSL)
  }
  
  result = (LenghtHangedupCalbesSL + LenghtHangedupCalbesCL - 
              LengthOfNewInfrastractureForSL)*input$DevelopmentParametersSet.RentCostForHangedCable
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


algorithm1_7_impl <- function(input, intermediate = NULL)
{
  #Determining the volume of annual labor costs for the operation of AOEs            
  AnnualLaborCostAOEsOperation = formula_1_7_1 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Volume of annual labor costs for the operation of AOEs, hours/year"), AnnualLaborCostAOEsOperation, sep = ": "))                            
  
  #Determination of electricity consumption by access organization equipment
  ElectricityConsuptionsbyAOEs = formula_1_7_2 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Electricity consumption by access organization equipment, kWt*hour/year"), ElectricityConsuptionsbyAOEs, sep = ": "))                            
  
  #Determining the cost of operating access organization equipment
  intermediate2 <- list (AnnualLaborCostAOEsOperation = 0.0, ElectricityConsuptionsbyAOEs = 0.0)
  intermediate2$AnnualLaborCostAOEsOperation <- AnnualLaborCostAOEsOperation
  intermediate2$ElectricityConsuptionsbyAOEs <- ElectricityConsuptionsbyAOEs
  
  CostOperAOEs = formula_1_7_3 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of operating access organization equipment, currency units/year"), CostOperAOEs, sep = ": "))                            
  
  #Determining the volume of annual labor costs for the operation of the TAOEs
  AnnualLaborCostTAOEsOperation = formula_1_7_4 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Volume of annual labor costs for the operation of the TAOEs, hours/year"), AnnualLaborCostTAOEsOperation, sep = ": "))                            
  
  #Determination of electricity consumption by technical access organization equipment            
  ElectricityConsuptionsbyTAOEs = formula_1_7_5 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Electricity consumption by technical access organization equipment, kWt*hour/year"), ElectricityConsuptionsbyTAOEs, sep = ": "))                                        
  
  #Determining the cost of operating technical access organization equipment
  intermediate2 <- list (AnnualLaborCostTAOEsOperation = 0.0, ElectricityConsuptionsbyTAOEs = 0.0)
  intermediate2$AnnualLaborCostTAOEsOperation <- AnnualLaborCostTAOEsOperation
  intermediate2$ElectricityConsuptionsbyTAOEs <- ElectricityConsuptionsbyTAOEs
  
  CostOperTAOEs = formula_1_7_6 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of operating technical access organization equipment, currency units/year"), CostOperTAOEs, sep = ": "))                            
  
  #Determination of equipment operating costs
  CostOperEq = CostOperAOEs + CostOperTAOEs
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Equipment operating costs, currency units/year"), CostOperEq, sep = ": "))                            
  
  
  #Determining the volume of annual labor costs for servicing subscriber lines inside buildings
  AnnualLaborCostSLOperation = formula_1_7_7 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Volume of annual labor costs for servicing subscriber lines inside buildings, hours/year"), AnnualLaborCostSLOperation, sep = ": "))                            
  
  #Determining the cost of servicing subscriber lines inside buildings
  intermediate2 <- list (AnnualLaborCostSLOperation = 0.0)
  intermediate2$AnnualLaborCostSLOperation <- AnnualLaborCostSLOperation
  
  CostOperSLInBuild = formula_1_7_8 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of servicing subscriber lines inside buildings, currency units/year"), CostOperSLInBuild, sep = ": "))                            
  
  #Determining the volume of annual labor costs for servicing hanged-up lines
  AnnualLaborCostHangedCableOperation = formula_1_7_9 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Volume of annual labor costs for servicing hanged-up lines, hours/year"), AnnualLaborCostHangedCableOperation, sep = ": "))                            
  
  #Determining the cost of servicing hanged-up lines
  intermediate2 <- list (AnnualLaborCostHangedCableOperation = 0.0)
  intermediate2$AnnualLaborCostHangedCableOperation <- AnnualLaborCostHangedCableOperation
  
  CostOperHangUpLines = formula_1_7_10 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of servicing hanged-up lines, currency units/year"), CostOperHangUpLines, sep = ": "))                            
  
  #Determining the volume of annual labor costs for servicing lines in cable duct
  AnnualLaborCostCableInDuctOperation = formula_1_7_11 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Volume of annual labor costs for servicing lines in cable duct, hours/year"), AnnualLaborCostCableInDuctOperation, sep = ": "))                            
  
  #Determining the cost of servicing lines in cable duct
  intermediate2 <- list (AnnualLaborCostCableInDuctOperation = 0.0)
  intermediate2$AnnualLaborCostCableInDuctOperation <- AnnualLaborCostCableInDuctOperation
  
  CostOperLinesInDuct = formula_1_7_12 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of servicing lines in cable duct, currency units/year"), CostOperLinesInDuct, sep = ": "))                            
  
  #Determining the volume of annual labor costs for servicing new infrastructure for hanging-up cables (pillars)
  AnnualLaborCostNewInfrastructure = formula_1_7_13 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Determining the volume of annual labor costs for servicing new infrastructure for hanging-up cables (pillars), hours/year"), AnnualLaborCostNewInfrastructure, sep = ": "))                            
  
  #Determining the cost of servicing new infrastructure for hanging-up cables (pillars)
  intermediate2 <- list (AnnualLaborCostNewInfrastructure = 0.0)
  intermediate2$AnnualLaborCostNewInfrastructure <- AnnualLaborCostNewInfrastructure
  
  CostOperNewInfrastructure = formula_1_7_14 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of servicing new infrastructure for hanging-up cables (pillars), currency units/year"), CostOperNewInfrastructure, sep = ": "))                            
  
  #Determination of costs of operating communication lines and new infrastructure for hanging-up cables
  CostOperLines = CostOperSLInBuild + CostOperHangUpLines + 
    CostOperLinesInDuct + CostOperNewInfrastructure
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Costs of operating communication lines and new infrastructure for hanging-up cables, currency units/year"), CostOperLines, sep = ": "))                            
  
  
  #Determining the cost of rent of place in the external cable duct for the cable
  CostOperRentDuct = formula_1_7_15 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of rent of place in the external cable duct for the cable, currency units/year"), CostOperRentDuct, sep = ": "))                            
  
  
  #Determining the cost of the rent of place on the pillars for the hanged-up cable
  CostOperRentPillars = formula_1_7_16 (input, intermediate)
  
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of the rent of place on the pillars for the hanged-up cable, currency units/year"), CostOperRentPillars, sep = ": "))                            
  
  #Determination of costs of the annual operation of an access network 

  
  result <- matrix (nrow = 1, ncol = 2)
  
  result [1,1] = i18n$t("Costs of the annual operation of an access network , currency units/year")
  result [1,2] = CostOperEq + CostOperLines + CostOperRentDuct +  CostOperRentPillars
  
  return (result)
}

algorithm1_7 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            req (input$Intermediate.NumberOfAOEs)
            req (input$AccessTechnologyOptionsSet.LaborCostsActiveEquipment)
            req (input$AccessTechnologyOptionsSet.PowerConsumptionEOA)
            req (input$Intermediate.AnnualLaborCostAOEsOperation)
            req (input$AccessTechnologyOptionsSet.CostServiceEequipment)
            req (input$Intermediate.ElectricityConsuptionsbyAOEs)
            req (input$AccessTechnologyOptionsSet.CostElectricity)
            req (input$Intermediate.NumberOfTAOEs)
            req (input$AccessTechnologyOptionsSet.PowerConsumptionTEOA)
            req (input$Intermediate.AnnualLaborCostTAOEsOperation)
            req (input$Intermediate.ElectricityConsuptionsbyTAOEs)
            req (input$AccessTechnologyOptionsSet.LaborCostsLengthInhouseSubscriberLines)
            req (input$Intermediate.AnnualLaborCostSLOperation)
            req (input$AccessTechnologyOptionsSet.CostServiceCableInfrastructure)
            req (input$Intermediate.LenghtHangedupCalbesSL)
            req (input$Intermediate.LenghtHangedupCalbesCL)
            req (input$AccessTechnologyOptionsSet.LaborCostsServiceSuspensionCable)
            req (input$Intermediate.AnnualLaborCostHangedCableOperation)
            req (input$AccessTechnologyOptionsSet.LaborCostsServiceCableSewerage)
            req (input$Intermediate.LenghtSLInExternalDuct)
            req (input$Intermediate.LenghtCLInExternalDuct)
            req (input$DevelopmentParametersSet.NormativeAnnualRentOfCableDuct)
            req (input$Intermediate.AnnualLaborCostCableInDuctOperation)
            req (input$Intermediate.LengthOfNewInfrastractureForCL)
            req (input$Intermediate.LengthOfNewInfrastractureForSL)
            req (input$AccessTechnologyOptionsSet.LaborCostsServicePillars)
            req (input$Intermediate.AnnualLaborCostNewInfrastructure)
            req (input$DevelopmentParametersSet.CostOfOperationNewInfrastructure)
            req (input$DevelopmentParametersSet.RentCostForCableInDuct)
            req (input$DevelopmentParametersSet.RentCostForHangedCable)
            req (input$Intermediate.LinesInBuildingsLength)            
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result <- algorithm1_7_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_1_7_1 = {#Determining the volume of annual labor costs for the operation of AOEs
            req (input$Intermediate.NumberOfAOEs)
            req (input$AccessTechnologyOptionsSet.LaborCostsActiveEquipment)
            
            result <- formula_1_7_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_1_7_2 = {#Determination of electricity consumption by access organization equipment
            req (input$Intermediate.NumberOfAOEs)
            req (input$AccessTechnologyOptionsSet.PowerConsumptionEOA)
            
            result <- formula_1_7_2 (input)
              
              output$data <- renderTable(result, colnames=FALSE)
            
            
          },
          FORMULA_1_7_3 = {#Determining the cost of operating access organization equipment
            req (input$Intermediate.AnnualLaborCostAOEsOperation)
            req (input$AccessTechnologyOptionsSet.CostServiceEequipment)
            req (input$Intermediate.ElectricityConsuptionsbyAOEs)
            req (input$AccessTechnologyOptionsSet.CostElectricity)
            
            result <- formula_1_7_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_4 = {#Determining the volume of annual labor costs for the operation of the TAOEs
            req (input$Intermediate.NumberOfTAOEs)
            req (input$AccessTechnologyOptionsSet.LaborCostsActiveEquipment)
            
            result <- formula_1_7_4 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
            
          },
          FORMULA_1_7_5 = {#Determination of electricity consumption by technical access organization equipment
            req (input$Intermediate.NumberOfTAOEs)
            req (input$AccessTechnologyOptionsSet.PowerConsumptionTEOA)
            
            result <- formula_1_7_5 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_6 = {#Determining the cost of operating technical access organization equipment
            req (input$Intermediate.AnnualLaborCostTAOEsOperation)
            req (input$AccessTechnologyOptionsSet.CostServiceEequipment)
            req (input$Intermediate.ElectricityConsuptionsbyTAOEs)
            req (input$AccessTechnologyOptionsSet.CostElectricity)
            
            result <- formula_1_7_6 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
            
          },
          FORMULA_1_7_7 = {#Determining the volume of annual labor costs for servicing subscriber lines inside buildings
            req (input$Intermediate.LinesInBuildingsLength)
            req (input$AccessTechnologyOptionsSet.LaborCostsLengthInhouseSubscriberLines)
            
            result <- formula_1_7_7 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_8 = {#Determining the cost of servicing subscriber lines inside buildings
            req (input$Intermediate.AnnualLaborCostSLOperation)
            req (input$AccessTechnologyOptionsSet.CostServiceCableInfrastructure)
            
            result <- formula_1_7_8 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_9 = {#Determining the volume of annual labor costs for servicing hanged-up lines
            req (input$Intermediate.LenghtHangedupCalbesSL)
            req (input$Intermediate.LenghtHangedupCalbesCL)
            req (input$AccessTechnologyOptionsSet.LaborCostsServiceSuspensionCable)
            
            result <- formula_1_7_9 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_10 = {#Determining the cost of servicing hanged-up lines
            req (input$Intermediate.AnnualLaborCostHangedCableOperation)
            req (input$AccessTechnologyOptionsSet.LaborCostsServiceCableSewerage)
            
            result <- formula_1_7_10 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_11 = {#Determining the volume of annual labor costs for servicing lines in cable duct
            req (input$Intermediate.LenghtSLInExternalDuct)
            req (input$Intermediate.LenghtCLInExternalDuct)
            req (input$DevelopmentParametersSet.NormativeAnnualRentOfCableDuct)

            result <- formula_1_7_11 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_12 = {#Determining the cost of servicing lines in cable duct
            req (input$Intermediate.AnnualLaborCostCableInDuctOperation)
            req (input$AccessTechnologyOptionsSet.CostServiceCableInfrastructure)
            
            result <- formula_1_7_12 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_13 = {#Determining the volume of annual labor costs for servicing new infrastructure for hanging-up cables (pillars)
            req (input$Intermediate.LengthOfNewInfrastractureForCL)
            req (input$Intermediate.LengthOfNewInfrastractureForSL)
            req (input$AccessTechnologyOptionsSet.LaborCostsServicePillars)
            
            result <- formula_1_7_13 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_14 = {#Determining the cost of servicing new infrastructure for hanging-up cables (pillars)
            req (input$Intermediate.AnnualLaborCostNewInfrastructure)
            req (input$DevelopmentParametersSet.CostOfOperationNewInfrastructure)
            
            result <- formula_1_7_14 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_15 = {#Determining the cost of rent of place in the external cable duct for the cable
            req (input$Intermediate.LenghtSLInExternalDuct)
            req (input$Intermediate.LenghtCLInExternalDuct)
            req (input$DevelopmentParametersSet.RentCostForCableInDuct)

            result <- formula_1_7_15 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_7_16 = {#Determining the cost of the rent of place on the pillars for the hanged-up cable
            req (input$Intermediate.LenghtHangedupCalbesSL)
            req (input$Intermediate.LenghtHangedupCalbesCL)
            req (input$Intermediate.LengthOfNewInfrastractureForCL)
            req (input$Intermediate.LengthOfNewInfrastractureForSL)
            req (input$DevelopmentParametersSet.RentCostForHangedCable)
            
            result <- formula_1_7_16 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          stop ("No!")
  )
  
}