library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#COst of work on the construction of an access network

#Duration of installation work on arrangement of places for AOEs
formula_1_6_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfAOEsPlaces <- 0 
  NumberOfAOEsPlaces <- input$Intermediate.NumberOfAOEsPlaces
  
  
   if (!is.null(intermediate))
   {
     NumberOfAOEsPlaces <- as.numeric (intermediate$NumberOfAOEsPlaces)
   }
  
  result = NumberOfAOEsPlaces*input$AccessTechnologyOptionsSet.LaborCostsEOA
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of installation work on the arrangement of places for AOEs
formula_1_6_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  DurationPlacesAOEs <- 0 
  DurationPlacesAOEs <- input$Intermediate.DurationPlacesAOEs
  
  
  if (!is.null(intermediate))
  {
    DurationPlacesAOEs <- as.numeric (intermediate$DurationPlacesAOEs)
  }
  
  result = DurationPlacesAOEs* input$AccessTechnologyOptionsSet.CostEquipmentInstallation
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Duration of installation works on arrangement of places for TAOEs
formula_1_6_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfTAOEsPlaces <- 0 
  NumberOfTAOEsPlaces <- input$Intermediate.NumberOfTAOEsPlaces
  
  
  if (!is.null(intermediate))
  {
    NumberOfTAOEsPlaces <- as.numeric (intermediate$NumberOfTAOEsPlaces)
  }
  
  result = NumberOfTAOEsPlaces * input$AccessTechnologyOptionsSet.LaborCostsTEOA
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of installation work on the arrangement of places for TAOEs
formula_1_6_4 <- function (input, intermediate = NULL)
{
  req (input)
  
  DurationPlacesTAOEs <- 0 
  DurationPlacesTAOEs <- input$Intermediate.DurationPlacesTAOEs
  
  
  if (!is.null(intermediate))
  {
    DurationPlacesTAOEs <- as.numeric (intermediate$DurationPlacesTAOEs)
  }
  
  result = DurationPlacesTAOEs*input$AccessTechnologyOptionsSet.CostEquipmentInstallation 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Duration of installation and configuration of AOEs
formula_1_6_5 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfAOEs <- 0 
  NumberOfAOEs <- input$Intermediate.NumberOfAOEs
  
  
  if (!is.null(intermediate))
  {
   NumberOfAOEs <- as.numeric (intermediate$NumberOfAOEs)
  }
  
  result = NumberOfAOEs* input$AccessTechnologyOptionsSet.LaborCostsEquipmentTuneup
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of installation and configuration of AOEs
formula_1_6_6 <- function (input, intermediate = NULL)
{
  req (input)
  
  DurationInstallationAOEs <- 0 
  DurationInstallationAOEs <- input$Intermediate.DurationInstallationAOEs
  
  
  if (!is.null(intermediate))
  {
    DurationInstallationAOEs <- as.numeric (intermediate$DurationInstallationAOEs)
   }
  
  result = DurationInstallationAOEs * input$AccessTechnologyOptionsSet.CostEquipmentTuneup
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Duration of installation and configuration of TAOEs
formula_1_6_7 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfTAOEs <- 0 
  NumberOfTAOEs <- input$Intermediate.NumberOfTAOEs
  
  
  if (!is.null(intermediate))
  {
    NumberOfTAOEs <- as.numeric (intermediate$NumberOfTAOEs)
  }
  
  result = NumberOfTAOEs * input$AccessTechnologyOptionsSet.LaborCostsEquipmentTuneup 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of installation and configuration of TAOEs
formula_1_6_8 <- function (input, intermediate = NULL)
{
  req (input)
  
  DurationInstallationTAOEs <- 0 
  DurationInstallationTAOEs <- input$Intermediate.DurationInstallationTAOEs
  
  
  if (!is.null(intermediate))
  {
    DurationInstallationTAOEs <- as.numeric (intermediate$DurationInstallationTAOEs)
  }
  
  result = DurationInstallationTAOEs*input$AccessTechnologyOptionsSet.CostEquipmentTuneup 
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Duration of installation of cable ducts inside buildings
formula_1_6_9 <- function (input, intermediate = NULL)
{
  req (input)
  
  LengthInternalCableDuctToBuild <- 0 
  LengthInternalCableDuctToBuild <- input$Intermediate.LengthInternalCableDuctToBuild
  
  
  if (!is.null(intermediate))
  {
    LengthInternalCableDuctToBuild <- as.numeric (intermediate$LengthInternalCableDuctToBuild)
  }
  
  result = LengthInternalCableDuctToBuild * input$AccessTechnologyOptionsSet.LaborCostsInternalSewerageConstruction
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of installation of cable ducts inside buildings
formula_1_6_10 <- function (input, intermediate = NULL)
{
  req (input)
  
  DurationInsideCableDuct <- 0 
  DurationInsideCableDuct <- input$Intermediate.DurationInsideCableDuct
  
  
  if (!is.null(intermediate))
  {
    DurationInsideCableDuct <- as.numeric (intermediate$DurationInsideCableDuct)
  }
  
  result = DurationInsideCableDuct * input$AccessTechnologyOptionsSet.CostInternalSewerageConstruction
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Duration of installation of subscriber lines inside buildings
formula_1_6_11 <- function (input, intermediate = NULL)
{
  req (input)
  
  LenghtHangedupCalbesSL <- 0 
  LenghtHangedupCalbesSL <- input$Intermediate.LenghtHangedupCalbesSL
  
  
  if (!is.null(intermediate))
  {
    LenghtHangedupCalbesSL <- as.numeric (intermediate$LenghtHangedupCalbesSL)
  }
  
  result = LenghtHangedupCalbesSL * input$AccessTechnologyOptionsSet.LaborCostsCableSewerage
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of installation of subscriber lines inside buildings
formula_1_6_12 <- function (input, intermediate = NULL)
{
  req (input)
  
  DurationSLInstallation <- 0 
  DurationSLInstallation <- input$Intermediate.DurationSLInstallation
  
  
  if (!is.null(intermediate))
  {
    DurationSLInstallation <- as.numeric (intermediate$DurationSLInstallation)
  }
  
  result = DurationSLInstallation*input$AccessTechnologyOptionsSet.CostInstallationSubscribersLines
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Duration of the construction of new infrastructure for hanging-up cables
formula_1_6_13 <- function (input, intermediate = NULL)
{
  req (input)
  
  LengthOfNewInfrastractureForSL <- 0 
  LengthOfNewInfrastractureForSL <- input$Intermediate.LengthOfNewInfrastractureForSL
  
  LengthOfNewInfrastractureForCL <- 0 
  LengthOfNewInfrastractureForCL <- input$Intermediate.LengthOfNewInfrastractureForCL
  
    
  if (!is.null(intermediate))
  {
    LengthOfNewInfrastractureForSL <- as.numeric (intermediate$LengthOfNewInfrastractureForSL)
    LengthOfNewInfrastractureForCL <- as.numeric (intermediate$LengthOfNewInfrastractureForCL)
  }
  
  result = (LengthOfNewInfrastractureForSL+LengthOfNewInfrastractureForCL)*input$AccessTechnologyOptionsSet.LaborCostsForConstractionOfNewInfrastructure
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of the construction of new infrastructure for hanging-up cables
formula_1_6_14 <- function (input, intermediate = NULL)
{
  req (input)
  
  DurationNewInfrastructure <- 0 
  DurationNewInfrastructure <- input$Intermediate.DurationNewInfrastructure
  
  
   if (!is.null(intermediate))
   {
     DurationNewInfrastructure <- as.numeric (intermediate$DurationNewInfrastructure)
   }
  
  result = DurationNewInfrastructure*input$AccessTechnologyOptionsSet.CostOfConstractionNewInfrastrucutre
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Duration of installation of external hanged-up lines
formula_1_6_15 <- function (input, intermediate = NULL)
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
  
  result = (LenghtHangedupCalbesSL+LenghtHangedupCalbesCL)*input$AccessTechnologyOptionsSet.LaborCostsSuspensionCable
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of installation of external hanged-up lines
formula_1_6_16 <- function (input, intermediate = NULL)
{
  req (input)
  
  DurationOfHangedUpCable <- 0 
  DurationOfHangedUpCable <- input$Intermediate.DurationOfHangedUpCable
  
  
  if (!is.null(intermediate))
   {
    DurationOfHangedUpCable <- as.numeric (intermediate$DurationOfHangedUpCable)
   }
  
  result = DurationOfHangedUpCable * input$AccessTechnologyOptionsSet.CostSettingSubscribersLines
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Duration of installation of external lines in the duct
formula_1_6_17 <- function (input, intermediate = NULL)
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
  
  result = (LenghtSLInExternalDuct+LenghtCLInExternalDuct)*input$AccessTechnologyOptionsSet.LaborCostsCableSewerage
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of installation of external lines in the duct
formula_1_6_18 <- function (input, intermediate = NULL)
{
  req (input)
  
  DurationInstallCableInDuct <- 0 
  DurationInstallCableInDuct <- input$Intermediate.DurationInstallCableInDuct
  
  
  if (!is.null(intermediate))
  {
    DurationInstallCableInDuct <- as.numeric (intermediate$DurationInstallCableInDuct)
   }
  
  result = DurationInstallCableInDuct*input$AccessTechnologyOptionsSet.CostCableSewerage
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


algorithm1_6_impl <- function(input, intermediate = NULL)
{

  #Duration of installation work on arrangement of places for AOEs                        
  DurationPlacesAOEs <- formula_1_6_1 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Duration of installation work on arrangement of places for AOEs, hours"), DurationPlacesAOEs, sep = ": "))                            
  
  #Cost of installation work on the arrangement of places for AOEs
  intermediate2 <- list (DurationPlacesAOEs = 0.0)
  intermediate2$DurationPlacesAOEs <- DurationPlacesAOEs
  
  CostOfInstallationPlacesAOEs  <- formula_1_6_2 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of installation work on the arrangement of places for AOEs, currency units"), CostOfInstallationPlacesAOEs, sep = ": "))                            
  
  #Duration of installation works on arrangement of places for TAOEs
  DurationPlacesTAOEs = formula_1_6_3 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Duration of installation works on arrangement of places for TAOEs, hours"), DurationPlacesTAOEs, sep = ": "))                            
  
  #Cost of installation work on the arrangement of places for TAOEs
  intermediate2 <- list (DurationPlacesTAOEs = 0.0)
  intermediate2$DurationPlacesTAOEs <- DurationPlacesTAOEs
  
  CostOfInstallationPlacesTAOEs <- formula_1_6_4 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of installation work on the arrangement of places for TAOEs, currency units"), CostOfInstallationPlacesTAOEs, sep = ": "))                            
  
  #Determination of cost of installation work on the arrangement of places for equipment
  CostOfInstallationPlaces = CostOfInstallationPlacesAOEs + CostOfInstallationPlacesTAOEs
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Determination of cost of installation work on the arrangement of places for equipment, currency units"), CostOfInstallationPlaces, sep = ": "))                            
  
  #Duration of installation and configuration of AOEs
  DurationInstallationAOEs = formula_1_6_5 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Duration of installation and configuration of AOEs, hours"), DurationInstallationAOEs, sep = ": "))                            
  
  #Cost of installation and configuration of AOEs            
  intermediate2 <- list (DurationInstallationAOEs = 0.0)
  intermediate2$DurationInstallationAOEs <- DurationInstallationAOEs
  
  CostOfInstallationAOEs <- formula_1_6_6 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of installation and configuration of AOEs, currency units"), CostOfInstallationAOEs, sep = ": "))                            
  
  #Duration of installation and configuration of TAOEs
  DurationInstallationTAOEs = formula_1_6_7 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Duration of installation and configuration of TAOEs, hours"), DurationInstallationTAOEs, sep = ": "))                            
  
  #Cost of installation and configuration of TAOEs
  intermediate2 <- list (DurationInstallationTAOEs = 0.0)
  intermediate2$DurationInstallationTAOEs <- DurationInstallationTAOEs
  
  CostOfInstallationTAOEs <- formula_1_6_8 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of installation and configuration of TAOEs, currency units"), CostOfInstallationTAOEs, sep = ": "))                            
  
  #Determination of cost of installation and configuration of active equipment
  CostOfInstallationEq = CostOfInstallationAOEs +  CostOfInstallationTAOEs
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Determination of cost of installation and configuration of active equipment, currency units"), CostOfInstallationEq, sep = ": "))                            
  
  #Duration of installation of cable ducts inside buildings
  DurationInsideCableDuct = formula_1_6_9 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Duration of installation of cable ducts inside buildings, hours"), DurationInsideCableDuct, sep = ": "))                            
  
  #Cost of installation of cable ducts inside buildings
  intermediate2 <- list (DurationInsideCableDuct = 0.0)
  intermediate2$DurationInsideCableDuct <- DurationInsideCableDuct
  
  CostOfInsideCableDuct <- formula_1_6_10 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of installation of cable ducts inside buildings, currency units"), CostOfInsideCableDuct, sep = ": "))                            
  
  #Duration of installation of subscriber lines inside buildings
  DurationSLInstallation = formula_1_6_11 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Duration of installation of subscriber lines inside buildings, hours"), DurationSLInstallation, sep = ": "))    
  
  #Cost of installation of subscriber lines inside buildings
  intermediate2 <- list (DurationSLInstallation = 0.0)
  intermediate2$DurationSLInstallation <- DurationSLInstallation
  
  CostSLInstallation <- formula_1_6_12 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of installation of subscriber lines inside buildings, currency units"), CostSLInstallation, sep = ": "))                            
  
  
  #Duration of the construction of new infrastructure for hanging-up cables
  DurationNewInfrastructure = formula_1_6_13 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Duration of the construction of new infrastructure for hanging-up cables, hours"), DurationNewInfrastructure, sep = ": "))                            
  
  #Cost of the construction of new infrastructure for hanging-up cables
  intermediate2 <- list (DurationNewInfrastructure = 0.0)
  intermediate2$DurationNewInfrastructure <- DurationNewInfrastructure
  
  CostNewInfrastructure <- formula_1_6_14 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of the construction of new infrastructure for hanging-up cables, currency units"), CostNewInfrastructure, sep = ": "))                            
  
  #Duration of installation of external hanged-up lines
  DurationOfHangedUpCable = formula_1_6_15 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Duration of installation of external hanged-up lines, hours"), DurationOfHangedUpCable, sep = ": "))                            
  
  #Cost of installation of external hanged-up lines            
  intermediate2 <- list (DurationOfHangedUpCable = 0.0)
  intermediate2$DurationOfHangedUpCable <- DurationOfHangedUpCable
  
  CostOfHangedUpCable <- formula_1_6_16 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of installation of external hanged-up lines, currency units"), CostOfHangedUpCable, sep = ": "))                            
  
  #Duration of installation of external lines in the duct            
  DurationInstallCableInDuct = formula_1_6_17 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Duration of installation of external lines in the duct, hours"), DurationInstallCableInDuct, sep = ": "))                            
  
  #Cost of installation of external lines in the duct            
  intermediate2 <- list (DurationInstallCableInDuct = 0.0)
  intermediate2$DurationInstallCableInDuct <- DurationInstallCableInDuct
  
  CostInstallCableInDuct <- formula_1_6_18 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of installation of external lines in the duct, currency units"), CostInstallCableInDuct, sep = ": "))                            
  
  #Determination of total cost of the construction of cable ducts and the installation of cable infrastructure            
  CostOfCableInstallation = CostOfInsideCableDuct +
    CostSLInstallation + CostNewInfrastructure + 
    CostOfHangedUpCable + CostInstallCableInDuct
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of the construction of cable ducts and the installation of cable infrastructure, currency units"), CostOfCableInstallation, sep = ": "))                            
  
  
  #Determination of total cost of the construction of the access network
  
  result <- matrix (nrow = 1, ncol = 2)
  result [1,1] = i18n$t("Total cost of the construction of the access network, currency units")
  result [1,2] = CostOfInstallationPlaces + CostOfInstallationEq + CostOfCableInstallation
  

  return (result)   
}

algorithm1_6 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            req (input$Intermediate.NumberOfAOEsPlaces)
            req (input$AccessTechnologyOptionsSet.LaborCostsEOA)
            req (input$AccessTechnologyOptionsSet.CostEquipmentInstallation)
            req (input$Intermediate.NumberOfTAOEsPlaces)
            req (input$AccessTechnologyOptionsSet.LaborCostsTEOA)
            req (input$Intermediate.NumberOfAOEs)
            req (input$AccessTechnologyOptionsSet.LaborCostsEquipmentTuneup)
            req (input$AccessTechnologyOptionsSet.CostEquipmentTuneup)
            req (input$Intermediate.NumberOfTAOEs)
            req (input$Intermediate.LengthInternalCableDuctToBuild)
            req (input$AccessTechnologyOptionsSet.LaborCostsInternalSewerageConstruction)
            req (input$AccessTechnologyOptionsSet.CostInternalSewerageConstruction)
            req (input$Intermediate.LenghtHangedupCalbesSL)
            req (input$AccessTechnologyOptionsSet.LaborCostsCableSewerage)
            req (input$AccessTechnologyOptionsSet.CostInstallationSubscribersLines)
            req (input$Intermediate.LengthOfNewInfrastractureForSL)
            req (input$Intermediate.LengthOfNewInfrastractureForCL)
            req (input$AccessTechnologyOptionsSet.LaborCostsForConstractionOfNewInfrastructure)
            req (input$AccessTechnologyOptionsSet.CostOfConstractionNewInfrastrucutre)
            req (input$Intermediate.LenghtHangedupCalbesSL)
            req (input$Intermediate.LenghtHangedupCalbesCL)
            req (input$AccessTechnologyOptionsSet.LaborCostsSuspensionCable)
            req (input$AccessTechnologyOptionsSet.CostSettingSubscribersLines)
            req (input$Intermediate.LenghtSLInExternalDuct)
            req (input$Intermediate.LenghtCLInExternalDuct)
            req (input$AccessTechnologyOptionsSet.LaborCostsCableSewerage)
            req (input$AccessTechnologyOptionsSet.CostCableSewerage)
                        
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result <- algorithm1_6_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_1_6_1 = {#Duration of installation work on arrangement of places for AOEs
            req (input$Intermediate.NumberOfAOEsPlaces)
            req (input$AccessTechnologyOptionsSet.LaborCostsEOA)
            
            result <- formula_1_6_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_6_2 = {#Cost of installation work on the arrangement of places for AOEs
            req (input$Intermediate.DurationPlacesAOEs)
            req (input$AccessTechnologyOptionsSet.CostEquipmentInstallation)
            
            result <- formula_1_6_2 (input)
              
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_1_6_3 = {#Duration of installation works on arrangement of places for TAOEs
            req (input$Intermediate.NumberOfTAOEsPlaces)
            req (input$AccessTechnologyOptionsSet.LaborCostsTEOA)
            
            result <- formula_1_6_3 (input)
                          
            output$data <- renderTable(result, colnames=FALSE)            
          },
          FORMULA_1_6_4 = {#Cost of installation work on the arrangement of places for TAOEs
            req (input$Intermediate.DurationPlacesTAOEs)
            req (input$AccessTechnologyOptionsSet.CostEquipmentInstallation)
            
            result <- formula_1_6_4 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_1_6_5 = {#Duration of installation and configuration of AOEs
            req (input$Intermediate.NumberOfAOEs)
            req (input$AccessTechnologyOptionsSet.LaborCostsEquipmentTuneup)
            
            result <- formula_1_6_5 (input)
            
            output$data <- renderTable(result, colnames=FALSE)            
          },
          FORMULA_1_6_6 = {#Cost of installation and configuration of AOEs
            req (input$Intermediate.DurationInstallationAOEs)
            req (input$AccessTechnologyOptionsSet.CostEquipmentTuneup)
            
            result <- formula_1_6_6 (input)
            
            output$data <- renderTable(result, colnames=FALSE)            
          },
          FORMULA_1_6_7 = {#Duration of installation and configuration of TAOEs
            req (input$Intermediate.NumberOfTAOEs)
            req (input$AccessTechnologyOptionsSet.LaborCostsEquipmentTuneup)
            
            result <- formula_1_6_7 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_1_6_8 = {#Cost of installation and configuration of TAOEs
            req (input$Intermediate.DurationInstallationTAOEs)
            req (input$AccessTechnologyOptionsSet.CostEquipmentTuneup)
            
            result <- formula_1_6_8 (input)
            
            output$data <- renderTable(result, colnames=FALSE)            
          },
          FORMULA_1_6_9 = {#Duration of installation of cable ducts inside buildings
            req (input$Intermediate.LengthInternalCableDuctToBuild)
            req (input$AccessTechnologyOptionsSet.LaborCostsInternalSewerageConstruction)
            
            result <- formula_1_6_9 (input)
            
            output$data <- renderTable(result, colnames=FALSE)               
            
          },
          FORMULA_1_6_10 = {#Cost of installation of cable ducts inside buildings
            req (input$Intermediate.DurationInsideCableDuct)
            req (input$AccessTechnologyOptionsSet.CostInternalSewerageConstruction)

            result <- formula_1_6_10 (input)
            
            output$data <- renderTable(result, colnames=FALSE)               
            
          },
          FORMULA_1_6_11 = {#Duration of installation of subscriber lines inside buildings
            req (input$Intermediate.LenghtHangedupCalbesSL)
            req (input$AccessTechnologyOptionsSet.LaborCostsCableSewerage)
            
            result <- formula_1_6_11 (input)
            
            output$data <- renderTable(result, colnames=FALSE)               
            
          },
          FORMULA_1_6_12 = {#Cost of installation of subscriber lines inside buildings
            req (input$Intermediate.DurationSLInstallation)
            req (input$AccessTechnologyOptionsSet.CostInstallationSubscribersLines)

            result <- formula_1_6_12 (input)
            
            output$data <- renderTable(result, colnames=FALSE)               
            
          },
          FORMULA_1_6_13 = {#Duration of the construction of new infrastructure for hanging-up cables
            req (input$Intermediate.LengthOfNewInfrastractureForSL)
            req (input$Intermediate.LengthOfNewInfrastractureForCL)
            req (input$AccessTechnologyOptionsSet.LaborCostsForConstractionOfNewInfrastructure)
            
            result <- formula_1_6_13 (input)
            
            output$data <- renderTable(result, colnames=FALSE)               
            
          },
          FORMULA_1_6_14 = {#Cost of the construction of new infrastructure for hanging-up cables
            req (input$Intermediate.DurationNewInfrastructure)
            req (input$AccessTechnologyOptionsSet.CostOfConstractionNewInfrastrucutre)
            
            result <- formula_1_6_14 (input)
            
            output$data <- renderTable(result, colnames=FALSE)               
            
          },
          FORMULA_1_6_15 = {#Duration of installation of external hanged-up lines
            req (input$Intermediate.LenghtHangedupCalbesSL)
            req (input$Intermediate.LenghtHangedupCalbesCL)
            req (input$AccessTechnologyOptionsSet.LaborCostsSuspensionCable)
            
            result <- formula_1_6_15 (input)
            
            output$data <- renderTable(result, colnames=FALSE)               
            
          },
          FORMULA_1_6_16 = {#Cost of installation of external hanged-up lines
            req (input$Intermediate.DurationOfHangedUpCable)
            req (input$AccessTechnologyOptionsSet.CostSettingSubscribersLines)
            
            result <- formula_1_6_16 (input)
            
            output$data <- renderTable(result, colnames=FALSE)               
            
          },
          FORMULA_1_6_17 = {#Duration of installation of external lines in the duct
            req (input$Intermediate.LenghtSLInExternalDuct)
            req (input$Intermediate.LenghtCLInExternalDuct)
            req (input$AccessTechnologyOptionsSet.LaborCostsCableSewerage)
            
            result <- formula_1_6_17 (input)
            
            output$data <- renderTable(result, colnames=FALSE)               
            
          },
          FORMULA_1_6_18 = {#Cost of installation of external lines in the duct
            req (input$Intermediate.DurationInstallCableInDuct)
            req (input$AccessTechnologyOptionsSet.CostCableSewerage)
            
            result <- formula_1_6_18 (input)
            
            output$data <- renderTable(result, colnames=FALSE)               
            
          },
          stop ("No!")
  )
  
}