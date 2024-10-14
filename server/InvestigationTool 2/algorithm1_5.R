library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Cost of equipment and materials

#Determining the cost of all AOEs
formula_1_5_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfAOEs <- 0 
 NumberOfAOEs <- input$Intermediate.NumberOfAOEs
  

   if (!is.null(intermediate))
   {
     NumberOfAOEs <- as.numeric (intermediate$NumberOfAOEs)
   }
  
  result = NumberOfAOEs*input$AccessTechnologyOptionsSet.CostEOA
  
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

#Determination of the cost of all distribution TAOEs
formula_1_5_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfTAOEs <- 0 
  NumberOfTAOEs <- input$Intermediate.NumberOfTAOEs
  
  
  if (!is.null(intermediate))
  {
    NumberOfTAOEs <- as.numeric (intermediate$NumberOfTAOEs)
  }
  
  
  result = NumberOfTAOEs*input$AccessTechnologyOptionsSet.CostTEOADistributing
  
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

#Determining the cost of materials for the arrangement of placements of AOEs
formula_1_5_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfAOEsPlaces <- 0 
  NumberOfAOEsPlaces <- input$Intermediate.NumberOfAOEsPlaces
  
  
  if (!is.null(intermediate))
  {
    NumberOfAOEsPlaces <- as.numeric (intermediate$NumberOfAOEsPlaces)
  }
  
  
  result = NumberOfAOEsPlaces*input$AccessTechnologyOptionsSet.CostMaterialsEOA
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determination of the cost of materials for arranging placements for distribution TAOEs
formula_1_5_4 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfTAOEsPlaces <- 0 
  NumberOfTAOEsPlaces <- input$Intermediate.NumberOfTAOEsPlaces
  
  
  if (!is.null(intermediate))
  {
    NumberOfTAOEsPlaces <- as.numeric (intermediate$NumberOfTAOEsPlaces)
  }
  
  
  result = NumberOfTAOEsPlaces*input$AccessTechnologyOptionsSet.CostMaterialsTEOA
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the total cost of materials for laying (or hanging-up) subscriber cables
formula_1_5_5 <- function (input, intermediate = NULL)
{
  req (input)
  
  LinesInBuildingsLength <- 0 
  LinesInBuildingsLength <- input$Intermediate.LinesInBuildingsLength
  
  LinesOutsideLength <- 0 
  LinesOutsideLength <- input$Intermediate.LinesOutsideLength
  
    
   if (!is.null(intermediate))
   {
     LinesInBuildingsLength <- as.numeric (intermediate$LinesInBuildingsLength)
     
     LinesOutsideLength <- as.numeric (intermediate$LinesOutsideLength)
   }
  
  
  result = (LinesInBuildingsLength + LinesOutsideLength)*input$AccessTechnologyOptionsSet.CostSubscribersLines
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determining the total cost of materials for laying (or hanging-up) distribution cable
formula_1_5_6 <- function (input, intermediate = NULL)
{
  req (input)
  
  LengthExternalCLDestributTrunk <- 0 
  LengthExternalCLDestributTrunk <- input$Intermediate.LengthExternalCLDestributTrunk
  
  
   if (!is.null(intermediate))
   {
     LengthExternalCLDestributTrunk <- as.numeric (intermediate$LengthExternalCLDestributTrunk)
   }
  
  
  result = LengthExternalCLDestributTrunk*input$AccessTechnologyOptionsSet.CostConnectingLines
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determination of the total cost of materials for laying (or hanging-up) cables of the aggregate segment
formula_1_5_7 <- function (input, intermediate = NULL)
{
  req (input)
  
  LengthExternalCLAggrTrunk <- 0 
  LengthExternalCLAggrTrunk <- input$Intermediate.LengthExternalCLAggrTrunk
  
  
  if (!is.null(intermediate))
  {
    LengthExternalCLAggrTrunk <- as.numeric (intermediate$LengthExternalCLAggrTrunk)
  }
  
  
  result = LengthExternalCLAggrTrunk*input$AccessTechnologyOptionsSet.CostConnectingLinesAggregateSegment
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determination of the total cost of materials for the construction of cable channels in residential buildings
formula_1_5_8 <- function (input, intermediate = NULL)
{
  req (input)
  
  LengthInternalCableDuctToBuild <- 0 
  LengthInternalCableDuctToBuild <- input$Intermediate.LengthInternalCableDuctToBuild
  
  
   if (!is.null(intermediate))
   {
     LengthInternalCableDuctToBuild <- as.numeric (intermediate$LengthInternalCableDuctToBuild)
   }
  
  
  result = LengthInternalCableDuctToBuild*input$AccessTechnologyOptionsSet.CostInternalSewerage
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Determination of the total cost of materials for the construction of new infrastructure, which should be built for the hanging-up of external connecting lines & subscriber cables
formula_1_5_9 <- function (input, intermediate = NULL)
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
  
  
  result = (LengthOfNewInfrastractureForSL+LengthOfNewInfrastractureForCL)*input$AccessTechnologyOptionsSet.CostPillars
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

algorithm1_5_impl <- function(input, intermediate = NULL)
{

  #Determining the cost of all AOEs
  CostOfAllAOEs <- formula_1_5_1 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of all AOEs, currency units"), CostOfAllAOEs, sep = ": "))                            
  
  #Determination of the cost of all distribution TAOEs
  CostOfAllTAOEs <- formula_1_5_2 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of all distribution TAOEs, currency units"), CostOfAllTAOEs, sep = ": "))                            
  
  #Determining the total cost of equipment
  CostOfEquipment = CostOfAllAOEs + CostOfAllTAOEs + input$AccessTechnologyOptionsSet.CostSN
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of equipment, currency units"), CostOfEquipment, sep = ": "))                            
  
  #Determining the cost of materials for the arrangement of placements of AOEs
  CostOfMaterialsForAOEsPlaces <- formula_1_5_3 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of materials for the arrangement of placements of AOEs, currency units"), CostOfMaterialsForAOEsPlaces, sep = ": "))                            
  
  #Determination of the cost of materials for arranging placements for distribution TAOEs
  CostOfMaterialsForTAOEsPlaces <- formula_1_5_4 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of materials for arranging placements for distribution TAOEs, currency units"), CostOfMaterialsForTAOEsPlaces, sep = ": "))                            
  
  
  #Determining the total cost of materials for arranging equipment locations
  CostOfMaterialsForPlaces = CostOfMaterialsForAOEsPlaces + CostOfMaterialsForTAOEsPlaces
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of materials for arranging equipment locations, currency units"), CostOfMaterialsForPlaces, sep = ": "))                            
  
  #Determining the total cost of materials for laying (or hanging-up) subscriber cables
  CostOfMaterialsForSLCable <- formula_1_5_5 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of materials for laying (or hanging-up) subscriber cables, currency units"), CostOfMaterialsForSLCable, sep = ": "))                            
  
  #Determining the total cost of materials for laying (or hanging-up) distribution cable
  CostOfMaterialsForCLCableDistr <- formula_1_5_6 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of materials for laying (or hanging-up) distribution cable, currency units"), CostOfMaterialsForCLCableDistr, sep = ": "))                            
  
  #Determination of the total cost of materials for laying (or hanging-up) cables of the aggregate segment            
  CostOfMaterialsForCLCableAggr <- formula_1_5_7 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of materials for laying (or hanging-up) cables of the aggregate segment, currency units"), CostOfMaterialsForCLCableAggr, sep = ": "))                            
  
  #Determination of the total cost of materials for the construction of cable channels in residential buildings
  CostOfMaterialsForInternalDuct <- formula_1_5_8 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of materials for the construction of cable channels in residential buildings, currency units"), CostOfMaterialsForInternalDuct, sep = ": "))                            
  
  #Determination of the total cost of materials for the construction of new infrastructure, which should be built for the hanging-up of external connecting lines & subscriber cables
  CostOfMaterialsForNewInfrastracture <- formula_1_5_9 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of materials for the construction of new infrastructure, which should be built for the hanging-up of external connecting lines & subscriber cables, currency units"), CostOfMaterialsForNewInfrastracture, sep = ": "))                            
  
  #Determining the total cost of materials 
  CostOfMaterials = CostOfMaterialsForPlaces + CostOfMaterialsForSLCable + 
    CostOfMaterialsForCLCableDistr  + CostOfMaterialsForCLCableAggr + CostOfMaterialsForInternalDuct + 
    CostOfMaterialsForNewInfrastracture
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of materials, currency units"), CostOfMaterials, sep = ": "))                            
  
  #Determining the total cost of equipment and materials
  CostOfEquipmentAndMaterials = CostOfEquipment + CostOfMaterials
  
  result <- matrix (nrow = 2, ncol = 2)
  result [1,1] = i18n$t("Total cost of equipment and materials, currency units")
  result [1,2] = CostOfEquipmentAndMaterials

  result [2,1] = i18n$t("Total cost of equipment, currency units")
  result [2,2] = CostOfEquipment
  
  return (result)
}

algorithm1_5 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            req (input$AccessTechnologyOptionsSet.CostSN)
            req (input$Intermediate.NumberOfAOEs)
            req (input$AccessTechnologyOptionsSet.CostEOA)
            
            req (input$Intermediate.NumberOfTAOEs)
            req (input$AccessTechnologyOptionsSet.CostTEOADistributing)
            
            req (input$Intermediate.NumberOfAOEsPlaces)
            req (input$AccessTechnologyOptionsSet.CostMaterialsEOA)
            
            req (input$Intermediate.NumberOfTAOEsPlaces)
            req (input$AccessTechnologyOptionsSet.CostMaterialsTEOA)
            
            req (input$Intermediate.LinesInBuildingsLength)
            req (input$Intermediate.LinesOutsideLength)
            req (input$AccessTechnologyOptionsSet.CostSubscribersLines)
            
            req (input$Intermediate.LengthExternalCLDestributTrunk)
            req (input$AccessTechnologyOptionsSet.CostConnectingLines)

            req (input$Intermediate.LengthExternalCLAggrTrunk)
            req (input$AccessTechnologyOptionsSet.CostConnectingLinesAggregateSegment)
            
            req (input$Intermediate.LengthInternalCableDuctToBuild)
            req (input$AccessTechnologyOptionsSet.CostInternalSewerage)
 
            req (input$Intermediate.LengthOfNewInfrastractureForSL)
            req (input$Intermediate.LengthOfNewInfrastractureForCL)
            req (input$AccessTechnologyOptionsSet.CostPillars)
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result <- algorithm1_5_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
                        
          },
          FORMULA_1_5_1 = {#Determining the cost of all AOEs
            req (input$Intermediate.NumberOfAOEs)
            req (input$AccessTechnologyOptionsSet.CostEOA)
            
            result <- formula_1_5_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_5_2 = {#Determination of the cost of all distribution TAOEs
            req (input$Intermediate.NumberOfTAOEs)
            req (input$AccessTechnologyOptionsSet.CostTEOADistributing)
            
            result <- formula_1_5_2 (input)
              
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_5_3 = {#Determining the cost of materials for the arrangement of placements of AOEs
            req (input$Intermediate.NumberOfAOEsPlaces)
            req (input$AccessTechnologyOptionsSet.CostMaterialsEOA)
            
            result <- formula_1_5_3 (input)
              
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_5_4 = {#Determination of the cost of materials for arranging placements for distribution TAOEs
            req (input$Intermediate.NumberOfTAOEsPlaces)
            req (input$AccessTechnologyOptionsSet.CostMaterialsTEOA)
            
            result <- formula_1_5_4 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_5_5 = {#Determining the total cost of materials for laying (or hanging-up) subscriber cables
            req (input$Intermediate.LinesInBuildingsLength)
            req (input$Intermediate.LinesOutsideLength)
            req (input$AccessTechnologyOptionsSet.CostSubscribersLines)
            
            result <- formula_1_5_5 (input)
            
              output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_5_6 = {#Determining the total cost of materials for laying (or hanging-up) distribution cable
            req (input$Intermediate.LengthExternalCLDestributTrunk)
            req (input$AccessTechnologyOptionsSet.CostConnectingLines)
            
            result <- formula_1_5_6 (input)
              
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_5_7 = {#Determination of the total cost of materials for laying (or hanging-up) cables of the aggregate segment
            req (input$Intermediate.LengthExternalCLAggrTrunk)
            req (input$AccessTechnologyOptionsSet.CostConnectingLinesAggregateSegment)
            
            result <- formula_1_5_7 (input)
              
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_1_5_8 = {#Determination of the total cost of materials for the construction of cable channels in residential buildings
            req (input$Intermediate.LengthInternalCableDuctToBuild)
            req (input$AccessTechnologyOptionsSet.CostInternalSewerage)
            
            result <- formula_1_5_8 (input)
              
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_1_5_9 = {#Determination of the total cost of materials for the construction of new infrastructure, which should be built for the hanging-up of external connecting lines & subscriber cables
            req (input$Intermediate.LengthOfNewInfrastractureForSL)
            req (input$Intermediate.LengthOfNewInfrastractureForCL)
            req (input$AccessTechnologyOptionsSet.CostPillars)
            
            result <- formula_1_5_9 (input)
              
            output$data <- renderTable(result, colnames=FALSE)
          },
          stop ("No!")
  )
  
}