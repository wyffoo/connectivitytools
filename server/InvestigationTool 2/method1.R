source ("algorithm1_1.R")
source ("algorithm1_2.R")
source ("algorithm1_3.R")
source ("algorithm1_4.R")
source ("algorithm1_5.R")
source ("algorithm1_6.R")
source ("algorithm1_7.R")
source ("algorithm1_8.R")

library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")


method1_impl <- function(input, intermediate = NULL)
{
  
  #Number of active AOEs and number of places for their installation
  
  result = algorithm1_1_impl (input)
  
  #Number of solvent population not covered by competitors (number of potential subscribers), people
  Subscribers <- as.numeric (result [1,2])
  
  #Number of places to place AOEs, determined by the building area, places
  NumberOfAOEsPlaces <- as.numeric (result [2,2])
  
  #Number of AOE units, units
  NumberOfAOEs <- as.numeric (result [3,2])
  
  #Building area, sq km
  BuildingArea <- as.numeric (result [4,2])
  
  #Number of active TAOEs and number of places for their installation
  
  NetworkArea <- 0
  
  intermediate <- list (BuiltupArea = 0.0, NumberOfAOEsPlaces = 0.0)
  
  intermediate$BuiltupArea = BuildingArea
  intermediate$NumberOfAOEsPlaces <- NumberOfAOEsPlaces
  
  result = algorithm1_2_impl (input, intermediate) 
  
  #Number of places for placement of TAOEs to cover all AOEs, uniformly located throughout the object, places
  NumberOfTAOEsPlaces <- as.numeric (result [1,2])
  #Number of TAOEs units required to the object, units                                         
  NumberOfTAOEs <- as.numeric (result [2,2])
  #Network Area, sq km
  NetworkArea <-as.numeric (result [3,2]) 
  #Network Radius, km
  NetworkRadius <-as.numeric (result [4,2]) 
  
  #Length of subscriber lines and volume of new infrastracture for hanging-up cables (if any)
  
  intermediate2 <- list (Subscribers = 0)
  intermediate2$Subscribers <- Subscribers
  result = algorithm1_3_impl (input, intermediate2) 
  
  # Length of aggregated lines in the building
  LengthAggrForInternal <- as.numeric (result [1,2])
  # Length of aggregated lines outside the buildings
  LengthAggrForExternal <- as.numeric (result [2,2])
  # Length of subscriber lines within buildings
  LengthAggrForInternal <- as.numeric (result [3,2])
  # Length of subscriber lines in the external cable ducts
  LenghtSLInExternalDuct <- as.numeric (result [4,2])
  # Length of the hanged-up cables of subscriber lines
  LenghtHangedupCalbesSL <- as.numeric (result [5,2])
  # Length of the house cable duct that must be built for laying subscriber lines
  LengthInternalCableDuctToBuild <- as.numeric (result [6,2])
  # Length of the new infrastructure for hanging-up cables of subscriber lines 
  LengthOfNewInfrastractureForSL <- as.numeric (result [7,2]) 
  
  
  LinesInBuildingsLength <- as.numeric (result [8,2]) 
  LinesOutsideLength <- as.numeric (result [9,2]) 
  
  #Length of connecting lines and volume of new infrastracture for hanging-up cables (if any)
  
  intermediate3 <- list (NetworkRadius = 0.0, NumberOfAOEsPlaces=1, NumberOfTAOEsPlaces=1)
  
  intermediate3$NetworkRadius <- NetworkRadius
  intermediate3$NumberOfAOEsPlaces <- NumberOfAOEsPlaces
  intermediate3$NumberOfTAOEsPlaces <- NumberOfTAOEsPlaces
  
  result = algorithm1_4_impl (input, intermediate3) 
  
  LengthOfAggrCableOfCLDestr <- as.numeric (result [2,2])
  LengthOfAggrCableOfCLAggr <- as.numeric (result [1,2])
  
  
  # Length of aggregate segment trunk cable 
  LengthExternalCLAggrTrunk <- LengthOfAggrCableOfCLAggr
  # Length of distribution segment trunk cable
  LengthExternalCLDestributTrunk <- LengthOfAggrCableOfCLDestr
  
  # Length of the hanged-up cables of distribution & aggregate segment
  LenghtHangedupCalbesCL <- as.numeric (result [3,2])
  
  
  # Length of distribution & aggregate segment in the external cable ducts
  LenghtCLInExternalDuct <-  as.numeric (result [4,2])
  # Total length of the new infrastructure for hanging-up cables of distribution & aggregate segment
  LengthOfNewInfrastractureForCL <- as.numeric (result [5,2])
  
  # Length of the hanged-up cables of distribution & aggregate segment
  LenghtHangedupCalbesCL <- LenghtHangedupCalbesCL + LengthOfNewInfrastractureForCL
  
  
  #Cost of equipment and materials
  
  intermediate4 <- list (
    NumberOfAOEs = 0,
    NumberOfTAOEs = 0,
    NumberOfAOEsPlaces = 0,
    NumberOfTAOEsPlaces = 0,
    LinesInBuildingsLength = 0,
    LinesOutsideLength = 0,
    LengthExternalCLDestributTrunk = 0,
    LengthExternalCLAggrTrunk = 0,
    LengthInternalCableDuctToBuild = 0,
    LengthOfNewInfrastractureForSL = 0,
    LengthOfNewInfrastractureForCL = 0)
  
  
  
  intermediate4$NumberOfAOEs <- NumberOfAOEs
  intermediate4$NumberOfTAOEs <- NumberOfTAOEs
  intermediate4$NumberOfAOEsPlaces <- NumberOfAOEsPlaces
  intermediate4$NumberOfTAOEsPlaces <- NumberOfTAOEsPlaces
  intermediate4$LinesInBuildingsLength <- LinesInBuildingsLength
  intermediate4$LinesOutsideLength <- LinesOutsideLength
  intermediate4$LengthExternalCLDestributTrunk <- LengthExternalCLDestributTrunk
  intermediate4$LengthExternalCLAggrTrunk <- LengthExternalCLAggrTrunk
  intermediate4$LengthInternalCableDuctToBuild <- LengthInternalCableDuctToBuild
  intermediate4$LengthOfNewInfrastractureForSL <- LengthOfNewInfrastractureForSL
  intermediate4$LengthOfNewInfrastractureForCL <- LengthOfNewInfrastractureForCL
  
  
  result <- algorithm1_5_impl (input, intermediate4) 
  
  #Determining the total cost of equipment and materials
  CostOfEquipmentAndMaterials <- as.numeric (result [1,2])
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of equipment and materials, currency units"), CostOfEquipmentAndMaterials, sep = ": "))                                  
  
  CostOfEquipment <- as.numeric (result [2,2])
  
  #COst of work on the construction of an access network
  
  intermediate5 <- list (
    NumberOfAOEsPlaces = 0,
    NumberOfTAOEsPlaces = 0,
    NumberOfAOEs = 0,
    NumberOfTAOEs = 0,
    LengthInternalCableDuctToBuild = 0.0,
    LenghtHangedupCalbesSL = 0.0,
    LengthOfNewInfrastractureForSL = 0.0,
    LengthOfNewInfrastractureForCL = 0.0,
    LenghtHangedupCalbesSL = 0.0,
    LenghtHangedupCalbesCL = 0.0,
    LenghtSLInExternalDuct = 0.0,
    LenghtCLInExternalDuct = 0.0
  )
  
  
  intermediate5$NumberOfAOEsPlaces <- NumberOfAOEsPlaces
  intermediate5$NumberOfTAOEsPlaces <- NumberOfTAOEsPlaces
  intermediate5$NumberOfAOEs <- NumberOfAOEs
  intermediate5$NumberOfTAOEs <- NumberOfTAOEs
  intermediate5$LengthInternalCableDuctToBuild <- LengthInternalCableDuctToBuild
  intermediate5$LenghtHangedupCalbesSL <- LenghtHangedupCalbesSL
  intermediate5$LengthOfNewInfrastractureForSL <- LengthOfNewInfrastractureForSL
  intermediate5$LengthOfNewInfrastractureForCL <- LengthOfNewInfrastractureForCL
  intermediate5$LenghtHangedupCalbesSL <- LenghtHangedupCalbesSL
  intermediate5$LenghtHangedupCalbesCL <- LenghtHangedupCalbesCL
  intermediate5$LenghtSLInExternalDuct <- LenghtSLInExternalDuct
  intermediate5$LenghtCLInExternalDuct <- LenghtCLInExternalDuct
  
  result = algorithm1_6_impl (input, intermediate5) 
  
  CostOfConstruction <- as.numeric (result [1,2])
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of the construction of the access network, currency units"), CostOfConstruction, sep = ": "))                                  
  
  
  
  #Cost of operating an access network
  
  intermediate6 <- list (
    NumberOfAOEs = 0.0,
    AnnualLaborCostAOEsOperation = 0.0,
    ElectricityConsuptionsbyAOEs = 0.0,
    NumberOfTAOEs = 0.0,
    AnnualLaborCostTAOEsOperation = 0.0,
    ElectricityConsuptionsbyTAOEs = 0.0,
    AnnualLaborCostSLOperation = 0.0,
    LenghtHangedupCalbesSL = 0.0,
    LenghtHangedupCalbesCL = 0.0,
    AnnualLaborCostHangedCableOperation = 0.0,
    LenghtSLInExternalDuct = 0.0,
    LenghtCLInExternalDuct = 0.0,
    AnnualLaborCostCableInDuctOperation = 0.0,
    LengthOfNewInfrastractureForCL = 0.0,
    LengthOfNewInfrastractureForSL = 0.0,
    AnnualLaborCostNewInfrastructure = 0.0,
    LinesInBuildingsLength = 0.0            
  )
  
  intermediate6$NumberOfAOEs <- NumberOfAOEs
  intermediate6$NumberOfTAOEs <- NumberOfTAOEs
  intermediate6$LenghtHangedupCalbesSL <- LenghtHangedupCalbesSL
  intermediate6$LenghtHangedupCalbesCL <- LenghtHangedupCalbesCL
  intermediate6$LenghtSLInExternalDuct <- LenghtSLInExternalDuct
  intermediate6$LenghtCLInExternalDuct <- LenghtCLInExternalDuct
  intermediate6$LengthOfNewInfrastractureForCL <- LengthOfNewInfrastractureForCL
  intermediate6$LengthOfNewInfrastractureForSL <- LengthOfNewInfrastractureForSL
  intermediate6$LinesInBuildingsLength <- LinesInBuildingsLength
  
  
  result = algorithm1_7_impl (input, intermediate6) 
  
  #Determination of costs of the annual operation of an access network 
  CostOfOperation =   as.numeric (result [1,2])
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of the annual operation of an access network , currency units/year"), CostOfOperation, sep = ": "))                                  
  
  
  #Calculating NPV
  
  #Design and licensing cost
  intermediate7 <- list (
    NumberOfAOEs = 0.0,
    CostOfEquipment = 0.0,
    CostOfConstruction = 0.0,
    Subscribers = 0.0,
    CostOfOperation = 0.0,
    CostOfEquipmentAndMaterials = 0.0
  )
  
  intermediate7$NumberOfAOEs <- NumberOfAOEs
  intermediate7$CostOfEquipment <- CostOfEquipment
  intermediate7$CostOfConstruction <- CostOfConstruction
  intermediate7$Subscribers <- Subscribers
  intermediate7$CostOfOperation <- CostOfOperation
  intermediate7$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
  
  result <- algorithm1_8_impl (input, intermediate7) 
  
  return (result)
}
  
mediumnamebynumber <- function (number)
{
  result <- ""
  switch (number,
          "1" = {
            result <- "Wireless"
          },
          "2" = {
            result <- "Copper"
          },
          "3" = {
            result <- "Fiber-Optic"
          }
  )
  
  return (result)
}


getdescriptionbyname <- function (myname)
{
  ret <- .GlobalEnv$myvariables [[myname]]
  
  return (ret)
}

method1_inv_impl <- function(input, intermediate = NULL)
{
 
  
  L <- reactiveValuesToList (input)
  
  
  l_len <- length(L)
  
  result <- matrix (nrow = (l_len-8), ncol = 5)
  curindex <- 1
    
        for (i in 1:l_len)
        {
          elem <- L[i]
          
          e_name <- names (elem [1])
          
          if ((e_name != "calculate") &
              (e_name != "investigate") & 
              (e_name != "formula") & 
              (e_name != "method") &
              (e_name != "algorithm") & 
              (e_name != "investigate") & 
              (e_name != "AccessTechnologyOptionsSet.MediumTypeOfSL") &
              (e_name != "AccessTechnologyOptionsSet.MediumTypeOfCL") &
              (e_name != "AccessTechnologyOptionsSet.MediumTypeOfCLAggr"))
          {
            result1 <- method1_impl (L)
            
            val <- as.numeric (elem [[1]])
            
            val2 <- val*2
            elem [[1]] <- val2
            
            L[i] <- elem
            
            result2 <- method1_impl (L)
            
   
            
            val3 <- val*4
            elem [[1]] <- val3
            
            L[i] <- elem
            
            result3 <- method1_impl (L)
            
                     
            val4 <- val/2
            elem [[1]] <- val4
            
            L[i] <- elem
            
            result4 <- method1_impl (L)
            
            val5 <- val/4
            elem [[1]] <- val5
            
            L[i] <- elem
            
            result5 <- method1_impl (L)
            
            elem [[1]] <- val
            
            L[i] <- elem
            
            dif <- round (as.numeric (result2[1,2]) / as.numeric (result1[1,2]), 2)
            
            dif2 <- round (as.numeric (result3[1,2]) / as.numeric (result1[1,2]), 2)
            
            dif3 <- round (as.numeric (result4[1,2]) / as.numeric (result1[1,2]), 2)
            
            dif4 <- round (as.numeric (result5[1,2]) / as.numeric (result1[1,2]), 2)
            
            result [curindex, 1] <- e_name
            result [curindex, 2] <- dif
            result [curindex, 3] <- dif2
            result [curindex, 4] <- dif3
            result [curindex, 5] <- dif4
            
            curindex <- curindex + 1

          }
          
        }
  
  
  ndx = order(result [,2], decreasing = T)
  
  
  result2 = result [ndx,]
  
        
   N <- (l_len-8)

  
   for (i in 1:N)
   {
     
     name <- result2 [i,1]
     
     descr <- getdescriptionbyname (name)
     
     result2 [i,1] <- descr
   }
  
  nm <- c ("Parameter", "NPV (Parameter*2) / NPV (Parameter)", "NPV (Parameter*4) / NPV (Parameter)", "NPV (Parameter/2) / NPV (Parameter)" , "NPV (Parameter/4) / NPV (Parameter)") 
   
  colnames (result2) <- nm
   
  
  return (result2)
}


method1 <- function(input, output)
{
  req (input)
  req (input$algorithm)
  req (output)
  
  switch (input$algorithm, 
    ALL = {
      
      
      req (input$AccessTechnologyOptionsSet.MediumTypeOfSL)
      req (input$AccessTechnologyOptionsSet.MediumTypeOfCL)
      req (input$AccessTechnologyOptionsSet.MediumTypeOfCLAggr)
      req (input$GeographicalParametersSet.Square)
      req (input$GeographicalParametersSet.BuiltupArea)
      req (input$GeographicalParametersSet.ReliefCoef)
      req (input$AccessTechnologyOptionsSet.RadiusEOA)
      req (input$DevelopmentParametersSet.PrivateHouses)
      req (input$AccessTechnologyOptionsSet.ReduceRadius)
      req (input$DevelopmentParametersSet.BuildingDensity)
      req (input$DevelopmentParametersSet.AverageFloorsLowrise)
      req (input$DevelopmentParametersSet.LowriseMultifamily)
      req (input$DevelopmentParametersSet.AverageMultistorey)
      req (input$DevelopmentParametersSet.MultistoryMultifamily)
      req (input$AccessTechnologyOptionsSet.QuantityFloorsEOA)
      req (input$AccessTechnologyOptionsSet.NumberSubscribersEOA)
      
      
      req (input$PopulationOptionsSet.Population)
      req (input$PopulationOptionsSet.TouristPopulation)
      req (input$AccessLevelOptionsSet.DemandTourist)
      req (input$AccessLevelOptionsSet.Household)
      req (input$DemandOptionsSet.PayableYounger)
      req (input$AccessLevelOptionsSet.DemandPopulationYounger)
      req (input$DemandOptionsSet.PayableAverageAge)
      req (input$AccessLevelOptionsSet.DemandPopulationAverageAge)
      req (input$DemandOptionsSet.PayableAdult)
      req (input$AccessLevelOptionsSet.DemandPopulationAdult)
      req (input$AccessLevelOptionsSet.PopulationCoveredBroadband)
      req (input$AccessTechnologyOptionsSet.MediumTypeOfCL)
      req (input$AccessTechnologyOptionsSet.MaximumLengthCommunicationChannelDistributing)
      req (input$AccessTechnologyOptionsSet.ReservationMode)
      req (input$AccessTechnologyOptionsSet.QuantityEOAToTEOA)
      req (input$AccessTechnologyOptionsSet.MaximumLengthOfSLInBuildings)
      req (input$DevelopmentParametersSet.PercentOfExistingInternalCableDuct)
      req (input$DevelopmentParametersSet.PercentOfExistingPillars)
      req (input$DevelopmentParametersSet.PercentOfExistingExternalDuct)
      req (input$AccessTechnologyOptionsSet.AggregationInternalLines)
      
      
      req (input$AccessTechnologyOptionsSet.AggregationExternalLines)
      req (input$AccessTechnologyOptionsSet.MaximumLengthCommunicationChannelDistributing)
      req (input$AccessTechnologyOptionsSet.MaximumCommunicationChannelAggregative)
      req (input$AccessTechnologyOptionsSet.AggregationExternalLinesDistributing)
      req (input$AccessTechnologyOptionsSet.AggregationExternalLinesAggregative)
      req (input$AccessTechnologyOptionsSet.CostSN)
      req (input$AccessTechnologyOptionsSet.CostEOA)
      req (input$AccessTechnologyOptionsSet.CostTEOADistributing)
      req (input$AccessTechnologyOptionsSet.CostMaterialsEOA)
      req (input$AccessTechnologyOptionsSet.CostMaterialsTEOA)
      req (input$AccessTechnologyOptionsSet.CostSubscribersLines)
      req (input$AccessTechnologyOptionsSet.CostConnectingLines)
      req (input$AccessTechnologyOptionsSet.CostConnectingLinesAggregateSegment)
      req (input$AccessTechnologyOptionsSet.CostInternalSewerage)
      req (input$AccessTechnologyOptionsSet.CostPillars)
      req (input$AccessTechnologyOptionsSet.LaborCostsEOA)
      req (input$AccessTechnologyOptionsSet.CostEquipmentInstallation)
      req (input$AccessTechnologyOptionsSet.LaborCostsTEOA)
      req (input$AccessTechnologyOptionsSet.LaborCostsEquipmentTuneup)
      req (input$AccessTechnologyOptionsSet.CostEquipmentTuneup)
      req (input$AccessTechnologyOptionsSet.LaborCostsInternalSewerageConstruction)
      
      
      
      req (input$AccessTechnologyOptionsSet.CostInternalSewerage)
      req (input$AccessTechnologyOptionsSet.LaborCostsCableSewerage)
      req (input$AccessTechnologyOptionsSet.CostInstallationSubscribersLines)
      req (input$AccessTechnologyOptionsSet.LaborCostsForConstractionOfNewInfrastructure)
      req (input$AccessTechnologyOptionsSet.CostOfConstractionNewInfrastrucutre)
      req (input$AccessTechnologyOptionsSet.LaborCostsSuspensionCable)
      req (input$AccessTechnologyOptionsSet.CostSettingSubscribersLines)
      req (input$AccessTechnologyOptionsSet.LaborCostsCableSewerage)
      req (input$AccessTechnologyOptionsSet.CostCableSewerage)
      req (input$AccessTechnologyOptionsSet.LaborCostsActiveEquipment)
      req (input$AccessTechnologyOptionsSet.PowerConsumptionEOA)
      req (input$AccessTechnologyOptionsSet.CostServiceEequipment)
      req (input$AccessTechnologyOptionsSet.CostElectricity)
      req (input$AccessTechnologyOptionsSet.PowerConsumptionTEOA)
      req (input$AccessTechnologyOptionsSet.LaborCostsLengthInhouseSubscriberLines)
      req (input$AccessTechnologyOptionsSet.CostServiceCableInfrastructure)
      req (input$AccessTechnologyOptionsSet.LaborCostsServiceSuspensionCable)
      req (input$AccessTechnologyOptionsSet.LaborCostsServiceCableSewerage)
      req (input$DevelopmentParametersSet.NormativeAnnualRentOfCableDuct)
      req (input$AccessTechnologyOptionsSet.LaborCostsServicePillars)
      req (input$DevelopmentParametersSet.CostOfOperationNewInfrastructure)
      req (input$DevelopmentParametersSet.RentCostForCableInDuct)
      req (input$DevelopmentParametersSet.RentCostForHangedCable)
      
      
      
      req (input$AccessTechnologyOptionSet.CostOfRFRLicense)
      req (input$PVOptionsSet.DesignConstrCoeff)
      
      req (input$PVOptionSet.ARPU)
      req (input$PVOptionSet.VATax)
      req (input$PVOptionSet.ProfitTax)
      
      req (input$AccessTechnologyOptionSet.AvrLifeTimeOfEqAndMat)
      req (input$PVOptionSet.DiscountRate)
      req (input$PVOptionSet.PaybackPeriod)
      
      .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
      
      result <- method1_impl (input)
      
      output$c_names <- NULL

      output$data <- renderTable(result, colnames=FALSE)
      
      output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
      
      
    },
    ALGORITHM1_1 = {#Number of active AOEs and number of places for their installation
      algorithm1_1 (input, output)
    },
    ALGORITHM1_2 = {#Number of active TAOEs and number of places for their installation
      algorithm1_2 (input, output)
    },
    ALGORITHM1_3 = {#Length of subscriber lines and volume of new infrastracture for hanging-up cables (if any)
          algorithm1_3 (input, output)
      },
    ALGORITHM1_4 = {#Length of connecting lines and volume of new infrastracture for hanging-up cables (if any)
      algorithm1_4 (input, output)
    },
    ALGORITHM1_5 = {#Cost of equipment and materials
      algorithm1_5 (input, output)
    },
    ALGORITHM1_6 = {#COst of work on the construction of an access network
      algorithm1_6 (input, output)
    },
    ALGORITHM1_7 = {#Cost of operating an access network
      algorithm1_7 (input, output)
    },
    ALGORITHM1_8 = {#Calculating NPV
      algorithm1_8 (input, output)
    },
    stop ("No!")
  )
  
}

method1_inv <- function(input, output)
{
  req (input)
  req (input$algorithm)
  req (output)
  
  switch (input$algorithm, 
          ALL = {

#            .GlobalEnv$mylog <- matrix("Detailed Investigation Log:")

            typeofSLMedium <-  mediumnamebynumber (input[["AccessTechnologyOptionsSet.MediumTypeOfSL"]])           
            typeofDLMedium <-  mediumnamebynumber (input[["AccessTechnologyOptionsSet.MediumTypeOfCL"]])           
            typeofALMedium <-  mediumnamebynumber (input[["AccessTechnologyOptionsSet.MediumTypeOfCLAggr"]])           
            
            
            result <- method1_inv_impl (input)
            
            output$c_names <- NULL
            
            output$data <- renderTable(result, colnames=TRUE)
            
           # output$log <- NULL
            
#            .GlobalEnv$mylog <- .GlobalEnv$mylog <- matrix (paste (i18n$t("Medium for subscriber's line segment", typeofSLMedium, "Medium for aggregation line segment", typeofDLMedium, "Medium for aggregation line segment"), typeofALMedium, sep = ": "))                                  
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          ALGORITHM1_1 = {#Number of active AOEs and number of places for their installation
            #algorithm1_1 (input, output)
          },
          stop ("No!")
  )
}
