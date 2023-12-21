library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Number of active AOEs and number of places for their installation

#Determination of building area
formula_1_1_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  result =  input$GeographicalParametersSet.Square * (input$GeographicalParametersSet.BuiltupArea/100)
  
  result <- round (as.numeric (result), digits = 2)
    
  return (result)
}

#Determination of the number of places to place AOEs to cover the undeveloped area of the object
formula_1_1_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  BuiltupArea = input$Intermediate.BuiltupArea
  
  
  if (!is.null(intermediate))
    BuiltupArea = intermediate$BuiltupArea
  
  undevArea = input$GeographicalParametersSet.Square - BuiltupArea
  
  realRadius = input$GeographicalParametersSet.ReliefCoef*input$AccessTechnologyOptionsSet.RadiusEOA
  
  result <- 0
  
  if ((undevArea <= 0 ) || (realRadius <=0))
  {
    result = 0
  }
  else
  {
    result =  round (undevArea/(pi*(realRadius^2)), digits = 0)  + 1  
  }
  
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

#Determination of the number of places to place AOEs to cover the area of an object built up with private sector (low-rise) buildings 
formula_1_1_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  BuiltupArea = input$Intermediate.BuiltupArea
  
  if (!is.null(intermediate))
    BuiltupArea = intermediate$BuiltupArea
  
  
  privateArea = BuiltupArea*(input$DevelopmentParametersSet.PrivateHouses/100)
  
  realRadius2 = input$AccessTechnologyOptionsSet.RadiusEOA*input$AccessTechnologyOptionsSet.ReduceRadius
  
  result <- 0
  
  if ((privateArea <= 0 ) || (realRadius2 <=0))
  {
    result = 0
  }
  else
  {
    result = round ((privateArea/(pi*(realRadius2^2))),digits = 0) + 1
  }
  
  result <- round (as.numeric (result), digits = 0)
  
  return (result)
}

#Determination of the number of places for placement of AOEs to cover the area of an object built up in multi-apartment buildings
formula_1_1_4 <- function (input, intermediate = NULL)
{
  req (input)
  
  BuildingArea =  input$GeographicalParametersSet.Square * (input$GeographicalParametersSet.BuiltupArea/100)            
  
  NumberOfBuildings = BuildingArea*input$DevelopmentParametersSet.BuildingDensity
  
  MeanFlorNumber = input$DevelopmentParametersSet.AverageFloorsLowrise*
    (input$DevelopmentParametersSet.LowriseMultifamily/100)+
    input$DevelopmentParametersSet.AverageMultistorey*
    2*(input$DevelopmentParametersSet.MultistoryMultifamily/100)
  
  
  result <- 0
  
  if ((NumberOfBuildings <= 0) || (MeanFlorNumber <=0) || (input$AccessTechnologyOptionsSet.QuantityFloorsEOA <= 0))
  {
    result = 0
  }
  else
  {
    result =  round ((NumberOfBuildings*MeanFlorNumber)/input$AccessTechnologyOptionsSet.QuantityFloorsEOA, digits = 0) + 1
  }
  
  result <- round (as.numeric (result), digits = 0)
  
  return (result)
}

#Determination of the number of places to place AOEs, determined by the building area
formula_1_1_5 <- function (input, intermediate = NULL)
{
  req (input)

  BuiltupArea <- 0
  BuiltupArea <- input$Intermediate.BuiltupArea
  
  if (!is.null(intermediate))
  {
    BuiltupArea <- intermediate$BuiltupArea
  }
    
  
  result <- 0 
  
  if ((BuiltupArea <= 0)|(input$AccessTechnologyOptionsSet.RadiusEOA <= 0))
  {
    result <- 0
  }
  else
  {
    result <-  round (BuiltupArea/(pi*(input$AccessTechnologyOptionsSet.RadiusEOA)^2), digits = 0) + 1  
  }
  
  result <- round (as.numeric (result), digits = 0)
  
  return (result)
}

#Determination of the number of buildings in the built-up part of the object
formula_1_1_6 <- function (input, intermediate = NULL)
{
  req (input)
  
  BuiltupArea = input$Intermediate.BuiltupArea
  
  if (!is.null(intermediate))
    BuiltupArea = intermediate$BuiltupArea
  
  result =  round (BuiltupArea*
                     input$DevelopmentParametersSet.BuildingDensity*
                     ((input$DevelopmentParametersSet.PrivateHouses/100)/input$AccessTechnologyOptionsSet.NumberSubscribersEOA+
                        (input$DevelopmentParametersSet.LowriseMultifamily+
                           input$DevelopmentParametersSet.MultistoryMultifamily)/100), digits = 0) + 1
  
  
  result <- round (as.numeric (result), digits = 0)
  
  return (result)
}

#Determination of the base population for calculating the potential subscriber base
formula_1_1_7 <- function (input, intermediate = NULL)
{
  req (input)
  
  result =  input$PopulationOptionsSet.Population + input$PopulationOptionsSet.TouristPopulation*(input$AccessLevelOptionsSet.DemandTourist/100)
  
  result <- round (as.numeric (result), digits = 0)
  
  return (result)
}

#Determination of the base number of solvent population in this object
formula_1_1_8 <- function (input, intermediate = NULL)
{
  req (input)
  
  BaseNumberOfPopulation = input$Intermediate.BaseNumberOfPopulation
  
  if (!is.null(intermediate))
    BaseNumberOfPopulation = intermediate$BaseNumberOfPopulation
  
  
  result =  round((BaseNumberOfPopulation/input$AccessLevelOptionsSet.Household)*
                    (((input$DemandOptionsSet.PayableYounger*input$AccessLevelOptionsSet.DemandPopulationYounger)/10000)+
                       ((input$DemandOptionsSet.PayableAverageAge*input$AccessLevelOptionsSet.DemandPopulationAverageAge)/10000)+
                       ((input$DemandOptionsSet.PayableAdult*input$AccessLevelOptionsSet.DemandPopulationAdult)/10000)))
  
  
  result <- round (as.numeric (result), digits = 0)
  
  return (result)
}

#Determination of the number of solvent population not covered by competitors (number of potential subscribers)
formula_1_1_9 <- function (input, intermediate = NULL)
{
  req (input)

  BaseSubscribers <- 0
  BaseSubscribers <- input$Intermediate.BaseSubscribers
  
  if (!is.null(intermediate))
    BaseSubscribers = intermediate$BaseSubscribers
  
  result =  round(BaseSubscribers*(1-(input$AccessLevelOptionsSet.PopulationCoveredBroadband/100)))
  
  result <- round (as.numeric (result), digits = 0)
  
  return (result)
}

#Determining the number of AOE units enough to cover the required number of subscribers
formula_1_1_10 <- function (input, intermediate = NULL)
{
  req (input)
  
  Subscribers <- 0 
  Subscribers <- input$Intermediate.Subscribers
  
  if (!is.null(intermediate))
    Subscribers = intermediate$Subscribers
  
  
  result =  round(Subscribers/input$AccessTechnologyOptionsSet.NumberSubscribersEOA, digits = 0) + 1
  
  return (result)
}

algorithm1_1_impl <- function(input)#, .GlobalEnv$mylog)
{
  #Determination of building area
  BuildingArea =  formula_1_1_1 (input)
  

  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste ("Building area, sq km", BuildingArea, sep = ": "))
  
  
  NumberOfAOEPlaces <- 0
  BaseNumberOfPopulation <- 0
  
  intermediate <- list (BuiltupArea = 0.0, BaseNumberOfPopulation = 0, BaseSubscribers = 0, Subscribers = 0)
  
  intermediate$BuiltupArea <- BuildingArea
  
  switch (input$AccessTechnologyOptionsSet.MediumTypeOfSL,
          "1" = {#Wireless
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Medium Type for Subscriber Lines"), i18n$t("Wireless"), sep = ": "))
            
            #Determination of the number of places to place AOEs to cover the undeveloped area of the object
            
            
            UndevelopedAreaCount =  formula_1_1_2 (input, intermediate)
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of places to place AOEs to cover the undeveloped area of the object, places"), UndevelopedAreaCount, sep = ": "))
            
            #Determination of the number of places to place AOEs to cover the area of an object built up with private sector (low-rise) buildings 
            
            PrivateSectorCount =  formula_1_1_3 (input, intermediate)
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of places to place AOEs to cover the area of an object built up with private sector (low-rise) buildings, places"), PrivateSectorCount, sep = ": "))
            
            #Determination of the number of places for placement of AOEs to cover the area of an object built up in multi-apartment buildings
            
            MultiApartmentCount =  formula_1_1_4 (input)
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of places for placement of AOEs to cover the area of an object built up in multi-apartment buildings, places"), MultiApartmentCount, sep = ": "))
            
            NumberOfAOEPlaces = UndevelopedAreaCount + 
              PrivateSectorCount + 
              MultiApartmentCount
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total number of places for placement of AOEs, places"), NumberOfAOEPlaces, sep = ": "))
            
            
            #Determination of the base population for calculating the potential subscriber base                
            BaseNumberOfPopulation = formula_1_1_7 (input)
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Base population for calculating the potential subscriber base, people"), BaseNumberOfPopulation, sep = ": "))                
            
          },
          "2" = {#Copper
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Medium Type for Subscriber Lines"), i18n$t("Copper"), sep = ": "))
            
            #Determination of the number of places to place AOEs, determined by the building area              
            
            NumberOfAOEPlacesByBuilding =  formula_1_1_5 (input, intermediate)                
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of places to place AOEs, determined by the building area, places"), NumberOfAOEPlacesByBuilding, sep = ": "))                                
            
            #Determination of the number of buildings in the built-up part of the object
            
            NumberOfAOEPlacesByBuiltUp =  formula_1_1_6 (input, intermediate)                
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of buildings in the built-up part of the object, buildings"), NumberOfAOEPlacesByBuiltUp, sep = ": "))                                
            
            NumberOfAOEPlaces = min (NumberOfAOEPlacesByBuilding, NumberOfAOEPlacesByBuiltUp)
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total number of places for placement of AOEs, places"), NumberOfAOEPlaces, sep = ": "))                
            
            BaseNumberOfPopulation = input$PopulationOptionsSet.Population
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Base population for calculating the potential subscriber base, people"), BaseNumberOfPopulation, sep = ": "))                
          },
          "3" = {#Fiber-optic
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Medium Type for Subscriber Lines"), i18n$t("Fiber-optic"), sep = ": "))
            
            #Determination of the number of places to place AOEs, determined by the building area              
            
            
            NumberOfAOEPlaces =  formula_1_1_5 (input, intermediate)  
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total number of places for placement of AOEs, places"), NumberOfAOEPlaces, sep = ": "))                
            
            BaseNumberOfPopulation = input$PopulationOptionsSet.Population
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Base population for calculating the potential subscriber base, people"), BaseNumberOfPopulation, sep = ": "))                
          }
  )

  intermediate$BaseNumberOfPopulation = BaseNumberOfPopulation
  
  #Determination of the base number of solvent population in this object
  
  BaseSubscribers =  formula_1_1_8 (input, intermediate)
  
  intermediate$BaseSubscribers = BaseSubscribers
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Base number of solvent population in this object, people"), BaseSubscribers, sep = ": "))                
  
  #Determination of the number of solvent population not covered by competitors (number of potential subscribers)            
  Subscribers =  formula_1_1_9 (input, intermediate)
  intermediate$Subscribers = Subscribers
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of solvent population not covered by competitors (number of potential subscribers), subscribers"), Subscribers, sep = ": "))                            
  
  #Determining the number of AOE units enough to cover the required number of subscribers
  NumberAOEToCoverSubsribers =   formula_1_1_10 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of AOE units enough to cover the required number of subscribers, units"), NumberAOEToCoverSubsribers, sep = ": "))
  
  #Number of AOE units
  NumberOfAOEs = max (NumberOfAOEPlaces, NumberAOEToCoverSubsribers)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of AOE units, units"), NumberOfAOEs, sep = ": "))
  
  result = matrix (nrow = 4, ncol = 2)
  
  #returnvalue$result <- matrix (nrow = 3, ncol = 2)
  
  result [1,1] <- i18n$t("Number of solvent population not covered by competitors (number of potential subscribers), people")
  result [1,2] <- Subscribers
  result [2,1] <- i18n$t("Number of places to place AOEs, determined by the building area, places")
  result [2,2] <- NumberOfAOEPlaces
  result [3,1] <- i18n$t("Number of AOE units, units")
  result [3,2] <- NumberOfAOEs
  result [4,1] <- i18n$t("Building area, sq km")
  result [4,2] <- BuildingArea
  
  #returnvalue$.GlobalEnv$mylog <- .GlobalEnv$mylog
  
  return (result)
  
}

algorithm1_1 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            req (input$AccessTechnologyOptionsSet.MediumTypeOfSL)
            req (input$GeographicalParametersSet.Square)
            req (input$GeographicalParametersSet.BuiltupArea)
            req (input$GeographicalParametersSet.Square)
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
            req (input$AccessTechnologyOptionsSet.NumberSubscribersEOA)

           # rm (globalenv()$.GlobalEnv$mylog)
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result = algorithm1_1_impl (input) #, .GlobalEnv$mylog)
            
            #.GlobalEnv$mylog <- globalenv()$.GlobalEnv$mylog
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames = FALSE)
            
            #output$log <- renderTable(result$.GlobalEnv$mylog, colnames = FALSE)
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
          },
          FORMULA_1_1_1 = {#Determination of building area
            req (input$GeographicalParametersSet.Square)
            req (input$GeographicalParametersSet.BuiltupArea)
            
            result =  formula_1_1_1 (input)
            
            output$data <- renderTable(result, colnames = FALSE)
          },
          FORMULA_1_1_2 = {#Determination of the number of places to place AOEs to cover the undeveloped area of the object
            req (input$GeographicalParametersSet.Square)
            req (input$Intermediate.BuiltupArea)
            req (input$GeographicalParametersSet.ReliefCoef)
            req (input$AccessTechnologyOptionsSet.RadiusEOA)
            
            result =  formula_1_1_2 (input)          
            
            output$data <- renderTable(result, colnames = FALSE)
            
            
          },
          FORMULA_1_1_3 = {#Determination of the number of places to place AOEs to cover the area of an object built up with private sector (low-rise) buildings 
            req (input$Intermediate.BuiltupArea)
            req (input$DevelopmentParametersSet.PrivateHouses)
            req (input$AccessTechnologyOptionsSet.RadiusEOA)
            req (input$AccessTechnologyOptionsSet.ReduceRadius)
            
            result =  formula_1_1_3 (input)
            
            output$data <- renderTable(result, colnames = FALSE)
            
            
          },
          FORMULA_1_1_4 = {#Determination of the number of places for placement of AOEs to cover the area of an object built up in multi-apartment buildings
            req (input$GeographicalParametersSet.Square)
            req (input$GeographicalParametersSet.BuiltupArea)
            req (input$DevelopmentParametersSet.BuildingDensity)
            req (input$DevelopmentParametersSet.AverageFloorsLowrise)
            req (input$DevelopmentParametersSet.LowriseMultifamily)
            req (input$DevelopmentParametersSet.AverageMultistorey)
            req (input$DevelopmentParametersSet.MultistoryMultifamily)
            req (input$AccessTechnologyOptionsSet.QuantityFloorsEOA)

            result =  formula_1_1_4 (input)
            
            output$data <- renderTable(result, colnames = FALSE)
            
            
          },
          FORMULA_1_1_5 = {#Determination of the number of places to place AOEs, determined by the building area
            req (input$Intermediate.BuiltupArea)
            req (input$AccessTechnologyOptionsSet.RadiusEOA)
            
            result =  formula_1_1_5 (input)              
            
            output$data <- renderTable(result, colnames = FALSE)
            
          },
          FORMULA_1_1_6 = {#Determination of the number of buildings in the built-up part of the object
            req (input$Intermediate.BuiltupArea)
            req (input$DevelopmentParametersSet.BuildingDensity)
            req (input$DevelopmentParametersSet.PrivateHouses)
            req (input$AccessTechnologyOptionsSet.NumberSubscribersEOA)
            req (input$DevelopmentParametersSet.LowriseMultifamily)
            req (input$DevelopmentParametersSet.MultistoryMultifamily)
            
            result =  formula_1_1_6 (input)
            
            output$data <- renderTable(result, colnames = FALSE)
            
          },
          FORMULA_1_1_7 = {#Determination of the base population for calculating the potential subscriber base
            req (input$PopulationOptionsSet.Population)
            req (input$PopulationOptionsSet.TouristPopulation)
            req (input$AccessLevelOptionsSet.DemandTourist)
            
            result =  formula_1_1_7 (input)
            
            output$data <- renderTable(result, colnames = FALSE)
            
          },
          FORMULA_1_1_8 = {#Determination of the base number of solvent population in this object
            req (input$Intermediate.BaseNumberOfPopulation)
            req (input$AccessLevelOptionsSet.Household)
            req (input$DemandOptionsSet.PayableYounger)
            req (input$AccessLevelOptionsSet.DemandPopulationYounger)
            req (input$DemandOptionsSet.PayableAverageAge)
            req (input$AccessLevelOptionsSet.DemandPopulationAverageAge)
            req (input$DemandOptionsSet.PayableAdult)
            req (input$AccessLevelOptionsSet.DemandPopulationAdult)
            
            result =  formula_1_1_8 (input)
            
            output$data <- renderTable(result, colnames = FALSE)
            
          },
          FORMULA_1_1_9 = {#Determination of the number of solvent population not covered by competitors (number of potential subscribers)
            req (input$Intermediate.BaseSubscribers)
            req (input$AccessLevelOptionsSet.PopulationCoveredBroadband)
            
            result =  formula_1_1_9 (input)
            
            output$data <- renderTable(result, colnames = FALSE)
            
          },
          FORMULA_1_1_10 = {#Determining the number of AOE units enough to cover the required number of subscribers
            
            req (input$Intermediate.Subscribers)
            req (input$AccessTechnologyOptionsSet.NumberSubscribersEOA)
            
            result =  formula_1_1_10 (input)
            
            output$data <- renderTable(result, colnames = FALSE)
            
          },
          stop ("No!")
  )
  
}