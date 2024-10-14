library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Length of subscriber lines and volume of new infrastracture for hanging-up cables (if any)

#Determination of the number of subscribers located in the near (30% of the maximum distance), middle (60% of the maximum distance) and far (100% of the distance) zones from the AOEs
formula_1_3_1 <- function (input, intermediate = NULL)
{
  req (input)
  Subscribers <- 0 
  Subscribers <- input$Intermediate.Subscribers
  
  if (!is.null(intermediate))
  {
    Subscribers <- as.numeric (intermediate$Subscribers)
  }

  
  result <- matrix (nrow = 3, ncol = 2)
  
  result [1,2] = round (Subscribers*0.09)
  result [1,1] = i18n$t("Number of subscribers located in the near (30% of the maximum distance) zone from the AOEs")
  result [2,2] = round (Subscribers*0.27)
  result [2,1] = i18n$t("Number of subscribers located in the middle (60% of the maximum distance) zone from the AOEs")
  result [3,2] = round (Subscribers*0.64)
  result [3,1] = i18n$t("Number of subscribers located in the far (100% of the distance) zone from the AOEs")
  
  
  return (result)
}

#Determination of the length of subscriber lines within the buildings and outside the buildings to cover subscribers located in the near, middle and far zones from the AOEs
formula_1_3_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  SubscribersNear <- 0 
  SubscribersNear <- as.numeric (input$Intermediate.SubscribersNear)
  
  SubscribersMiddle <- 0
  SubscribersMiddle <- as.numeric (input$Intermediate.SubscribersMiddle)
  
  SubscribersFar <- 0
  SubscribersFar <- as.numeric (input$Intermediate.SubscribersFar)
  
  NearLength <- 0
  NearLength <- as.numeric (input$Intermediate.NearLength)
  
  MiddleLength <- 0
  MiddleLength <- as.numeric (input$Intermediate.MiddleLength)
  
  FarLength <- 0
  FarLength <- as.numeric (input$Intermediate.FarLength)
  
  RadiusEOA <- 0
  RadiusEOA <- as.numeric (input$AccessTechnologyOptionsSet.RadiusEOA)
  
  
  if (!is.null(intermediate))
  {
    SubscribersNear <- as.numeric (intermediate$SubscribersNear)
    SubscribersMiddle <- as.numeric (intermediate$SubscribersMiddle)
    SubscribersFar <- as.numeric (intermediate$SubscribersFar)
    NearLength <- as.numeric (intermediate$NearLength)
    MiddleLength <- as.numeric (intermediate$MiddleLength)
    FarLength <- as.numeric (intermediate$FarLength)
    RadiusEOA <- as.numeric (intermediate$RadiusEOA)
  }
  
  
  result <- matrix (nrow = 2, ncol = 2)
  result [1,1] = i18n$t("Length of subscriber lines within the buildings to cover subscribers located in the near, middle and far zones from the AOEs")
  result [1,2] <- SubscribersNear*NearLength +
    SubscribersMiddle*MiddleLength +
    SubscribersFar*FarLength
  
  
  
  result [2,1] = i18n$t("Length of subscriber lines outside the buildings to cover subscribers located in the near, middle and far zones from the AOEs")
  result [2,2] <- SubscribersNear*(0.3*input$AccessTechnologyOptionsSet.RadiusEOA - NearLength)+
    SubscribersMiddle*(0.6*input$AccessTechnologyOptionsSet.RadiusEOA - MiddleLength)+
    SubscribersFar*(input$AccessTechnologyOptionsSet.RadiusEOA - FarLength)
  
  if (result [2,2] < 0 )
    result [2,2] <- 0
  
  result [1,2] <- round (as.numeric (result [1,2]), digits = 2)
  result [2,2] <- round (as.numeric (result [2,2]), digits = 2)
  
  return (result)
}

formula_1_3_3 <- function (input, intermediate = NULL)
{
  req (input)
  LinesInBuildingsLength <- 0
  LinesInBuildingsLength <- as.numeric (input$Intermediate.LinesInBuildingsLength)
  
  LinesOutsideLength <- 0 
  LinesOutsideLength <- as.numeric (input$Intermediate.LinesOutsideLength)

  
  if (!is.null(intermediate))
  {
    LinesInBuildingsLength <- as.numeric (intermediate$LinesInBuildingsLength)
    LinesOutsideLength <- as.numeric (intermediate$LinesOutsideLength)
  }
  
  
  result <- matrix (nrow = 2, ncol = 2)
  
  result [1,1] = i18n$t("Determining the length of aggregated cables for internal lines ")
  result [1,2] <- LinesInBuildingsLength*input$AccessTechnologyOptionsSet.AggregationInternalLines
  
  result [2,1] = i18n$t("Determining the length of aggregated cables for external lines ")
  result [2,2] <- LinesOutsideLength*input$AccessTechnologyOptionsSet.AggregationExternalLines
  

  result [1,2] <- round (as.numeric (result [1,2]), digits = 2)
  result [2,2] <- round (as.numeric (result [2,2]), digits = 2)
  
  
  return (result)
}

algorithm1_3_impl <- function(input, intermediate = NULL)
{
  
  
  result <- matrix (nrow = 9, ncol = 2)
  
  result [1,1] = i18n$t("Length of aggregated lines in the building, km")
  result [1,2] = 0
  result [2,1] = i18n$t("Length of aggregated lines outside the buildings, km")
  result [2,2] = 0
  result [3,1] = i18n$t("Length of subscriber lines within buildings, km")
  result [3,2] = 0
  result [4,1] = i18n$t("Length of subscriber lines in the external cable ducts, km")
  result [4,2] = 0
  result [5,1] = i18n$t("Length of the hanged-up cables of subscriber lines, km")
  result [5,2] = 0
  result [6,1] = i18n$t("Length of the house cable duct that must be built for laying subscriber lines, km")
  result [6,2] = 0
  result [7,1] = i18n$t("Length of the new infrastructure for hanging-up cables of subscriber lines, km")
  result [7,2] = 0
  result [8,1] = i18n$t("Length of subscriber lines within the buildings, km")
  result [8,2] = 0
  result [9,1] = i18n$t("Length of subscriber lines outside of the buildings, km")
  result [9,2] = 0
  
  
  if (input$AccessTechnologyOptionsSet.MediumTypeOfSL == 1)
  {
    
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Medium Type for Subscriber Lines"), i18n$t("Wireless"), sep = ": "))
    
    return(result)
  }
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Medium Type for Subscriber Lines"), i18n$t("Copper or Fiber"), sep = ": "))
  
  
  #Determination of the number of subscribers located in the near (30% of the maximum distance), middle (60% of the maximum distance) and far (100% of the distance) zones from the AOEs
  
  res <- formula_1_3_1 (input, intermediate)
  
  SubscribersNear <- res [1,2]
  SubscribersMiddle <- res [2,2]
  SubscribersFar <- res [3,2]
  
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of subscribers located in the near (30% of the maximum distance) zones from the AOEs, subsribers"), SubscribersNear, sep = ": "))            
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of subscribers located in the middle (60% of the maximum distance) zones from the AOEs, subsribers"), SubscribersMiddle, sep = ": "))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of subscribers located in the far (100% of the distance) zones from the AOEs, subsribers"), SubscribersFar, sep = ": "))            
  
  #Determination of the length of subscriber lines within the buildings and outside the buildings to cover subscribers located in the near, middle and far zones from the AOEs
  
  NearLength = min (0.3*input$AccessTechnologyOptionsSet.RadiusEOA, input$AccessTechnologyOptionsSet.MaximumLengthOfSLInBuildings)
  MiddleLength  = min (0.6*input$AccessTechnologyOptionsSet.RadiusEOA, input$AccessTechnologyOptionsSet.MaximumLengthOfSLInBuildings)
  FarLength = min (input$AccessTechnologyOptionsSet.RadiusEOA, input$AccessTechnologyOptionsSet.MaximumLengthOfSLInBuildings)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Length of subscriber lines within the buildings and outside the buildings to cover subscribers located in the near (30% of the maximum distance) zones from the AOEs, km"), NearLength, sep = ": "))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Length of subscriber lines within the buildings and outside the buildings to cover subscribers located in the middle (60% of the maximum distance) zones from the AOEs, km"), MiddleLength, sep = ": "))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Length of subscriber lines within the buildings and outside the buildings to cover subscribers located in the far (100% of the distance) zones from the AOEs, km"), FarLength, sep = ": "))                        
  
  intermediate2 <- list (SubscribersNear = 0, SubscribersMiddle = 0, SubscribersFar = 0, NearLength = 0.0, MiddleLength = 0.0, FarLength = 0.0  )
  
  intermediate2$SubscribersNear <- SubscribersNear
  intermediate2$SubscribersMiddle <- SubscribersMiddle
  intermediate2$SubscribersFar <- SubscribersFar
  
  intermediate2$NearLength <- NearLength
  intermediate2$MiddleLength <- MiddleLength
  intermediate2$FarLength <- FarLength
  
  res <- formula_1_3_2 (input, intermediate2)
  
  LinesInBuildingsLength <- res [1,2]
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Length of subscriber lines within the buildings, km"), LinesInBuildingsLength, sep = ": "))            
  

  LinesOutsideLength <- res [2,2]         
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Length of subscriber lines outside of the buildings, km"), LinesOutsideLength, sep = ": "))            
  
  #Determining the length of aggregated cables for internal and external lines       
  
  intermediate3 <- list  (LinesInBuildingsLength = 0.0, LinesOutsideLength = 0.0 )
  
  intermediate3$LinesInBuildingsLength <- LinesInBuildingsLength
  intermediate3$LinesOutsideLength <- LinesOutsideLength
  
  res <- formula_1_3_3 (input, intermediate3)
  
  LengthAggrForInternal <- as.numeric ( res[1,2])
  
  LengthAggrForExternal <- as.numeric ( res[2,2])
  
  
  # Length of aggregated lines in the building
  result [1,2] = LengthAggrForInternal
  # Length of aggregated lines outside the buildings
  result [2,2] = LengthAggrForExternal
  
  
  
  
  # Length of subscriber lines within buildings
  result [3,2] = LengthAggrForInternal
  # Length of subscriber lines in the external cable ducts
  result [4,2] = LengthAggrForExternal * (input$DevelopmentParametersSet.PercentOfExistingExternalDuct/100)
  # Length of the hanged-up cables of subscriber lines
  result [5,2] = LengthAggrForExternal*(input$DevelopmentParametersSet.PercentOfExistingPillars/100)
  # Length of the house cable duct that must be built for laying subscriber lines
  result [6,2] = LengthAggrForInternal*(100-input$DevelopmentParametersSet.PercentOfExistingInternalCableDuct)/100
  # Length of the new infrastructure for hanging-up cables of subscriber lines 
  result [7,2] = LengthAggrForExternal - as.numeric (result [4,2]) - as.numeric (result [5,2])
  
  if (as.numeric (result [7,2]) < 0)
    result [7,2] = 0
  
  # Length of the hanged-up cables of subscriber lines
  result [5,2] = as.numeric(result [5,2]) + as.numeric(result [7,2])
  
  result [8,2] = LinesInBuildingsLength
  
  result [9,2] = LinesOutsideLength
  
  return (result)
}



algorithm1_3 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            req (input$AccessTechnologyOptionsSet.MediumTypeOfSL)
            req (input$AccessTechnologyOptionsSet.MaximumLengthOfSLInBuildings)
            req (input$DevelopmentParametersSet.PercentOfExistingInternalCableDuct)
            req (input$DevelopmentParametersSet.PercentOfExistingPillars)
            req (input$DevelopmentParametersSet.PercentOfExistingExternalDuct)
            req (input$Intermediate.Subscribers)
            req (input$AccessTechnologyOptionsSet.RadiusEOA)
            req (input$AccessTechnologyOptionsSet.AggregationInternalLines)
            req (input$AccessTechnologyOptionsSet.AggregationExternalLines)
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result = algorithm1_3_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_1_3_1 = {#Determination of the number of subscribers located in the near (30% of the maximum distance), middle (60% of the maximum distance) and far (100% of the distance) zones from the AOEs
            req (input$Intermediate.Subscribers)
            
            result =  formula_1_3_1 (input) 
            
            output$data <- renderTable(result, colnames=FALSE)
            
            
          },
          FORMULA_1_3_2 = {#Determination of the length of subscriber lines within the buildings and outside the buildings to cover subscribers located in the near, middle and far zones from the AOEs
            req (input$Intermediate.SubscribersNear)
            req (input$Intermediate.SubscribersMiddle)
            req (input$Intermediate.SubscribersFar)
            req (input$Intermediate.NearLength)
            req (input$Intermediate.MiddleLength)
            req (input$Intermediate.FarLength)
            req (input$AccessTechnologyOptionsSet.RadiusEOA)
            
            result =  formula_1_3_2 (input) 
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_3_3 = {#Determining the length of aggregated cables for internal and external lines 
          
            req (input$Intermediate.LinesInBuildingsLength)
            req (input$Intermediate.LinesOutsideLength)
            req (input$AccessTechnologyOptionsSet.AggregationInternalLines)
            req (input$AccessTechnologyOptionsSet.AggregationExternalLines)
            
            result =  formula_1_3_3 (input) 
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          stop ("No!")
  )
  
}