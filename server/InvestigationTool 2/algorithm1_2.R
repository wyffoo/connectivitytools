library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Number of active TAOEs and number of places for their installation

#Determination of the number of places for placement of TAOEs to cover all AOEs, uniformly located throughout the object
formula_1_2_1 <- function (input, intermediate = NULL)
{
  req (input)
  NetworkArea <- 0 
  NetworkArea <- input$Intermediate.NetworkArea
  
  
  if (!is.null(intermediate))
  {
    NetworkArea <- as.numeric (intermediate$NetworkArea)
  }
  
  result =  round (NetworkArea/(pi*(input$GeographicalParametersSet.ReliefCoef*
          input$AccessTechnologyOptionsSet.MaximumLengthCommunicationChannelDistributing)^2), digits = 0) + 1
  
  
  return (result)
}

#Network radius determination
formula_1_2_2 <- function (input, intermediate = NULL)
{
  req (input)

  NetworkArea = input$Intermediate.NetworkArea
  
  if (!is.null(intermediate))
    NetworkArea = intermediate$NetworkArea
  
  result =  sqrt(NetworkArea/pi)
  
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

#Determination of the number of TAOE?s units required to connect a given number of AOEs throughout the object
formula_1_2_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfAOEsPlaces = input$Intermediate.NumberOfAOEsPlaces
  
  if (!is.null(intermediate))
    NumberOfAOEsPlaces = intermediate$NumberOfAOEsPlaces
  
  
  result =  round ((NumberOfAOEsPlaces*input$AccessTechnologyOptionsSet.ReservationMode)/
                     input$AccessTechnologyOptionsSet.QuantityEOAToTEOA, digits = 0) + 1
  
  return (result)
}

algorithm1_2_impl <- function(input, intermediate = NULL)
{
  
  NetworkArea <- 0

  BuiltupArea <- as.numeric (input$Intermediate.BuiltupArea)
  
  if (!is.null(intermediate))
    BuiltupArea <- as.numeric (intermediate$BuiltupArea)
  
  
  switch (input$AccessTechnologyOptionsSet.MediumTypeOfCL,
          "1" = {#Wireless
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Medium Type of Destribution Segment"), i18n$t("Wireless"), sep = ": "))                
            
            NetworkArea <- as.numeric(input$GeographicalParametersSet.Square)
          },
          "2" = {#Copper
            
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Medium Type of Destribution Segment"), i18n$t("Copper"), sep = ": "))                
            
            NetworkArea <- as.numeric (BuiltupArea)
          },
          "3" = {#Fiber-optic
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Medium Type of Destribution Segment"), i18n$t("Fiber-Optic"), sep = ": "))                
            NetworkArea <- as.numeric (BuiltupArea)
          }
  )
  
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Network Area, sq km"), NetworkArea, sep = ": "))                
  
  intermediate2 <- list (NetworkArea = 0.0)
  
  intermediate2$NetworkArea <- NetworkArea
  
  
  
  #Determination of the number of places for placement of TAOEs to cover all AOEs, uniformly located throughout the object
  
  NumberOfTAOEsPlaces = formula_1_2_1 (input, intermediate2)
  
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of places for placement of TAOEs to cover all AOEs, uniformly located throughout the object, places"), NumberOfTAOEsPlaces, sep = ": "))                
  
  #Network radius determination
  
  
  NetworkRadius = formula_1_2_2 (input, intermediate2)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Network Radius, km"), NetworkRadius, sep = ": "))                
  
  #Determination of the number of TAOEs units required to connect a given number of AOEs throughout the object
  NumberOfTAOEs = formula_1_2_3 (input, intermediate)
  
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of TAOEs units required to connect a given number of AOEs throughout the object, units"), NumberOfTAOEs, sep = ": "))                
  
  NumberOfTAOEs = max (NumberOfTAOEs, NumberOfTAOEsPlaces)          
  
  result = matrix (nrow = 4, ncol = 2)
  
  result [1,1] = i18n$t("Number of places for placement of TAOEs to cover all AOEs, uniformly located throughout the object, places")
  result [1,2] = NumberOfTAOEsPlaces
  result [2,1] = i18n$t("Number of TAOEs units required to the object, units")
  result [2,2] = NumberOfTAOEs
  result [3,1] = i18n$t("Network Area, sq km")
  result [3,2] = NetworkArea
  result [4,1] = i18n$t("Network Radius, km")
  result [4,2] = NetworkRadius
  
  
  return (result)
  
}

algorithm1_2 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  
  switch (input$formula, 
          ALL = {

            req (input$AccessTechnologyOptionsSet.MediumTypeOfCL)
            req (input$GeographicalParametersSet.Square)
            req (input$Intermediate.BuiltupArea)
            req (input$GeographicalParametersSet.ReliefCoef)
            req (input$AccessTechnologyOptionsSet.MaximumLengthCommunicationChannelDistributing)
            req (input$Intermediate.NumberOfAOEsPlaces)
            req (input$AccessTechnologyOptionsSet.ReservationMode)
            req (input$AccessTechnologyOptionsSet.QuantityEOAToTEOA)
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result = algorithm1_2_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames = FALSE)
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
          },
          FORMULA_1_2_1 = {#Determination of the number of places for placement of TAOEs to cover all AOEs, uniformly located throughout the object
            req (input$Intermediate.NetworkArea)
            req (input$GeographicalParametersSet.ReliefCoef)
            req (input$AccessTechnologyOptionsSet.MaximumLengthCommunicationChannelDistributing)
            
            result =  formula_1_2_1 (input) 
            
            output$data <- renderTable(result, colnames = FALSE)
          },
          FORMULA_1_2_2 = {#Network radius determination
            req (input$Intermediate.NetworkArea)
            
            result =  formula_1_2_2 (input) 
              
            output$data <- renderTable(result, colnames = FALSE)
            
          },
          FORMULA_1_2_3 = {#Determination of the number of TAOE?s units required to connect a given number of AOEs throughout the object
            req (input$Intermediate.NumberOfAOEsPlaces)
            req (input$AccessTechnologyOptionsSet.ReservationMode)
            req (input$AccessTechnologyOptionsSet.QuantityEOAToTEOA)
            
            result =  formula_1_2_3 (input) 
              
            output$data <- renderTable(result, colnames = FALSE)
            
          },
          stop ("No!")
  )
  
}