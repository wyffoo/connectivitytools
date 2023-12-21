library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Length of connecting lines and volume of new infrastracture for hanging-up cables (if any)

#Determination of the length of the external connecting lines of the distribution segment
formula_1_4_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  NetworkRadius <- 0 
  NetworkRadius <- input$Intermediate.NetworkRadius
  
  NumberOfAOEsPlaces <- 0 
  NumberOfAOEsPlaces <- input$Intermediate.NumberOfAOEsPlaces
  
  if (!is.null(intermediate))
  {
    NetworkRadius <- as.numeric (intermediate$NetworkRadius)
    
    NumberOfAOEsPlaces <- as.numeric (intermediate$NumberOfAOEsPlaces)
  }
  
  
  result = 0.829*min (input$AccessTechnologyOptionsSet.MaximumLengthCommunicationChannelDistributing, 
                      NetworkRadius)*NumberOfAOEsPlaces
  
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

#Determination of the length of the external connecting lines of the aggregate segment
formula_1_4_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  NetworkRadius <- 0 
  NetworkRadius <- input$Intermediate.NetworkRadius
  
  NumberOfTAOEsPlaces <- 0 
  NumberOfTAOEsPlaces <- input$Intermediate.NumberOfTAOEsPlaces
  
  if (!is.null(intermediate))
  {
    NetworkRadius <- as.numeric (intermediate$NetworkRadius)
    
    NumberOfTAOEsPlaces <- as.numeric (intermediate$NumberOfTAOEsPlaces)
  }
  
  
  result = 0.829*min (input$AccessTechnologyOptionsSet.MaximumCommunicationChannelAggregative, 
                      NetworkRadius)*NumberOfTAOEsPlaces
  
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

#Determining the length of the aggregated cables of the connecting lines of the distribution segment
formula_1_4_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  LengthExternalCLDestribut <- 0
  LengthExternalCLDestribut <- input$Intermediate.LengthExternalCLDestribut
  
  if (!is.null(intermediate))
  {
    LengthExternalCLDestribut <- as.numeric (intermediate$LengthExternalCLDestribut)
    
  }
  
  
  result = LengthExternalCLDestribut *
    input$AccessTechnologyOptionsSet.AggregationExternalLinesDistributing
  
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

#Determining the length of aggregated cables for aggregate segment connecting lines
formula_1_4_4 <- function (input, intermediate = NULL)
{
  req (input)
  
  LengthExternalCLAggr <- 0
  LengthExternalCLAggr <- input$Intermediate.LengthExternalCLAggr
  
  if (!is.null(intermediate))
  {
    LengthExternalCLAggr <- as.numeric (intermediate$LengthExternalCLAggr)
    
  }
  
  
  result = LengthExternalCLAggr*
    input$AccessTechnologyOptionsSet.AggregationExternalLinesAggregative
  
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

algorithm1_4_impl <- function(input, intermediate = NULL)
{
  LengthExternalCLDestribut <- 0
  LengthExternalCLAggr <- 0 
  LengthOfAggrCableOfCLDestr <- 0 
  LengthOfAggrCableOfCLAggr <- 0 
  
  
  if (input$AccessTechnologyOptionsSet.MediumTypeOfCL == 1)
  {
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Medium Type of Destribution Segment"), i18n$t("Wireless"), sep = ": "))                
  }
  else
  {
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Medium Type of Destribution Segment"), i18n$t("Copper or Fiber"), sep = ": "))                
  }
  
  #Determination of the length of the external connecting lines of the distribution segment
  
  if (input$AccessTechnologyOptionsSet.MediumTypeOfCL != 1)
  {
    
    LengthExternalCLDestribut <- formula_1_4_1 (input, intermediate)
    #0.829*min(input$AccessTechnologyOptionsSet.MaximumLengthCommunicationChannelDistributing, 
    #input$Intermediate.NetworkRadius)*input$Intermediate.NumberOfAOEsPlaces
  }
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Length of the external connecting lines of the distribution segment, km"), LengthExternalCLDestribut, sep = ": "))                
  
  
  #Determination of the length of the external connecting lines of the aggregate segment
  if (input$AccessTechnologyOptionsSet.MediumTypeOfCLAggr != 1)
  {
    
    #LengthExternalCLDestribut <- formula_1_4_1 (input, intermediate)
    LengthExternalCLAggr <- formula_1_4_2 (input, intermediate)
    #0.829*min (input$AccessTechnologyOptionsSet.MaximumCommunicationChannelAggregative,
    #input$Intermediate.NetworkRadius)*input$Intermediate.NumberOfTAOEsPlaces
  }
    
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Length of the external connecting lines of the aggregate segment, km"), LengthExternalCLAggr, sep = ": "))                            
  
  #Determining the length of the aggregated cables of the connecting lines of the distribution segment
  if (input$AccessTechnologyOptionsSet.MediumTypeOfCL != 1)
  {
    intermediate2 <- list (LengthExternalCLDestribut = 0.0)
    
    intermediate2$LengthExternalCLDestribut <- LengthExternalCLDestribut
    
    LengthOfAggrCableOfCLDestr <- formula_1_4_3 (input,intermediate2)
    
      #LengthExternalCLDestribut*input$AccessTechnologyOptionsSet.AggregationExternalLinesDistributing
    
  }
    
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Determining the length of the aggregated cables of the connecting lines of the distribution segment, km"), LengthOfAggrCableOfCLDestr, sep = ": "))                              
  
  #Determining the length of aggregated cables for aggregate segment connecting lines            
  if (input$AccessTechnologyOptionsSet.MediumTypeOfCLAggr != 1)
  {
    intermediate2 <- list (LengthExternalCLAggr = 0.0)
    
    intermediate2$LengthExternalCLAggr <- LengthExternalCLAggr
    
    LengthOfAggrCableOfCLAggr = formula_1_4_4 (input,intermediate2)
      #LengthExternalCLAggr*input$AccessTechnologyOptionsSet.AggregationExternalLinesAggregative
  }

  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Determining the length of aggregated cables for aggregate segment connecting lines, km"), LengthOfAggrCableOfCLAggr, sep = ": "))                              
  
  
  result <- matrix (nrow = 5, ncol = 2)
  result [1,1] <- i18n$t("Length of aggregate segment trunk cable, km")
  result [1,2] <- as.numeric (LengthOfAggrCableOfCLAggr)
  result [2,1] <- i18n$t("Length of distribution segment trunk cable, km")
  result [2,2] <- as.numeric (LengthOfAggrCableOfCLDestr)
  result [3,1] <- i18n$t("Length of the hanged-up cables of distribution & aggregate segment, km")
  result [3,2] <- as.numeric ((LengthOfAggrCableOfCLAggr + LengthOfAggrCableOfCLDestr)*(input$DevelopmentParametersSet.PercentOfExistingPillars/100))
  temppil <- as.numeric (result [3,2])
  
  result [4,1] <- i18n$t("Length of distribution & aggregate segment in the external cable ducts, km")
  result [4,2] <- as.numeric ((LengthOfAggrCableOfCLAggr + LengthOfAggrCableOfCLDestr)*(input$DevelopmentParametersSet.PercentOfExistingExternalDuct/100))
  tempduct <- as.numeric (result [4,2])
  
  result [5,1] <- i18n$t("Total length of the new infrastructure for hanging-up cables of distribution & aggregate segment, km")
  result [5,2] <- as.numeric ((LengthOfAggrCableOfCLAggr + LengthOfAggrCableOfCLDestr) - temppil - tempduct)
  temptot <- as.numeric (result [5,2])
  
  if (temptot < 0)
  {
    temptot <- 0 
    result [5,2] <- 0
  }
  
  result [3,2] <- temppil + temptot
  
  return (result)
   
}

algorithm1_4 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
              
            req (input$AccessTechnologyOptionsSet.MediumTypeOfCL)
            req (input$AccessTechnologyOptionsSet.MediumTypeOfCLAggr)
            
            req (input$AccessTechnologyOptionsSet.MaximumLengthCommunicationChannelDistributing)
            req (input$Intermediate.NumberOfAOEsPlaces)
            req (input$Intermediate.NumberOfTAOEsPlaces)
            req (input$Intermediate.NetworkRadius)
            req (input$AccessTechnologyOptionsSet.MaximumCommunicationChannelAggregative)
            req (input$AccessTechnologyOptionsSet.AggregationExternalLinesDistributing)
            req (input$AccessTechnologyOptionsSet.AggregationExternalLinesAggregative)
            req (input$DevelopmentParametersSet.PercentOfExistingPillars)
            req (input$DevelopmentParametersSet.PercentOfExistingExternalDuct)
  
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result = algorithm1_4_impl (input)           
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_1_4_1 = {#Determination of the length of the external connecting lines of the distribution segment
            req (input$AccessTechnologyOptionsSet.MaximumLengthCommunicationChannelDistributing)
            req (input$Intermediate.NumberOfAOEsPlaces)
            req (input$Intermediate.NetworkRadius)
            
            result = formula_1_4_1(input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_1_4_2 = {#Determination of the length of the external connecting lines of the aggregate segment
            req (input$AccessTechnologyOptionsSet.MaximumCommunicationChannelAggregative)
            req (input$Intermediate.NumberOfTAOEsPlaces)
            req (input$Intermediate.NetworkRadius)
            
            result = formula_1_4_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_4_3 = {#Determining the length of the aggregated cables of the connecting lines of the distribution segment
            req (input$Intermediate.LengthExternalCLDestribut)
            req (input$AccessTechnologyOptionsSet.AggregationExternalLinesDistributing)
            
            result = formula_1_4_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_1_4_4 = {#Determining the length of aggregated cables for aggregate segment connecting lines
            req (input$Intermediate.LengthExternalCLAggr)
            req (input$AccessTechnologyOptionsSet.AggregationExternalLinesAggregative)
            
            result = formula_1_4_4 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          stop ("No!")
  )
  
}

