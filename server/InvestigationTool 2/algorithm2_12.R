library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Potential rental income for additional fibers and cable duct (extra capacity) (currency units/year)

#Extra bandwidth of channel (without Internet) that could be provided for rent (Mbps*km)
formula_2_12_1 <- function (input, intermediate = NULL)
{
  req (input)



  result <- as.numeric (0)


  RequiredCapacity <- 0
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth

  Technology <- 0
  Technology <- input$Intermediate.Technology


  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
    Technology <- as.numeric (intermediate$Technology)
  }



  if (Technology == 0)
  {
    result <- as.numeric (input$InitialDataFOCL.ChannelCapacity) -  RequiredCapacity
  }
  else
  {
    result <- as.numeric (input$InitialDataRTS.ChannelCapacity) -  RequiredCapacity
  }

  if (result > 0)
  {

    if (exists ("bWriteLog", input))
       .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste ("Extra capacity of channel (without Internet) that could be provided for rent, Mbps", result, sep = ": "))

    TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

    distancekmbyroads <- round (input$SchoolSpecificData.Length * (1 + TopCoeff), digits = 2)

    result <- as.numeric (result) * as.numeric (distancekmbyroads)
  }
  else
  {
    result <- as.numeric (0)
  }
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


#Number of fibers used to connect school
formula_2_12_2 <- function (input, intermediate = NULL)
{
  req (input)


  RequiredCapacity <- 0
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth

  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }

  result =  round (RequiredCapacity/input$InitialDataFOCL.ChannelCapacity, digits = 0) + 1

  return (result)
}

#Total length of additional fibers that could be provided to rent (km)
formula_2_12_3 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfUsedFibers <- 0
  NumberOfUsedFibers <- input$Intermediate.NumberOfUsedFibers

  if (!is.null(intermediate))
  {
    NumberOfUsedFibers <- as.numeric (intermediate$NumberOfUsedFibers)
  }

  result =  0

  if (input$InitialDataFOCL.TotalFibersInTheCable > NumberOfUsedFibers)
  {
    TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

    distancekmbyroads <- round (input$SchoolSpecificData.Length * (1 + TopCoeff), digits = 2)

    result <- (input$InitialDataFOCL.TotalFibersInTheCable - NumberOfUsedFibers)*distancekmbyroads
  }
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total length of cable duct that could be provided to rent (km)
formula_2_12_4 <- function (input, intermediate = NULL)
{
  req (input)


  FOCLSectionLengthCD <- 0
  FOCLSectionLengthCD <- input$Intermediate.FOCLSectionLengthCD


  if (!is.null(intermediate))
  {
    FOCLSectionLengthCD <- as.numeric (intermediate$FOCLSectionLengthCD)
  }


  result <-  0

  if (input$InitialDataFOCL.TotalNumberOfCablesInDuct > 1)
  {
    result <- (input$InitialDataFOCL.TotalNumberOfCablesInDuct - 1)*FOCLSectionLengthCD

  }

  result <- round (as.numeric (result), digits = 2)
  return (result)
}


#Potential rental income for additional fibers, cable duct and bandwidth (extra capacity) (currency units/year)
formula_2_12_5 <- function (input, intermediate = NULL)
{
  req (input)


  TotalLengthOfFibersForRent <- 0
  TotalLengthOfFibersForRent <- input$Intermediate.TotalLengthOfFibersForRent
  TotalLengthOfDuctForRent <- 0
  TotalLengthOfDuctForRent <- input$Intermediate.TotalLengthOfDuctForRent
  TotalExtraBandwidth <- 0
  TotalExtraBandwidth <- input$Intermediate.TotalExtraBandwidth


 if (!is.null(intermediate))
 {
   TotalLengthOfFibersForRent <- as.numeric (intermediate$TotalLengthOfFibersForRent)
   TotalLengthOfDuctForRent <- as.numeric (intermediate$TotalLengthOfDuctForRent)
   TotalExtraBandwidth <- as.numeric (intermediate$TotalExtraBandwidth)
 }

  result =  TotalLengthOfFibersForRent*input$InitialDataFOCL.DemandForFibers*input$InitialDataFOCL.RentalCostForFibers +
    TotalLengthOfDuctForRent*input$InitialDataFOCL.DemandForDuct*input$InitialDataFOCL.RentalCostForDuct +
    TotalExtraBandwidth*input$InitialDataFOCL.DemandForChannel*input$InitialDataFOCL.AnnualRentChannelCost
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


algorithm2_12_impl <- function(input, intermediate = NULL)
{
  #Extra bandwidth of channel (without Internet) that could be provided for rent (Mbps*km)
  TotalExtraBandwidth = formula_2_12_1 (input, intermediate)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Extra bandwidth of channel (without Internet) that could be provided for rent, Mbps*km"), TotalExtraBandwidth, sep = ": "))

  NumberOfUsedFibers <- 0
  TotalLengthOfFibersForRent <- 0
  TotalLengthOfDuctForRent <- 0
  logstrfiberrts <- i18n$t("Potential rental income for additional bandwidth (extra capacity), currency units/year")

  Technology <- 0
  Technology <- input$Intermediate.Technology


  if (!is.null(intermediate))
  {
    Technology <- as.numeric (intermediate$Technology)
  }


  intermediate2 <- list (NumberOfUsedFibers = 0.0, TotalLengthOfFibersForRent = 0.0, TotalLengthOfDuctForRent = 0.0, TotalExtraBandwidth = 0.0)

  intermediate2$TotalExtraBandwidth <- TotalExtraBandwidth

  if (Technology == 0)
  {
    #Number of fibers used to connect school
    NumberOfUsedFibers =  formula_2_12_2 (input, intermediate)
    if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of fibers used to connect school, fibers"), NumberOfUsedFibers, sep = ": "))


    intermediate2$NumberOfUsedFibers <- NumberOfUsedFibers



    #Total length of additional fibers that could be provided to rent (km)
    TotalLengthOfFibersForRent =  formula_2_12_3 (input, intermediate2)
    if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total length of additional fibers that could be provided to rent, km"), TotalLengthOfFibersForRent, sep = ": "))
    intermediate2$TotalLengthOfFibersForRent <- TotalLengthOfFibersForRent


    #Total length of cable duct that could be provided to rent (km)
    TotalLengthOfDuctForRent =  formula_2_12_4 (input, intermediate)
    if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total length of cable duct that could be provided to rent, km"), TotalLengthOfDuctForRent, sep = ": "))
    intermediate2$TotalLengthOfDuctForRent <- TotalLengthOfDuctForRent
    logstrfiberrts <- i18n$t("Potential rental income for additional fibers, cable duct and bandwidth (extra capacity) (currency units/year)")

  }


  #Potential rental income for additional fibers, cable duct and bandwidth (extra capacity) (currency units/year)
  #print (.GlobalEnv$mylog)

  PotentialRent <- formula_2_12_5 (input, intermediate2)

  result <- matrix (nrow = 2, ncol = 2)

  result [1,1] <- logstrfiberrts

  result [1,2] <- PotentialRent


  result [2,1] <- i18n$t("Extra bandwidth of channel (without Internet) that could be provided for rent (Mbps*km)")

  result [2,2] <- TotalExtraBandwidth




  return (result)
}

algorithm2_12 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)

  switch (input$formula,
          ALL = {

            req (input$InitialDataFOCL.ChannelCapacity)
            req (input$SchoolSpecificData.RequiredBandwidth)

            req (input$SchoolSpecificData.Length)
            req (input$InitialDataFOCL.TopographyCoeff)
            req (input$InitialDataFOCL.TotalFibersInTheCable)

            req (input$Intermediate.FOCLSectionLengthCD)
            req (input$InitialDataFOCL.TotalNumberOfCablesInDuct)

            req (input$InitialDataFOCL.RentalCostForFibers)
            req (input$InitialDataFOCL.RentalCostForDuct)
            req (input$InitialDataFOCL.DemandForFibers)
            req (input$InitialDataFOCL.DemandForDuct)

            req (input$Intermediate.Technology)
            req (input$InitialDataRTS.ChannelCapacity)
            req (input$SchoolSpecificData.Length)

            req (input$InitialDataFOCL.AnnualRentChannelCost)
            req (input$InitialDataFOCL.DemandForChannel)


            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

            result <- algorithm2_12_impl (input)

            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

          },
          FORMULA_2_12_1 = {#Extra bandwidth of channel (without Internet) that could be provided for rent (Mbps*km)

            req (input$InitialDataFOCL.ChannelCapacity)
            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$Intermediate.Technology)
            req (input$InitialDataRTS.ChannelCapacity)
            req (input$SchoolSpecificData.Length)
            req (input$InitialDataFOCL.TopographyCoeff)


            result <- formula_2_12_1 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_12_2 = {#Number of fibers used to connect school

            req (input$InitialDataFOCL.ChannelCapacity)
            req (input$SchoolSpecificData.RequiredBandwidth)

            result <- formula_2_12_2 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_12_3 = {#Total length of additional fibers that could be provided to rent (km)

            req (input$SchoolSpecificData.Length)
            req (input$InitialDataFOCL.TopographyCoeff)
            req (input$Intermediate.NumberOfUsedFibers)
            req (input$InitialDataFOCL.TotalFibersInTheCable)

            result <- formula_2_12_3 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_12_4 = {#Total length of cable duct that could be provided to rent (km)

            req (input$Intermediate.FOCLSectionLengthCD)
            req (input$InitialDataFOCL.TotalNumberOfCablesInDuct)

            result <- formula_2_12_4 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_12_5 = {#Potential rental income for additional fibers, cable duct and bandwidth (extra capacity) (currency units/year)

            req (input$Intermediate.TotalLengthOfFibersForRent)
            req (input$Intermediate.TotalLengthOfDuctForRent)

            req (input$Intermediate.TotalExtraBandwidth)


            req (input$InitialDataFOCL.RentalCostForFibers)
            req (input$InitialDataFOCL.RentalCostForDuct)

            req (input$InitialDataFOCL.AnnualRentChannelCost)

            req (input$InitialDataFOCL.DemandForFibers)
            req (input$InitialDataFOCL.DemandForDuct)

            req (input$InitialDataFOCL.DemandForChannel)


            result <- formula_2_12_5 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },

          stop ("No!")

  )
}
