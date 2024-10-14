library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Algorithm for total RTS construction cost evaluation between object and SN in locality

#Number of retransmission sites calculation
formula_2_3_1 <- function (input, intermediate = NULL)
{
  req (input)

  result <- 0

  if (input$SchoolSpecificData.Length > 0)
  {
    result =  round ((input$SchoolSpecificData.Length)/input$InitialDataRadio.RetransmissionLength, digits = 0) + 1
  }


  return (result)
}

#Number of repeaters by selected technology calculation
formula_2_3_2 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfRetransmissionSites <- 0
  NumberOfRetransmissionSites <- input$Intermediate.NumberOfRetransmissionSites


  if (!is.null(intermediate))
  {
    NumberOfRetransmissionSites <- as.numeric (intermediate$NumberOfRetransmissionSites)
  }

  result <- 0

  if (NumberOfRetransmissionSites > 0)
  {
    result =  NumberOfRetransmissionSites - 1
  }


  return (result)
}

#Internal RTS devices total cost
formula_2_3_3 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters


  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }

  result =  input$InitialDataRadio.RTSDevicesCost*(2*NumberOfRepeaters+input$InitialDataRadio.NumberOfTerminals)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Antenna feeder devices total cost
formula_2_3_4 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters


  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }

  result =  input$InitialDataRadio.AFDCost*(2*NumberOfRepeaters + input$InitialDataRadio.NumberOfTerminals)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Main material total cost for RTS pylon construction
formula_2_3_5 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters


  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }

  result =  input$InitialDataRadio.PylonCost*(NumberOfRepeaters + input$InitialDataRadio.NumberOfTerminals)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost for geodetic work at RTS pylon location
formula_2_3_6 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters


  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }

  result =  input$InitialDataRadio.GeoPylonCost*(NumberOfRepeaters + input$InitialDataRadio.NumberOfTerminals)*input$InitialDataRadio.LaborNormsGeoPylon
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost for pylon construction of RTS antenna feeder devices
formula_2_3_7 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters


  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }

  result =  input$InitialDataRadio.PylonConstractionCost*(NumberOfRepeaters + input$InitialDataRadio.NumberOfTerminals)*input$InitialDataRadio.LaborNormsPylon
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost for all antenna feeder devices installation and commissioning for along RTS route
formula_2_3_8 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters


  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }

  result =  input$InitialDataRadio.CostNormsAFD*(NumberOfRepeaters + input$InitialDataRadio.NumberOfTerminals)*input$InitialDataRadio.LaborNormsAFDinst
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost for internal devices installation and commissioning for RTS
formula_2_3_9 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters


  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }

  result =  input$InitialDataRadio.CostAFDinst*(NumberOfRepeaters + input$InitialDataRadio.NumberOfTerminals)*input$InitialDataRadio.LaborNormsInternalRTS
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost for design solutions coordination on RTS construction
formula_2_3_10 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters


  if (!is.null(intermediate))
  {
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }

  result =  input$InitialDataRadio.CostDesing*input$InitialDataRadio.LaborNormsDesign*(NumberOfRepeaters + input$InitialDataRadio.NumberOfTerminals)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Design cost
formula_2_3_11 <- function (input, intermediate = NULL)
{
  req (input)

  CostOfAllAFDInstall <- 0
  CostOfAllAFDInstall <- input$Intermediate.CostOfAllAFDInstall

  CostOfAllDevicesInstall <- 0
  CostOfAllDevicesInstall <- input$Intermediate.CostOfAllDevicesInstall

  CostOfAllPylonsInstall <- 0
  CostOfAllPylonsInstall <- input$Intermediate.CostOfAllPylonsInstall

  DesignCoeff <- as.numeric (input$InitialDataRadio.DesignCoeff)

  if (!is.null(intermediate))
  {
    CostOfAllAFDInstall <- as.numeric (intermediate$CostOfAllAFDInstall)
    CostOfAllDevicesInstall <- as.numeric (intermediate$CostOfAllDevicesInstall)
    CostOfAllPylonsInstall <- as.numeric (intermediate$CostOfAllPylonsInstall)
  }

  result =  (CostOfAllAFDInstall + CostOfAllDevicesInstall + CostOfAllPylonsInstall)*DesignCoeff
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Width of frequency channel required for implementation of RTS channel of required bandwidth (throughput) per one RTS channel
formula_2_3_12 <- function (input, intermediate = NULL)
{
  req (input)

  RequiredCapacity <- 0
  RequiredCapacity <- as.numeric (input$SchoolSpecificData.RequiredBandwidth)

  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)

  }

  BandwidthToFrequencyCoeff <- as.numeric (input$InitialDataRadio.BandwidthToFrequencyCoeff)

  result <- RequiredCapacity*BandwidthToFrequencyCoeff

  result <- round (as.numeric (result), digits = 0)
  return (result)
}

#Spectrum licensing cost per transmitter (end-terminals and repeaters)
formula_2_3_13 <- function (input, intermediate = NULL)
{
  req (input)

  WidthFrequencyChannel <- 0
  WidthFrequencyChannel <- input$Intermediate.WidthFrequencyChannel


  if (!is.null(intermediate))
  {
    WidthFrequencyChannel <- as.numeric (intermediate$WidthFrequencyChannel)
  }

  result <- WidthFrequencyChannel*as.numeric (input$InitialDataRadio.BasicSpectrumFee)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Spectrum licensing cost for whole channel
formula_2_3_14 <- function (input, intermediate = NULL)
{
  req (input)

  SpectrumCostPerTransmitter <- 0
  SpectrumCostPerTransmitter <- input$Intermediate.SpectrumCostPerTransmitter
  SpectrumCostPerTransmitter <- as.numeric (SpectrumCostPerTransmitter)

  NumberOfRepeaters <- 0
  NumberOfRepeaters <- input$Intermediate.NumberOfRepeaters
  NumberOfRepeaters <- as.numeric (NumberOfRepeaters)


  NumberOfTerminals <- as.numeric (input$InitialDataRadio.NumberOfTerminals)


  if (!is.null(intermediate))
  {
    SpectrumCostPerTransmitter <- as.numeric (intermediate$SpectrumCostPerTransmitter)
    NumberOfRepeaters <- as.numeric (intermediate$NumberOfRepeaters)
  }

  allTerm <- NumberOfTerminals + NumberOfRepeaters


  result <- allTerm*SpectrumCostPerTransmitter

  result <- round (as.numeric (result), digits = 2)

  return (result)
}

#RTS construction total cost
formula_2_3_15 <- function (input, intermediate = NULL)
{
  req (input)

  CostOfAllAFDInstall <- 0
  CostOfAllAFDInstall <- input$Intermediate.CostOfAllAFDInstall

  CostOfAllDevicesInstall <- 0
  CostOfAllDevicesInstall <- input$Intermediate.CostOfAllDevicesInstall

  CostOfAllPylonsInstall <- 0
  CostOfAllPylonsInstall <- input$Intermediate.CostOfAllPylonsInstall

  CostOfInternalDevices <- 0
  CostOfInternalDevices <- input$Intermediate.CostOfInternalDevices

  CostOfAFDUnits <- 0
  CostOfAFDUnits <- input$Intermediate.CostOfAFDUnits

  CostOfPylonsMaterials <- 0
  CostOfPylonsMaterials <- input$Intermediate.CostOfPylonsMaterials

  CostOfDesign <- 0
  CostOfDesign <- input$Intermediate.CostOfDesign

  CostOfDesignSolutions <- 0
  CostOfDesignSolutions <- input$Intermediate.CostOfDesignSolutions

  SpectrumLicenseCost <- 0
  SpectrumLicenseCost <- input$Intermediate.SpectrumLicenseCost


  CostOfGeodeticWork <- 0
  CostOfGeodeticWork <- input$Intermediate.CostOfGeodeticWork



  if (!is.null(intermediate))
  {
    CostOfAllAFDInstall <- as.numeric (intermediate$CostOfAllAFDInstall)
    CostOfAllDevicesInstall <- as.numeric (intermediate$CostOfAllDevicesInstall)
    CostOfAllPylonsInstall <- as.numeric (intermediate$CostOfAllPylonsInstall)
    CostOfInternalDevices <- as.numeric (intermediate$CostOfInternalDevices)
    CostOfAFDUnits <- as.numeric (intermediate$CostOfAFDUnits)
    CostOfPylonsMaterials <- as.numeric (intermediate$CostOfPylonsMaterials)
    CostOfDesign <- as.numeric (intermediate$CostOfDesign)
    CostOfDesignSolutions <- as.numeric (intermediate$CostOfDesignSolutions)
    SpectrumLicenseCost <- as.numeric (intermediate$SpectrumLicenseCost)
    CostOfGeodeticWork <- as.numeric (intermediate$CostOfGeodeticWork)
  }

  result =  CostOfAllAFDInstall+CostOfAllDevicesInstall+
    CostOfAllPylonsInstall+ CostOfInternalDevices+
    CostOfAFDUnits+CostOfPylonsMaterials+
    CostOfDesign+CostOfDesignSolutions+SpectrumLicenseCost + CostOfGeodeticWork
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of equipment and materials for the construction of the RTS
formula_2_3_16 <- function (input, intermediate = NULL)
{
  req (input)

  CostOfPylonsMaterials <- 0
  CostOfPylonsMaterials <- input$Intermediate.CostOfPylonsMaterials

  CostOfInternalDevices <- 0
  CostOfInternalDevices <- input$Intermediate.CostOfInternalDevices

  CostOfAFDUnits <- 0
  CostOfAFDUnits <- input$Intermediate.CostOfAFDUnits


  if (!is.null(intermediate))
  {
    CostOfPylonsMaterials <- as.numeric (intermediate$CostOfPylonsMaterials)
    CostOfInternalDevices <- as.numeric (intermediate$CostOfInternalDevices)
    CostOfAFDUnits <- as.numeric (intermediate$CostOfAFDUnits)
  }


#  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of spectrum license for whole RTS, currency unit"), input$InitialDataRadio.CostOfSpectrumLicence, sep = ": "))
  result =  CostOfPylonsMaterials + CostOfInternalDevices  +  CostOfAFDUnits
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


algorithm2_3_impl <- function(input, intermediate = NULL)
{

  #Number of retransmission sites calculation
  NumberOfRetransmissionSites =  formula_2_3_1 (input, intermediate)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of retransmission sites calculation, units"), NumberOfRetransmissionSites, sep = ": "))

  #Number of repeaters by selected technology calculation
  intermediate2 <- list (NumberOfRetransmissionSites = 0, NumberOfRepeaters = 0)
  intermediate2$NumberOfRetransmissionSites <- NumberOfRetransmissionSites

  NumberOfRepeaters =  formula_2_3_2 (input, intermediate2)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of repeaters by selected technology calculation, units"), NumberOfRepeaters, sep = ": "))

  intermediate2$NumberOfRepeaters <- NumberOfRepeaters

  CostOfAllDevicesInstall <- 0
  CostOfAFDUnits  <- 0
  CostOfPylonsMaterials  <- 0
  CostOfGeodeticWork <- 0
  CostOfAllPylonsInstall <- 0
  CostOfAllAFDInstall <- 0
  CostOfAllAFDInstall <- 0
  CostOfInternalDevices <- 0
  CostOfDesignSolutions <- 0

  if (input$SchoolSpecificData.Length > 0)
  {
    #Internal RTS devices total cost
    CostOfAllDevicesInstall =  formula_2_3_3 (input, intermediate2)
    #Antenna feeder devices total cost
    CostOfAFDUnits =  formula_2_3_4 (input, intermediate2)
    #Main material total cost for RTS pylon construction
    CostOfPylonsMaterials =  formula_2_3_5 (input, intermediate2)
    #Total cost for geodetic work at RTS pylon location
    CostOfGeodeticWork =  formula_2_3_6 (input, intermediate2)
    #Total cost for pylon construction of RTS antenna feeder devices
    CostOfAllPylonsInstall =  formula_2_3_7 (input, intermediate2)
    #Total cost for all antenna feeder devices installation and commissioning for along RTS route
    CostOfAllAFDInstall =  formula_2_3_8 (input, intermediate2)
    #Total cost for internal devices installation and commissioning for RTS
    CostOfInternalDevices =  formula_2_3_9 (input, intermediate2)
    #Total cost for design solutions coordination on RTS construction
    CostOfDesignSolutions =  formula_2_3_10 (input, intermediate2)

  }


  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Internal RTS devices total cost, currency units"), CostOfAllDevicesInstall, sep = ": "))
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Antenna feeder devices total cost , currency units"), CostOfAFDUnits, sep = ": "))
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Main material total cost for RTS pylon construction, currency units"), CostOfPylonsMaterials, sep = ": "))
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost for geodetic work at RTS pylon location, currency units"), CostOfGeodeticWork, sep = ": "))
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost for pylon construction of RTS antenna feeder devices, currency units"), CostOfAllPylonsInstall, sep = ": "))
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost for all antenna feeder devices installation and commissioning for along RTS route, currency units"), CostOfAllAFDInstall, sep = ": "))
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost for internal devices installation and commissioning for RTS, currency units"), CostOfInternalDevices, sep = ": "))
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost for design solutions coordination on RTS construction, currency units"), CostOfDesignSolutions, sep = ": "))

  intermediate3 <- list (CostOfAllAFDInstall = 0.0,
                         CostOfAllDevicesInstall  = 0.0,
                         CostOfAllPylonsInstall  = 0.0,
                         CostOfGeodeticWork  = 0.0,
                         CostOfInternalDevices = 0.0,
                           CostOfAFDUnits = 0.0,
                          CostOfPylonsMaterials = 0.0,
                           CostOfDesign = 0.0,
                         CostOfDesignSolutions = 0.0,
                         WidthFrequencyChannel = 0.0,
                         SpectrumCostPerTransmitter = 0.0,
                         SpectrumLicenseCost = 0.0,
                         NumberOfRepeaters = 0.0
                         )

  intermediate3$CostOfAllAFDInstall <- CostOfAllAFDInstall
  intermediate3$CostOfAllDevicesInstall <- CostOfAllDevicesInstall
  intermediate3$CostOfAllPylonsInstall <- CostOfAllPylonsInstall
  intermediate3$CostOfGeodeticWork <- CostOfGeodeticWork


  #Design cost
  CostOfDesign =  formula_2_3_11 (input, intermediate3)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Design cost, currency units"), CostOfDesign, sep = ": "))

  intermediate3$CostOfInternalDevices <- CostOfInternalDevices
  intermediate3$CostOfAFDUnits <- CostOfAFDUnits
  intermediate3$CostOfPylonsMaterials <- CostOfPylonsMaterials
  intermediate3$CostOfDesign <- CostOfDesign
  intermediate3$CostOfDesignSolutions <- CostOfDesignSolutions
  intermediate3$NumberOfRepeaters <- NumberOfRepeaters

  # Width of frequency channel required for implementation of RTS channel of required bandwidth (throughput) per one RTS channel
  WidthFrequencyChannel <- formula_2_3_12 (input, intermediate)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Width of frequency channel required for implementation of RTS channel of required bandwidth (throughput) per one RTS channel, MHz"), WidthFrequencyChannel, sep = ": "))



  # Spectrum licensing cost per transmitter (end-terminals and repeaters)
  intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel
  SpectrumCostPerTransmitter <- formula_2_3_13 (input, intermediate3)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Spectrum licensing cost per transmitter (end-terminals and repeaters), currency units"), SpectrumCostPerTransmitter, sep = ": "))

  # Spectrum licensing cost for whole channel
  intermediate3$SpectrumCostPerTransmitter <- SpectrumCostPerTransmitter
  SpectrumLicenseCost <- formula_2_3_14 (input, intermediate3)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Spectrum licensing cost for whole channel, currency units"), SpectrumLicenseCost, sep = ": "))

  intermediate3$SpectrumLicenseCost <- SpectrumLicenseCost

  #RTS construction total cost


  result <- matrix (nrow = 4, ncol = 2)
  result [1,1] = i18n$t("RTS construction total cost, currency units")
  result [1,2] = formula_2_3_15 (input, intermediate3)

  result [2,1] = i18n$t("Number of repeaters by selected technology calculation, units")
  result [2,2] = NumberOfRepeaters

  result [3,1] = i18n$t("Total cost of equipment and materials for the construction of the RTS, currency units")
  result [3,2] = formula_2_3_16 (input, intermediate3)

  result [4,1] = i18n$t("Width of frequency channel required for implementation of RTS channel of required bandwidth (throughput) per one RTS channel, MHz")
  result [4,2] = WidthFrequencyChannel


  return (result)
}



algorithm2_3 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)

  switch (input$formula,
          ALL = {

            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataRadio.BandwidthToFrequencyCoeff)
            req (input$InitialDataRadio.BasicSpectrumFee)


            req (input$SchoolSpecificData.Length)
            req (input$InitialDataRadio.RetransmissionLength)
            req (input$InitialDataRadio.RTSDevicesCost)
            req (input$InitialDataRadio.NumberOfTerminals)
            req (input$InitialDataRadio.AFDCost)
            req (input$InitialDataRadio.PylonCost)
            req (input$InitialDataRadio.GeoPylonCost)
            req (input$InitialDataRadio.LaborNormsGeoPylon)
            req (input$InitialDataRadio.PylonConstractionCost)
            req (input$InitialDataRadio.LaborNormsPylon)
            req (input$InitialDataRadio.CostNormsAFD)
            req (input$InitialDataRadio.LaborNormsAFDinst)
            req (input$InitialDataRadio.CostAFDinst)
            req (input$InitialDataRadio.LaborNormsInternalRTS)
            req (input$InitialDataRadio.CostDesing)
            req (input$InitialDataRadio.LaborNormsDesign)

            req (input$InitialDataRadio.DesignCoeff)

            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

            result <- algorithm2_3_impl (input)

            output$c_names <- NULL

            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

          },
          FORMULA_2_3_1 = {#Number of retransmission sites calculation

            req (input$SchoolSpecificData.Length)
            req (input$InitialDataRadio.RetransmissionLength)

            result <- formula_2_3_1 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_2 = {#Number of repeaters by selected technology calculation

            req (input$Intermediate.NumberOfRetransmissionSites)

            result <- formula_2_3_2 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_3 = {#Internal RTS devices total cost

            req (input$InitialDataRadio.RTSDevicesCost)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)

            result <- formula_2_3_3 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_4 = {#Antenna feeder devices total cost
            req (input$InitialDataRadio.AFDCost)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)

            result <- formula_2_3_4 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_5 = {#Main material total cost for RTS pylon construction
            req (input$InitialDataRadio.PylonCost)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)

            result <- formula_2_3_5 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_6 = {#Total cost for geodetic work at RTS pylon location
            req (input$InitialDataRadio.GeoPylonCost)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)
            req (input$InitialDataRadio.LaborNormsGeoPylon)

            result <- formula_2_3_6 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_7 = {#Total cost for pylon construction of RTS antenna feeder devices
            req (input$InitialDataRadio.PylonConstractionCost)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)
            req (input$InitialDataRadio.LaborNormsPylon)

            result <- formula_2_3_7 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_8 = {#Total cost for all antenna feeder devices installation and commissioning for along RTS route
            req (input$InitialDataRadio.CostNormsAFD)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)
            req (input$InitialDataRadio.LaborNormsAFDinst)

            result <- formula_2_3_8 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_9 = {#Total cost for internal devices installation and commissioning for RTS
            req (input$InitialDataRadio.CostAFDinst)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)
            req (input$InitialDataRadio.LaborNormsInternalRTS)

            result <- formula_2_3_9 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_10 = {#Total cost for design solutions coordination on RTS construction
            req (input$InitialDataRadio.CostDesing)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)
            req (input$InitialDataRadio.LaborNormsDesign)

            result <- formula_2_3_10 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_11 = {#Design cost
            req (input$Intermediate.CostOfAllAFDInstall)
            req (input$Intermediate.CostOfAllDevicesInstall)
            req (input$Intermediate.CostOfAllPylonsInstall)
            req (input$InitialDataRadio.DesignCoeff)

            result <- formula_2_3_11 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_12 = {#Width of frequency channel required for implementation of RTS channel of required bandwidth (throughput) per one RTS channel

            req (input$InitialDataRadio.BandwidthToFrequencyCoeff)
            req (input$SchoolSpecificData.RequiredBandwidth)

            result <- formula_2_3_12 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_13 = {#Spectrum licensing cost per transmitter (end-terminals and repeaters)

            req (input$Intermediate.WidthFrequencyChannel)
            req (input$InitialDataRadio.BasicSpectrumFee)


            result <- formula_2_3_13 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_14 = {#Spectrum licensing cost for whole channel
            req (input$Intermediate.SpectrumCostPerTransmitter)
            req (input$Intermediate.NumberOfRepeaters)
            req (input$InitialDataRadio.NumberOfTerminals)

            result <- formula_2_3_14 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_15 = {#RTS construction total cost
            req (input$Intermediate.CostOfAllAFDInstall)
            req (input$Intermediate.CostOfAllDevicesInstall)
            req (input$Intermediate.CostOfAllPylonsInstall)
            req (input$Intermediate.CostOfInternalDevices)
            req (input$Intermediate.CostOfAFDUnits)
            req (input$Intermediate.CostOfPylonsMaterials)
            req (input$Intermediate.CostOfDesign)
            req (input$Intermediate.CostOfDesignSolutions)
            req (input$Intermediate.SpectrumLicenseCost)
            req (input$Intermediate.CostOfGeodeticWork)


            result <- formula_2_3_15 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_3_16 = {#Total cost of equipment and materials for the construction of the RTS
            req (input$Intermediate.CostOfPylonsMaterials)
            req (input$Intermediate.CostOfInternalDevices)
            req (input$Intermediate.CostOfAFDUnits)

            result <- formula_2_3_16 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
         stop ("No!")

  )
}
