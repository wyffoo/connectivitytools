library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")


#Algorithm for overall FOCL construction cost evaluation

#Overall length of the FOCL construction site
formula_2_1_1 <- function (input, intermediate = NULL)
{
  req (input)


  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)
  HorDrillCoeff <- as.numeric(input$InitialDataFOCL.HorDrillCoeff)

  distancekmbyroads <- round (input$SchoolSpecificData.Length * (1 + TopCoeff), digits = 2)


  result =  as.numeric (distancekmbyroads* (1 + HorDrillCoeff))
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#The average FOCL sections length requiring the construction of cable ducts
formula_2_1_2 <- function (input, intermediate = NULL)
{
  req (input)

  FOCLLenghtTotal <- 0
  FOCLLenghtTotal <- input$Intermediate.FOCLLenghtTotal

  CableDuctCoeff <- as.numeric(input$InitialDataFOCL.CableDuctCoeff)

  if (!is.null(intermediate))
  {
    FOCLLenghtTotal <- as.numeric (intermediate$FOCLLenghtTotal)

  }

  result =  FOCLLenghtTotal*CableDuctCoeff
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#The average FOCL sections length requiring cable-laying machine
formula_2_1_3 <- function (input, intermediate = NULL)
{
  req (input)

  FOCLLenghtTotal <- 0
  FOCLLenghtTotal <- input$Intermediate.FOCLLenghtTotal


  CableLayingMachineCoeff <- as.numeric(input$InitialDataFOCL.CableLayingMachineCoeff)

  if (!is.null(intermediate))
  {
    FOCLLenghtTotal <- as.numeric (intermediate$FOCLLenghtTotal)

  }


  result =  FOCLLenghtTotal*CableLayingMachineCoeff
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Number of cable manholes (CMh)
formula_2_1_4 <- function (input, intermediate = NULL)
{
  req (input)

  FOCLSectionLengthCD <- 0
  FOCLSectionLengthCD <- input$Intermediate.FOCLSectionLengthCD

  NumberOfManholesPerKM <- as.numeric(input$InitialDataFOCL.NumberOfManholesPerKM)

  if (!is.null(intermediate))
  {
    FOCLSectionLengthCD <- as.numeric (intermediate$FOCLSectionLengthCD)

  }

  result =  round (FOCLSectionLengthCD*NumberOfManholesPerKM, digits = 0)

  return (result)
}

#Number of cable couplings
formula_2_1_5 <- function (input, intermediate = NULL)
{
  req (input)

  FOCLLenghtTotal <- 0
  FOCLLenghtTotal <- input$Intermediate.FOCLLenghtTotal


  NumberOfCableCouplingsPerKM <- as.numeric(input$InitialDataFOCL.NumberOfCableCouplingsPerKM)

  if (!is.null(intermediate))
  {
    FOCLLenghtTotal <- as.numeric (intermediate$FOCLLenghtTotal)

  }

  result =  round (FOCLLenghtTotal*NumberOfCableCouplingsPerKM, digits = 0)

  return (result)
}

#Total scope of geodetic work along the object route
formula_2_1_6 <- function (input, intermediate = NULL)
{
  req (input)

  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  distancekmbyroads <- round (input$SchoolSpecificData.Length * (1 + TopCoeff), digits = 2)


  result =  distancekmbyroads * input$InitialDataFOCL.LaborCostNormGeo *
    input$InitialDataFOCL.CostNormGeo

  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total FOCL cost along route
formula_2_1_7 <- function (input, intermediate = NULL)
{
  req (input)

  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)
  MarginCoeff <- as.numeric(input$InitialDataFOCL.MarginCoeff)

  distancekmbyroads <- round (input$SchoolSpecificData.Length * (1 + TopCoeff), digits = 2)

  CostNormCC1km <- input$InitialDataFOCL.CostNormCC1km

  NumberCC <- 0
  NumberCC <- input$Intermediate.NumberCC

  if (!is.null(intermediate))
  {
    NumberCC <- as.numeric (intermediate$NumberCC)

  }


  result =  input$InitialDataFOCL.CostNormFOCLinst*distancekmbyroads* (1 + MarginCoeff) + NumberCC*CostNormCC1km
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Overall cost for FOCL sections construction of horizontal directional drilling, located at the crossings of roads
formula_2_1_8 <- function (input, intermediate = NULL)
{
  req (input)

  FOCLLenghtHDD <- 0
  FOCLLenghtHDD <- input$Intermediate.FOCLLenghtHDD


  if (!is.null(intermediate))
  {
    FOCLLenghtHDD <- as.numeric (intermediate$FOCLLenghtHDD)

  }

  result =  FOCLLenghtHDD*input$InitialDataFOCL.LaborCostNormHDD*input$InitialDataFOCL.CostNormHDD
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Overall cost for FOCL section construction requiring the cable duct
formula_2_1_9 <- function (input, intermediate = NULL)
{
  req (input)

  FOCLSectionLengthCD <- 0
  FOCLSectionLengthCD <- input$Intermediate.FOCLSectionLengthCD


  if (!is.null(intermediate))
  {
    FOCLSectionLengthCD <- as.numeric (intermediate$FOCLSectionLengthCD)

  }

  NumberCMh <- 0
  NumberCMh <- input$Intermediate.NumberCMh


  if (!is.null(intermediate))
  {
    NumberCMh <- as.numeric (intermediate$NumberCMh)

  }

  result =  FOCLSectionLengthCD * (input$InitialDataFOCL.LaborCostNormCDC*
                                                       input$InitialDataFOCL.CostNormCDC + input$InitialDataFOCL.CostNormMaterialsCD) +

    NumberCMh * (input$InitialDataFOCL.LaborCostNormCMh * input$InitialDataFOCL.CostNormCMh + input$InitialDataFOCL.CostNormMaterialsCMh)

  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Overall cost for FOCL section construction requiring the cable laying machine
formula_2_1_10 <- function (input, intermediate = NULL)
{
  req (input)

  FOCLSectionLengthCLM <- 0
  FOCLSectionLengthCLM <- input$Intermediate.FOCLSectionLengthCLM


  if (!is.null(intermediate))
  {
    FOCLSectionLengthCLM <- as.numeric (intermediate$FOCLSectionLengthCLM)

  }

  NumberCC <- 0
  NumberCC <- input$Intermediate.NumberCC


  if (!is.null(intermediate))
  {
    NumberCC <- as.numeric (intermediate$NumberCC)

  }


  result =  FOCLSectionLengthCLM*
    input$InitialDataFOCL.LaborCostNormLC*input$InitialDataFOCL.CostNormLC +
    NumberCC * (input$InitialDataFOCL.LaborCostNormCCI*input$InitialDataFOCL.CostNormCCI + input$InitialDataFOCL.CostNormCC1km)

  result <- round (as.numeric (result), digits = 2)
  return (result)
}


#Overall cost for FOCL signaling test
formula_2_1_11 <- function (input, intermediate = NULL)
{
  req (input)

  FOCLLenghtTotal <- 0
  FOCLLenghtTotal <- input$Intermediate.FOCLLenghtTotal


  if (!is.null(intermediate))
  {
    FOCLLenghtTotal <- as.numeric (intermediate$FOCLLenghtTotal)

  }

  result =  FOCLLenghtTotal*input$InitialDataFOCL.LaborCostNormST*input$InitialDataFOCL.CostNormST
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of technical specifications design
formula_2_1_12 <- function (input, intermediate = NULL)
{
  req (input)


  result =  input$InitialDataFOCL.LaborCostNormTS*input$InitialDataFOCL.CostNormTS
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of design solutions coordination
formula_2_1_13 <- function (input, intermediate = NULL)
{
  req (input)


  result =  input$InitialDataFOCL.LaborCostNormSC*input$InitialDataFOCL.CostNormSC
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


#Design cost
formula_2_1_14 <- function (input, intermediate = NULL)
{
  req (input)

  TotalFOCLcost <- 0
  TotalFOCLcost <- input$Intermediate.TotalFOCLcost


  OverllCostFOCLSC <- 0
  OverllCostFOCLSC <- input$Intermediate.OverllCostFOCLSC

  OverllCostFOCLCD <- 0
  OverllCostFOCLCD <- input$Intermediate.OverllCostFOCLCD

  OverllCostFOCLCLM <- 0
  OverllCostFOCLCLM <- input$Intermediate.OverllCostFOCLCLM


  DesignCoeff <- as.numeric(input$InitialDataFOCL.DesignCoeff)

  if (!is.null(intermediate))
  {
    TotalFOCLcost <- as.numeric (intermediate$TotalFOCLcost)

    OverllCostFOCLSC <- as.numeric (intermediate$OverllCostFOCLSC)
    OverllCostFOCLCD <- as.numeric (intermediate$OverllCostFOCLCD)
    OverllCostFOCLCLM <- as.numeric (intermediate$OverllCostFOCLCLM)

  }

  #TotalFOCLcost - Total FOCL cost along route
  #OverllCostFOCLSC - Overall cost for FOCL sections construction of horizontal directional drilling, located at the crossings of roads
  #OverllCostFOCLCD - Overall cost for FOCL section construction requiring the cable duct
  #OverllCostFOCLCLM -  Overall cost for FOCL section construction requiring the cable laying machine

  result =  (TotalFOCLcost+
               OverllCostFOCLSC+
               OverllCostFOCLCD +
               OverllCostFOCLCLM)*DesignCoeff

  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Overall cost of FOCL installation
formula_2_1_15 <- function (input, intermediate = NULL)
{
  req (input)

  OverllCostFOCLGeo <- 0
  OverllCostFOCLGeo <- input$Intermediate.OverllCostFOCLGeo

  TotalFOCLcost <- 0
  TotalFOCLcost <- input$Intermediate.TotalFOCLcost

  OverllCostFOCLSC <- 0
  OverllCostFOCLSC <- input$Intermediate.OverllCostFOCLSC

  OverllCostFOCLCD <- 0
  OverllCostFOCLCD <- input$Intermediate.OverllCostFOCLCD

  OverllCostFOCLCLM <- 0
  OverllCostFOCLCLM <- input$Intermediate.OverllCostFOCLCLM

  OverllCostFOCLTS <- 0
  OverllCostFOCLTS <- input$Intermediate.OverllCostFOCLTS

  OverllCostFOCLSC <- 0
  OverllCostFOCLSC <- input$Intermediate.OverllCostFOCLSC

  OverllCostFOCLST <- 0
  OverllCostFOCLST <- input$Intermediate.OverllCostFOCLST

  OverllCostFOCLDesign <- 0
  OverllCostFOCLDesign <- input$Intermediate.OverllCostFOCLDesign

  OverllCostFOCLSC2 <- 0
  OverllCostFOCLSC2 <- input$Intermediate.OverllCostFOCLSC2


  if (!is.null(intermediate))
  {
    OverllCostFOCLGeo <- as.numeric (intermediate$OverllCostFOCLGeo)
    TotalFOCLcost <- as.numeric (intermediate$TotalFOCLcost)
    OverllCostFOCLSC <- as.numeric (intermediate$OverllCostFOCLSC)
    OverllCostFOCLCD <- as.numeric (intermediate$OverllCostFOCLCD)
    OverllCostFOCLCLM <- as.numeric (intermediate$OverllCostFOCLCLM)
    OverllCostFOCLTS <- as.numeric (intermediate$OverllCostFOCLTS)
    OverllCostFOCLSC <- as.numeric (intermediate$OverllCostFOCLSC)
    OverllCostFOCLST <- as.numeric (intermediate$OverllCostFOCLST)
    OverllCostFOCLDesign <- as.numeric (intermediate$OverllCostFOCLDesign)
    OverllCostFOCLSC2 <- as.numeric (intermediate$OverllCostFOCLSC2)

  }

  #OverllCostFOCLGeo - Total scope of geodetic work along the object route
  #TotalFOCLcost - Total FOCL cost along route
  # OverllCostFOCLSC- Overall cost for FOCL sections construction of horizontal directional drilling, located at the crossings of roads
  # OverllCostFOCLCD - Overall cost for FOCL section construction requiring the cable duct
  # OverllCostFOCLCLM - Overall cost for FOCL section construction requiring the cable laying machine
  # OverllCostFOCLTS - Total cost of technical specifications design
  # OverllCostFOCLST - Overall cost for FOCL signaling test
  # OverllCostFOCLDesign - Design cost

  #CostTramsEquipment -> Cost of data transmission equipment for organizing L2 channel
  #OverllCostFOCLSC2 -> Total cost of design solutions coordination

  CostTramsEquipment <- input$InitialDataFOCL.CostTramsEquipment

  result =  OverllCostFOCLGeo +
    TotalFOCLcost +
    OverllCostFOCLSC  +
    OverllCostFOCLCD  +
    OverllCostFOCLCLM  +
    OverllCostFOCLTS  +
    OverllCostFOCLST  +
    OverllCostFOCLSC2  +
    OverllCostFOCLDesign + CostTramsEquipment

  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Сost of equipment and materials for the construction of fiber optic lines
formula_2_1_16 <- function (input, intermediate = NULL)
{
  req (input)

  TotalFOCLcost <- 0
  TotalFOCLcost <- input$Intermediate.TotalFOCLcost


  if (!is.null(intermediate))
  {
    TotalFOCLcost <- as.numeric (intermediate$TotalFOCLcost)

  }

  result =    TotalFOCLcost + input$InitialDataFOCL.CostTramsEquipment

  result <- round (as.numeric (result), digits = 2)
  return (result)
}


algorithm2_1_impl <- function(input, intermediate = NULL)
{




  #Overall length of the FOCL construction site

  FOCLLenghtTotal =  formula_2_1_1 (input, intermediate)

  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Overall length of the FOCL construction site, km"), FOCLLenghtTotal, sep = ": "))

  #The average FOCL sections length requiring the construction of cable ducts
  intermediate2 <- list (FOCLLenghtTotal = 0.0, FOCLLenghtHDD = 0.0, FOCLSectionLengthCD = 0.0, NumberCC = 0.0)
  intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal


  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)
  HorDrillCoeff <- as.numeric(input$InitialDataFOCL.HorDrillCoeff)

  distancekmbyroads <- round (input$SchoolSpecificData.Length * (1 + TopCoeff), digits = 2)

  intermediate2$FOCLLenghtHDD <-  as.numeric (distancekmbyroads* HorDrillCoeff)

  FOCLSectionLengthCD =  formula_2_1_2 (input, intermediate2)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The average FOCL sections length requiring the construction of cable ducts, km"), FOCLSectionLengthCD, sep = ": "))

  intermediate2$FOCLSectionLengthCD <-  as.numeric (FOCLSectionLengthCD)

  #The average FOCL sections length requiring cable-laying machine

  FOCLSectionLengthCLM =  formula_2_1_3 (input, intermediate2)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The average FOCL sections length requiring cable-laying machine, km"), FOCLSectionLengthCLM, sep = ": "))

  #Number of cable manholes (CMh)

  NumberCMh =  formula_2_1_4 (input, intermediate2)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of cable manholes (CMh), units"), NumberCMh, sep = ": "))

  #Number of cable couplings

  NumberCC =  formula_2_1_5 (input, intermediate2)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of cable couplings, units"), NumberCC, sep = ": "))

  intermediate2$NumberCC <-  as.numeric (NumberCC)

  #Overall geodetic work along the object route

  OverllCostFOCLGeo =  formula_2_1_6 (input, intermediate)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Overall geodetic work cost along the object route, currency units"), OverllCostFOCLGeo, sep = ": "))

  #Total FOCL cost along route

  TotalFOCLcost =  formula_2_1_7 (input, intermediate2)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total FOCL cost along route, currency units"), TotalFOCLcost, sep = ": "))

  #Overall cost for FOCL sections construction of horizontal directional drilling, located at the crossings of roads

  OverllCostFOCLSC =  formula_2_1_8 (input, intermediate2)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Overall cost for FOCL sections construction of horizontal directional drilling, located at the crossings of roads, currency units"), OverllCostFOCLSC, sep = ": "))

  #Overall cost for FOCL section construction requiring the cable duct
  intermediate3 <- list (FOCLSectionLengthCD = 0.0, FOCLSectionLengthCLM = 0.0, NumberCMh = 0, NumberCC = 0)
  intermediate3$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate3$FOCLSectionLengthCLM <- FOCLSectionLengthCLM
  intermediate3$NumberCMh <- NumberCMh
  intermediate3$NumberCC <- NumberCC

  OverllCostFOCLCD =  formula_2_1_9 (input, intermediate3)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Overall cost for FOCL section construction requiring the cable duct, currency units"), OverllCostFOCLCD, sep = ": "))


  #Overall cost for FOCL section construction requiring the cable laying machine

  OverllCostFOCLCLM =  formula_2_1_10 (input, intermediate3)

  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Overall cost for FOCL section construction requiring the cable laying machine, currency units"), OverllCostFOCLCLM, sep = ": "))



  #Overall cost for FOCL signaling test

  OverllCostFOCLST =  formula_2_1_11 (input, intermediate2)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Overall cost for FOCL signaling test, currency units"), OverllCostFOCLST, sep = ": "))

  OverllCostFOCLTS <- 0
  OverllCostFOCLSC2 <- 0
  OverllCostODF <- 0

  if (FOCLLenghtTotal > 0)
  {
    #Total cost of technical specifications design

    OverllCostFOCLTS =  formula_2_1_12 (input, intermediate)
    #Total cost of design solutions coordination

    OverllCostFOCLSC2 =  formula_2_1_13 (input, intermediate)

  }

  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of technical specifications design, currency units"), OverllCostFOCLTS, sep = ": "))

  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of design solutions coordination, currency unists"), OverllCostFOCLSC2, sep = ": "))


  #Design cost
  intermediate4 <- list (TotalFOCLcost = 0.0,
                           OverllCostFOCLSC = 0.0,
                           OverllCostFOCLCD = 0.0,
                           OverllCostFOCLCLM  = 0.0,
                         OverllCostFOCLSC2 = 0.0,
                         OverllCostFOCLDesign = 0.0,
                         OverllCostFOCLGeo = 0.0,
                         OverllCostFOCLTS = 0.0,
                         OverllCostFOCLST = 0.0)

  intermediate4$TotalFOCLcost <- TotalFOCLcost
  intermediate4$OverllCostFOCLSC <- OverllCostFOCLSC
  intermediate4$OverllCostFOCLCD <- OverllCostFOCLCD
  intermediate4$OverllCostFOCLCLM <- OverllCostFOCLCLM
  intermediate4$OverllCostFOCLGeo  <- OverllCostFOCLGeo
  intermediate4$OverllCostFOCLTS  <- OverllCostFOCLTS
  intermediate4$OverllCostFOCLST  <- OverllCostFOCLST
  intermediate4$OverllCostFOCLSC2 <- OverllCostFOCLSC2

  OverllCostFOCLDesign =  formula_2_1_14 (input, intermediate4)
  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Design cost, currency units"), OverllCostFOCLDesign, sep = ": "))

  #Overall cost of FOCL installation

  intermediate4$OverllCostFOCLDesign  <- OverllCostFOCLDesign

  OverCostOfFOCLInst <- formula_2_1_15 (input, intermediate4)


  result21 <- matrix (nrow = 4, ncol = 2)

  result21 [1,1] <- i18n$t("Overall cost of FOCL installation, currency units")
  result21 [1,2] <- as.numeric (OverCostOfFOCLInst)


  result21 [2,1] <- i18n$t("Overall length of the FOCL construction site, km")
  result21 [2,2] <- as.numeric (FOCLLenghtTotal)

  result21 [3,1] <- i18n$t("The average FOCL sections length requiring the construction of cable ducts, km")
  result21 [3,2] <- as.numeric (FOCLSectionLengthCD )




  intermediate5 <- list ( TotalFOCLcost  = 0.0)

  intermediate5$TotalFOCLcost <- TotalFOCLcost

  CostOfEqAndMat <- formula_2_1_16 (input, intermediate5)

  result21 [4,1] <- i18n$t("Сost of equipment and materials for the construction of fiber optic lines, currency units")
  result21 [4,2] <- as.numeric (CostOfEqAndMat)

  return (result21)
}



algorithm2_1 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)

  switch (input$formula,
          ALL = {
            req (input$SchoolSpecificData.Length)
            req (input$InitialDataFOCL.TopographyCoeff)
            req (input$InitialDataFOCL.HorDrillCoeff)
            req (input$InitialDataFOCL.CableDuctCoeff)
            req (input$InitialDataFOCL.CableLayingMachineCoeff)

            req (input$InitialDataFOCL.LaborCostNormGeo)
            req (input$InitialDataFOCL.CostNormGeo)
            req (input$InitialDataFOCL.CostNormFOCLinst)
            req (input$InitialDataFOCL.LaborCostNormHDD)
            req (input$InitialDataFOCL.CostNormHDD)
            req (input$InitialDataFOCL.LaborCostNormCDC)
            req (input$InitialDataFOCL.CostNormCDC)
            req (input$InitialDataFOCL.CostNormMaterialsCD)
            req (input$InitialDataFOCL.LaborCostNormLC)
            req (input$InitialDataFOCL.CostNormLC)
            req (input$InitialDataFOCL.CostNormMaterialsCMh)
            req (input$InitialDataFOCL.LaborCostNormCMh)
            req (input$InitialDataFOCL.CostNormCMh)
            req (input$InitialDataFOCL.CostNormCC1km)
            req (input$InitialDataFOCL.LaborCostNormCCI)
            req (input$InitialDataFOCL.CostNormCCI)
            req (input$InitialDataFOCL.LaborCostNormST)
            req (input$InitialDataFOCL.CostNormST)
            req (input$InitialDataFOCL.LaborCostNormTS)
            req (input$InitialDataFOCL.CostNormTS)
            req (input$InitialDataFOCL.LaborCostNormSC)
            req (input$InitialDataFOCL.CostNormSC)
            req (input$InitialDataFOCL.CostTramsEquipment)

            req (input$InitialDataFOCL.NumberOfManholesPerKM)
            req (input$InitialDataFOCL.NumberOfCableCouplingsPerKM)
            req (input$InitialDataFOCL.MarginCoeff)
            req (input$InitialDataFOCL.DesignCoeff)

            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))


            result <- algorithm2_1_impl (input)

            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

          },
          FORMULA_2_1_1 = {#Overall length of the FOCL construction site
            req (input$SchoolSpecificData.Length)
            req (input$InitialDataFOCL.TopographyCoeff)
            req (input$InitialDataFOCL.HorDrillCoeff)

            result <- formula_2_1_1 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_1_2 = {#The average FOCL sections length requiring the construction of cable ducts
            req (input$Intermediate.FOCLLenghtTotal)
            req (input$InitialDataFOCL.CableDuctCoeff)

            result <- formula_2_1_2 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_3 = {#The average FOCL sections length requiring cable-laying machine
            req (input$Intermediate.FOCLLenghtTotal)
            req (input$InitialDataFOCL.CableLayingMachineCoeff)

            result <- formula_2_1_3 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_4 = {#Number of cable manholes (CMh)
            req (input$Intermediate.FOCLLenghtTotal)
            req (input$InitialDataFOCL.NumberOfManholesPerKM)

            result <- formula_2_1_4 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_5 = {#Number of cable couplings
            req (input$Intermediate.FOCLLenghtTotal)
            req (input$InitialDataFOCL.NumberOfCableCouplingsPerKM)

            result <- formula_2_1_5 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_6 = {#Overall geodetic work along the object route

            req (input$SchoolSpecificData.Length)
            req (input$InitialDataFOCL.TopographyCoeff)
            req (input$InitialDataFOCL.LaborCostNormGeo)
            req (input$InitialDataFOCL.CostNormGeo)


            result <- formula_2_1_6 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_7 = {#Total FOCL cost along route

            req (input$InitialDataFOCL.CostNormFOCLinst)
            req (input$SchoolSpecificData.Length)
            req (input$InitialDataFOCL.TopographyCoeff)
            req (input$InitialDataFOCL.MarginCoeff)
            req (input$Intermediate.NumberCC)
            req (input$InitialDataFOCL.CostNormCC1km)


            result <- formula_2_1_7 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_8 = {#Overall cost for FOCL sections construction of horizontal directional drilling, located at the crossings of roads
            req (input$InitialDataFOCL.LaborCostNormHDD)
            req (input$InitialDataFOCL.CostNormHDD)
            req (input$Intermediate.FOCLLenghtHDD)


            result <- formula_2_1_8 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_9 = {#Overall cost for FOCL section construction requiring the cable duct
            req (input$Intermediate.FOCLSectionLengthCD)
            req (input$InitialDataFOCL.LaborCostNormCDC)
            req (input$InitialDataFOCL.CostNormCDC)
            req (input$InitialDataFOCL.CostNormMaterialsCD)
            req (input$Intermediate.NumberCMh)
            req (input$InitialDataFOCL.CostNormMaterialsCMh)
            req (input$InitialDataFOCL.LaborCostNormCMh)
            req (input$InitialDataFOCL.CostNormCMh)


            result <- formula_2_1_9 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_10 = {#Overall cost for FOCL section construction requiring the cable laying machine
            req (input$Intermediate.FOCLSectionLengthCLM)
            req (input$InitialDataFOCL.LaborCostNormLC)
            req (input$InitialDataFOCL.CostNormLC)
            req (input$Intermediate.NumberCC)
            req (input$InitialDataFOCL.CostNormCC1km)
            req (input$InitialDataFOCL.LaborCostNormCCI)
            req (input$InitialDataFOCL.CostNormCCI)


            result <- formula_2_1_10 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_11 = {#Overall cost for FOCL signaling test
            req (input$Intermediate.FOCLLenghtTotal)
            req (input$InitialDataFOCL.LaborCostNormST)
            req (input$InitialDataFOCL.CostNormST)

            result <- formula_2_1_11 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_12 = {#Total cost of technical specifications design
            req (input$InitialDataFOCL.LaborCostNormTS)
            req (input$InitialDataFOCL.CostNormTS)

            result <- formula_2_1_12 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_13 = {#Total cost of design solutions coordination
            req (input$InitialDataFOCL.LaborCostNormSC)
            req (input$InitialDataFOCL.CostNormSC)

            result <- formula_2_1_13 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_14 = {#Design cost
            req (input$Intermediate.TotalFOCLcost)
            req (input$Intermediate.OverllCostFOCLSC)
            req (input$Intermediate.OverllCostFOCLCD)
            req (input$Intermediate.OverllCostFOCLCLM)
            req (input$InitialDataFOCL.DesignCoeff)

            result <- formula_2_1_14 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_15 = {#Overall cost of FOCL installation
            req (input$Intermediate.OverllCostFOCLGeo)
            req (input$Intermediate.TotalFOCLcost)
            req (input$Intermediate.OverllCostFOCLSC)
            req (input$Intermediate.OverllCostFOCLCD)
            req (input$Intermediate.OverllCostFOCLCLM)
            req (input$Intermediate.OverllCostFOCLTS)
            req (input$Intermediate.OverllCostFOCLSC2)
            req (input$Intermediate.OverllCostFOCLST)
            req (input$Intermediate.OverllCostFOCLDesign)
            req (input$InitialDataFOCL.CostTramsEquipment)

            result <- formula_2_1_15 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_1_16 = {#Сost of equipment and materials for the construction of fiber optic lines
            req (input$Intermediate.TotalFOCLcost)
            req (input$InitialDataFOCL.CostTramsEquipment)


            result <- formula_2_1_16 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          stop ("No!")

        )
}
