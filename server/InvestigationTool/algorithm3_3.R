library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Determining the cost of the public wireless segment (PWS) based on the HOTSPOT organization

# The number of wireless routers for the organization of PWS, units
formula_3_3_1 <- function (input, intermediate = NULL)
{
  req (input)
  
    
  AreaOfAllClassRooms <- 0
  AreaOfAllClassRooms <- input$Intermediate.AreaOfAllClassRooms


  if (!is.null(intermediate))
  {
    AreaOfAllClassRooms <- as.numeric (intermediate$AreaOfAllClassRooms)
  }

  result <- round ((input$GeneralVariables.CoeffReserveOfEquipment*
                      (input$BuildingParameters.Width*input$BuildingParameters.Length - AreaOfAllClassRooms))/
                     (pi*((input$WiFiEquipment.RadiusOfHotSpot*input$WiFiEquipment.AdapterReductionFactor)^2)/2), digits = 0)  + 1
  
  return (result)
}

# Total length of LAN cables from STC to HOTSPOT routers, meters
formula_3_3_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  AvCableLenFromSTCToClassroom <- 0
  AvCableLenFromSTCToClassroom <- input$Intermediate.AvCableLenFromSTCToClassroom


  NumberOfHotSpots <- 0
  NumberOfHotSpots <- input$Intermediate.NumberOfHotSpots

  if (!is.null(intermediate))
  {
    AvCableLenFromSTCToClassroom <- as.numeric (intermediate$AvCableLenFromSTCToClassroom)
    NumberOfHotSpots <- as.numeric (intermediate$NumberOfHotSpots)
  }


  result <- AvCableLenFromSTCToClassroom * NumberOfHotSpots
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Number of switches of the selected model for connection of HOTSPOT routers, units
formula_3_3_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfHotSpots <- 0
  NumberOfHotSpots <- input$Intermediate.NumberOfHotSpots


  if (!is.null(intermediate))
  {
    NumberOfHotSpots <- as.numeric (intermediate$NumberOfHotSpots)
  }


  result <- round ((NumberOfHotSpots/input$GeneralVariables.BusyCoeff)/input$LANEquipment.NumberOfPortsInSwitch, digits = 0)  + 1
  
  return (result)
}

# The cost of purchasing cables to connect HOTSPOT routers with a margin of 10% of the length for laying cables, currency units
formula_3_3_4 <- function (input, intermediate = NULL)
{
  req (input)
  

  NumberOfHotSpots <- 0
  NumberOfHotSpots <- input$Intermediate.NumberOfHotSpots


  AvCableLenFromSTCToClassroom <- 0
  AvCableLenFromSTCToClassroom <- input$Intermediate.AvCableLenFromSTCToClassroom

  if (!is.null(intermediate))
  {
    NumberOfHotSpots <- as.numeric (intermediate$NumberOfHotSpots)
    AvCableLenFromSTCToClassroom <- as.numeric (intermediate$AvCableLenFromSTCToClassroom)
  }


  result <- input$LANEquipment.CostOfCable*NumberOfHotSpots*AvCableLenFromSTCToClassroom
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Total cost of laying cables to connect HOTSPOT routers, currency units
formula_3_3_5 <- function (input, intermediate = NULL)
{
  req (input)
  

  CostOfAllCablesToConnectHotSpots <- 0
  CostOfAllCablesToConnectHotSpots <- input$Intermediate.CostOfAllCablesToConnectHotSpots

  NumberOfHotSpots <- 0
  NumberOfHotSpots <- input$Intermediate.NumberOfHotSpots

  AvCableLenFromSTCToClassroom <- 0
  AvCableLenFromSTCToClassroom <- input$Intermediate.AvCableLenFromSTCToClassroom

  if (!is.null(intermediate))
  {
    CostOfAllCablesToConnectHotSpots <- as.numeric (intermediate$CostOfAllCablesToConnectHotSpots)
    NumberOfHotSpots <- as.numeric (intermediate$NumberOfHotSpots)
    AvCableLenFromSTCToClassroom <- as.numeric (intermediate$AvCableLenFromSTCToClassroom)
  }


  result <- CostOfAllCablesToConnectHotSpots + input$GeneralVariables.LaborCostNormsForCableInst*
    input$GeneralVariables.CostNormsForCableInst*NumberOfHotSpots*AvCableLenFromSTCToClassroom

  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# The cost of purchasing HOTSPOT (taking into account the materials required to install and obtain permission to use radio frequencies in regions where necessary) for PWS, currency units
formula_3_3_6 <- function (input, intermediate = NULL)
{
  req (input)
  

  NumberOfHotSpots <- 0
  NumberOfHotSpots <- input$Intermediate.NumberOfHotSpots


  if (!is.null(intermediate))
  {
    NumberOfHotSpots <- as.numeric (intermediate$NumberOfHotSpots)
  }


  result <- input$WiFiEquipment.CostOfOneHotSpotPouter * NumberOfHotSpots
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# The total cost of purchasing and installation of HOTSPOT routers, currency units
formula_3_3_7 <- function (input, intermediate = NULL)
{
  req (input)
  
  CostOfAllHotSpots <- 0
  CostOfAllHotSpots <- input$Intermediate.CostOfAllHotSpots


  NumberOfHotSpots <- 0
  NumberOfHotSpots <- input$Intermediate.NumberOfHotSpots

  if (!is.null(intermediate))
  {
    CostOfAllHotSpots <- as.numeric (intermediate$CostOfAllHotSpots)
    NumberOfHotSpots <- as.numeric (intermediate$NumberOfHotSpots)
  }


  result <- CostOfAllHotSpots + input$GeneralVariables.LaborCostNormsForHotSpotInst *
    input$GeneralVariables.CostNormsForHotSpotInst * NumberOfHotSpots
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

# Total cost of purchasing access switches for the PWS organization, currency units
formula_3_3_8 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfSwitchesToConnectAllHotSpots <- 0
  NumberOfSwitchesToConnectAllHotSpots <- input$Intermediate.NumberOfSwitchesToConnectAllHotSpots


  if (!is.null(intermediate))
  {
    NumberOfSwitchesToConnectAllHotSpots <- as.numeric (intermediate$NumberOfSwitchesToConnectAllHotSpots)
  }


  result <- input$LANEquipment.CostOfSwitch * NumberOfSwitchesToConnectAllHotSpots
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Total cost of installation and commissioning of access switch equipment, currency units
formula_3_3_9 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  CostOfAllSwitchesToConnectAllHotspots <- 0
  CostOfAllSwitchesToConnectAllHotspots <- input$Intermediate.CostOfAllSwitchesToConnectAllHotspots


  NumberOfSwitchesToConnectAllHotSpots <- 0
  NumberOfSwitchesToConnectAllHotSpots <- input$Intermediate.NumberOfSwitchesToConnectAllHotSpots

  if (!is.null(intermediate))
  {
    CostOfAllSwitchesToConnectAllHotspots <- as.numeric (intermediate$CostOfAllSwitchesToConnectAllHotspots)
    NumberOfSwitchesToConnectAllHotSpots <- as.numeric (intermediate$NumberOfSwitchesToConnectAllHotSpots)
  }

  result <- CostOfAllSwitchesToConnectAllHotspots + input$GeneralVariables.LaborCostNormsForSwitchInst *
    input$GeneralVariables.CostNormsForSwitchInst * NumberOfSwitchesToConnectAllHotSpots
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

# Design cost
formula_3_3_10 <- function (input, intermediate = NULL)
{
  req (input)
  

  TotalCostOfSwitchesForHotSpotInst <- 0
  TotalCostOfSwitchesForHotSpotInst <- input$Intermediate.TotalCostOfSwitchesForHotSpotInst

  TotalCostOfPushAndInstaOfHotSpots <- 0
  TotalCostOfPushAndInstaOfHotSpots <- input$Intermediate.TotalCostOfPushAndInstaOfHotSpots

  TotalCostOfLayingCablesForHotSpots <- 0
  TotalCostOfLayingCablesForHotSpots <- input$Intermediate.TotalCostOfLayingCablesForHotSpots

  if (!is.null(intermediate))
  {
    TotalCostOfSwitchesForHotSpotInst <- as.numeric (intermediate$TotalCostOfSwitchesForHotSpotInst)
    TotalCostOfPushAndInstaOfHotSpots <- as.numeric (intermediate$TotalCostOfPushAndInstaOfHotSpots)
    TotalCostOfLayingCablesForHotSpots <- as.numeric (intermediate$TotalCostOfLayingCablesForHotSpots)
  }


  result <- (TotalCostOfSwitchesForHotSpotInst+TotalCostOfPushAndInstaOfHotSpots+TotalCostOfLayingCablesForHotSpots)*(input$GeneralVariables.DesignConstrCoeff/100)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Total cost of organization for LAN WES, currency units
formula_3_3_11 <- function (input, intermediate = NULL)
{
  req (input)
  
  TotalCostOfSwitchesForHotSpotInst <- 0
  TotalCostOfSwitchesForHotSpotInst <- input$Intermediate.TotalCostOfSwitchesForHotSpotInst

  TotalCostOfPushAndInstaOfHotSpots <- 0
  TotalCostOfPushAndInstaOfHotSpots <- input$Intermediate.TotalCostOfPushAndInstaOfHotSpots

  TotalCostOfLayingCablesForHotSpots <- 0
  TotalCostOfLayingCablesForHotSpots <- input$Intermediate.TotalCostOfLayingCablesForHotSpots

  DesignCostHotSPot <- 0
  DesignCostHotSPot <- input$Intermediate.DesignCostHotSPot

  if (!is.null(intermediate))
  {
    TotalCostOfSwitchesForHotSpotInst <- as.numeric (intermediate$TotalCostOfSwitchesForHotSpotInst)
    TotalCostOfPushAndInstaOfHotSpots <- as.numeric (intermediate$TotalCostOfPushAndInstaOfHotSpots)
    TotalCostOfLayingCablesForHotSpots <- as.numeric (intermediate$TotalCostOfLayingCablesForHotSpots)
    DesignCostHotSPot <- as.numeric (intermediate$DesignCostHotSPot)
  }


  result <- TotalCostOfSwitchesForHotSpotInst + TotalCostOfPushAndInstaOfHotSpots + TotalCostOfLayingCablesForHotSpots + DesignCostHotSPot
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Cable maintenance cost in areas from STC to HOTSPOT routers, currency units per year
formula_3_3_12 <- function (input, intermediate = NULL)
{
  req (input)

    TotalLenOfCablesToHotSpot <- 0
  TotalLenOfCablesToHotSpot <- input$Intermediate.TotalLenOfCablesToHotSpot


  if (!is.null(intermediate))
  {
    TotalLenOfCablesToHotSpot <- as.numeric (intermediate$TotalLenOfCablesToHotSpot)
  }

  result <-   input$GeneralVariables.AnnualLaborCostOfCableOperation *
    input$GeneralVariables.CostOfOneCableOperation * TotalLenOfCablesToHotSpot
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

# Determining the cost of access switches maintenance to connect HOTSPOT routers, currency units per year
formula_3_3_13 <- function (input, intermediate = NULL)
{
  req (input)
  

  NumberOfSwitchesToConnectAllHotSpots <- 0
  NumberOfSwitchesToConnectAllHotSpots <- input$Intermediate.NumberOfSwitchesToConnectAllHotSpots


  if (!is.null(intermediate))
  {
    NumberOfSwitchesToConnectAllHotSpots <- as.numeric (intermediate$NumberOfSwitchesToConnectAllHotSpots)
  }


  result <-   input$GeneralVariables.AnnualLaborCostOfSwitchOperation *
    input$GeneralVariables.CostOfOneSwitchOperation * NumberOfSwitchesToConnectAllHotSpots

  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Determining the cost of HOTSPOT routers maintenance, currency units per year
formula_3_3_14 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfHotSpots <- 0
  NumberOfHotSpots <- input$Intermediate.NumberOfHotSpots


  if (!is.null(intermediate))
  {
    NumberOfHotSpots <- as.numeric (intermediate$NumberOfHotSpots)
  }

  result <-   input$GeneralVariables.AnnualLaborCostOfHotSpotOperation *
    input$GeneralVariables.CostOfHotSpotOperation * NumberOfHotSpots
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Determination of the total cost of PWS LAN maintenance during the maintenance period, cyrrency units
formula_3_3_15 <- function (input, intermediate = NULL)
{
  req (input)

  AllHotSpotCableMaintenanceCost <- 0
  AllHotSpotCableMaintenanceCost <- input$Intermediate.AllHotSpotCableMaintenanceCost

  AllSwitchesForHotSpotMaintenanceCost <- 0
  AllSwitchesForHotSpotMaintenanceCost <- input$Intermediate.AllSwitchesForHotSpotMaintenanceCost

  AllHotSpotsMaintenanceCost <- 0
  AllHotSpotsMaintenanceCost <- input$Intermediate.AllHotSpotsMaintenanceCost

  if (!is.null(intermediate))
  {
    AllHotSpotCableMaintenanceCost <- as.numeric (intermediate$AllHotSpotCableMaintenanceCost)
    AllSwitchesForHotSpotMaintenanceCost <- as.numeric (intermediate$AllSwitchesForHotSpotMaintenanceCost)
    AllHotSpotsMaintenanceCost <- as.numeric (intermediate$AllHotSpotsMaintenanceCost)
  }


  result <- (AllHotSpotCableMaintenanceCost+AllSwitchesForHotSpotMaintenanceCost+AllHotSpotsMaintenanceCost)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

algorithm3_3_impl <- function(input, intermediate = NULL)
{
  intermediate2 <- list (
    AreaOfAllClassRooms = 0.0,
    AvCableLenFromSTCToClassroom = 0.0,
    NumberOfHotSpots = 0.0,
    CostOfAllCablesToConnectHotSpots = 0.0,
    CostOfAllHotSpots = 0.0,
    NumberOfSwitchesToConnectAllHotSpots = 0.0,
    CostOfAllSwitchesToConnectAllHotspots = 0.0,
    TotalCostOfSwitchesForHotSpotInst = 0.0,
    TotalCostOfPushAndInstaOfHotSpots = 0.0,
    TotalCostOfLayingCablesForHotSpots = 0.0,
    DesignCostHotSPot = 0.0,
    TotalLenOfCablesToHotSpot = 0.0,
    AllHotSpotCableMaintenanceCost = 0.0,
    AllSwitchesForHotSpotMaintenanceCost = 0.0,
    AllHotSpotsMaintenanceCost = 0.0
  )
  
  if (!is.null(intermediate))
  {
     intermediate2$AreaOfAllClassRooms <- as.numeric (intermediate$AreaOfAllClassRooms)
     intermediate2$AvCableLenFromSTCToClassroom <- as.numeric (intermediate$AvCableLenFromSTCToClassroom)
  }
  else
  {
    intermediate2$AreaOfAllClassRooms <- input$Intermediate.AreaOfAllClassRooms
     intermediate2$AvCableLenFromSTCToClassroom <- input$Intermediate.AvCableLenFromSTCToClassroom
  }
  
 
#   The number of wireless routers for the organization of PWS, units
  NumberOfHotSpots =  formula_3_3_1 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The number of wireless routers for the organization of PWS, units"), NumberOfHotSpots, sep = ": "))  
  intermediate2$NumberOfHotSpots <- NumberOfHotSpots
  
#   Total length of LAN cables from STC to HOTSPOT routers, meters
  TotalLenOfCablesToHotSpot =  formula_3_3_2 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total length of LAN cables from STC to HOTSPOT routers, meters"), TotalLenOfCablesToHotSpot, sep = ": "))  
  intermediate2$TotalLenOfCablesToHotSpot <- TotalLenOfCablesToHotSpot
  
#   Number of switches of the selected model for connection of HOTSPOT routers, units
  NumberOfSwitchesToConnectAllHotSpots =  formula_3_3_3 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of switches of the selected model for connection of HOTSPOT routers, units"), NumberOfSwitchesToConnectAllHotSpots, sep = ": "))  
  intermediate2$NumberOfSwitchesToConnectAllHotSpots <- NumberOfSwitchesToConnectAllHotSpots
  
#   The cost of purchasing cables to connect HOTSPOT routers with a margin of 10% of the length for laying cables, currency units
  CostOfAllCablesToConnectHotSpots =  formula_3_3_4 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The cost of purchasing cables to connect HOTSPOT routers with a margin of 10% of the length for laying cables, currency units"), CostOfAllCablesToConnectHotSpots, sep = ": "))  
  intermediate2$CostOfAllCablesToConnectHotSpots <- CostOfAllCablesToConnectHotSpots
  
#   Total cost of laying cables to connect HOTSPOT routers, currency units
  TotalCostOfLayingCablesForHotSpots =  formula_3_3_5 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of laying cables to connect HOTSPOT routers, currency units"), TotalCostOfLayingCablesForHotSpots, sep = ": "))  
  intermediate2$TotalCostOfLayingCablesForHotSpots <- TotalCostOfLayingCablesForHotSpots
  
#   The cost of purchasing HOTSPOT (taking into account the materials required to install and obtain permission to use radio frequencies in regions where necessary) for PWS, currency units
  CostOfAllHotSpots =  formula_3_3_6 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The cost of purchasing HOTSPOT (taking into account the materials required to install and obtain permission to use radio frequencies in regions where necessary) for PWS, currency units"), CostOfAllHotSpots, sep = ": "))  
  intermediate2$CostOfAllHotSpots <- CostOfAllHotSpots
  
#   The total cost of purchasing and installation of HOTSPOT routers, currency units
  TotalCostOfPushAndInstaOfHotSpots =  formula_3_3_7 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The total cost of purchasing and installation of HOTSPOT routers, currency units"), TotalCostOfPushAndInstaOfHotSpots, sep = ": "))  
  intermediate2$TotalCostOfPushAndInstaOfHotSpots <- TotalCostOfPushAndInstaOfHotSpots
  
#   Total cost of purchasing access switches for the PWS organization, currency units
  CostOfAllSwitchesToConnectAllHotspots =  formula_3_3_8 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of purchasing access switches for the PWS organization, currency units"), CostOfAllSwitchesToConnectAllHotspots, sep = ": "))  
  intermediate2$CostOfAllSwitchesToConnectAllHotspots <- CostOfAllSwitchesToConnectAllHotspots
  
#   Total cost of installation and commissioning of access switch equipment, currency units
  TotalCostOfSwitchesForHotSpotInst =  formula_3_3_9 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of installation and commissioning of access switch equipment, currency units"), TotalCostOfSwitchesForHotSpotInst, sep = ": "))  
  intermediate2$TotalCostOfSwitchesForHotSpotInst <- TotalCostOfSwitchesForHotSpotInst
  
#   Design cost, currency units
  DesignCostHotSPot =  formula_3_3_10 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Design cost, currency units"), DesignCostHotSPot, sep = ": "))  
  intermediate2$DesignCostHotSPot <- DesignCostHotSPot
  
#   Total cost of organization of PWS LAN, currency units
  TotalCAPEXOfPWSLAN =  formula_3_3_11 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of organization of PWS LAN, currency units"), TotalCAPEXOfPWSLAN, sep = ": "))  
  
#   Cable maintenance cost in areas from STC to HOTSPOT routers, currency units per year
  AllHotSpotCableMaintenanceCost =  formula_3_3_12 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cable maintenance cost in areas from STC to HOTSPOT routers, currency units per year"), AllHotSpotCableMaintenanceCost, sep = ": "))  
  intermediate2$AllHotSpotCableMaintenanceCost <- AllHotSpotCableMaintenanceCost
  
#   Determining the cost of access switches maintenance to connect HOTSPOT routers, currency units per year
  AllSwitchesForHotSpotMaintenanceCost =  formula_3_3_13 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Determining the cost of access switches maintenance to connect HOTSPOT routers, currency units per year"), AllSwitchesForHotSpotMaintenanceCost, sep = ": "))  
  intermediate2$AllSwitchesForHotSpotMaintenanceCost <- AllSwitchesForHotSpotMaintenanceCost
  
#   Determining the cost of HOTSPOT routers maintenance, currency units per year
  AllHotSpotsMaintenanceCost =  formula_3_3_14 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Determining the cost of HOTSPOT routers maintenance, currency units per year"), AllHotSpotsMaintenanceCost, sep = ": "))  
  intermediate2$AllHotSpotsMaintenanceCost <- AllHotSpotsMaintenanceCost
  
#   Determination of the total cost of PWS LAN maintenance during the maintenance period, cyrrency units
  TotalOPEXOfPWSLAN =  formula_3_3_15 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Determination of the total cost of PWS LAN maintenance during the maintenance period, cyrrency units"), TotalOPEXOfPWSLAN, sep = ": "))  
  
  

  result <- matrix (nrow = 2, ncol = 2)
  result [1,1] = i18n$t("Total cost of the public wireless segment (PWS) based on the HOTSPOT organization, currency units")
  result [1,2] = TotalCAPEXOfPWSLAN
  
  
  result [2,1] = i18n$t("Total maintenance cost of the public wireless segment (PWS) based on the HOTSPOT for the entire maintenance period, currency units")
  result [2,2] = TotalOPEXOfPWSLAN
  
  
  return (result)
  
}

algorithm3_3 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            
            req (input$GeneralVariables.CoeffReserveOfEquipment)
            req (input$BuildingParameters.Width)
            req (input$BuildingParameters.Length)
            req (input$WiFiEquipment.RadiusOfHotSpot)
            req (input$WiFiEquipment.AdapterReductionFactor)
            req (input$GeneralVariables.BusyCoeff)
            req (input$LANEquipment.NumberOfPortsInSwitch)
            req (input$LANEquipment.CostOfCable)
            req (input$GeneralVariables.LaborCostNormsForCableInst)
            req (input$GeneralVariables.CostNormsForCableInst)
            req (input$WiFiEquipment.CostOfOneHotSpotPouter)
            req (input$GeneralVariables.LaborCostNormsForHotSpotInst)
            req (input$GeneralVariables.CostNormsForHotSpotInst)
            req (input$LANEquipment.CostOfSwitch)
            req (input$GeneralVariables.LaborCostNormsForSwitchInst)
            req (input$GeneralVariables.CostNormsForSwitchInst)
            req (input$GeneralVariables.DesignConstrCoeff)
            req (input$GeneralVariables.AnnualLaborCostOfCableOperation)
            req (input$GeneralVariables.CostOfOneCableOperation)
            req (input$GeneralVariables.AnnualLaborCostOfSwitchOperation)
            req (input$GeneralVariables.CostOfOneSwitchOperation)
            req (input$GeneralVariables.AnnualLaborCostOfHotSpotOperation)
            req (input$GeneralVariables.CostOfHotSpotOperation)
            req (input$GeneralVariables.PeriodOfLANOperation)
            
            
            result <- algorithm3_3_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)     
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_3_3_1 = {#The number of wireless routers for the organization of PWS, units

            req (input$GeneralVariables.CoeffReserveOfEquipment)
            req (input$BuildingParameters.Width)
            req (input$BuildingParameters.Length)
            req (input$Intermediate.AreaOfAllClassRooms)
            req (input$WiFiEquipment.RadiusOfHotSpot)
            req (input$WiFiEquipment.AdapterReductionFactor)
            
            result <- formula_3_3_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_2 = {#Total length of LAN cables from STC to HOTSPOT routers, meters
            
            req (input$Intermediate.AvCableLenFromSTCToClassroom)
            req (input$Intermediate.NumberOfHotSpots)
            

            result <- formula_3_3_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_3 = {#Number of switches of the selected model for connection of HOTSPOT routers, units

            req (input$Intermediate.NumberOfHotSpots)
            req (input$GeneralVariables.BusyCoeff)
            req (input$LANEquipment.NumberOfPortsInSwitch)
            
            
            result <- formula_3_3_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_4 = {#The cost of purchasing cables to connect HOTSPOT routers with a margin of 10% of the length for laying cables, currency units

            req (input$Intermediate.NumberOfHotSpots)
            req (input$LANEquipment.CostOfCable)
            req (input$Intermediate.AvCableLenFromSTCToClassroom)
            
            result <- formula_3_3_4 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_5 = {#Total cost of laying cables to connect HOTSPOT routers, currency units


            req (input$Intermediate.CostOfAllCablesToConnectHotSpots)
            req (input$GeneralVariables.LaborCostNormsForCableInst)
            req (input$GeneralVariables.CostNormsForCableInst)
            req (input$Intermediate.NumberOfHotSpots)
            req (input$Intermediate.AvCableLenFromSTCToClassroom)
            
            result <- formula_3_3_5 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_6 = {#The cost of purchasing HOTSPOT (taking into account the materials required to install and obtain permission to use radio frequencies in regions where necessary) for PWS, currency units

            req (input$WiFiEquipment.CostOfOneHotSpotPouter)
            req (input$Intermediate.NumberOfHotSpots)
            
            result <- formula_3_3_6 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_7 = {#The total cost of purchasing and installation of HOTSPOT routers, currency units

            req (input$Intermediate.CostOfAllHotSpots)
            req (input$GeneralVariables.LaborCostNormsForHotSpotInst)
            req (input$GeneralVariables.CostNormsForHotSpotInst)
            req (input$Intermediate.NumberOfHotSpots)
            
            result <- formula_3_3_7 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_8 = {#Total cost of purchasing access switches for the PWS organization, currency units

            req (input$LANEquipment.CostOfSwitch)
            req (input$Intermediate.NumberOfSwitchesToConnectAllHotSpots)
            
            result <- formula_3_3_8 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_9 = {#Total cost of installation and commissioning of access switch equipment, currency units

            req (input$Intermediate.CostOfAllSwitchesToConnectAllHotspots)
            req (input$GeneralVariables.LaborCostNormsForSwitchInst)
            req (input$GeneralVariables.CostNormsForSwitchInst)
            req (input$Intermediate.NumberOfSwitchesToConnectAllHotSpots)
            

            result <- formula_3_3_9 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_10 = {#Design cost, currency units

            req (input$Intermediate.TotalCostOfSwitchesForHotSpotInst)
            req (input$Intermediate.TotalCostOfPushAndInstaOfHotSpots)
            req (input$Intermediate.TotalCostOfLayingCablesForHotSpots)
            req (input$GeneralVariables.DesignConstrCoeff)
            
            result <- formula_3_3_10 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_11 = {#Total cost of organization for LAN WES, currency units

            req (input$Intermediate.TotalCostOfSwitchesForHotSpotInst)
            req (input$Intermediate.TotalCostOfPushAndInstaOfHotSpots)
            req (input$Intermediate.TotalCostOfLayingCablesForHotSpots)
            req (input$Intermediate.DesignCostHotSPot)
            
            result <- formula_3_3_11 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_12 = {#Cable maintenance cost in areas from STC to HOTSPOT routers, currency units per year

            req (input$Intermediate.TotalLenOfCablesToHotSpot)
            req (input$GeneralVariables.AnnualLaborCostOfCableOperation)
            req (input$GeneralVariables.CostOfOneCableOperation)
            
            result <- formula_3_3_12 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_13 = {#Determining the cost of access switches maintenance to connect HOTSPOT routers, currency units per year

            req (input$Intermediate.NumberOfSwitchesToConnectAllHotSpots)
            req (input$GeneralVariables.AnnualLaborCostOfSwitchOperation)
            req (input$GeneralVariables.CostOfOneSwitchOperation)
            
            
            result <- formula_3_3_13 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_14 = {#Determining the cost of HOTSPOT routers maintenance, currency units per year

            req (input$Intermediate.NumberOfHotSpots)
            req (input$GeneralVariables.AnnualLaborCostOfHotSpotOperation)
            req (input$GeneralVariables.CostOfHotSpotOperation)
            

            result <- formula_3_3_14 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_3_15 = {#Determination of the total cost of PWS LAN maintenance during the maintenance period, cyrrency units

            req (input$Intermediate.AllHotSpotCableMaintenanceCost)
            req (input$Intermediate.AllSwitchesForHotSpotMaintenanceCost)
            req (input$Intermediate.AllHotSpotsMaintenanceCost)
            req (input$GeneralVariables.PeriodOfLANOperation)
            
            result <- formula_3_3_15 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          stop ("No!")
          
  )
}