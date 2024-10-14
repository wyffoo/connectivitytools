library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Determining the total cost of installation, commissioning and maintenance of the institution LAN wireless segment

# The total area of classrooms in EI 
formula_3_2_1 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfClassrooms <- 0 
  NumberOfClassrooms <- input$Intermediate.NumberOfClassrooms
  

  if (!is.null(intermediate))
  {
    NumberOfClassrooms <- as.numeric (intermediate$NumberOfClassrooms)
  }
  
    
  result <- input$BuildingParameters.LengthOfClassroom * input$BuildingParameters.WidthOfClassroom * NumberOfClassrooms
    
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# The number of access points for the organization of WES
formula_3_2_2 <- function (input, intermediate = NULL)
{
  req (input)
  

  AreaOfAllClassRooms <- 0 
  AreaOfAllClassRooms <- input$Intermediate.AreaOfAllClassRooms
  
  
  if (!is.null(intermediate))
  {
    AreaOfAllClassRooms <- as.numeric (intermediate$AreaOfAllClassRooms)
  }
  
  result <- round ((input$GeneralVariables.CoeffReserveOfEquipment*AreaOfAllClassRooms)/(pi*((input$WiFiEquipment.RadiusOfAP*input$WiFiEquipment.AdapterReductionFactor)^2)/2), digits = 0)  + 1
  
  return (result)
}

# Total length of LAN cables from STC to WIFI AP
formula_3_2_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  AvCableLenFromSTCToClassroom <- 0 
  AvCableLenFromSTCToClassroom <- input$Intermediate.AvCableLenFromSTCToClassroom
  

  NumberOfAP <- 0 
  NumberOfAP <- input$Intermediate.NumberOfAP
  
  if (!is.null(intermediate))
  {
    AvCableLenFromSTCToClassroom <- as.numeric (intermediate$AvCableLenFromSTCToClassroom)
    NumberOfAP <- as.numeric (intermediate$NumberOfAP)
  }
  
  
  result <- AvCableLenFromSTCToClassroom * NumberOfAP
  result <- round (as.numeric (result), digits = 2)  
  return (result)
}

# Number of switches to connect WIFI AP of selected model
formula_3_2_4 <- function (input, intermediate = NULL)
{
  req (input)
  

  NumberOfAP <- 0 
  NumberOfAP <- input$Intermediate.NumberOfAP
  
  
  if (!is.null(intermediate))
  {
    NumberOfAP <- as.numeric (intermediate$NumberOfAP)
  }
  
  
  result <- round ((NumberOfAP/input$GeneralVariables.BusyCoeff)/input$LANEquipment.NumberOfPortsInSwitch, digits = 0)  + 1
  
  return (result)
}

# Cost of purchasing cables to connect WIFI access points
formula_3_2_5 <- function (input, intermediate = NULL)
{
  req (input)
  

  NumberOfAP <- 0 
  NumberOfAP <- input$Intermediate.NumberOfAP
  
  
  AvCableLenFromSTCToClassroom <- 0 
  AvCableLenFromSTCToClassroom <- input$Intermediate.AvCableLenFromSTCToClassroom
  
  if (!is.null(intermediate))
  {
    NumberOfAP <- as.numeric (intermediate$NumberOfAP)
    AvCableLenFromSTCToClassroom <- as.numeric (intermediate$AvCableLenFromSTCToClassroom)
  }
  
  
  result <- input$LANEquipment.CostOfCable*NumberOfAP*AvCableLenFromSTCToClassroom
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Total cost of laying cables to connect WIFI access points
formula_3_2_6 <- function (input, intermediate = NULL)
{
  req (input)
  

  CostOfAllCablesToConnectAP <- 0 
  CostOfAllCablesToConnectAP <- input$Intermediate.CostOfAllCablesToConnectAP
  
  NumberOfAP <- 0 
  NumberOfAP <- input$Intermediate.NumberOfAP
  
  AvCableLenFromSTCToClassroom <- 0 
  AvCableLenFromSTCToClassroom <- input$Intermediate.AvCableLenFromSTCToClassroom
  
  if (!is.null(intermediate))
  {
    CostOfAllCablesToConnectAP <- as.numeric (intermediate$CostOfAllCablesToConnectAP)
    NumberOfAP <- as.numeric (intermediate$NumberOfAP)
    AvCableLenFromSTCToClassroom <- as.numeric (intermediate$AvCableLenFromSTCToClassroom)
  }
  
  
  result <- CostOfAllCablesToConnectAP + input$GeneralVariables.LaborCostNormsForCableInst*
  input$GeneralVariables.CostNormsForCableInst*NumberOfAP*AvCableLenFromSTCToClassroom
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Cost of purchasing WIFI access points for installation in classrooms
formula_3_2_7 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  NumberOfAP <- 0 
  NumberOfAP <- input$Intermediate.NumberOfAP
  
  
  if (!is.null(intermediate))
  {
    NumberOfAP <- as.numeric (intermediate$NumberOfAP)
  }
  
    
  result <- input$WiFiEquipment.CostOfOneAP * NumberOfAP
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Total cost of purchasing and installation of WIFI AP
formula_3_2_8 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  CostOfAllWiFiAP <- 0 
  CostOfAllWiFiAP <- input$Intermediate.CostOfAllWiFiAP
  
  
  NumberOfAP <- 0 
  NumberOfAP <- input$Intermediate.NumberOfAP
  
  if (!is.null(intermediate))
  {
    CostOfAllWiFiAP <- as.numeric (intermediate$CostOfAllWiFiAP)
    NumberOfAP <- as.numeric (intermediate$NumberOfAP)
  }
  
  
  result <- CostOfAllWiFiAP + input$GeneralVariables.LaborCostNormsForWiFiAPInst * 
  input$GeneralVariables.CostNormsForWiFiAPInst * NumberOfAP
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Total cost of purchasing of access switches for connecting APs with ports backups
formula_3_2_9 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  
  NumberOfSwitchesToConnectAllAP <- 0 
  NumberOfSwitchesToConnectAllAP <- input$Intermediate.NumberOfSwitchesToConnectAllAP
  
  
  if (!is.null(intermediate))
  {
    NumberOfSwitchesToConnectAllAP <- as.numeric (intermediate$NumberOfSwitchesToConnectAllAP)
  }
  
  
  result <- input$LANEquipment.CostOfSwitch * NumberOfSwitchesToConnectAllAP
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Total cost of installation and commissioning of access switches
formula_3_2_10 <- function (input, intermediate = NULL)
{
  req (input)
  
  

  CostOfAllSwitchesToConnectAllAP <- 0 
  CostOfAllSwitchesToConnectAllAP <- input$Intermediate.CostOfAllSwitchesToConnectAllAP
  
  
  NumberOfSwitchesToConnectAllAP <- 0 
  NumberOfSwitchesToConnectAllAP <- input$Intermediate.NumberOfSwitchesToConnectAllAP
  
  if (!is.null(intermediate))
  {
    CostOfAllSwitchesToConnectAllAP <- as.numeric (intermediate$CostOfAllSwitchesToConnectAllAP)
    NumberOfSwitchesToConnectAllAP <- as.numeric (intermediate$NumberOfSwitchesToConnectAllAP)
  }
  
  result <- CostOfAllSwitchesToConnectAllAP + input$GeneralVariables.LaborCostNormsForSwitchInst *
  input$GeneralVariables.CostNormsForSwitchInst * NumberOfSwitchesToConnectAllAP
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

# Design cost
formula_3_2_11 <- function (input, intermediate = NULL)
{
  req (input)
  

  TotalCostOfSwitchesForAPInst <- 0 
  TotalCostOfSwitchesForAPInst <- input$Intermediate.TotalCostOfSwitchesForAPInst
  
  TotalCostOfPushAndInstaOfAP <- 0 
  TotalCostOfPushAndInstaOfAP <- input$Intermediate.TotalCostOfPushAndInstaOfAP
  
  TotalCostOfLayingCablesForAP <- 0 
  TotalCostOfLayingCablesForAP <- input$Intermediate.TotalCostOfLayingCablesForAP
  
  if (!is.null(intermediate))
  {
    TotalCostOfSwitchesForAPInst <- as.numeric (intermediate$TotalCostOfSwitchesForAPInst)
    TotalCostOfPushAndInstaOfAP <- as.numeric (intermediate$TotalCostOfPushAndInstaOfAP)
    TotalCostOfLayingCablesForAP <- as.numeric (intermediate$TotalCostOfLayingCablesForAP)
  }
  
  
  result <- (TotalCostOfSwitchesForAPInst+TotalCostOfPushAndInstaOfAP+TotalCostOfLayingCablesForAP)*(input$GeneralVariables.DesignConstrCoeff/100)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Total cost of the STC LAN organization
formula_3_2_12 <- function (input, intermediate = NULL)
{
  req (input)
  
  TotalCostOfSwitchesForAPInst <- 0 
  TotalCostOfSwitchesForAPInst <- input$Intermediate.TotalCostOfSwitchesForAPInst
  
  TotalCostOfPushAndInstaOfAP <- 0 
  TotalCostOfPushAndInstaOfAP <- input$Intermediate.TotalCostOfPushAndInstaOfAP
  
  TotalCostOfLayingCablesForAP <- 0 
  TotalCostOfLayingCablesForAP <- input$Intermediate.TotalCostOfLayingCablesForAP
  
  DesignCostWLAN <- 0 
  DesignCostWLAN <- input$Intermediate.DesignCostWLAN
  
  if (!is.null(intermediate))
  {
    TotalCostOfSwitchesForAPInst <- as.numeric (intermediate$TotalCostOfSwitchesForAPInst)
    TotalCostOfPushAndInstaOfAP <- as.numeric (intermediate$TotalCostOfPushAndInstaOfAP)
    TotalCostOfLayingCablesForAP <- as.numeric (intermediate$TotalCostOfLayingCablesForAP)
    DesignCostWLAN <- as.numeric (intermediate$DesignCostWLAN)
  }
  
    
  result <- TotalCostOfSwitchesForAPInst + TotalCostOfPushAndInstaOfAP + TotalCostOfLayingCablesForAP + DesignCostWLAN
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Cable maintenance cost in areas from STC to WIFI AP
formula_3_2_13 <- function (input, intermediate = NULL)
{
  req (input)
  
  TotalLenOfCablesToWiFi <- 0 
  TotalLenOfCablesToWiFi <- input$Intermediate.TotalLenOfCablesToWiFi
  
  
  if (!is.null(intermediate))
  {
    TotalLenOfCablesToWiFi <- as.numeric (intermediate$TotalLenOfCablesToWiFi)
  }
  
  result <-   input$GeneralVariables.AnnualLaborCostOfCableOperation *
  input$GeneralVariables.CostOfOneCableOperation * TotalLenOfCablesToWiFi
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Cost of access switches maintenance for APs connection
formula_3_2_14 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  NumberOfSwitchesToConnectAllAP <- 0 
  NumberOfSwitchesToConnectAllAP <- input$Intermediate.NumberOfSwitchesToConnectAllAP
  
  
  if (!is.null(intermediate))
  {
    NumberOfSwitchesToConnectAllAP <- as.numeric (intermediate$NumberOfSwitchesToConnectAllAP)
  }
  
  
  result <-   input$GeneralVariables.AnnualLaborCostOfSwitchOperation * 
  input$GeneralVariables.CostOfOneSwitchOperation * NumberOfSwitchesToConnectAllAP
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# APs maintenance cost
formula_3_2_15 <- function (input, intermediate = NULL)
{
  req (input)
  
  NumberOfAP <- 0 
  NumberOfAP <- input$Intermediate.NumberOfAP
  
  
  if (!is.null(intermediate))
  {
    NumberOfAP <- as.numeric (intermediate$NumberOfAP)
  }
  
  result <-   input$GeneralVariables.AnnualLaborCostOfAPOperation *
  input$GeneralVariables.CostOfAPOperation * NumberOfAP
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

# Total maintenance cost of LAN WES for the entire maintenance period
formula_3_2_16 <- function (input, intermediate = NULL)
{
  req (input)


  AllCableMaintenanceCost <- 0 
  AllCableMaintenanceCost <- input$Intermediate.AllCableMaintenanceCost
  
  AllSwitchesForAPsMaintenanceCost <- 0 
  AllSwitchesForAPsMaintenanceCost <- input$Intermediate.AllSwitchesForAPsMaintenanceCost
  
  AllAPsMaintenanceCost <- 0 
  AllAPsMaintenanceCost <- input$Intermediate.AllAPsMaintenanceCost
  
  if (!is.null(intermediate))
  {
    AllCableMaintenanceCost <- as.numeric (intermediate$AllCableMaintenanceCost)
    AllSwitchesForAPsMaintenanceCost <- as.numeric (intermediate$AllSwitchesForAPsMaintenanceCost)
    AllAPsMaintenanceCost <- as.numeric (intermediate$AllAPsMaintenanceCost)
  }
  
  
  result <- (AllCableMaintenanceCost+AllSwitchesForAPsMaintenanceCost+AllAPsMaintenanceCost)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

algorithm3_2_impl <- function(input, intermediate = NULL)
{
  intermediate2 <- list (
    NumberOfClassrooms = 0.0,
    AreaOfAllClassRooms = 0.0,
    AvCableLenFromSTCToClassroom = 0.0,
    NumberOfAP = 0.0,
    CostOfAllCablesToConnectAP = 0.0,
    CostOfAllWiFiAP = 0.0,
    NumberOfSwitchesToConnectAllAP = 0.0,
    CostOfAllSwitchesToConnectAllAP = 0.0,
    TotalCostOfSwitchesForAPInst = 0.0,
    TotalCostOfPushAndInstaOfAP = 0.0,
    TotalCostOfLayingCablesForAP = 0.0,
    DesignCostWLAN = 0.0,
    TotalLenOfCablesToWiFi = 0.0,
    AllCableMaintenanceCost = 0.0,
    AllSwitchesForAPsMaintenanceCost = 0.0,
    AllAPsMaintenanceCost = 0.0
  )
  
  if (!is.null(intermediate))
  {
    intermediate2$NumberOfClassrooms <- as.numeric (intermediate$NumberOfClassrooms)
    intermediate2$AvCableLenFromSTCToClassroom <- as.numeric (intermediate$AvCableLenFromSTCToClassroom)
  }
  else
  {
    intermediate2$NumberOfClassrooms <- input$Intermediate.NumberOfClassrooms
    intermediate2$AvCableLenFromSTCToClassroom <- input$Intermediate.AvCableLenFromSTCToClassroom
    
  }
  
  # The total area of classrooms in EI, square meters
  AreaOfAllClassRooms =  formula_3_2_1 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The total area of classrooms in EI, square meters"), AreaOfAllClassRooms, sep = ": "))  
  intermediate2$AreaOfAllClassRooms <- AreaOfAllClassRooms
  
  # The number of access points for the organization of WES, units
  NumberOfAP =  formula_3_2_2 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The number of access points for the organization of WES, units"), NumberOfAP, sep = ": "))  
  intermediate2$NumberOfAP <- NumberOfAP
  
  # Total length of LAN cables from STC to WIFI AP, meters
  TotalLenOfCablesToWiFi =  formula_3_2_3 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total length of LAN cables from STC to WIFI AP, meters"), TotalLenOfCablesToWiFi, sep = ": "))  
  intermediate2$TotalLenOfCablesToWiFi <- TotalLenOfCablesToWiFi
  
  # Number of switches to connect WIFI AP of selected model, units
  NumberOfSwitchesToConnectAllAP =  formula_3_2_4 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of switches to connect WIFI AP of selected model, units"), NumberOfSwitchesToConnectAllAP, sep = ": "))  
  intermediate2$NumberOfSwitchesToConnectAllAP <- NumberOfSwitchesToConnectAllAP
  
  # Cost of purchasing cables to connect WIFI access points, currency units
  CostOfAllCablesToConnectAP =  formula_3_2_5 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of purchasing cables to connect WIFI access points, currency units"), CostOfAllCablesToConnectAP, sep = ": "))  
  intermediate2$CostOfAllCablesToConnectAP <- CostOfAllCablesToConnectAP
  
  # Total cost of laying cables to connect WIFI access points, currency units
  TotalCostOfLayingCablesForAP =  formula_3_2_6 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of laying cables to connect WIFI access points, currency units"), TotalCostOfLayingCablesForAP, sep = ": "))  
  intermediate2$TotalCostOfLayingCablesForAP <- TotalCostOfLayingCablesForAP
  
  # Cost of purchasing WIFI access points for installation in classrooms, currency units
  CostOfAllWiFiAP =  formula_3_2_7 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of purchasing WIFI access points for installation in classrooms, currency units"), CostOfAllWiFiAP, sep = ": "))  
  intermediate2$CostOfAllWiFiAP <- CostOfAllWiFiAP
  
  # Total cost of purchasing and installation of WIFI AP, currency units
  TotalCostOfPushAndInstaOfAP =  formula_3_2_8 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of purchasing and installation of WIFI AP, currency units"), TotalCostOfPushAndInstaOfAP, sep = ": "))  
  intermediate2$TotalCostOfPushAndInstaOfAP <- TotalCostOfPushAndInstaOfAP
  
  # Total cost of purchasing of access switches for connecting APs with ports backups, currency units
  CostOfAllSwitchesToConnectAllAP =  formula_3_2_9 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of purchasing of access switches for connecting APs with ports backups, currency units"), CostOfAllSwitchesToConnectAllAP, sep = ": "))  
  intermediate2$CostOfAllSwitchesToConnectAllAP <- CostOfAllSwitchesToConnectAllAP
  
  # Total cost of installation and commissioning of access switches, currency units
  TotalCostOfSwitchesForAPInst =  formula_3_2_10 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of installation and commissioning of access switches, currency units"), TotalCostOfSwitchesForAPInst, sep = ": "))  
  intermediate2$TotalCostOfSwitchesForAPInst <- TotalCostOfSwitchesForAPInst
  
  # Design cost, currency units
  DesignCostWLAN =  formula_3_2_11 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Design cost, currency units"), DesignCostWLAN, sep = ": "))  
  intermediate2$DesignCostWLAN <- DesignCostWLAN
  
  # Total cost of the LAN WES organization, currency units
  TotalCAPEXOfLANWESOrg =  formula_3_2_12 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of the LAN WES organization, currency units"), TotalCAPEXOfLANWESOrg, sep = ": "))  
  
  # Cable maintenance cost in areas from STC to WIFI AP, currency units per year
  AllCableMaintenanceCost =  formula_3_2_13 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cable maintenance cost in areas from STC to WIFI AP, currency units per year"), AllCableMaintenanceCost, sep = ": "))  
  intermediate2$AllCableMaintenanceCost <- AllCableMaintenanceCost
  
  # Cost of access switches maintenance for APs connection, currency units per year
  AllSwitchesForAPsMaintenanceCost =  formula_3_2_14 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of access switches maintenance for APs connection, currency units per year"), AllSwitchesForAPsMaintenanceCost, sep = ": "))  
  intermediate2$varname <- AllSwitchesForAPsMaintenanceCost
  
  # APs maintenance cost, currency units per year
  AllAPsMaintenanceCost =  formula_3_2_15 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("APs maintenance cost, currency units per year"), AllAPsMaintenanceCost, sep = ": "))  
  intermediate2$AllAPsMaintenanceCost <- AllAPsMaintenanceCost
  
  # Total maintenance cost of LAN WES for the entire maintenance period, currency units
  TotalOPEXOfLANWESOrg =  formula_3_2_16 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total maintenance cost of LAN WES for the entire maintenance period, currency units"), TotalOPEXOfLANWESOrg, sep = ": "))  
  
  
  result <- matrix (nrow = 3, ncol = 2)
  result [1,1] = i18n$t("Total cost of the LAN WES organization, currency units")
  result [1,2] = TotalCAPEXOfLANWESOrg
  
  
  result [2,1] = i18n$t("Total maintenance cost of LAN WES for the entire maintenance period, currency units")
  result [2,2] = TotalOPEXOfLANWESOrg
  

  result [3,1] = i18n$t("The total area of classrooms in EI, square meters")
  result [3,2] = AreaOfAllClassRooms
  
  return (result)
  
}

algorithm3_2 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            req (input$BuildingParameters.LengthOfClassroom)
            req (input$BuildingParameters.WidthOfClassroom)
            req (input$GeneralVariables.CoeffReserveOfEquipment)
            req (input$WiFiEquipment.RadiusOfAP)
            req (input$WiFiEquipment.AdapterReductionFactor)
            req (input$GeneralVariables.BusyCoeff)
            req (input$LANEquipment.NumberOfPortsInSwitch)
            req (input$LANEquipment.CostOfCable)
            req (input$GeneralVariables.LaborCostNormsForCableInst)
            req (input$GeneralVariables.CostNormsForCableInst)
            req (input$WiFiEquipment.CostOfOneAP)
            req (input$GeneralVariables.LaborCostNormsForWiFiAPInst)
            req (input$GeneralVariables.CostNormsForWiFiAPInst)
            req (input$LANEquipment.CostOfSwitch)
            req (input$GeneralVariables.LaborCostNormsForSwitchInst)
            req (input$GeneralVariables.CostNormsForSwitchInst)
            req (input$GeneralVariables.DesignConstrCoeff)
            req (input$GeneralVariables.AnnualLaborCostOfCableOperation)
            req (input$GeneralVariables.CostOfOneCableOperation)
            req (input$GeneralVariables.AnnualLaborCostOfSwitchOperation)
            req (input$GeneralVariables.CostOfOneSwitchOperation)
            req (input$GeneralVariables.AnnualLaborCostOfAPOperation)
            req (input$GeneralVariables.CostOfAPOperation)
            req (input$GeneralVariables.PeriodOfLANOperation)
            
            
            result <- algorithm3_2_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)     
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_3_2_1 = {#The total area of classrooms in EI , square meters
            
            req (input$BuildingParameters.LengthOfClassroom)
            req (input$BuildingParameters.WidthOfClassroom)
            req (input$Intermediate.NumberOfClassrooms)
            

            result <- formula_3_2_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_2 = {#The number of access points for the organization of WES, units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$GeneralVariables.CoeffReserveOfEquipment)
            req (input$Intermediate.AreaOfAllClassRooms)
            req (input$WiFiEquipment.RadiusOfAP)
            req (input$WiFiEquipment.AdapterReductionFactor)
            
            
            result <- formula_3_2_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_3 = {#Total length of LAN cables from STC to WIFI AP, meters

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.AvCableLenFromSTCToClassroom)
            req (input$Intermediate.NumberOfAP)
            
            result <- formula_3_2_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_4 = {#Number of switches to connect WIFI AP of selected model, units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.NumberOfAP)
            req (input$GeneralVariables.BusyCoeff)
            req (input$LANEquipment.NumberOfPortsInSwitch)
            
            
            result <- formula_3_2_4 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_5 = {#Cost of purchasing cables to connect WIFI access points, currency units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.NumberOfAP)
            req (input$LANEquipment.CostOfCable)
            req (input$Intermediate.AvCableLenFromSTCToClassroom)
            
            result <- formula_3_2_5 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_6 = {#Total cost of laying cables to connect WIFI access points, currency units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.CostOfAllCablesToConnectAP)
            req (input$GeneralVariables.LaborCostNormsForCableInst)
            req (input$GeneralVariables.CostNormsForCableInst)
            req (input$Intermediate.NumberOfAP)
            req (input$Intermediate.AvCableLenFromSTCToClassroom)
            
            result <- formula_3_2_6 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_7 = {#Cost of purchasing WIFI access points for installation in classrooms, currency units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$WiFiEquipment.CostOfOneAP)
            req (input$Intermediate.NumberOfAP)
            
            result <- formula_3_2_7 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_8 = {#Total cost of purchasing and installation of WIFI AP, currency units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.CostOfAllWiFiAP)
            req (input$GeneralVariables.LaborCostNormsForWiFiAPInst)
            req (input$GeneralVariables.CostNormsForWiFiAPInst)
            req (input$Intermediate.NumberOfAP)
            
            result <- formula_3_2_8 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_9 = {#Total cost of purchasing of access switches for connecting APs with ports backups, currency units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$LANEquipment.CostOfSwitch)
            req (input$Intermediate.NumberOfSwitchesToConnectAllAP)
            
            result <- formula_3_2_9 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_10 = {#Total cost of installation and commissioning of access switches, currency units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.CostOfAllSwitchesToConnectAllAP)
            req (input$GeneralVariables.LaborCostNormsForSwitchInst)
            req (input$GeneralVariables.CostNormsForSwitchInst)
            req (input$Intermediate.NumberOfSwitchesToConnectAllAP)
            
            result <- formula_3_2_10 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_11 = {#Design cost, currency units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.TotalCostOfSwitchesForAPInst)
            req (input$Intermediate.TotalCostOfPushAndInstaOfAP)
            req (input$Intermediate.TotalCostOfLayingCablesForAP)
            req (input$GeneralVariables.DesignConstrCoeff)
            
            result <- formula_3_2_11 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_12 = {#Total cost of the LAN WES organization, currency units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.TotalCostOfSwitchesForAPInst)
            req (input$Intermediate.TotalCostOfPushAndInstaOfAP)
            req (input$Intermediate.TotalCostOfLayingCablesForAP)
            req (input$Intermediate.DesignCostWLAN)
            
            result <- formula_3_2_12 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_13 = {#Cable maintenance cost in areas from STC to WIFI AP, currency units per year

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.TotalLenOfCablesToWiFi)
            req (input$GeneralVariables.AnnualLaborCostOfCableOperation)
            req (input$GeneralVariables.CostOfOneCableOperation)
            
            result <- formula_3_2_13 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_14 = {#Cost of access switches maintenance for APs connection, currency units per year

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.NumberOfSwitchesToConnectAllAP)
            req (input$GeneralVariables.AnnualLaborCostOfSwitchOperation)
            req (input$GeneralVariables.CostOfOneSwitchOperation)
            
            result <- formula_3_2_14 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_15 = {#APs maintenance cost, currency units per year
            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.NumberOfAP)
            req (input$GeneralVariables.AnnualLaborCostOfAPOperation)
            req (input$GeneralVariables.CostOfAPOperation)
            
            result <- formula_3_2_15 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_2_16 = {#Total maintenance cost of LAN WES for the entire maintenance period, currency units

            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Intermediate.AllCableMaintenanceCost)
            req (input$Intermediate.AllSwitchesForAPsMaintenanceCost)
            req (input$Intermediate.AllAPsMaintenanceCost)
            req (input$GeneralVariables.PeriodOfLANOperation)
            
            result <- formula_3_2_16 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          stop ("No!")
          
  )
}