library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Determining the total cost of installation, commissioning and operation of a wired LAN for educational segment of IT equipment


#The average length of a section of one cable from the STC to the point of entry of cables into the classroom
formula_3_1_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  result <- (input$GeneralVariables.STCPlaceChangeCoeff*
               (input$BuildingParameters.Width+ input$BuildingParameters.Length) + 
               input$BuildingParameters.HeightPlus*input$BuildingParameters.Levels)*
      input$GeneralVariables.STCPlaceLevel*input$GeneralVariables.CableBackUp
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#The total telecommunication outlets (TO)s number to be installed for related educational equipment, which also requires a LAN connection (for example, a video wall, teacher?s PC, interactive whiteboard, etc.)
formula_3_1_2 <- function (input, intermediate = NULL)
{
  req (input)

  result <- matrix (nrow = 2, ncol = 2)
  result [1,1] = i18n$t("The total telecommunication outlets (TO)s number to be installed for related educational equipment, units")
  
  result [2,1] = i18n$t("Average number of students in one class, students")
  
  result [1,2] <- (input$GeneralVariables.CableBackUp*input$SchoolSpecific.PupilsNumber*(input$GeneralVariables.ClassroomWithInternet/100)/input$GeneralVariables.NumberOfShifts)
  result [2,2] <- ((input$SchoolSpecific.PupilsNumber*(input$GeneralVariables.ClassroomWithInternet/100)/input$GeneralVariables.PupilsInInOneClass)/input$GeneralVariables.NumberOfShifts)
  
  result [1,2] <- round (as.numeric (result [1,2]), digits = 2)
  result [2,2] <- round (as.numeric (result [2,2]), digits = 2)
  
  return (result)
}

#The average length of the cable section from the classroom entry point up to the STC
formula_3_1_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  result <- (input$BuildingParameters.LengthOfClassroom + 
               input$BuildingParameters.WidthOfClassroom + 
               input$BuildingParameters.HeightMinus )*input$BuildingParameters.Levels*input$GeneralVariables.CableBackUp
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Length of LAN cables within the classroom to the telecommunication outlets (to connect all the necessary educating places to the designed network)
formula_3_1_4 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  AvCableLenFromClassroomEntryToOutlet <- 0 
  AvCableLenFromClassroomEntryToOutlet <- input$Intermediate.AvCableLenFromClassroomEntryToOutlet
  
  NumberOfTOs <- 0 
  NumberOfTOs <- input$Intermediate.NumberOfTOs
  
  if (!is.null(intermediate))
  {
    AvCableLenFromClassroomEntryToOutlet <- as.numeric (intermediate$AvCableLenFromClassroomEntryToOutlet)
    NumberOfTOs <- as.numeric (intermediate$NumberOfTOs)
  }
  
  

  result <- AvCableLenFromClassroomEntryToOutlet*NumberOfTOs
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Length of LAN cables from STC to classrooms
formula_3_1_5 <- function (input, intermediate = NULL)
{
  req (input)
  
  AvCableLenFromSTCToClassroom <- 0 
  AvCableLenFromSTCToClassroom <- input$Intermediate.AvCableLenFromSTCToClassroom
  NumberOfTOs <- 0 
  NumberOfTOs <- input$Intermediate.NumberOfTOs
  
  if (!is.null(intermediate))
  {
    AvCableLenFromSTCToClassroom <- as.numeric (intermediate$AvCableLenFromSTCToClassroom)
    NumberOfTOs <- as.numeric (intermediate$NumberOfTOs)
  }
  
  result <- AvCableLenFromSTCToClassroom*NumberOfTOs
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cable length of the wired segment LAN
formula_3_1_6 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  AllCableLenFromClassroomEntryToOutlet <- 0 
  AllCableLenFromClassroomEntryToOutlet <- input$Intermediate.AllCableLenFromClassroomEntryToOutlet
  AllCableLenFromSTCToClassroom <- 0 
  AllCableLenFromSTCToClassroom <- input$Intermediate.AllCableLenFromSTCToClassroom
  
  if (!is.null(intermediate))
  {
    AllCableLenFromClassroomEntryToOutlet <- as.numeric (intermediate$AllCableLenFromClassroomEntryToOutlet)
    AllCableLenFromSTCToClassroom <- as.numeric (intermediate$AllCableLenFromSTCToClassroom)
  }
  
  result <- AllCableLenFromClassroomEntryToOutlet + AllCableLenFromSTCToClassroom
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#The number of switches of the selected model (Kres_port is usually assumed by default that  0.6) is the initial load factor of ports that are considered as "busy", other ports in the design are considered as backup
formula_3_1_7 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  NumberOfTOs <- 0 
  NumberOfTOs <- input$Intermediate.NumberOfTOs
  
  if (!is.null(intermediate))
  {
    NumberOfTOs <- as.numeric (intermediate$NumberOfTOs)
  }
  
  result <- round ((NumberOfTOs/input$GeneralVariables.BusyCoeff)/input$LANEquipment.NumberOfPortsInSwitch, digits = 0)  + 1  
  
  return (result)
}

#Cost of purchasing cables for LAN installation with a margin of 10% of the length for laying cables
#TODO Table 5.9 - 1 (+ 1 param)
formula_3_1_8 <- function (input, intermediate = NULL)
{
  req (input)
  
  

  NumberOfClassrooms <- 0 
  NumberOfClassrooms <- input$Intermediate.NumberOfClassrooms
  
  AllCableLenForWiredLAN <- 0 
  AllCableLenForWiredLAN <- input$Intermediate.AllCableLenForWiredLAN
  
  
  if (!is.null(intermediate))
  {
    NumberOfClassrooms <- as.numeric (intermediate$NumberOfClassrooms)
    
    AllCableLenForWiredLAN <- as.numeric (intermediate$AllCableLenForWiredLAN)
  }
  
  result <- input$LANEquipment.CostOfCable*AllCableLenForWiredLAN
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of laying cables for LAN installation
formula_3_1_9 <- function (input, intermediate = NULL)
{
  req (input)
  
  

  CostOfAllCablesPlus <- 0 
  CostOfAllCablesPlus <- input$Intermediate.CostOfAllCablesPlus
  AllCableLenForWiredLAN <- 0 
  AllCableLenForWiredLAN <- input$Intermediate.AllCableLenForWiredLAN
  
  if (!is.null(intermediate))
  {
    CostOfAllCablesPlus <- as.numeric (intermediate$CostOfAllCablesPlus)
    AllCableLenForWiredLAN <- as.numeric (intermediate$AllCableLenForWiredLAN)
  }
  
  result <- CostOfAllCablesPlus + input$GeneralVariables.LaborCostNormsForCableInst*input$GeneralVariables.CostNormsForCableInst* AllCableLenForWiredLAN
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of purchasing of TOs
formula_3_1_10 <- function (input, intermediate = NULL)
{
  req (input)
  
  

  NumberOfTOs <- 0 
  NumberOfTOs <- input$Intermediate.NumberOfTOs
  
  if (!is.null(intermediate))
  {
    NumberOfTOs <- as.numeric (intermediate$NumberOfTOs)
  }
  
  
  result <- input$LANEquipment.CostOfOutlet*NumberOfTOs
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of purchasing, installation and TOs commissioning
formula_3_1_11 <- function (input, intermediate = NULL)
{
  req (input)
  

  CostOfAllOutlets <- 0 
  CostOfAllOutlets <- input$Intermediate.CostOfAllOutlets
  NumberOfTOs <- 0 
  NumberOfTOs <- input$Intermediate.NumberOfTOs
  
  if (!is.null(intermediate))
  {
    CostOfAllOutlets <- as.numeric (intermediate$CostOfAllOutlets)
    NumberOfTOs <- as.numeric (intermediate$NumberOfTOs)
  }
  
  result <- CostOfAllOutlets + input$GeneralVariables.LaborCostNormsForTOInst*input$GeneralVariables.CostNormsForTOInst*NumberOfTOs
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of purchasing switching equipment with ports backup
formula_3_1_12 <- function (input, intermediate = NULL)
{
  req (input)
  

  NumberOfSwitches <- 0 
  NumberOfSwitches <- input$Intermediate.NumberOfSwitches
  
  if (!is.null(intermediate))
  {
    NumberOfSwitches <- as.numeric (intermediate$NumberOfSwitches)
  }
  
  result <- input$LANEquipment.CostOfSwitch*NumberOfSwitches
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of installation and commissioning of switches
formula_3_1_13 <- function (input, intermediate = NULL)
{
  req (input)
  

  CostOfAllSwitchesPlus <- 0 
  CostOfAllSwitchesPlus <- input$Intermediate.CostOfAllSwitchesPlus
  NumberOfSwitches <- 0 
  NumberOfSwitches <- input$Intermediate.NumberOfSwitches
  
  if (!is.null(intermediate))
  {
    CostOfAllSwitchesPlus <- as.numeric (intermediate$CostOfAllSwitchesPlus)
    NumberOfSwitches <- as.numeric (intermediate$NumberOfSwitches)
  }
  
  result <- CostOfAllSwitchesPlus + input$GeneralVariables.LaborCostNormsForSwitchInst*input$GeneralVariables.CostNormsForSwitchInst*NumberOfSwitches
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Design cost
formula_3_1_14 <- function (input, intermediate = NULL)
{
  req (input)
  

  CostOfInstallationAllSwitches <- 0 
  CostOfInstallationAllSwitches <- input$Intermediate.CostOfInstallationAllSwitches
  CostOfInstallationAllTOs <- 0 
  CostOfInstallationAllTOs <- input$Intermediate.CostOfInstallationAllTOs
  CostOfInstallationAllCable <- 0 
  CostOfInstallationAllCable <- input$Intermediate.CostOfInstallationAllCable
  
  if (!is.null(intermediate))
  {
    CostOfInstallationAllSwitches <- as.numeric (intermediate$CostOfInstallationAllSwitches)
    CostOfInstallationAllTOs <- as.numeric (intermediate$CostOfInstallationAllTOs)
    CostOfInstallationAllCable <- as.numeric (intermediate$CostOfInstallationAllCable)
  }
  
  result <- (CostOfInstallationAllSwitches+CostOfInstallationAllTOs+CostOfInstallationAllCable)*(input$GeneralVariables.DesignConstrCoeff/100)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total cost of organizing a wired LAN segment
formula_3_1_15 <- function (input, intermediate = NULL)
{
  req (input)
  

  CostOfInstallationAllSwitches <- 0 
  CostOfInstallationAllSwitches <- input$Intermediate.CostOfInstallationAllSwitches
  CostOfInstallationAllTOs <- 0 
  CostOfInstallationAllTOs <- input$Intermediate.CostOfInstallationAllTOs
  CostOfInstallationAllCable <- 0 
  CostOfInstallationAllCable <- input$Intermediate.CostOfInstallationAllCable
  DesignCost <- 0
  DesignCost <- input$Intermediate.DesignCost
  
  if (!is.null(intermediate))
  {
    CostOfInstallationAllSwitches <- as.numeric (intermediate$CostOfInstallationAllSwitches)
    CostOfInstallationAllTOs <- as.numeric (intermediate$CostOfInstallationAllTOs)
    CostOfInstallationAllCable <- as.numeric (intermediate$CostOfInstallationAllCable)
    DesignCost <- as.numeric (intermediate$DesignCost)
  }
  
  result <- CostOfInstallationAllSwitches+CostOfInstallationAllTOs+CostOfInstallationAllCable+ DesignCost
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of LAN cable maintenance
formula_3_1_16 <- function (input, intermediate = NULL)
{
  req (input)
  

  AllCableLenForWiredLAN <- 0 
  AllCableLenForWiredLAN <- input$Intermediate.AllCableLenForWiredLAN
  

    if (!is.null(intermediate))
  {
    AllCableLenForWiredLAN <- as.numeric (intermediate$AllCableLenForWiredLAN)
    
  }

  result <- input$GeneralVariables.CostOfOneCableOperation * AllCableLenForWiredLAN * 
    input$GeneralVariables.AnnualLaborCostOfCableOperation
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Cost of switches maintenance
formula_3_1_17 <- function (input, intermediate = NULL)
{
  req (input)
  


  NumberOfSwitches <- 0 
  NumberOfSwitches <- input$Intermediate.NumberOfSwitches
  
    if (!is.null(intermediate))
  {
      NumberOfSwitches <- as.numeric (intermediate$NumberOfSwitches)
  }
  
  result <- input$GeneralVariables.AnnualLaborCostOfSwitchOperation*input$GeneralVariables.CostOfOneSwitchOperation*NumberOfSwitches
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Total maintenance cost of a wired LAN for the entire maintenance period
formula_3_1_18 <- function (input, intermediate = NULL)
{
  req (input)
  

  CostOfAllCableMaintenance <- 0 
  CostOfAllCableMaintenance <- input$Intermediate.CostOfAllCableMaintenance
  
  CostOfAllSwitchesMaintenance <- 0 
  CostOfAllSwitchesMaintenance <- input$Intermediate.CostOfAllSwitchesMaintenance
  
  if (!is.null(intermediate))
  {
      CostOfAllCableMaintenance <- as.numeric (intermediate$CostOfAllCableMaintenance)
      CostOfAllSwitchesMaintenance <- as.numeric (intermediate$CostOfAllSwitchesMaintenance)
  }
  
  result <- (CostOfAllCableMaintenance + CostOfAllSwitchesMaintenance)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


algorithm3_1_impl <- function(input, intermediate = NULL)
{
  intermediate2 <- list (
  NumberOfClassrooms = 0.0,
  AvCableLenFromClassroomEntryToOutlet = 0.0,
  NumberOfTOs = 0.0,
  AvCableLenFromSTCToClassroom = 0.0,
  NumberOfTOs = 0.0,
  AllCableLenFromClassroomEntryToOutlet = 0.0,
  AllCableLenFromSTCToClassroom = 0.0,
  NumberOfClassrooms = 0.0,
  AllCableLenForWiredLAN = 0.0,
  CostOfAllCablesPlus = 0.0,
  CostOfAllOutlets = 0.0,
  NumberOfSwitches = 0.0,
  CostOfAllSwitchesPlus = 0.0,
  CostOfInstallationAllSwitches = 0.0,
  CostOfInstallationAllTOs = 0.0,
  CostOfInstallationAllCable = 0.0,
  DesignCost = 0.0,
  AllCableLenForWiredLAN = 0.0,
  AnnualLaborCostOfCableOperation = 0.0
  )
  
  
  # Average length of a section of one cable from the STC to the point of entry of cables into the classroom
  AvCableLenFromSTCToClassroom =  formula_3_1_1 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Average length of a section of one cable from the STC to the point of entry of cables into the classroom, meters"), AvCableLenFromSTCToClassroom, sep = ": "))  

  
  # Total TOs number to be installed for related educational equipment
  res = formula_3_1_2 (input, intermediate)
  NumberOfTOs =  res [1,2]
  NumberOfClassrooms = res [2,2]
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total TOs number to be installed for related educational equipment, units"), NumberOfTOs, sep = ": "))  
  intermediate2$NumberOfTOs <- NumberOfTOs
  intermediate2$NumberOfClassrooms <- NumberOfClassrooms
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste ("Number of classrooms, classrooms", NumberOfClassrooms, sep = ": "))  
  
  # The average length of the cable section from the classroom entry point up to the TO
  AvCableLenFromClassroomEntryToOutlet =  formula_3_1_3 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The average length of the cable section from the classroom entry point up to the STC, meters"), AvCableLenFromClassroomEntryToOutlet, sep = ": "))  
  
  
  if ((AvCableLenFromSTCToClassroom + AvCableLenFromClassroomEntryToOutlet) > 95)
  {
    AvCableLenFromSTCToClassroom = 95
    AvCableLenFromClassroomEntryToOutlet = 0
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Via large size of school building average lenght of one cable from switch to TO is limited by 95 meters (should be organized by desctribution switches in the different places of building)"))  
  }
  

  intermediate2$AvCableLenFromSTCToClassroom <- AvCableLenFromSTCToClassroom
  intermediate2$AvCableLenFromClassroomEntryToOutlet <- AvCableLenFromClassroomEntryToOutlet

    
  # Length of LAN cables within the classroom to the telecommunication outlets (to connect all the necessary educating places to the designed network)
  AllCableLenFromClassroomEntryToOutlet =  formula_3_1_4 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Length of LAN cables within the classroom to the telecommunication outlets (to connect all the necessary educating places to the designed network), meters"), AllCableLenFromClassroomEntryToOutlet, sep = ": "))  
  intermediate2$AllCableLenFromClassroomEntryToOutlet <- AllCableLenFromClassroomEntryToOutlet
  
  
  # Length of LAN cables from STC to classrooms
  AllCableLenFromSTCToClassroom =  formula_3_1_5 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Length of LAN cables from STC to classrooms, meters"), AllCableLenFromSTCToClassroom, sep = ": "))  
  intermediate2$AllCableLenFromSTCToClassroom <- AllCableLenFromSTCToClassroom
  
  
  # Total cable length of the wired segment LAN
  AllCableLenForWiredLAN =  formula_3_1_6 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cable length of the wired segment LAN, meters"), AllCableLenForWiredLAN, sep = ": "))  
  intermediate2$AllCableLenForWiredLAN <- AllCableLenForWiredLAN
  
  # The number of switches of the selected model (Kres_port is usually assumed by default that  0.6) is the initial load factor of ports that are considered as "busy", other ports in the design are considered as backup
  NumberOfSwitches =  formula_3_1_7 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The number of switches of the selected model (Kres_port is usually assumed by default that  0.6) is the initial load factor of ports that are considered as busy, other ports in the design are considered as backup, units"), NumberOfSwitches, sep = ": "))  
  intermediate2$NumberOfSwitches <- NumberOfSwitches
  
  
  # Cost of purchasing cables for LAN installation with a margin of 10% of the length for laying cables
  CostOfAllCablesPlus =  formula_3_1_8 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of purchasing cables for LAN installation with a margin of 10% of the length for laying cables, currency units"), CostOfAllCablesPlus, sep = ": "))  
  intermediate2$CostOfAllCablesPlus <- CostOfAllCablesPlus
  
  
  # Total cost of laying cables for LAN installation
  CostOfInstallationAllCable =  formula_3_1_9 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of laying cables for LAN installation, currency units"), CostOfInstallationAllCable, sep = ": "))  
  intermediate2$CostOfInstallationAllCable <- CostOfInstallationAllCable
  
  
  # Cost of purchasing of TOs
  CostOfAllOutlets =  formula_3_1_10 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of purchasing of TOs, currency units"), CostOfAllOutlets, sep = ": "))  
  intermediate2$CostOfAllOutlets <- CostOfAllOutlets
  
  # Total cost of purchasing, installation and TOs commissioning
  CostOfInstallationAllTOs =  formula_3_1_11 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of purchasing, installation and TOs commissioning, currency units"), CostOfInstallationAllTOs, sep = ": "))  
  intermediate2$CostOfInstallationAllTOs <- CostOfInstallationAllTOs
  
  # Cost of purchasing switching equipment with ports backup
  CostOfAllSwitchesPlus =  formula_3_1_12 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of purchasing switching equipment with ports backup, currency units"), CostOfAllSwitchesPlus, sep = ": "))  
  intermediate2$CostOfAllSwitchesPlus <- CostOfAllSwitchesPlus
  
  
  # Total cost of installation and commissioning of switches
  CostOfInstallationAllSwitches =  formula_3_1_13 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of installation and commissioning of switches, currency units"), CostOfInstallationAllSwitches, sep = ": "))  
  intermediate2$CostOfInstallationAllSwitches <- CostOfInstallationAllSwitches
  
  # Design cost
  DesignCost =  formula_3_1_14 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Design cost, currency units"), DesignCost, sep = ": "))  
  intermediate2$DesignCost <- DesignCost
  
  # Total cost of organizing a wired LAN segment
  TotalCAPEXOfWiredLANSegment <- 0
  TotalCAPEXOfWiredLANSegment =  formula_3_1_15 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of organizing a wired LAN segment, currency units"), TotalCAPEXOfWiredLANSegment, sep = ": "))  
  
  # Cost of LAN cable maintenance
  CostOfAllCableMaintenance =  formula_3_1_16 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of LAN cable maintenance, currency units per year"), CostOfAllCableMaintenance, sep = ": "))  
  intermediate2$CostOfAllCableMaintenance <- CostOfAllCableMaintenance
  
  # Cost of switches maintenance
  CostOfAllSwitchesMaintenance =  formula_3_1_17 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of switches maintenance, currency units per year"), CostOfAllSwitchesMaintenance, sep = ": "))  
  intermediate2$CostOfAllSwitchesMaintenance <- CostOfAllSwitchesMaintenance
  
  # Total maintenance cost of a wired LAN for the entire maintenance period
  TotalOPEXOfWiredLANSegment =  formula_3_1_18 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total maintenance cost of a wired LAN for the entire maintenance period, currency units"), TotalOPEXOfWiredLANSegment, sep = ": "))  
  
  
  result <- matrix (nrow = 4, ncol = 2)
  result [1,1] = i18n$t("Total cost of organizing a wired LAN segment, currency units")
  result [1,2] = TotalCAPEXOfWiredLANSegment
  
  result [2,1] = i18n$t("Total maintenance cost of a wired LAN for the entire maintenance period, currency units")
  result [2,2] = TotalOPEXOfWiredLANSegment
  
  result [3,1] = i18n$t("Number of classrooms, classrooms")
  result [3,2] = NumberOfClassrooms

  result [4,1] = i18n$t("Length of LAN cables from STC to classrooms")
  result [4,2] = AvCableLenFromSTCToClassroom
  
  return (result)
  
}
  
algorithm3_1 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$BuildingParameters.Width)
            req (input$BuildingParameters.Length)
            req (input$BuildingParameters.HeightPlus)
            req (input$BuildingParameters.Levels)
            req (input$GeneralVariables.STCPlaceLevel)
            req (input$GeneralVariables.CableBackUp)
            req (input$SchoolSpecific.PupilsNumber)
            req (input$GeneralVariables.ClassroomWithInternet)
            req (input$GeneralVariables.NumberOfShifts)
            req (input$GeneralVariables.PupilsInInOneClass)
            req (input$BuildingParameters.LengthOfClassroom)
            req (input$BuildingParameters.WidthOfClassroom)
            req (input$BuildingParameters.HeightMinus)
            req (input$GeneralVariables.BusyCoeff)
            req (input$LANEquipment.NumberOfPortsInSwitch)
            req (input$LANEquipment.CostOfCable)
            req (input$GeneralVariables.LaborCostNormsForCableInst)
            req (input$GeneralVariables.CostNormsForCableInst)
            req (input$LANEquipment.CostOfOutlet)
            req (input$GeneralVariables.LaborCostNormsForTOInst)
            req (input$GeneralVariables.CostNormsForTOInst)
            req (input$LANEquipment.CostOfSwitch)
            req (input$GeneralVariables.LaborCostNormsForSwitchInst)
            req (input$GeneralVariables.CostNormsForSwitchInst)
            req (input$GeneralVariables.DesignConstrCoeff)
            req (input$GeneralVariables.AnnualLaborCostOfCableOperation)
            req (input$GeneralVariables.CostOfOneCableOperation)
            req (input$GeneralVariables.AnnualLaborCostOfSwitchOperation)
            req (input$GeneralVariables.CostOfOneSwitchOperation)
            req (input$GeneralVariables.PeriodOfLANOperation)
            
            
            result <- algorithm3_1_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)     
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_3_1_1 = {#The average length of a section of one cable from the STC to the point of entry of cables into the classroom
            
            req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$BuildingParameters.Width)
            req (input$BuildingParameters.Length)
            req (input$BuildingParameters.HeightPlus)
            req (input$BuildingParameters.Levels)
            req (input$GeneralVariables.STCPlaceLevel)
            req (input$GeneralVariables.CableBackUp)
            
            
            result <- formula_3_1_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_1_2 = {#The total telecommunication outlets (TO)s number to be installed for related educational equipment, which also requires a LAN connection (for example, a video wall, teacher?s PC, interactive whiteboard, etc.)
            
            req (input$GeneralVariables.CableBackUp)
            req (input$SchoolSpecific.PupilsNumber)
            req (input$GeneralVariables.ClassroomWithInternet)
            req (input$GeneralVariables.NumberOfShifts)
            req (input$GeneralVariables.PupilsInInOneClass) #Average number of students in one class
            
            result <- formula_3_1_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_3 = {#The average length of the cable section from the classroom entry point up to the STC
            
            req (input$BuildingParameters.LengthOfClassroom)
            req (input$BuildingParameters.WidthOfClassroom)
            req (input$BuildingParameters.HeightMinus)
            req (input$BuildingParameters.Levels)
            req (input$GeneralVariables.CableBackUp)
            
            result <- formula_3_1_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_4 = {#Length of LAN cables within the classroom to the telecommunication outlets (to connect all the necessary educating places to the designed network)
            
            req (input$Intermediate.AvCableLenFromClassroomEntryToOutlet)
            req (input$Intermediate.NumberOfTOs)
            
            result <- formula_3_1_4 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_5 = {#Length of LAN cables from STC to classrooms
            
            req (input$Intermediate.AvCableLenFromSTCToClassroom)
            req (input$Intermediate.NumberOfTOs)
            
            result <- formula_3_1_5 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_6 = {#Total cable length of the wired segment LAN
            req (input$Intermediate.AllCableLenFromClassroomEntryToOutlet)
            req (input$Intermediate.AllCableLenFromSTCToClassroom)
            
            
            result <- formula_3_1_6 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_7 = {#The number of switches of the selected model (Kres_port is usually assumed by default that  0.6) is the initial load factor of ports that are considered as "busy", other ports in the design are considered as backup
            req (input$Intermediate.NumberOfTOs)
            req (input$GeneralVariables.BusyCoeff)
            req (input$LANEquipment.NumberOfPortsInSwitch)
            
            result <- formula_3_1_7 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_8 = {#Cost of purchasing cables for LAN installation with a margin of 10% of the length for laying cables
            req (input$Intermediate.NumberOfClassrooms)
            req (input$LANEquipment.CostOfCable)
            req (input$Intermediate.AllCableLenForWiredLAN) 
            
            result <- formula_3_1_8 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_9 = {#Total cost of laying cables for LAN installation
            req (input$Intermediate.CostOfAllCablesPlus)
            req (input$GeneralVariables.LaborCostNormsForCableInst)
            req (input$GeneralVariables.CostNormsForCableInst)
            req (input$Intermediate.AllCableLenForWiredLAN)
            
            result <- formula_3_1_9 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_10 = {#Cost of purchasing of TOs
            req (input$LANEquipment.CostOfOutlet)
            req (input$Intermediate.NumberOfTOs)
            
            result <- formula_3_1_10 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_11 = {#Total cost of purchasing, installation and TOs commissioning
            req (input$Intermediate.CostOfAllOutlets)
            req (input$GeneralVariables.LaborCostNormsForTOInst)
            req (input$GeneralVariables.CostNormsForTOInst)
            req (input$Intermediate.NumberOfTOs)
            
            result <- formula_3_1_11 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_12 = {#Cost of purchasing switching equipment with ports backup
            req (input$LANEquipment.CostOfSwitch)
            req (input$Intermediate.NumberOfSwitches)
            
            result <- formula_3_1_12 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_13 = {#Total cost of installation and commissioning of switches
            req (input$Intermediate.CostOfAllSwitchesPlus)
            req (input$GeneralVariables.LaborCostNormsForSwitchInst)
            req (input$GeneralVariables.CostNormsForSwitchInst)
            req (input$Intermediate.NumberOfSwitches)
            
            result <- formula_3_1_13 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_14 = {#Design cost
            req (input$Intermediate.CostOfInstallationAllSwitches)
            req (input$Intermediate.CostOfInstallationAllTOs)
            req (input$Intermediate.CostOfInstallationAllCable)
            req (input$GeneralVariables.DesignConstrCoeff)
            
            result <- formula_3_1_14 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_15 = {#Total cost of organizing a wired LAN segment
            req (input$Intermediate.CostOfInstallationAllSwitches)
            req (input$Intermediate.CostOfInstallationAllTOs)
            req (input$Intermediate.CostOfInstallationAllCable)
            req (input$Intermediate.DesignCost)
            
            result <- formula_3_1_15 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_16 = {#Cost of LAN cable maintenance
            req (input$Intermediate.AllCableLenForWiredLAN)
            req (input$Intermediate.AnnualLaborCostOfCableOperation)
            req (input$GeneralVariables.CostOfOneCableOperation)
            
            result <- formula_3_1_16 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_17 = {#Cost of switches maintenance
            req (input$Intermediate.NumberOfSwitches)
            req (input$GeneralVariables.AnnualLaborCostOfSwitchOperation)
            req (input$GeneralVariables.CostOfOneSwitchOperation)
            
            result <- formula_3_1_17 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_1_18 = {#Total maintenance cost of a wired LAN for the entire maintenance period
            req (input$Intermediate.CostOfAllCableMaintenance)
            req (input$Intermediate.CostOfAllSwitchesMaintenance)
            req (input$GeneralVariables.PeriodOfLANOperation)
            
            result <- formula_3_1_18 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          stop ("No!")
          
  )
}