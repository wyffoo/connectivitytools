#Methodologies for cost estimation and technologies selection for building local area networks (LAN) in schools

source ("algorithm3_0.R")
source ("algorithm3_1.R")
source ("algorithm3_2.R")
source ("algorithm3_3.R")

library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")


method3_impl <- function(input, intermediate = NULL)
{
  #Determination of the total cost of STC installation, commissioning and operation

  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("School Telecommunications Center CAPEX & OPEX Calculation"))                            
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "---------------------------------------------------------")                
  
  
  res0 <- algorithm3_0_impl (input)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")      
  
  #Determining the total cost of installation, commissioning and operation of a wired LAN for educational segment of IT equipment

  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Wired LAN CAPEX & OPEX Calculation"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "-----------------------------------")                
  
  res1 <- algorithm3_1_impl (input)
  
  intermediate2 <- list (
    NumberOfClassrooms = 0.0,
    AvCableLenFromSTCToClassroom = 0.0,
    AreaOfAllClassRooms = 0,0
  )
  
  intermediate2$NumberOfClassrooms <- res1[3,2] 
  intermediate2$AvCableLenFromSTCToClassroom  <- res1[4,2]
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")      

  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Wireless LAN CAPEX & OPEX Calculation"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "-------------------------------------")                
  
  #Determining the total cost of installation, commissioning and maintenance of the institution LAN wireless segment
  res2 <- algorithm3_2_impl (input, intermediate2)
  
  intermediate2$AreaOfAllClassRooms <- res2[3,2]
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")      
  
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("WiFi HotSpot CAPEX & OPEX Calculation"))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "-------------------------------------")                
  
  
  #Determining the cost of the public wireless segment (PWS) based on the HOTSPOT organization
  res3 <- algorithm3_3_impl (input, intermediate2)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")      

  result <- matrix (nrow = 8, ncol = 2)
  
  result [1,1] = i18n$t("Cost of installation and commissioning of the STC, currency units")
  result [1,2] = as.numeric(res0[1,2])
  
  result [2,1] = i18n$t("Annual cost of operation of the STC, currency units per year")
  result [2,2] = as.numeric(res0[2,2])
  
  result [3,1] = i18n$t("Cost of installation and commissioning of a wired LAN, currency units")
  result [3,2] = as.numeric(res1[1,2])
  
  result [4,1] = i18n$t("Annual cost of operation of a wired LAN, currency units per year")
  result [4,2] = as.numeric(res1[2,2])
  
  result [5,1] = i18n$t("Cost of installation and commissioning of LAN WES, currency units")
  result [5,2] = as.numeric(res2[1,2]) 
  
  result [6,1] = i18n$t("Annual cost of operation of LAN WES, currency units per year")
  result [6,2] =  as.numeric(res2[2,2])
  
  result [7,1] = i18n$t("Cost of installation and commissioning of the public wireless segment (HOTSPOT), currency units")
  result [7,2] = as.numeric(res3[1,2]) 
  
  result [8,1] = i18n$t("Annual cost of operation of the public wireless segment (HOTSPOT), currency units per year")
  result [8,2] = as.numeric(res3[2,2])  
  
      
  return (result)
}

method3_inv_impl <- function(input, intermediate = NULL)
{
  
  
  result <- 0 
  
  return (result)
}

method3 <- function(input, output)
{
  switch (input$algorithm, 
          ALL = {
  
            req (input$GeneralVariables.CostOfSTCEq)
            
            req (input$GeneralVariables.LaborCostNormsForSTCOrg)
            req (input$GeneralVariables.CostNormsSTCOrg)
            req (input$GeneralVariables.AnnualLaborCostNormsForSTCOper)
            req (input$GeneralVariables.CostNormsSTCOper)
            req (input$GeneralVariables.PeriodOfLANOperation)
            
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
            
            req (input$GeneralVariables.CoeffReserveOfEquipment)
            req (input$WiFiEquipment.RadiusOfAP)
            req (input$WiFiEquipment.AdapterReductionFactor)
            req (input$WiFiEquipment.CostOfOneAP)
            req (input$GeneralVariables.LaborCostNormsForWiFiAPInst)
            req (input$GeneralVariables.CostNormsForWiFiAPInst)
            req (input$GeneralVariables.AnnualLaborCostOfAPOperation)
            req (input$GeneralVariables.CostOfAPOperation)
            
            req (input$WiFiEquipment.RadiusOfHotSpot)
            req (input$WiFiEquipment.CostOfOneHotSpotPouter)
            req (input$GeneralVariables.LaborCostNormsForHotSpotInst)
            req (input$GeneralVariables.CostNormsForHotSpotInst)
            req (input$GeneralVariables.AnnualLaborCostOfHotSpotOperation)
            req (input$GeneralVariables.CostOfHotSpotOperation)
            
            
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            
            result <- method3_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE) 
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
            
          },
          ALGORITHM3_0 = {#Determination of the total cost of STC installation, commissioning and operation
            algorithm3_0 (input, output)
          },
          ALGORITHM3_1 = {#Determining the total cost of installation, commissioning and operation of a wired LAN for educational segment of IT equipment
            algorithm3_1 (input, output)
          },
          ALGORITHM3_2 = {#Determining the total cost of installation, commissioning and maintenance of the institution LAN wireless segment
            algorithm3_2 (input, output)
          },
          ALGORITHM3_3 = {#Determining the cost of the public wireless segment (PWS) based on the HOTSPOT organization
            algorithm3_3 (input, output)
          },
          stop ("No!")
  )
}
  

method3_inv <- function(input, output)
{
  req (input)
  req (input$algorithm)
  req (output)
  
}