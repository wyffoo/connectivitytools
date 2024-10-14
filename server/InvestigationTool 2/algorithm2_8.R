library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Determination of the total cost of solar panels and school power station installation and commissioning


# Determining the total power to supply power to active network equipment in the school (Watt)
formula_2_8_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  RequiredCapacity <- 0 
  RequiredCapacity <- input$SchoolSpecificData.RequiredBandwidth
  
  if (!is.null(intermediate))
  {
    RequiredCapacity <- as.numeric (intermediate$RequiredCapacity)
  }
  
  
  
  result =  RequiredCapacity * input$PowerInSchool.AverageOfPowerUsing
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Daily need of school for electrical energy to power network equipment (Watt*hour)
formula_2_8_2 <- function (input, intermediate = NULL)
{
  req (input)

  TotalRequiredPower <- 0 
  TotalRequiredPower <- input$Intermediate.TotalRequiredPower
  
  if (!is.null(intermediate))
  {
    TotalRequiredPower <- as.numeric (intermediate$TotalRequiredPower)
  }
  
  result =  (TotalRequiredPower * input$PowerInSchool.TimeForSupplingOfEquipment)/input$PowerInSchool.BatteryKPD
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Required number of solar panels (units)
formula_2_8_3 <- function (input, intermediate = NULL)
{
  req (input)

  DailyNeedInEnergy <- 0 
  DailyNeedInEnergy <- input$Intermediate.DailyNeedInEnergy
  
  if (!is.null(intermediate))
  {
    DailyNeedInEnergy <- as.numeric (intermediate$DailyNeedInEnergy)
  }

  DailyNeedInEnergy <- DailyNeedInEnergy/1000
    
  result =  round (DailyNeedInEnergy/(input$PowerInSchool.SolarPanelKPD*input$PowerInSchool.InsolationLevel*input$PowerInSchool.SolarPanelArea*cos (input$PowerInSchool.SolarPanelAngle)), digits = 0) + 1
  
  
  return (result)
}

# Required power of school power station (Watt)
formula_2_8_4 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfSolarPanels <- 0 
  NumberOfSolarPanels <- input$Intermediate.NumberOfSolarPanels
  
  if (!is.null(intermediate))
  {
    NumberOfSolarPanels <- as.numeric (intermediate$NumberOfSolarPanels)
  }
  

  result =  NumberOfSolarPanels*input$PowerInSchool.NominalPowerOfOnePanel
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Cost of equipment and materials  to supply power to active network equipment in the school (currency units)

formula_2_8_5 <- function (input, intermediate = NULL)
{
  req (input)


  PowerOfSchoolStation <- 0 
  PowerOfSchoolStation <- input$Intermediate.PowerOfSchoolStation
  NumberOfSolarPanels <- 0 
  NumberOfSolarPanels <- input$Intermediate.NumberOfSolarPanels
  
  if (!is.null(intermediate))
  {
    PowerOfSchoolStation <- as.numeric (intermediate$PowerOfSchoolStation)
    NumberOfSolarPanels <- as.numeric (intermediate$NumberOfSolarPanels)
  }
  
  invertorNorm <- round (PowerOfSchoolStation/1000, digits = 0) + 1
      
  result =  invertorNorm*input$PowerInSchool.CostOfInverterPerKW +
    NumberOfSolarPanels*input$PowerInSchool.CostOfSolarPanel +
    NumberOfSolarPanels*(input$PowerInSchool.AvCableLenPerPanel*input$PowerInSchool.CostOfElectroCable + input$PowerInSchool.CostOfSolarPanelMaterials)
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

# Total cost of labor for solar panels and school power station installation and commissioning
formula_2_8_6 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfSolarPanels <- 0 
  NumberOfSolarPanels <- input$Intermediate.NumberOfSolarPanels
  
  if (!is.null(intermediate))
  {
    NumberOfSolarPanels <- as.numeric (intermediate$NumberOfSolarPanels)
  }
  
  result =  input$PowerInSchool.CostNormsOfElectro*(input$PowerInSchool.LaborCostNormForInverterInst + 
                                                      NumberOfSolarPanels*input$PowerInSchool.LaborCostNormForPanelInst)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

algorithm2_8_impl <- function(input, intermediate = NULL)
{
  
  # Determining the total power to supply power to active network equipment in the school (Watt)
  
  TotalRequiredPower =  formula_2_8_1 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Determining the total power to supply power to active network equipment in the school, Watt"), TotalRequiredPower, sep = ": "))  
  
  
  # Daily need of school for electrical energy to power network equipment (Watt*hour)
  
  intermediate2 <- list (TotalRequiredPower = 0.0, DailyNeedInEnergy = 0.0, NumberOfSolarPanels = 0.0, PowerOfSchoolStation = 0.0)
  intermediate2$TotalRequiredPower <- TotalRequiredPower
  
  DailyNeedInEnergy =  formula_2_8_2 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Daily need of school for electrical energy to power network equipment, Watt*hour"), DailyNeedInEnergy, sep = ": "))    

  
  # Required number of solar panels (units)
  
  intermediate2$DailyNeedInEnergy <- DailyNeedInEnergy
  NumberOfSolarPanels =  formula_2_8_3 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Required number of solar panels, units"), NumberOfSolarPanels, sep = ": "))    
  
  # Required power of school power station (Watt)
  intermediate2$NumberOfSolarPanels <- NumberOfSolarPanels
  PowerOfSchoolStation =  formula_2_8_4 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Required power of school power station (Watt)"), PowerOfSchoolStation, sep = ": "))    
  
  # Cost of equipment and materials  to supply power to active network equipment in the school (currency units)
  intermediate2$PowerOfSchoolStation <- PowerOfSchoolStation
  CostOfEquipmentAndMaterials =  formula_2_8_5 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Cost of equipment and materials  to supply power to active network equipment in the school, currency units"), CostOfEquipmentAndMaterials, sep = ": "))    
  
  # Total cost of labor for solar panels and school power station installation and commissioning
  CostOfLabor =  formula_2_8_6 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of labor for solar panels and school power station installation and commissioning, currency units"), CostOfLabor, sep = ": "))    
  
  result <- matrix (nrow = 2, ncol = 2)
  result [1,1] = i18n$t("Total cost of solar panels and school power station installation and commissioning, currency units")
  result [1,2] = CostOfEquipmentAndMaterials + CostOfLabor
  
  result [2,1] = i18n$t("Cost of equipment and materials  to supply power to active network equipment in the school, currency units")
  result [2,2] = CostOfEquipmentAndMaterials
  
  
  return (result)
}


algorithm2_8 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {

            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            req (input$PowerInSchool.AverageOfPowerUsing)
            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$PowerInSchool.TimeForSupplingOfEquipment)
            req (input$PowerInSchool.InsolationLevel)
            req (input$PowerInSchool.SolarPanelAngle)
            req (input$PowerInSchool.SolarPanelArea)
            req (input$PowerInSchool.NominalPowerOfOnePanel)
            req (input$PowerInSchool.CostOfInverterPerKW)
            req (input$PowerInSchool.CostOfSolarPanel)
            req (input$PowerInSchool.AvCableLenPerPanel)
            req (input$PowerInSchool.CostOfElectroCable)
            req (input$PowerInSchool.CostOfSolarPanelMaterials)
            req (input$PowerInSchool.LaborCostNormForInverterInst)
            req (input$PowerInSchool.LaborCostNormForPanelInst)
            req (input$PowerInSchool.CostNormsOfElectro)
            req (input$PowerInSchool.BatteryKPD)
            req (input$PowerInSchool.SolarPanelKPD)
            
            result <- algorithm2_8_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)     
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_2_8_1 = {#Determining the total power to supply power to active network equipment in the school (Watt)
            
            req (input$PowerInSchool.AverageOfPowerUsing)
            req (input$SchoolSpecificData.RequiredBandwidth)
            
            
            result <- formula_2_8_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_8_2 = {#Daily need of school for electrical energy to power network equipment (Watt*hour)
            
            req (input$Intermediate.TotalRequiredPower)
            req (input$PowerInSchool.TimeForSupplingOfEquipment)
            req (input$PowerInSchool.BatteryKPD)
            
            result <- formula_2_8_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_8_3 = {#Required number of solar panels (units)
            req (input$Intermediate.DailyNeedInEnergy)
            req (input$PowerInSchool.InsolationLevel)
            req (input$PowerInSchool.SolarPanelAngle)
            req (input$PowerInSchool.SolarPanelArea)
            req (input$PowerInSchool.SolarPanelKPD)
            
            result <- formula_2_8_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_8_4 = {#Required power of school power station (Watt)
            req (input$Intermediate.NumberOfSolarPanels)
            req (input$PowerInSchool.NominalPowerOfOnePanel)
            
            result <- formula_2_8_4 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_8_5 = {#Cost of equipment and materials  to supply power to active network equipment in the school (currency units)
            req (input$Intermediate.PowerOfSchoolStation)
            req (input$PowerInSchool.CostOfInverterPerKW)
            req (input$Intermediate.NumberOfSolarPanels)
            req (input$PowerInSchool.CostOfSolarPanel)
            req (input$PowerInSchool.AvCableLenPerPanel)
            req (input$PowerInSchool.CostOfElectroCable)
            req (input$PowerInSchool.CostOfSolarPanelMaterials)
            
            result <- formula_2_8_5 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_8_6 = {#Total cost of labor for solar panels and school power station installation and commissioning
            req (input$Intermediate.NumberOfSolarPanels)
            req (input$PowerInSchool.LaborCostNormForInverterInst)
            req (input$PowerInSchool.LaborCostNormForPanelInst)
            req (input$PowerInSchool.CostNormsOfElectro)
            
            result <- formula_2_8_6 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          
          stop ("No!")
          
  )
}