library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#NPV matrix calculations

source ("algorithm2_1.R")
source ("algorithm2_2.R")
source ("algorithm2_3.R")
source ("algorithm2_4.R")
source ("algorithm2_5.R")
source ("algorithm2_6.R")
source ("algorithm2_7.R")


#NPV matrix calculation (fiber vs microwave)
formula_4_2_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  objects_l <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)
  
  req (objects_l)
  
  numberofobj_l <- nrow(objects_l)
  
  
  objects <- vroom::vroom(input$Files.DistanceMatrix, altrep = FALSE)
  
  req (objects)
  
  numberofobj1 <- nrow(objects)
  numberofobj2 <- ncol(objects)
  
  if (numberofobj1 != numberofobj2)
  {
    print ("ERROR")
    return (0)
  }
  
  if (numberofobj1 != numberofobj_l)
  {
    print ("ERROR")
    return (0)
  }
  
  input2 <- reactiveValuesToList (input)
  
  result <- matrix (nrow = numberofobj1, ncol = numberofobj1)
  
  for (i in 1: (numberofobj1-1))
  {
    for (j in (i+1):numberofobj1)
    {
      
      distancekm <- as.numeric (objects [i,j])
      #ALGORITHM2_3
      
      
      
      input2$InitialDataRadio.DistanceRoute <- distancekm
      
      TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)
      
      input2$InitialDataFOCL.Length <- distancekm * (1 + TopCoeff) 
      
      
      #Algorithm for overall FOCL construction cost evaluation
      
      result2 =  algorithm2_1_impl (input2)
      
      #Overall length of the FOCL construction site
      FOCLLenghtTotal = as.numeric (result2 [2,2])
      
      FOCLSectionLengthCD =   as.numeric (result2 [3,2])
      
      #Overall cost of FOCL installation  
      TotalCAPEXFOCL =  as.numeric (result2 [1,2])
      
      #Сost of equipment and materials for the construction of fiber optic lines
      CostOfEqAndMatFOCL =  as.numeric (result2 [4,2])
      
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Overall cost of FOCL installation, currency units"), TotalCAPEXFOCL, sep = ": "))  
      
      
      #Algorithm for overall FOCL maintenance cost evaluation
      
      intermediate2 <- list (FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0)
      intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal 
      intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
      
      result3 =  algorithm2_2_impl (input2, intermediate2)
      
      #Total cost for FOCL maintenance for the entire period of operation            
      TotalOPEXFOCL =  as.numeric (result3 [1,2])
      
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost for FOCL maintenance for the entire period of operation, currency units/year"), TotalOPEXFOCL, sep = ": "))              
      
      
      #Algorithm for total RTS construction cost evaluation between object and SN in locality
      result4 =  algorithm2_3_impl (input2, intermediate)
      
      TotalCAPEXRTS <-   as.numeric (result4 [1,2])
      
      NumberOfRepeaters <-   as.numeric (result4 [2,2])
      
      #Total cost of equipment and materials for the construction of the RTS
      CostOfEqAndMatRTS <-   as.numeric (result4 [3,2])
      
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("RTS construction total cost, currency units"), TotalCAPEXRTS, sep = ": "))  
      
      
      #Algorithm for total RTS maintenance cost evaluation between object and SN in locality
      
      intermediate3 <- list (NumberOfRepeaters = 0)
      intermediate3$NumberOfRepeaters <- NumberOfRepeaters
      result5 =  algorithm2_4_impl (input2, intermediate3)
      
      TotalOPEXRTS = as.numeric (result5 [1,2])
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of RTS maintenance, currency units/year"), TotalOPEXRTS, sep = ": "))            
      
      #NPV - FOCL
      intermediate5 <- list (NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0)
      TotalInvest <- TotalCAPEXFOCL
      CostOfOperation  <- TotalOPEXFOCL
      CostOfEquipmentAndMaterials  <- CostOfEqAndMatFOCL
      intermediate5$TotalInvest <- TotalInvest
      intermediate5$CostOfOperation <- CostOfOperation
      intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
      
      npv_focl <- algorithm2_7_impl (input2, intermediate5)
      
      npv_f = as.numeric(npv_focl[1,2])
      
      
      #NPV - Microwave 
      intermediate5 <- list (NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0)
      
      TotalInvest <- TotalCAPEXRTS
      CostOfOperation  <- TotalOPEXRTS
      CostOfEquipmentAndMaterials  <- CostOfEqAndMatRTS
      
      intermediate5$TotalInvest <- TotalInvest
      intermediate5$CostOfOperation <- CostOfOperation
      intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
      
      npv_microwave <- algorithm2_7_impl (input2, intermediate5)
      
      npv_m <- as.numeric(npv_microwave[1,2])
      
      npv <- 0
      mt <- "Unkrnow:"
      
      if (npv_f > npv_m)
      {
        npv <- npv_f
        mt <- "Fiber"
      }
      else
      {
        npv <- npv_m
        mt <- "Microwave"
      }
      
      
      finr <- paste (mt, round (as.numeric(npv), digits = 2) , sep = " ")
      
      result [i,j] <- finr
      result [j,i] <- finr
      
      
    }
    result [i,i] <- 0
    
  }
  
  result [numberofobj1,numberofobj1] <- 0
  
  
  return (result)
}


#NPV matrix filtering (by possible satellite connections)
formula_4_2_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  objects_l <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)
  
  req (objects_l)
  
  numberofobj_l <- nrow(objects_l)
  
  
  objects <- vroom::vroom(input$Files.NPVMatrix, altrep = FALSE)
  
  req (objects)
  
  numberofobj1 <- nrow(objects)
  numberofobj2 <- ncol(objects)
  
  if (numberofobj1 != numberofobj2)
  {
    print ("ERROR")
    return (0)
  }
  
  if (numberofobj1 != numberofobj_l)
  {
    print ("ERROR")
    return (0)
  }
  
  if (numberofobj1 < 1)
  {
    print ("ERROR")
    return (0)
    
  }
  
  input2 <- reactiveValuesToList (input)
  
  input2$PVOptionSet.ProfitTax = 17
  input2$AccessTechnologyOptionSet.AvrLifeTimeOfEqAndMat = 10
  
  
  #Detect nodes with existing fiber connection of nearest to it 
  nodeindexes <- matrix (nrow = numberofobj1, ncol = 1)
  
  
  numberofnodes <- 0
  defaultnodeindex <- 1
  minval <- objects_l [1,5]
  
  for (i in 1: numberofobj1)
  {
    dtf <- objects_l [i,5]
    
    if (dtf == 0)
    {
      numberofnodes <- numberofnodes + 1
      nodeindexes [numberofnodes, 1] <- i
      #nodeindexes <- 
      #append(nodeindexes, c(i))        
    }
    else
    {
      if (dtf < minval)
      {
        minval <- dtf 
        defaultnodeindex <- i
      }
    }
  }
  
  
  if (numberofnodes == 0)
  {
    numberofnodes <- 1
    nodeindexes [numberofnodes, 1] <- defaultnodeindex
  }
  
  
  numberofsats <- 0
  satindexes <- matrix (nrow = numberofobj1, ncol = 2)
  for (i in 1: numberofobj1)
  {
    isnode <- F
    for (j in 1: numberofnodes)
    {
      if (i == nodeindexes[j,1])
      {
        isnode <- T
        break
      }
    }
    
    if (isnode)
      next
    
    rb <- objects_l [i,4]
    intermediate4 <- list (NumberVSATsets = 0, RequiredCapacity = 0.0)
    intermediate4$RequiredCapacity  <- rb
    
    result6 =  algorithm2_5_impl (input2, intermediate4)
    
    # Required number of VSAT sets
    
    NumberVSATsets = as.numeric (result6 [2,2])
    
    TotalCAPEXSetellite =  as.numeric (result6 [1,2])
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of establishing a satellite communication channel, currency units"), TotalCAPEXSetellite, sep = ": "))  
    
    #Total cost of VSAT equipment and installation materials
    CostOfVSATEqAndMat =  as.numeric (result6 [3,2])
    
    
    #Algorithm for determining the total cost of the maintenance of the satellite communication channel 
    
    
    intermediate4$NumberVSATsets <- NumberVSATsets
    
    
    
    result7 =  algorithm2_6_impl (input2, intermediate4)
    
    
    TotalOPEXSatellite =  as.numeric (result7 [1,2])
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Annual cost of maintenance of the satellite channel, currency units/year"), TotalOPEXSatellite, sep = ": "))              
    
    
    #NPV - Satellite 
    intermediate5 <- list (NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0)
    TotalInvest <- TotalCAPEXSetellite
    CostOfOperation  <- TotalOPEXSatellite
    CostOfEquipmentAndMaterials  <- CostOfVSATEqAndMat
    
    intermediate5$NetIncome <- 0
    intermediate5$TotalInvest <- TotalInvest
    intermediate5$CostOfOperation <- CostOfOperation
    intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
    
    npv_satellite <- algorithm2_7_impl (input2, intermediate5)
    
    npv_s <- as.numeric(npv_satellite[1,2])
    
    
    change_to_sat <- F
    
    for (j in 1:numberofobj1)
    {
      if (i == j)
        next
      
      npv <- as.numeric (objects [i,j])
      
      if (npv_s > npv)      
        change_to_sat <- T
      else
      {
        change_to_sat <- F
        break
      }
    }
    
    if (change_to_sat)
    {
      numberofsats <- numberofsats + 1
      satindexes[numberofsats, 1] <- i
      satindexes[numberofsats, 2] <- round (as.numeric(npv_s), digits = 2)
    }
    
  }
  
  
  result <- matrix (nrow = numberofobj1, ncol = numberofobj1)
  #result = unlist (objects)
  
  
  for (i in 1:numberofobj1)
  {
    for (j in 1:numberofobj1)
    {
      val <- objects [i,j]
      result [i,j] <- as.numeric(val)
    }
  }
  
  
  basenode <- nodeindexes [1,1]
  
  for (i in 1: numberofsats)
  {
    satindex <- satindexes [i,1]
    
    for (j in 1:numberofobj1)
    {
      result [satindex,j] <- NA
      result [j,satindex] <- NA
      
      if (satindex == j)
        result [j,satindex] <- 0
    }
    
    result [satindex,basenode] <- satindexes [i,2]
    result [basenode, satindex] <- satindexes [i,2]
    
    
  }
  
  
  return (result)
}

algorithm4_2_impl <- function(input, intermediate = NULL)
{
  
  result <- 0
  
  return (result)
  
  
}

algorithm4_2 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            
            result <- algorithm4_2_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)     
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_4_2_1 = {#NPV matrix calculation (fiber vs microwave)
            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Files.ListOfObjects)
            req (input$Files.DistanceMatrix)
            
            req (input$InitialDataFOCL.TopographyCoeff)
            
            req (input$InitialDataFOCL.LaborCostNormGeo)
            req (input$InitialDataFOCL.CostNormGeo)
            req (input$InitialDataFOCL.CostNormFOCLinst)
            req (input$InitialDataFOCL.LaborCostNormHDD)
            req (input$InitialDataFOCL.CostNormHDD)
            req (input$Intermediate.FOCLSectionLengthCD)
            req (input$InitialDataFOCL.LaborCostNormCDC)
            req (input$InitialDataFOCL.CostNormCDC)
            req (input$InitialDataFOCL.CostNormMaterialsCD)
            req (input$Intermediate.FOCLSectionLengthCLM)
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
            req (input$InitialDataFOCL.CostMatCableGlands)
            req (input$InitialDataFOCL.LaborCostNormODF)
            req (input$InitialDataFOCL.CostNormODFinst)
            req (input$InitialDataFOCL.AnnualLaborNormFOCLm)
            req (input$InitialDataFOCL.CostNormFOCLm)
            req (input$InitialDataFOCL.AnnualLaborNormCDm)
            req (input$InitialDataFOCL.CostNormCDm)
            req (input$InitialDataFOCL.NumberBEF)
            req (input$InitialDataFOCL.AnnualLaborNormBEFm)
            req (input$InitialDataFOCL.CostNormBEFm)
            req (input$InitialDataFOCL.NumberODF)
            req (input$InitialDataFOCL.AnnualLaborNormODFm)
            req (input$InitialDataFOCL.CostNormODFm)
            
            req (input$InitialDataFOCL.CostTramsEquipment)
            
            
            req (input$InitialDataRadio.ReliefCoef)
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
            req (input$InitialDataRadio.CostOfSpectrumLicence)
            
            #ALGORITHM2_4
            req (input$InitialDataRadio.AnnualLaborPylon)
            req (input$InitialDataRadio.CostLaborPylonm)
            req (input$InitialDataRadio.NumberOfTerminals)
            req (input$InitialDataRadio.AnnualLaborAFD)
            req (input$InitialDataRadio.CostLaborAFDm)
            req (input$InitialDataRadio.AnnualLaborRTS)
            req (input$InitialDataRadio.CostLaborRTSm)
            
            #ALGORITHM2_7
            #req (input$PVOptionSet.VATax)
            req (input$AccessTechnologyOptionSet.AvrLifeTimeOfEqAndMat)
            req (input$PVOptionSet.ProfitTax)
            req (input$PVOptionSet.DiscountRate)
            req (input$PVOptionSet.PaybackPeriod)
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result <- formula_4_2_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_4_2_2 = {#NPV matrix filtering (by possible satellite connections)
            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            
            req (input$Files.ListOfObjects)
            req (input$Files.NPVMatrix)
            req (input$InitialDataSatelite.ChannelCapacity)
            req (input$InitialDataSatelite.CostOfOneVSAT)
            req (input$InitialDataSatelite.CostOfOneVSATmat)
            req (input$InitialDataSatelite.CostOfInstallVSAT)
            req (input$InitialDataSatelite.LaborOfInstallVSAT)
            req (input$InitialDataSatelite.AnnualLaborOfServiceVSAT)
            req (input$InitialDataSatelite.CostOfServiceVSAT)
            req (input$InitialDataSatelite.AnnualRentBand)
            req (input$PVOptionSet.DiscountRate)
            req (input$PVOptionSet.PaybackPeriod)
            
            
            #req (input$Files.ListOfObjects)
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            
            result <- formula_4_2_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          
          stop ("No!")
          
  )
}