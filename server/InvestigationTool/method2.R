source ("algorithm2_0.R")
source ("algorithm2_1.R")
source ("algorithm2_2.R")
source ("algorithm2_3.R")
source ("algorithm2_4.R")
source ("algorithm2_5.R")
source ("algorithm2_6.R")
source ("algorithm2_7.R")
source ("algorithm2_8.R")
source ("algorithm2_9.R")
source ("algorithm2_10.R")
source ("algorithm2_11.R")
source ("algorithm2_12.R")
#source ("algorithm2_13.R")

library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

method2_impl <- function(input, intermediate = NULL)
{


  bUseFocl <- T
  bUseRts <- T
  bUseSatellite <- T
  bUseCellular <- T

  if (!is.null(input$InitialData.UseFOCL))
      if  (input$InitialData.UseFOCL == 0)
          bUseFocl <- F

  if (!is.null(input$InitialData.UseRTS))
    if (input$InitialData.UseRTS == 0)
      bUseRts <- F

  if (!is.null(input$InitialData.UseSatellite))
    if (input$InitialData.UseSatellite == 0)
      bUseSatellite <- F

  if (!is.null(input$InitialData.UseCellular))
    if (input$InitialData.UseCellular == 0)
      bUseCellular <- F


  #Algorithm for necessary network channel capacity calculation

  EnergyCAPEX <- 0
  EnergyMaterials <- 0
  EnergyOPEX <- 0

  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Required Bandwidth"))

  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "------------------")

#  result <- algorithm2_0_impl (input)
  RequiredCapacity =  as.numeric (input$SchoolSpecificData.RequiredBandwidth)

  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Necessary (sufficient) network channel capacity, Mbit/s"), RequiredCapacity, sep = ": "))

  if (exists ("bWriteLog", input))
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")


  if (input$SchoolSpecificData.IsEnergyInSchool == 0)  #Availability of electricity = No
  {
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Independent power supply"))
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "------------------------")

    intermediate_energy <- list (RequiredCapacity = 0.0)
    intermediate_energy$RequiredCapacity <- RequiredCapacity

    energy_res_cap <- algorithm2_8_impl (input, intermediate_energy)

    EnergyCAPEX <- as.numeric(energy_res_cap[1,2])
    EnergyMaterials <- as.numeric(energy_res_cap[2,2])

    energy_res_opex <- algorithm2_9_impl (input)
    EnergyOPEX <- as.numeric(energy_res_opex[1,2])

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")
  }

  result <- matrix (nrow = 26, ncol = 2)
  intermediate51 <- list (RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
  intermediate5 <- list (NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)

  if (bUseFocl)
  {
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("FOCL CAPEX Caclulation"))

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "----------------------")

    #Algorithm for overall FOCL construction cost evaluation

    result2 =  algorithm2_1_impl (input)

    #Overall length of the FOCL construction site
    FOCLLenghtTotal = as.numeric (result2 [2,2])

    FOCLSectionLengthCD =   as.numeric (result2 [3,2])

    #Overall cost of FOCL installation
    TotalCAPEXFOCL =  as.numeric (result2 [1,2])

    #Сost of equipment and materials for the construction of fiber optic lines
    CostOfEqAndMatFOCL =  as.numeric (result2 [4,2]) + EnergyMaterials

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Overall cost of FOCL installation, currency units"), TotalCAPEXFOCL, sep = ": "))

    TotalCAPEXFOCL =  TotalCAPEXFOCL + EnergyCAPEX


    result [1,1] = i18n$t("Overall cost of FOCL installation, currency units")
    result [1,2] = round (TotalCAPEXFOCL, digits = 2)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("FOCL OPEX Caclulation"))
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "---------------------")


    #Algorithm for overall FOCL maintenance cost evaluation

    intermediate2 <- list (FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
    intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
    intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
    intermediate2$RequiredCapacity <- RequiredCapacity

    result3 =  algorithm2_2_impl (input, intermediate2)

    #Total cost for FOCL maintenance for the entire period of operation
    TotalOPEXFOCL =  as.numeric (result3 [1,2])

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost for FOCL maintenance for the entire period of operation, currency units/year"), TotalOPEXFOCL, sep = ": "))

    TotalOPEXFOCL =  TotalOPEXFOCL + EnergyOPEX

    result [2,1] = "Annual cost for FOCL maintenance, currency units/year"
    result [2,2] = round (TotalOPEXFOCL, digits = 2)


    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")



    intermediate51$RequiredCapacity  <- RequiredCapacity
    intermediate51$FOCLSectionLengthCD <- FOCLSectionLengthCD
    intermediate51$Technology <- 0

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("FOCL Potential Income Caclulation"))
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "---------------------------------")

    result81 =  algorithm2_12_impl (input, intermediate51)
    NetIncome <- as.numeric (result81 [1,2])
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (result81 [1,1], NetIncome, sep = ": "))




    #NPV - FOCL
    TotalInvest <- TotalCAPEXFOCL
    CostOfOperation  <- TotalOPEXFOCL
    CostOfEquipmentAndMaterials  <- CostOfEqAndMatFOCL
    intermediate5$NetIncome <- NetIncome
    intermediate5$TotalInvest <- TotalInvest
    intermediate5$CostOfOperation <- CostOfOperation
    intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
    intermediate5$PaybackPeriod <- input$PVOptionSet.PaybackPeriod

    result [25,1] = i18n$t("Potential rental income for additional fibers, cable duct and bandwidth (extra capacity), currency units/year")
    result [25,2] = round (NetIncome, digits = 2)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("FOCL Net Present Value Caclulation"))
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "-----------------------------------")

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))

    npv_focl <- algorithm2_7_impl (input, intermediate5)

    result [9,1] = i18n$t("FOCL - Net Present Value, currency units")
    result [9,2] = round (as.numeric(npv_focl[1,2]), digits = 2)


    intermediate5$PaybackPeriod <- 10
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_focl10 <- algorithm2_7_impl (input, intermediate5)

    result [13,1] = i18n$t("FOCL - Net Present Value (10 years), currency units")
    result [13,2] = round (as.numeric(npv_focl10[1,2]), digits = 2)

    intermediate5$PaybackPeriod <- 15
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_focl15 <- algorithm2_7_impl (input, intermediate5)

    result [14,1] = i18n$t("FOCL - Net Present Value (15 years), currency units")
    result [14,2] = round (as.numeric(npv_focl15[1,2]), digits = 2)

    intermediate5$PaybackPeriod <- 20
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_focl20 <- algorithm2_7_impl (input, intermediate5)

    result [15,1] = i18n$t("FOCL - Net Present Value (20 years), currency units")
    result [15,2] = round (as.numeric(npv_focl20[1,2]), digits = 2)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")


  }

  if (bUseRts)
  {

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Microwave (Radio Transmission System - RTS) CAPEX Caclulation"))
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "-------------------------------------------------------------")



    #Algorithm for total RTS construction cost evaluation between object and SN in locality
    result4 =  algorithm2_3_impl (input, intermediate)

    TotalCAPEXRTS <-   as.numeric (result4 [1,2])

    NumberOfRepeaters <-   as.numeric (result4 [2,2])

    #Total cost of equipment and materials for the construction of the RTS
    CostOfEqAndMatRTS <-   as.numeric (result4 [3,2]) + EnergyMaterials

    WidthFrequencyChannel <- as.numeric (result4 [4,2])
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("RTS construction total cost, currency units"), TotalCAPEXRTS, sep = ": "))

    TotalCAPEXRTS <-   TotalCAPEXRTS + EnergyCAPEX

    result [3,1] = i18n$t("RTS construction total cost, currency units")
    result [3,2] = round (TotalCAPEXRTS, digits = 2)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Microwave (RTS) OPEX Caclulation"))
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "--------------------------------")


    #Algorithm for total RTS maintenance cost evaluation between object and SN in locality

    intermediate3 <- list (NumberOfRepeaters = 0, RequiredCapacity = 0.0, NumberOfTerminals = 0.0, WidthFrequencyChannel = 0.0)
    intermediate3$NumberOfRepeaters <- NumberOfRepeaters
    intermediate3$RequiredCapacity <- RequiredCapacity
    intermediate3$NumberOfTerminals <- 2
    intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel

    if (TotalCAPEXRTS == 0)
    {
      intermediate3$NumberOfTerminals <- 0
    }

    result5 =  algorithm2_4_impl (input, intermediate3)

    TotalOPEXRTS = as.numeric (result5 [1,2])
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of RTS maintenance, currency units/year"), TotalOPEXRTS, sep = ": "))

    TotalOPEXRTS = TotalOPEXRTS + EnergyOPEX

    result [4,1] = i18n$t("Annual cost of RTS maintenance, currency units/year")
    result [4,2] = round (TotalOPEXRTS, digits = 2)

    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")

    #NPV - Microwave

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Microwave (RTS) Potential Income Caclulation"))
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "--------------------------------------------")

    intermediate51$Technology <- 1

    result81 =  algorithm2_12_impl (input, intermediate51)
    NetIncome <- as.numeric (result81 [1,2])
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (result81 [1,1], NetIncome, sep = ": "))

    result [26,1] = i18n$t("Potential rental income for extra bandwidth (extra capacity), currency units/year")
    result [26,2] = round (NetIncome, digits = 2)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")


    TotalInvest <- TotalCAPEXRTS
    CostOfOperation  <- TotalOPEXRTS
    CostOfEquipmentAndMaterials  <- CostOfEqAndMatRTS
    intermediate5$NetIncome <- NetIncome
    intermediate5$TotalInvest <- TotalInvest
    intermediate5$CostOfOperation <- CostOfOperation
    intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
    intermediate5$PaybackPeriod <- input$PVOptionSet.PaybackPeriod

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Microwave (RTS) Net Present Value Caclulation"))
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "---------------------------------------------")

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_microwave <- algorithm2_7_impl (input, intermediate5)

    result [10,1] = i18n$t("RTS - Net Present Value, currency units")
    result [10,2] = round (as.numeric(npv_microwave[1,2]), digits = 2)


    intermediate5$PaybackPeriod <- 10
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_microwave10 <- algorithm2_7_impl (input, intermediate5)

    result [16,1] = i18n$t("RTS - Net Present Value (10 years), currency units")
    result [16,2] = round (as.numeric(npv_microwave10[1,2]), digits = 2)

    intermediate5$PaybackPeriod <- 15
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_microwave15 <- algorithm2_7_impl (input, intermediate5)

    result [17,1] = i18n$t("RTS - Net Present Value (15 years), currency units")
    result [17,2] = round (as.numeric(npv_microwave15[1,2]), digits = 2)

    intermediate5$PaybackPeriod <- 20
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_microwave20 <- algorithm2_7_impl (input, intermediate5)

    result [18,1] = i18n$t("RTS - Net Present Value (20 years), currency units")
    result [18,2] = round (as.numeric(npv_microwave20[1,2]), digits = 2)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")

  }

  if (bUseSatellite)
  {
    if (exists ("bWriteLog", input))
    {
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Satellite CAPEX Caclulation"))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "---------------------------")
    }



    #Algorithm for determining the total cost of the installation and configuration of the satellite communication channel

    intermediate4 <- list (NumberVSATsets = 0, RequiredCapacity = 0.0)
    intermediate4$RequiredCapacity  <- RequiredCapacity

    result6 =  algorithm2_5_impl (input, intermediate4)

    # Required number of VSAT sets

    NumberVSATsets = as.numeric (result6 [2,2])

    TotalCAPEXSetellite =  as.numeric (result6 [1,2])
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of establishing a satellite communication channel, currency units"), TotalCAPEXSetellite, sep = ": "))

    TotalCAPEXSetellite =  TotalCAPEXSetellite + EnergyCAPEX

    #Total cost of VSAT equipment and installation materials
    CostOfVSATEqAndMat =  as.numeric (result6 [3,2]) + EnergyMaterials

    result [5,1] = i18n$t("Total cost of establishing a satellite communication channel, currency units")
    result [5,2] = round (TotalCAPEXSetellite, digits = 2)

    if (exists ("bWriteLog", input))
    {
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")


      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Satellite OPEX Caclulation"))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "--------------------------")

    }

    #Algorithm for determining the total cost of the maintenance of the satellite communication channel


    intermediate4$NumberVSATsets <- NumberVSATsets



    result7 =  algorithm2_6_impl (input, intermediate4)


    TotalOPEXSatellite =  as.numeric (result7 [1,2])
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Annual cost of maintenance of the satellite channel, currency units/year"), TotalOPEXSatellite, sep = ": "))

    TotalOPEXSatellite =  TotalOPEXSatellite + EnergyOPEX

    result [6,1] = i18n$t("Annual cost of maintenance of the satellite channel, currency units/year")
    result [6,2] = round (TotalOPEXSatellite, digits = 2)

    if (exists ("bWriteLog", input))
    {
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")

      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Satellite Net Present Value Caclulation"))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "---------------------------------------")

    }

    #NPV - Satellite
    TotalInvest <- TotalCAPEXSetellite
    CostOfOperation  <- TotalOPEXSatellite
    CostOfEquipmentAndMaterials  <- CostOfVSATEqAndMat

    intermediate5$NetIncome <- 0
    intermediate5$TotalInvest <- TotalInvest
    intermediate5$CostOfOperation <- CostOfOperation
    intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
    intermediate5$PaybackPeriod <- input$PVOptionSet.PaybackPeriod
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_satellite <- algorithm2_7_impl (input, intermediate5)
    result [11,1] = i18n$t("Satellite - Net Present Value, currency units")
    result [11,2] = round (as.numeric(npv_satellite[1,2]), digits = 2)

    intermediate5$PaybackPeriod <- 10
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_satellite10 <- algorithm2_7_impl (input, intermediate5)

    result [19,1] = i18n$t("Satellite - Net Present Value (10 years), currency units")
    result [19,2] = round (as.numeric(npv_satellite10[1,2]), digits = 2)

    intermediate5$PaybackPeriod <- 15
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_satellite15 <- algorithm2_7_impl (input, intermediate5)

    result [20,1] = i18n$t("Satellite - Net Present Value (15 years), currency units")
    result [20,2] = round (as.numeric(npv_satellite15[1,2]), digits = 2)

    intermediate5$PaybackPeriod <- 20
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_satellite20 <- algorithm2_7_impl (input, intermediate5)

    result [21,1] = i18n$t("Satellite - Net Present Value (20 years), currency units")
    result [21,2] = round (as.numeric(npv_satellite20[1,2]), digits = 2)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")


  }


  if (bUseCellular)
  {
    if (exists ("bWriteLog", input))
    {
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cellular CAPEX Caclulation"))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "--------------------------")

    }

    #Algorithm for determining the total cost of the installation and configuration of the cellular communication channel

    intermediate41 <- list (NumberCellularsets = 0, RequiredCapacity = 0.0)
    intermediate41$RequiredCapacity  <- RequiredCapacity

    result61 =  algorithm2_10_impl (input, intermediate41)

    # Required number of Cellular sets

    NumberCellularsets = as.numeric (result61 [2,2])

    TotalCAPEXCellular =  as.numeric (result61 [1,2])
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total cost of establishing a Cellular communication channel, currency units"), TotalCAPEXCellular, sep = ": "))

    TotalCAPEXCellular =  TotalCAPEXCellular + EnergyCAPEX

    #Total cost of Cellular equipment and installation materials
    CostOfCellularEqAndMat =  as.numeric (result61 [3,2]) + EnergyMaterials

    result [7,1] = i18n$t("Total cost of establishing a Cellular communication channel, currency units")
    result [7,2] = round (TotalCAPEXCellular, digits = 2)

    if (exists ("bWriteLog", input))
    {
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")

      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cellular OPEX Caclulation"))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "-------------------------")


    }

    #Algorithm for determining the total cost of the maintenance of the Cellular communication channel


    intermediate41$NumberCellularsets <- NumberCellularsets



    result71 =  algorithm2_11_impl (input, intermediate41)


    TotalOPEXCellular =  as.numeric (result71 [1,2])
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Annual cost of maintenance of the Cellular channel, currency units/year"), TotalOPEXCellular, sep = ": "))

    TotalOPEXCellular =  TotalOPEXCellular + EnergyOPEX

    result [8,1] = i18n$t("Annual cost of maintenance of the Cellular channel, currency units/year")
    result [8,2] = round (TotalOPEXCellular, digits = 2)

    if (exists ("bWriteLog", input))
    {
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")

      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cellular Net Present Value Caclulation"))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "--------------------------------------")

    }

    #NPV - Cellular
    TotalInvest <- TotalCAPEXCellular
    CostOfOperation  <- TotalOPEXCellular
    CostOfEquipmentAndMaterials  <- CostOfCellularEqAndMat

    intermediate5$NetIncome <- 0
    intermediate5$TotalInvest <- TotalInvest
    intermediate5$CostOfOperation <- CostOfOperation
    intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
    intermediate5$PaybackPeriod <- input$PVOptionSet.PaybackPeriod
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_cellular <- algorithm2_7_impl (input, intermediate5)
    result [12,1] = i18n$t("Cellular - Net Present Value, currency units")
    result [12,2] = round (as.numeric(npv_cellular[1,2]), digits = 2)

    intermediate5$PaybackPeriod <- 10
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_cellular10 <- algorithm2_7_impl (input, intermediate5)

    result [22,1] = i18n$t("Cellular - Net Present Value (10 years), currency units")
    result [22,2] = round (as.numeric(npv_cellular10[1,2]), digits = 2)

    intermediate5$PaybackPeriod <- 15
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_cellular15 <- algorithm2_7_impl (input, intermediate5)

    result [23,1] = i18n$t("Cellular - Net Present Value (15 years), currency units")
    result [23,2] = round (as.numeric(npv_cellular15[1,2]), digits = 2)

    intermediate5$PaybackPeriod <- 20
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Payback period, years"), intermediate5$PaybackPeriod ,sep = ":"))
    npv_cellular20 <- algorithm2_7_impl (input, intermediate5)

    result [24,1] = i18n$t("Cellular - Net Present Value (20 years), currency units")
    result [24,2] = round (as.numeric(npv_cellular20[1,2]), digits = 2)
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, "")

  }


  return (result)
}

method2_inv_impl <- function(input, intermediate = NULL)
{


  L <- reactiveValuesToList (input)


  l_len <- length(L)

  result <- matrix (nrow = (l_len-5), ncol = 5)
  curindex <- 1

  for (i in 1:l_len)
  {
    elem <- L[i]

    e_name <- names (elem [1])

    if ((e_name != "calculate") &
        (e_name != "investigate") &
        (e_name != "formula") &
        (e_name != "method") &
        (e_name != "algorithm") &
        (e_name != "investigate") )
    {
      result1 <- method2_impl (L)

      val <- as.numeric (elem [[1]])

      val2 <- val*2
      elem [[1]] <- val2

      L[i] <- elem

      result2 <- method2_impl (L)



      val3 <- val*4
      elem [[1]] <- val3

      L[i] <- elem

      result3 <- method2_impl (L)


      val4 <- val/2
      elem [[1]] <- val4

      L[i] <- elem

      result4 <- method2_impl (L)

      val5 <- val/4
      elem [[1]] <- val5

      L[i] <- elem

      result5 <- method2_impl (L)

      elem [[1]] <- val

      L[i] <- elem

      base1 <- (as.numeric (result1[7,2])  +  as.numeric (result1[8,2]) + as.numeric (result1[9,2]))/3
      base2 <- (as.numeric (result2[7,2])  +  as.numeric (result2[8,2]) + as.numeric (result2[9,2]))/3
      base3 <- (as.numeric (result3[7,2])  +  as.numeric (result3[8,2]) + as.numeric (result3[9,2]))/3
      base4 <- (as.numeric (result4[7,2])  +  as.numeric (result4[8,2]) + as.numeric (result4[9,2]))/3
      base5 <- (as.numeric (result5[7,2])  +  as.numeric (result5[8,2]) + as.numeric (result5[9,2]))/3

      dif <- round (base2 /base1 , 2)

      dif2 <- round (base3 /base1, 2)

      dif3 <- round (base4 /base1, 2)

      dif4 <- round (base5 /base1, 2)

      result [curindex, 1] <- e_name
      result [curindex, 2] <- dif
      result [curindex, 3] <- dif2
      result [curindex, 4] <- dif3
      result [curindex, 5] <- dif4

      curindex <- curindex + 1

    }

  }


  ndx = order(result [,2], decreasing = T)


  result2 = result [ndx,]


  N <- (l_len-5)


  # for (i in 1:N)
  # {
  #
  #   name <- result2 [i,1]
  #
  #   descr <- getdescriptionbyname (name)
  #
  #   result2 [i,1] <- descr
  # }

  nm <- c ("Parameter", "Mean (NPV (Parameter*2)) / Mean (NPV (Parameter))", "Mean (NPV (Parameter*4)) / Mean (NPV (Parameter))", "Mean (NPV (Parameter/2)) / Mean (NPV (Parameter))" , "NPV (Parameter/4) / NPV (Parameter)")

  colnames (result2) <- nm


  return (result2)
}



method2 <- function(input, output)
{

  switch (input$algorithm,
          ALL = {

           # req (input$Files.Traffic)
            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataRadio.BandwidthToFrequencyCoeff)
            req (input$InitialDataRadio.BasicSpectrumFee)



            req (input$SchoolSpecificData.IsEnergyInSchool)

            req (input$InitialDataTraffic.LevelOfQuality)
            req (input$SchoolSpecificData.Length)
            req (input$InitialDataFOCL.LaborCostNormGeo)
            req (input$InitialDataFOCL.CostNormGeo)
            req (input$InitialDataFOCL.CostNormFOCLinst)
            req (input$InitialDataFOCL.LaborCostNormHDD)
            req (input$InitialDataFOCL.CostNormHDD)



            req (input$InitialDataFOCL.CostTramsEquipment)

            req (input$InitialDataFOCL.LaborCostNormCDC)
            req (input$InitialDataFOCL.CostNormCDC)
            req (input$InitialDataFOCL.CostNormMaterialsCD)





            req (input$InitialDataFOCL.LaborCostNormLC)
            req (input$InitialDataFOCL.CostNormLC)
            req (input$InitialDataFOCL.CostNormMaterialsCMh)
            req (input$InitialDataFOCL.LaborCostNormCMh)
            req (input$InitialDataFOCL.CostNormCMh)
            req (input$InitialDataFOCL.CostNormCC1km)


            req (input$SchoolSpecificData.RequiredBandwidth)
            req (input$InitialDataFOCL.LaborCostNormCCI)
            req (input$InitialDataFOCL.CostNormCCI)
            req (input$InitialDataFOCL.LaborCostNormST)
            req (input$InitialDataFOCL.CostNormST)
            req (input$InitialDataFOCL.LaborCostNormTS)
            req (input$InitialDataFOCL.CostNormTS)
            req (input$InitialDataFOCL.LaborCostNormSC)
            req (input$InitialDataFOCL.CostNormSC)
            req (input$InitialDataFOCL.AnnualLaborNormFOCLm)
            req (input$InitialDataFOCL.CostNormFOCLm)
            req (input$InitialDataFOCL.AnnualLaborNormCDm)


            req (input$InitialDataFOCL.CostNormCDm)
            #req (input$InitialDataFOCL.NumberBEF)
            #req (input$InitialDataFOCL.AnnualLaborNormBEFm)
            #req (input$InitialDataFOCL.CostNormBEFm)
            #req (input$InitialDataFOCL.NumberODF)
            #req (input$InitialDataFOCL.AnnualLaborNormODFm)
            #req (input$InitialDataFOCL.CostNormODFm)

            req (input$InitialDataFOCL.AnnualRentBand)



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
            req (input$InitialDataRadio.AnnualLaborPylon)
            req (input$InitialDataRadio.CostLaborPylonm)
            req (input$InitialDataRadio.AnnualLaborAFD)

            req (input$InitialDataRadio.CostLaborAFDm)
            req (input$InitialDataRadio.AnnualLaborRTS)
            req (input$InitialDataRadio.CostLaborRTSm)

            req (input$InitialDataRadio.AnnualRentBand)


            #req (input$InitialDataSatelite.RequiredCapacity)
            req (input$InitialDataSatelite.ChannelCapacity)
            req (input$InitialDataSatelite.CostOfOneVSAT)

            req (input$InitialDataSatelite.CostOfOneVSATmat)
            req (input$InitialDataSatelite.CostOfInstallVSAT)
            req (input$InitialDataSatelite.LaborOfInstallVSAT)

            req (input$InitialDataSatelite.AnnualLaborOfServiceVSAT)
            req (input$InitialDataSatelite.CostOfServiceVSAT)

            req (input$InitialDataSatelite.AnnualRentBand)


            req (input$InitialDataCellular.ChannelCapacity)
            req (input$InitialDataCellular.CostOfOneCellular)
            req (input$InitialDataCellular.CostOfOneCellularmat)
            req (input$InitialDataCellular.CostOfInstallCellular)
            req (input$InitialDataCellular.LaborOfInstallCellular)

            req (input$InitialDataCellular.AnnualLaborOfServiceCellular)
            req (input$InitialDataCellular.CostOfServiceCellular)
            req (input$InitialDataCellular.AnnualRentBand)

            req (input$InitialDataFOCL.ChannelCapacity)

            req (input$InitialDataFOCL.TotalFibersInTheCable)

            req (input$InitialDataFOCL.TotalNumberOfCablesInDuct)

            print (6)
            req (input$InitialDataFOCL.RentalCostForFibers)
            req (input$InitialDataFOCL.RentalCostForDuct)
            req (input$InitialDataFOCL.DemandForFibers)
            req (input$InitialDataFOCL.DemandForDuct)

            req (input$InitialDataFOCL.HorDrillCoeff)
            req (input$InitialDataFOCL.CableDuctCoeff)
            req (input$InitialDataFOCL.CableLayingMachineCoeff)
            req (input$InitialDataFOCL.NumberOfManholesPerKM)
            req (input$InitialDataFOCL.NumberOfCableCouplingsPerKM)
            req (input$InitialDataFOCL.MarginCoeff)
            req (input$InitialDataFOCL.DesignCoeff)


            req (input$InitialDataRadio.DesignCoeff)

            req (input$InitialDataSatelite.CostOfEqCoeff) #0.4
            req (input$InitialDataSatelite.DesignCoeff) #0.05

            req (input$InitialDataCellular.CostOfEqCoeff) #0.4
            req (input$InitialDataCellular.DesignCoeff) #0.05



            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

            result <- method2_impl (input)

            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)


          },
          ALGORITHM2_0 = {#Algorithm for necessary network channel capacity calculation
            algorithm2_0 (input, output)
          },
          ALGORITHM2_1 = {#Algorithm for overall FOCL construction cost evaluation
            algorithm2_1 (input, output)
          },
          ALGORITHM2_2 = {#Algorithm for overall FOCL maintenance cost evaluation
            algorithm2_2 (input, output)
          },
          ALGORITHM2_3 = {#Algorithm for total RTS construction cost evaluation between object and SN in locality
            algorithm2_3 (input, output)
          },
          ALGORITHM2_4 = {#Algorithm for total RTS maintenance cost evaluation between object and SN in locality
            algorithm2_4 (input, output)
          },
          ALGORITHM2_5 = {#Algorithm for determining the total cost of the installation and configuration of the satellite communication channel
            algorithm2_5 (input, output)
          },
          ALGORITHM2_6 = {#Algorithm for determining the total cost of the maintenance of the satellite communication channel
            algorithm2_6 (input, output)
          },
          ALGORITHM2_7 = {#Algorithm for caclulating NPV
            algorithm2_7 (input, output)
          },
          ALGORITHM2_8 = {#Determination of the total cost of solar panels and school power station installation and commissioning
            algorithm2_8 (input, output)
          },
          ALGORITHM2_9 = {#Determination of annual operation cost of solar panels and school power station
            algorithm2_9 (input, output)
          },
          ALGORITHM2_10 = {#Algorithm for determining the total cost of the installation and configuration of the cellular communication channel
            algorithm2_10 (input, output)
          },
          ALGORITHM2_11 = {#Algorithm for determining the total cost of the maintenance of the cellular communication channel
            algorithm2_11 (input, output)
          },
          ALGORITHM2_12 = {#Potential rental income for additional fibers and cable duct (extra capacity) (currency units/year)
            algorithm2_12 (input, output)
          },
          ALGORITHM2_13 = {#Simplefied approach for calculation CAPEX and OPEX
            algorithm2_13 (input, output)
          },

          stop ("No!")
  )
}

method2_inv <- function(input, output)
{
  req (input)
  req (input$algorithm)
  req (output)

  switch (input$algorithm,
          ALL = {


            result <- method2_inv_impl (input)

            output$c_names <- NULL

            output$data <- renderTable(result, colnames=TRUE)

             output$log <- NULL

            #output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

          },
          ALGORITHM2_0 = {#

          },
          stop ("No!")
  )
}
