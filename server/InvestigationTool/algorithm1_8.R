library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Calculating NPV

#Design and licensing cost
formula_1_8_1 <- function (input, intermediate = NULL)
{
  req (input)
  
   NumberOfAOEs <- 0 
   NumberOfAOEs <- input$Intermediate.NumberOfAOEs
  

   CostOfConstruction <- 0 
   CostOfConstruction <- input$Intermediate.CostOfConstruction
   
     
  if (!is.null(intermediate))
  {
     NumberOfAOEs <- as.numeric (intermediate$NumberOfAOEs)
     
     
     CostOfConstruction <- as.numeric (intermediate$CostOfConstruction)
  }
  
  result =  input$AccessTechnologyOptionSet.CostOfRFRLicense * NumberOfAOEs +
    CostOfConstruction*(input$PVOptionsSet.DesignConstrCoeff/100)
  
  result <- round (as.numeric (result), digits = 2)  
  return (result)
}

#Total investment
formula_1_8_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  CostOfEquipment <- 0 
  CostOfEquipment <- input$Intermediate.CostOfEquipment
  
  CostOfConstruction <- 0 
  CostOfConstruction <- input$Intermediate.CostOfConstruction
  
  DesignAndLicensing <- 0 
  DesignAndLicensing <- input$Intermediate.DesignAndLicensing
  
    
  if (!is.null(intermediate))
  {
    CostOfEquipment <- as.numeric (intermediate$CostOfEquipment)
    CostOfConstruction <- as.numeric (intermediate$CostOfConstruction)
    DesignAndLicensing <- as.numeric (intermediate$DesignAndLicensing)
  }
  
  result = CostOfEquipment + CostOfConstruction + DesignAndLicensing
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Net Income
formula_1_8_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  Subscribers <- 0 
  Subscribers <- input$Intermediate.Subscribers
  
  
  if (!is.null(intermediate))
  {
    Subscribers <- as.numeric (intermediate$Subscribers)
  }
  
  result = (Subscribers*input$PVOptionSet.ARPU*12)*(1-(input$PVOptionSet.VATax/100))
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Tax on Profit Calculation
formula_1_8_4 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  NetIncome <- 0 
  NetIncome <- input$Intermediate.NetIncome

  CostOfOperation <- 0 
  CostOfOperation <- input$Intermediate.CostOfOperation

  CostOfEquipmentAndMaterials <- 0 
  CostOfEquipmentAndMaterials <- input$Intermediate.CostOfEquipmentAndMaterials
  
  
  if (!is.null(intermediate))
  {
    NetIncome <- as.numeric (intermediate$NetIncome)
    CostOfOperation <- as.numeric (intermediate$CostOfOperation)
    CostOfEquipmentAndMaterials <- as.numeric (intermediate$CostOfEquipmentAndMaterials)
  }
  
  NetProfit1 = NetIncome - CostOfOperation
  CorpTax <-  0
  
  if (NetProfit1 > 0)
    CorpTax = NetProfit1*(input$PVOptionSet.ProfitTax/100) - (CostOfEquipmentAndMaterials/input$AccessTechnologyOptionSet.AvrLifeTimeOfEqAndMat)
  
  if (CorpTax < 0)
    CorpTax <- 0
  
  result = CorpTax
  
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}

#Net profit (Annual cash flow) calculation
formula_1_8_5 <- function (input, intermediate = NULL)
{
  req (input)

  NetIncome <- 0 
  NetIncome <- input$Intermediate.NetIncome
  
  CostOfOperation <- 0 
  CostOfOperation <- input$Intermediate.CostOfOperation
  
  CorporateTax <- 0 
  CorporateTax <- input$Intermediate.CorporateTax
  
  
    
  if (!is.null(intermediate))
  {
    NetIncome <- as.numeric (intermediate$NetIncome)
    CostOfOperation <- as.numeric (intermediate$CostOfOperation)
    
    CorporateTax <- as.numeric (intermediate$CorporateTax)

  }
  
  if (CorporateTax < 0)
    CorporateTax = 0
  
  result = NetIncome -  CostOfOperation - CorporateTax
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Discounted cash flow
formula_1_8_6 <- function (input, intermediate = NULL)
{
  req (input)
  
  CashFlow <- 0 
  CashFlow <- input$Intermediate.CashFlow
  
  
  if (!is.null(intermediate))
  {
    CashFlow <- as.numeric (intermediate$CashFlow)
  }
  
  result <- 0 
  
  for (i in 1:input$PVOptionSet.PaybackPeriod)
  {
    result = result + (CashFlow * (1/(1+input$PVOptionSet.DiscountRate/100)^i))
  }
  
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#NPV
formula_1_8_7 <- function (input, intermediate = NULL)
{
  req (input)
  
  DiscountedCF <- 0 
  DiscountedCF <- input$Intermediate.DiscountedCF
  
  TotalInvest <- 0 
  TotalInvest <- input$Intermediate.TotalInvest
  
    
  if (!is.null(intermediate))
  {
    DiscountedCF <- as.numeric (intermediate$DiscountedCF)
    TotalInvest <- as.numeric (intermediate$TotalInvest)
  }
  
  result = DiscountedCF - TotalInvest
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

algorithm1_8_impl <- function(input, intermediate = NULL)
{
  
  #Design and licensing cost
  DesignAndLicensing <- as.bigz (0)
  DesignAndLicensing =  formula_1_8_1 (input, intermediate)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Design and licensing cost, currency units"), DesignAndLicensing, sep = ": "))                            
  
  #Total investment
  
  CostOfEquipment <- 0 
  CostOfEquipment <- input$Intermediate.CostOfEquipment
  
  CostOfConstruction <- 0 
  CostOfConstruction <- input$Intermediate.CostOfConstruction

  
  if (!is.null(intermediate))
  {
    CostOfEquipment <- as.numeric (intermediate$CostOfEquipment)
    CostOfConstruction <- as.numeric (intermediate$CostOfConstruction)
  }
  
  
  intermediate2 <- list (DesignAndLicensing = 0.0, CostOfEquipment = 0.0, CostOfConstruction = 0.0)
  intermediate2$DesignAndLicensing <- DesignAndLicensing
  intermediate2$CostOfEquipment <- CostOfEquipment
  intermediate2$CostOfConstruction <- CostOfConstruction
  
  
  TotalInvest = formula_1_8_2 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Total investment, currency units"), TotalInvest, sep = ": "))                            
  
  #Net Income
  NetIncome = formula_1_8_3 (input, intermediate)
  
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Net Income, currency units/year"), NetIncome, sep = ": "))                            

    
  #Tax on Profit Calculation
  CostOfOperation <- 0 
  CostOfOperation <- input$Intermediate.CostOfOperation
  CostOfEquipmentAndMaterials <- 0
  CostOfEquipmentAndMaterials = input$Intermediate.CostOfEquipmentAndMaterials
  
  
  if (!is.null(intermediate))
  {
    CostOfOperation <- as.numeric (intermediate$CostOfOperation)
    CostOfEquipmentAndMaterials <- as.numeric (intermediate$CostOfEquipmentAndMaterials)
  }
  
  
  intermediate2 <- list (NetIncome = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, CorporateTax = 0.0)
  intermediate2$NetIncome <- NetIncome
  intermediate2$CostOfOperation <- CostOfOperation
  intermediate2$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
  
    
  TaxOnProfit = formula_1_8_4 (input, intermediate2)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Corporate Tax, currency units/year"), TaxOnProfit, sep = ": "))                            
  
  # Net profit (Annual cash flow)

  intermediate2$CorporateTax <- TaxOnProfit
  
  CashFlow = formula_1_8_5 (input, intermediate2)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Annual cash flow, currency units/year"), CashFlow, sep = ": "))                            
  
  #Discounted cash flow
  intermediate2 <- list (CashFlow = 0.0)
  intermediate2$CashFlow <- CashFlow
  
  
  DiscountedCF <- formula_1_8_6 (input, intermediate2)
  
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Discounted cash flow, currency units/year"), DiscountedCF, sep = ": "))                            
  
  #NPV
  
  result <- matrix (nrow = 1, ncol = 2)
  result [1,1] = i18n$t("Net Present Value, currency units")

  intermediate2 <- list (DiscountedCF = 0.0, TotalInvest = 0.0)
  intermediate2$DiscountedCF <- DiscountedCF
  intermediate2$TotalInvest <- TotalInvest
  
    
  NPV <- formula_1_8_7 (input, intermediate2)
  
  result [1,2] = as.numeric (NPV)
  
  return (result) 
}


algorithm1_8 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {

            req (input$AccessTechnologyOptionSet.CostOfRFRLicense)
            req (input$Intermediate.NumberOfAOEs)
            req (input$Intermediate.CostOfEquipment)
            req (input$Intermediate.CostOfConstruction)
            req (input$PVOptionsSet.DesignConstrCoeff)
            req (input$Intermediate.Subscribers)
            req (input$PVOptionSet.ARPU)
            req (input$PVOptionSet.VATax)
            req (input$Intermediate.CostOfOperation)
            req (input$PVOptionSet.ProfitTax)
            req (input$Intermediate.CostOfEquipmentAndMaterials)
            req (input$AccessTechnologyOptionSet.AvrLifeTimeOfEqAndMat)
            req (input$PVOptionSet.DiscountRate)
            req (input$PVOptionSet.PaybackPeriod)
            
          
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            result <- algorithm1_8_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_1_8_1 = {#Design and licensing cost
            req (input$AccessTechnologyOptionSet.CostOfRFRLicense)
            req (input$Intermediate.NumberOfAOEs)
            req (input$Intermediate.CostOfConstruction)
            req (input$PVOptionsSet.DesignConstrCoeff)
            
            result <- formula_1_8_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
            
          },
          FORMULA_1_8_2 = {#Total investment
            req (input$Intermediate.CostOfEquipment)
            req (input$Intermediate.CostOfConstruction)
            req (input$Intermediate.DesignAndLicensing)
            
            result <- formula_1_8_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          
            
          },
          FORMULA_1_8_3 = {#Net Income
            
            req (input$Intermediate.Subscribers)
            req (input$PVOptionSet.ARPU)
            req (input$PVOptionSet.VATax)
            
            result <- formula_1_8_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_1_8_4 = {#Tax on Profit Calculation
            req (input$Intermediate.NetIncome)
            req (input$Intermediate.CostOfOperation)
            req (input$PVOptionSet.ProfitTax)
            req (input$Intermediate.CostOfEquipmentAndMaterials)
            req (input$AccessTechnologyOptionSet.AvrLifeTimeOfEqAndMat)
            
            
            result <- formula_1_8_4 (input)
            
            output$data <- renderTable(result, colnames=FALSE)            
          },
          FORMULA_1_8_5 = {#Net Profit (Annual cash flow)
            req (input$Intermediate.NetIncome)
            req (input$Intermediate.CostOfOperation)
            req (input$Intermediate.CorporateTax)
            
            result <- formula_1_8_5 (input)
            
            output$data <- renderTable(result, colnames=FALSE)            
          },
          FORMULA_1_8_6 = {#Discounted cash flow
            req (input$PVOptionSet.DiscountRate)
            req (input$PVOptionSet.PaybackPeriod)
            req (input$Intermediate.CashFlow)
            
            result <- formula_1_8_6 (input)
            
            output$data <- renderTable(result, colnames=FALSE)            
            
          },
          FORMULA_1_8_7 = {#NPV
            req (input$Intermediate.DiscountedCF)
            req (input$Intermediate.TotalInvest)
            
            result <- formula_1_8_7 (input)
            
            output$data <- renderTable(result, colnames=FALSE)            
          },
          stop ("No!")
  )
  
}