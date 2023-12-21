library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Calculating NPV without Income

#Tax on Profit Calculation
formula_2_7_1 <- function (input, intermediate = NULL)
{
  req (input)

  NetIncome <- 0
  NetIncome <-  as.numeric (input$Intermediate.NetIncome)

  CostOfOperation <- 0
  CostOfOperation <-  as.numeric (input$Intermediate.CostOfOperation)

  CostOfEquipmentAndMaterials <- 0
  CostOfEquipmentAndMaterials <-  as.numeric (input$Intermediate.CostOfEquipmentAndMaterials)


  if (!is.null(intermediate))
  {
    NetIncome <- as.numeric (intermediate$NetIncome)
    CostOfOperation <- as.numeric (intermediate$CostOfOperation)
    CostOfEquipmentAndMaterials <- as.numeric (intermediate$CostOfEquipmentAndMaterials)
  }

  NetProfit1 =  as.numeric (NetIncome - CostOfOperation)
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
formula_2_7_2 <- function (input, intermediate = NULL)
{
  req (input)

  NetIncome <- 0
  NetIncome <- as.numeric (input$Intermediate.NetIncome)

  CostOfOperation <- 0
  CostOfOperation <- as.numeric (input$Intermediate.CostOfOperation)

  CorporateTax <- 0
  CorporateTax <- as.numeric (input$Intermediate.CorporateTax)



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
formula_2_7_3 <- function (input, intermediate = NULL)
{
  req (input)

  CashFlow <- 0
  CashFlow <- input$Intermediate.CashFlow

  PaybackPeriod <- 0
  PaybackPeriod <- input$PVOptionSet.PaybackPeriod

  if (!is.null(intermediate))
  {
    CashFlow <- as.numeric (intermediate$CashFlow)
    PaybackPeriod <- as.numeric (intermediate$PaybackPeriod)
  }

  result <- 0

  for (i in 1:PaybackPeriod)
  {
    result = result + (CashFlow * (1/(1+input$PVOptionSet.DiscountRate/100)^i))
  }

  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#NPV
formula_2_7_4 <- function (input, intermediate = NULL)
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

algorithm2_7_impl <- function(input, intermediate = NULL)
{

  intermediate2 <- list (NetIncome = 0.0, CostOfOperation = 0.0, CorporateTax = 0.0)

  TaxOnProfit = formula_2_7_1 (input, intermediate)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Corporate Tax, currency units/year"), TaxOnProfit, sep = ": "))


  # Net profit (Annual cash flow)

  intermediate2$CorporateTax <- TaxOnProfit
  if (!is.null(intermediate))
  {
    intermediate2$CostOfOperation <- intermediate$CostOfOperation
    intermediate2$NetIncome <- intermediate$NetIncome
  }



  CashFlow = formula_2_7_2 (input, intermediate2)

  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Annual cash flow, currency units/year"), CashFlow, sep = ": "))

  #Discounted cash flow
  intermediate3 <- list (CashFlow = 0.0, PaybackPeriod = 0)
  intermediate3$CashFlow <- CashFlow

  intermediate3$PaybackPeriod <- input$PVOptionSet.PaybackPeriod
  if (!is.null(intermediate))
  {
    intermediate3$PaybackPeriod <- intermediate$PaybackPeriod
  }


  DiscountedCF <- formula_2_7_3 (input, intermediate3)
  if (exists ("bWriteLog", input))
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Discounted cash flow, currency units/year"), DiscountedCF, sep = ": "))


  #NPV

  result <- matrix (nrow = 1, ncol = 2)
  result [1,1] = i18n$t("Net Present Value, currency units")

  intermediate4 <- list (DiscountedCF = 0.0, TotalInvest = 0.0)
  intermediate4$DiscountedCF <- DiscountedCF


  TotalInvest <- 0
  TotalInvest <- input$Intermediate.TotalInvest


  if (!is.null(intermediate))
  {
    TotalInvest <- as.numeric (intermediate$TotalInvest)
  }

  intermediate4$TotalInvest <- TotalInvest

  NPV <- formula_2_7_4 (input, intermediate4)

  result [1,2] = as.numeric (NPV)

  return (result)
}


algorithm2_7 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)

  switch (input$formula,
          ALL = {
            req (input$Intermediate.TotalInvest)
            req (input$PVOptionSet.VATax)
            req (input$PVOptionSet.ProfitTax)
            req (input$PVOptionSet.DiscountRate)
            req (input$PVOptionSet.PaybackPeriod)
            req (input$Intermediate.CostOfOperation)
            req (input$AccessTechnologyOptionSet.AvrLifeTimeOfEqAndMat)


            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

            result <- algorithm2_7_impl (input)

            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

          },
          FORMULA_2_7_1 = {#Tax on Profit Calculation
            req (input$Intermediate.NetIncome)
            req (input$Intermediate.CostOfOperation)
            req (input$PVOptionSet.ProfitTax)
            req (input$Intermediate.CostOfEquipmentAndMaterials)
            req (input$AccessTechnologyOptionSet.AvrLifeTimeOfEqAndMat)


            result <- formula_2_7_1 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_7_2 = {#Net profit (Annual cash flow) calculation
            req (input$Intermediate.NetIncome)
            req (input$Intermediate.CostOfOperation)
            req (input$Intermediate.CorporateTax)


            result <- formula_2_7_2 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_7_3 = {#Discounted cash flow
            req (input$PVOptionSet.DiscountRate)
            req (input$PVOptionSet.PaybackPeriod)
            req (input$Intermediate.CashFlow)

            result <- formula_2_7_3 (input)

            output$data <- renderTable(result, colnames=FALSE)

          },
          FORMULA_2_7_4 = {#NPV
            req (input$Intermediate.DiscountedCF)
            req (input$Intermediate.TotalInvest)

            result <- formula_2_7_4 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          stop ("No!")
  )

}
