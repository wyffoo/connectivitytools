library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Determination of the total cost of STC installation, commissioning and operation


#The total cost of acquisition and installation of equipment and materials for the construction of a STC (currency units)
formula_3_0_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  result <- input$GeneralVariables.CostOfSTCEq +   input$GeneralVariables.LaborCostNormsForSTCOrg  * input$GeneralVariables.CostNormsSTCOrg
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#The operational cost of STC (currency units per year)
formula_3_0_2 <- function (input, intermediate = NULL)
{
  req (input)
  
  result <- input$GeneralVariables.AnnualLaborCostNormsForSTCOper * input$GeneralVariables.CostNormsSTCOper
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#The total cost of installation, commissioning and operation of the STC (currency units)
formula_3_0_3 <- function (input, intermediate = NULL)
{
  req (input)
  
  
  TotalCostOfSTCConstruction <- 0 
  TotalCostOfSTCConstruction <- input$Intermediate.TotalCostOfSTCConstruction
  
  AnnualCostOfSTCOperation <- 0 
  AnnualCostOfSTCOperation <- input$Intermediate.AnnualCostOfSTCOperation
  
  if (!is.null(intermediate))
  {
    TotalCostOfSTCConstruction <- as.numeric (intermediate$TotalCostOfSTCConstruction)
    AnnualCostOfSTCOperation <- as.numeric (intermediate$AnnualCostOfSTCOperation)
  }
  
  result <- TotalCostOfSTCConstruction + AnnualCostOfSTCOperation*input$GeneralVariables.PeriodOfLANOperation
  result <- round (as.numeric (result), digits = 2)
  return (result)
}


algorithm3_0_impl <- function(input, intermediate = NULL)
{
  intermediate2 <- list (
    TotalCostOfSTCConstruction = 0.0,
    AnnualCostOfSTCOperation = 0.0)

  # The total cost of acquisition and installation of equipment and materials for the construction of a STC (currency units)

  TotalCostOfSTCConstruction =  formula_3_0_1 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The total cost of acquisition and installation of equipment and materials for the construction of a STC (currency units)"), TotalCostOfSTCConstruction, sep = ": "))  
  intermediate2$TotalCostOfSTCConstruction <- TotalCostOfSTCConstruction
  
  
  # The operational cost of STC (currency units per year)

  AnnualCostOfSTCOperation =  formula_3_0_2 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The operational cost of STC (currency units per year)"), AnnualCostOfSTCOperation, sep = ": "))  
  intermediate2$AnnualCostOfSTCOperation <- AnnualCostOfSTCOperation
  
  # The total cost of installation, commissioning and operation of the STC (currency units)

  TotalSTC =  formula_3_0_3 (input, intermediate2)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("The total cost of installation, commissioning and operation of the STC (currency units)"), TotalSTC, sep = ": "))  
  
  
  result <- matrix (nrow = 3, ncol = 2)
  result [1,1] = i18n$t("The total cost of acquisition and installation of equipment and materials for the construction of a STC (currency units)")
  result [1,2] = TotalCostOfSTCConstruction
  
  
  result [2,1] = i18n$t("The operational cost of STC (currency units per year)")
  result [2,2] = AnnualCostOfSTCOperation
  
  result [3,1] = i18n$t("The total cost of installation, commissioning and operation of the STC (currency units)")
  result [3,2] = TotalSTC
  
    
  return (result)
  

}

algorithm3_0 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            req (input$GeneralVariables.CostOfSTCEq)
            req (input$GeneralVariables.LaborCostNormsForSTCOrg)
            req (input$GeneralVariables.CostNormsSTCOrg)
            req (input$GeneralVariables.AnnualLaborCostNormsForSTCOper)
            req (input$GeneralVariables.CostNormsSTCOper)
            req (input$GeneralVariables.PeriodOfLANOperation)
            
            result <- algorithm3_0_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)     
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_3_0_1 = {#The total cost of acquisition and installation of equipment and materials for the construction of a STC (currency units)
            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$GeneralVariables.CostOfSTCEq)
            req (input$GeneralVariables.LaborCostNormsForSTCOrg)
            req (input$GeneralVariables.CostNormsSTCOrg)
            

            
            result <- formula_3_0_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_3_0_2 = {#The operational cost of STC (currency units per year)
            req (input$GeneralVariables.AnnualLaborCostNormsForSTCOper)
            req (input$GeneralVariables.CostNormsSTCOper)
            
            
            result <- formula_3_0_2 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          FORMULA_3_0_3 = {#The total cost of installation, commissioning and operation of the STC (currency units)
            req (input$Intermediate.TotalCostOfSTCConstruction)
            req (input$Intermediate.AnnualCostOfSTCOperation)
            req (input$GeneralVariables.PeriodOfLANOperation)
            
            result <- formula_3_0_3 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
            
          },
          stop ("No!")
          
  )
}