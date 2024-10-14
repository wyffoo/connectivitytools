library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Determination of annual operation cost of solar panels and school power station 


# Annual operation cost of solar panels and school power station (currency units/year)
formula_2_9_1 <- function (input, intermediate = NULL)
{
  req (input)
  
  result =  input$PowerInSchool.CostNormsOfElectro *input$PowerInSchool.AnnualLaborCostNormsElectro
  result <- round (as.numeric (result), digits = 2)
  
  return (result)
}


algorithm2_9_impl <- function(input, intermediate = NULL)
{
  OPEXElectro =  formula_2_9_1 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Annual operation cost of solar panels and school power station, currency units/year"), OPEXElectro, sep = ": "))  

  result <- matrix (nrow = 1, ncol = 2)
  result [1,1] = i18n$t("Annual operation cost of solar panels and school power station, currency units/year")
  result [1,2] = OPEXElectro


  return (result)
}


algorithm2_9 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            req (input$PowerInSchool.AnnualLaborCostNormsElectro)
            req (input$PowerInSchool.CostNormsOfElectro)
            
            
            result <- algorithm2_9_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)     
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_2_9_1 = {#Annual operation cost of solar panels and school power station (currency units/year)
 
            req (input$PowerInSchool.AnnualLaborCostNormsElectro)
            req (input$PowerInSchool.CostNormsOfElectro)
            
            result <- formula_2_9_1 (input)
            
            output$data <- renderTable(result, colnames=FALSE)
          },
         stop ("No!")
          
  )
}