library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Optimal path selection between set of objects using NPV connectivity matrix

library(igraph)


#Optimal path calculation (by better NPV)
formula_4_3_1 <- function (input, output, intermediate = NULL)
{
  req (input)
  
  #objects <- vroom::vroom(input$Files.NPVFilteredMatrix, altrep = FALSE)
  
  mat <-  as.matrix(read.table(input$Files.NPVFilteredMatrix, header=T, sep = ";" ))
  
  req (mat)
  
  nms <- mat[,1]
 # mat <- mat[, -1]
  
  
  colnames(mat) <- rownames(mat) <- nms
  mat[is.na(mat)] <- 0
  
  numberofobj <- nrow(mat)
  
  g <- graph.adjacency(mat, weighted=TRUE)

  
  #g2 <- simplify(g, remove.multiple = T, remove.loops = T, edge.attr.comb=c(weight="min"))
  
  (s.paths <- shortest.paths(g, algorithm = "dijkstra"))
  
  numberofobj1 <- nrow (s.paths)
  
  if (numberofobj != numberofobj1)
  {
    print ("ERROR")
    return (0)
  }
  
  
  result <- matrix (nrow = numberofobj, ncol = numberofobj)
  
  #result <- g2 
  #as_data_frame(g2, what="edges")
  
  for (i in 1:numberofobj)
  {
    for (j in 1:numberofobj)
    {
      result [i,j] <- as.numeric(0)
      if (mat [i,j] == s.paths [i,j])
        result [i,j] <- mat [i,j]
    }
  }

  
  g <- graph.adjacency(result, weighted=TRUE)
  
  return (g)
}



algorithm4_3_impl <- function(input, intermediate = NULL)
{
  
  result <- 0
  
  return (result)
  
  
}

algorithm4_3 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)
  
  switch (input$formula, 
          ALL = {
            
            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
            
            
            result <- algorithm4_3_impl (input)
            
            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)     
            
            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
            
          },
          FORMULA_4_3_1 = {#Optimal path calculation (by better NPV)
            
            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Files.NPVFilteredMatrix)
            
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            
            result <- formula_4_3_1 (input, output)
            
  #          output$data <- renderTable(result, colnames=FALSE)
            output$myplot <- renderPlot(plot(result, edge.arrow.size=.4,vertex.label=NA))
            
            #output$myplot <- 
          },
         
          stop ("No!")
          
  )
}