library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

library(geosphere)

#library(gmapsdistance)

#Geodesic distance calculation between investigated objects (localities) in estimated broadband network


#Geodesic distance between two objects, meters
formula_4_1_1 <- function (input, intermediate = NULL)
{
  req (input)

  result <- distm (c(as.numeric (input$Coordinates.LonFirst), as.numeric (input$Coordinates.LatFirst)),c(as.numeric (input$Coordinates.LonSecond), as.numeric (input$Coordinates.LatSecond)),fun=distHaversine)
  result <- round (as.numeric (result), digits = 2)
  return (result)
}

#Distance connectivity matrix between investigated objects (localities) in estimated broadband network
formula_4_1_2 <- function (input, intermediate = NULL)
{
  req (input)

  objects <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)

  req (objects)

  numberofobj <- nrow(objects)

  result <- matrix (nrow = numberofobj, ncol = numberofobj)


  for (i in 1: (numberofobj-1))
  {
    for (j in (i+1):numberofobj)
    {
      lon1 <- objects [i,2]
      lat1 <- objects [i,3]

      lon2 <- objects [j,2]
      lat2 <- objects [j,3]

      distancem <- distm (c(as.numeric (lon1), as.numeric (lat1)),c(as.numeric (lon2), as.numeric (lat2)),fun=distHaversine)
      distancekm <- as.numeric (distancem/1000)

      result [i,j] <- distancekm
      result [j,i] <- distancekm

    }
    result [i,i] <- 0
  }

  result [numberofobj,numberofobj] <- 0


  return (result)
}

#Identification of distance to the fiber for all not-connected objects (localities)
formula_4_1_3 <- function (input, intermediate = NULL)
{
  req (input)

  objects <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)

  req (objects)

  #Filtering objects where distance to the fiber is unknown
  objects_with_unknown_DTF <- filter (objects,is.na (DTF))


  req (objects_with_unknown_DTF)

  #Filtering objects where distance to the fiber is defined
  objects_with_known_DTF <- filter (objects,DTF != '')


  req (objects_with_known_DTF)

  number_of_objects_with_known_DTF <- nrow(objects_with_known_DTF)


  if (number_of_objects_with_known_DTF == 0)
  {
	.GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("No objects connected to the fiber"))
	.GlobalEnv$top_calc_status  = 312
	return (0)
  }

  number_of_objects_with_unknown_DTF <- nrow(objects_with_unknown_DTF)




  if (number_of_objects_with_unknown_DTF == 0)
  {
	.GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("No objects for calculation distance to the fiber"))
	.GlobalEnv$top_calc_status  = 313
	return (0)
  }


  distances <- matrix (nrow = number_of_objects_with_unknown_DTF, ncol = number_of_objects_with_known_DTF)

  result <- matrix (nrow = number_of_objects_with_unknown_DTF, ncol = 3)

  for (i in 1:number_of_objects_with_unknown_DTF)
  {
	lon1 <- objects_with_unknown_DTF [i,2]
	lat1 <- objects_with_unknown_DTF [i,3]

	for (j in 1:number_of_objects_with_known_DTF)
	{

	  lon2 <- objects_with_known_DTF [j,2]
	  lat2 <- objects_with_known_DTF [j,3]
	  dtf <- objects_with_known_DTF [j,5]



	  distancem <- distm (c(as.numeric (lon1), as.numeric (lat1)),c(as.numeric (lon2), as.numeric (lat2)),fun=distHaversine)

	  distancekm <- as.numeric (distancem/1000)


	  distances [i,j] <- round (distancekm + as.numeric(dtf), digits = 2)
	  #print (distances [i,j])
	}

	indexofmin <- which.min(distances [i,])


	mindistance <- as.numeric (distances[i,indexofmin])


	result [i,1] <- as.character(objects_with_unknown_DTF [i,1])
	result [i,2] <- mindistance
	result [i,3] <- as.character(objects_with_known_DTF [indexofmin, 1])

  }


  return (result)
}


algorithm4_1_impl <- function(input, intermediate = NULL)
{

  # Geodesic distance between two objects, meters

  DistanceBetweenTwoObjects =  formula_4_1_1 (input, intermediate)
  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Geodesic distance between two objects, meters"), DistanceBetweenTwoObjects, sep = ": "))


  result <- matrix (nrow = 1, ncol = 2)
  result [1,1] = i18n$t("Geodesic distance between two objects, meters")
  result [1,2] = DistanceBetweenTwoObjects

  return (result)


}

algorithm4_1 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)

  switch (input$formula,
          ALL = {

            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))


            result <- algorithm4_1_impl (input)

            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

          },
          FORMULA_4_1_1 = {#Geodesic distance between two objects, meters

            #req (input$GeneralVariables.STCPlaceChangeCoeff)
            req (input$Coordinates.LonFirst)
            req (input$Coordinates.LatFirst)
            req (input$Coordinates.LonSecond)
            req (input$Coordinates.LatSecond)

            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")

            result <- formula_4_1_1 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_4_1_2 = {#Distance connectivity matrix between investigated objects (localities) in estimated broadband network

            #req (input$GeneralVariables.STCPlaceChangeCoeff)

            req (input$Files.ListOfObjects)
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")

            result <- formula_4_1_2 (input)

            output$data <- renderTable(result, colnames=FALSE)
		  },
		  FORMULA_4_1_3 = {#Identification of distance to the fiber for all not-connected objects (localities)

			req (input$Files.ListOfObjects)
			.GlobalEnv$mylog <- matrix("Detailed Calculation Log:")

			result <- formula_4_1_3 (input)

			output$data <- renderTable(result, colnames=FALSE)
          },
         stop ("No!")

  )
}
