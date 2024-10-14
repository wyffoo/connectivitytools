#Topology accounting methodology for a large number of objects

source ("algorithm4_1.R")
source ("algorithm4_2.R")
source ("algorithm4_3.R")
source ("algorithm4_4.R")
source ("algorithm4_5_smpl.R")
source ("algorithm4_5.R")

source ("algorithm2_1.R")
source ("algorithm2_2.R")
source ("algorithm2_3.R")
source ("algorithm2_4.R")
source ("algorithm2_5.R")
source ("algorithm2_6.R")
source ("algorithm2_7.R")
source ("algorithm2_10.R")
source ("algorithm2_11.R")
source ("algorithm2_12.R")


library(shiny.i18n)

centralpointforclaster <- 0

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

top_summary <- matrix (nrow = 10, ncol = 3)

top_calc_status = 0

#100 - Successfully calculated
  #101 - ... Steps passed

#300-799 Errors
# 300-399 - General Topology Procedure
# 400-499 - FOCL
# 500-599 - Microwave
# 600-699 - Cellular
# 700-799 - Satellite


# 101 Distance Matrix was calculated
# 102 NPV Matrix was calculated
# 103 Filtering was finished
# 104 Removing triangles was finished
# 105 Removing big loops was finished
# 106 Internal clatering was finished
#
# 301 Unexpected error in the pre-clastering
# 302 Cannot make pre-clastering. Too large claster detected
# 303 Cannot make pre-clastering. Too many clasters
# 304 Cannot make pre-clastering. Not enough information
# 305 Cannot calculate topology. Too many records
# 306 Cannot calculate topology for less than two objects
# 307 Cannot read list of objects for calculation
# 308 Construction of the network is impossible without choosing FOCL or Microwave
# 309 Removing inter-claster links: Clastering is wrong
# 310 Cannot optimize network by using dijkstra
# 311 Cannot construct graph for the network
#
# 401 Cannot calculate NPV for FOCL connection of basenode to PoP
# 402 Cannot calculate INCOME for FOCL connection of basenode to PoP
# 403 Cannot calculate OPEX for FOCL connection of basenode to PoP
# 404 Cannot calculate CAPEX for FOCL connection of basenode to PoP
# 405 Cannot calculate NPV of FOCL for some link
# 406 Cannot calculate potential income from FOCL for some link
# 407 Cannot calculate overall FOCL maintenance cost for some link
# 408 Cannot calculate overall FOCL construction cost for some link
#
# 501 Cannot calculate NPV for RTS connection of basenode to PoP
# 502 Cannot calculate INCOME for RTS connection of basenode to PoP
# 503 Cannot calculate OPEX for RTS connection of basenode to PoP
# 504 Cannot calculate CAPEX for RTS connection of basenode to PoP
# 505 Cannot calculate NPV for RTS for some link
# 506 Cannot calculate potential income from RTS for some link
# 507 Cannot calculate total RTS maintenance cost for some link
# 508 Cannot calculate total RTS construction cost for some link
#
# 601 Cannot calculate NPV of cellular for some link
# 602 Cannot calculate OPEX of cellular for some link
# 603 Cannot calculate CAPEX of cellular for some link
#
# 701 Cannot calculate NPV of satellite for some link
# 702 Cannot calculate OPEX of satellite for some link
# 703 Cannot calculate CAPEX of satellite for some link


init_summary <- function (bCalcNPV = F)
{

  #.GlobalEnv$top_summary [1,1] = i18n$t("Total Cost of ownership of whole network")
  .GlobalEnv$top_summary [1,1] = i18n$t("Total_Cost_of_ownership")
  if (bCalcNPV)
    #.GlobalEnv$top_summary [1,1] = i18n$t("Total NPV of whole network")
    .GlobalEnv$top_summary [1,1] = i18n$t("Total_NPV")

  .GlobalEnv$top_summary [1,2] = 0
  .GlobalEnv$top_summary [1,3] = i18n$t("USD")

  #.GlobalEnv$top_summary [2,1] = i18n$t("Total number of links of whole network")
  .GlobalEnv$top_summary [2,1] = i18n$t("Total_number_of_links")
  .GlobalEnv$top_summary [2,2] = 0
  .GlobalEnv$top_summary [2,3] = i18n$t("links")

  #.GlobalEnv$top_summary [3,1] = i18n$t("Number of links where FOCL is recommended for deployment")
  .GlobalEnv$top_summary [3,1] = i18n$t("Number_of_links_FOCL")
  .GlobalEnv$top_summary [3,2] = 0
  .GlobalEnv$top_summary [3,3] = i18n$t("links")

  #.GlobalEnv$top_summary [4,1] = i18n$t("Number of links where Microwave is recommended for deployment")
  .GlobalEnv$top_summary [4,1] = i18n$t("Number_of_links_Microwave")
  .GlobalEnv$top_summary [4,2] = 0
  .GlobalEnv$top_summary [4,3] = i18n$t("links")

  #.GlobalEnv$top_summary [5,1] = i18n$t("Number of links where Satellite is recommended for deployment")
  .GlobalEnv$top_summary [5,1] = i18n$t("Number_of_links_Satellite")
  .GlobalEnv$top_summary [5,2] = 0
  .GlobalEnv$top_summary [5,3] = i18n$t("links")

  #.GlobalEnv$top_summary [6,1] = i18n$t("Number of links where Cellular is recommended for deployment")
  .GlobalEnv$top_summary [6,1] = i18n$t("Number_of_links_Cellular")
  .GlobalEnv$top_summary [6,2] = 0
  .GlobalEnv$top_summary [6,3] = i18n$t("links")

  #.GlobalEnv$top_summary [7,1] = i18n$t("Cost of ownership of links where FOCL is recommended for deployment")
  .GlobalEnv$top_summary [7,1] = i18n$t("Cost_of_ownership_FOCL")
  if (bCalcNPV)
    #.GlobalEnv$top_summary [7,1] = i18n$t("NPV of links where FOCL is recommended for deployment")
    .GlobalEnv$top_summary [7,1] = i18n$t("NPV_of_links_FOCL")

  .GlobalEnv$top_summary [7,2] = 0
  .GlobalEnv$top_summary [7,3] = i18n$t("USD")

  #.GlobalEnv$top_summary [8,1] = i18n$t("Cost of ownership of links where Microwave is recommended for deployment")
  .GlobalEnv$top_summary [8,1] = i18n$t("Cost_of_ownership_Microwave")

  if (bCalcNPV)
#    .GlobalEnv$top_summary [8,1] = i18n$t("NPV of links where Microwave is recommended for deployment")
    .GlobalEnv$top_summary [8,1] = i18n$t("NPV_of_links_Microwave")

  .GlobalEnv$top_summary [8,2] = 0
  .GlobalEnv$top_summary [8,3] = i18n$t("USD")

  #.GlobalEnv$top_summary [9,1] = i18n$t("Cost of ownership of links where Satellite is recommended for deployment")
  .GlobalEnv$top_summary [9,1] = i18n$t("Cost_of_ownership_Satellite")

  if (bCalcNPV)
#    .GlobalEnv$top_summary [9,1] = i18n$t("NPV of links where Satellite is recommended for deployment")
    .GlobalEnv$top_summary [9,1] = i18n$t("NPV_of_links_Satellite")

  .GlobalEnv$top_summary [9,2] = 0
  .GlobalEnv$top_summary [9,3] = i18n$t("USD")

  #.GlobalEnv$top_summary [10,1] = i18n$t("Cost of ownership of links where Cellular is recommended for deployment")
  .GlobalEnv$top_summary [10,1] = i18n$t("Cost_of_ownership_Cellular")

  if (bCalcNPV)
  #  .GlobalEnv$top_summary [10,1] = i18n$t("NPV of links where Cellular is recommended for deployment")
    .GlobalEnv$top_summary [10,1] = i18n$t("NPV_of_links_Cellular")

  .GlobalEnv$top_summary [10,2] = 0
  .GlobalEnv$top_summary [10,3] = i18n$t("USD")


  # .GlobalEnv$top_summary [11,1] = i18n$t("CAPEX of links where FOCL is recommended for deployment")
  # .GlobalEnv$top_summary [11,2] = 0
  # .GlobalEnv$top_summary [11,3] = i18n$t("USD")
  #
  # .GlobalEnv$top_summary [12,1] = i18n$t("CAPEX of links where Microwave is recommended for deployment")
  # .GlobalEnv$top_summary [12,2] = 0
  # .GlobalEnv$top_summary [12,3] = i18n$t("USD")
  #
  # .GlobalEnv$top_summary [13,1] = i18n$t("CAPEX of links where Satellite is recommended for deployment")
  # .GlobalEnv$top_summary [13,2] = 0
  # .GlobalEnv$top_summary [13,3] = i18n$t("USD")
  #
  # .GlobalEnv$top_summary [14,1] = i18n$t("CAPEX of links where Cellular is recommended for deployment")
  # .GlobalEnv$top_summary [14,2] = 0
  # .GlobalEnv$top_summary [14,3] = i18n$t("USD")
  #
  # .GlobalEnv$top_summary [15,1] = i18n$t("OPEX of links where FOCL is recommended for deployment")
  # .GlobalEnv$top_summary [15,2] = 0
  # .GlobalEnv$top_summary [15,3] = i18n$t("USD per year")
  #
  # .GlobalEnv$top_summary [16,1] = i18n$t("OPEX of links where Microwave is recommended for deployment")
  # .GlobalEnv$top_summary [16,2] = 0
  # .GlobalEnv$top_summary [16,3] = i18n$t("USD per year")
  #
  # .GlobalEnv$top_summary [17,1] = i18n$t("OPEX of links where Satellite is recommended for deployment")
  # .GlobalEnv$top_summary [17,2] = 0
  # .GlobalEnv$top_summary [17,3] = i18n$t("USD per year")
  #
  # .GlobalEnv$top_summary [18,1] = i18n$t("OPEX of links where Cellular is recommended for deployment")
  # .GlobalEnv$top_summary [18,2] = 0
  # .GlobalEnv$top_summary [18,3] = i18n$t("USD per year")
  #
  # .GlobalEnv$top_summary [19,1] = i18n$t("INCOME of links where FOCL is recommended for deployment")
  # .GlobalEnv$top_summary [19,2] = 0
  # .GlobalEnv$top_summary [19,3] = i18n$t("USD per year")
  #
  # .GlobalEnv$top_summary [20,1] = i18n$t("INCOME of links where Microwave is recommended for deployment")
  # .GlobalEnv$top_summary [20,2] = 0
  # .GlobalEnv$top_summary [20,3] = i18n$t("USD per year")
  #
  # .GlobalEnv$top_summary [21,1] = i18n$t("Total CAPEX of whole network, USD")
  # .GlobalEnv$top_summary [21,2] = 0
  # .GlobalEnv$top_summary [21,3] = i18n$t("USD")
  #
  # .GlobalEnv$top_summary [22,1] = i18n$t("Total OPEX of whole network, USD per year")
  # .GlobalEnv$top_summary [22,2] = 0
  # .GlobalEnv$top_summary [22,3] = i18n$t("USD per year")
  #
  # .GlobalEnv$top_summary [23,1] = i18n$t("Total INCOME of whole network, USD per year")
  # .GlobalEnv$top_summary [23,2] = 0
  # .GlobalEnv$top_summary [23,3] = i18n$t("USD per year")

  return (0)
}

calculate_summary <- function(result)
{
  #Total number of objects
  numberofobj <- nrow(result)

  if (numberofobj < 3)
    return (0)

  totalnumberoflinks <- 0
  techcounter_focl <- 0
  techcounter_rts <- 0
  techcounter_cel <- 0
  techcounter_sat <- 0

  moneycounter_focl <- 0
  moneycounter_rts <- 0
  moneycounter_cel <- 0
  moneycounter_sat <- 0

  capexcounter_focl <- 0
  capexcounter_rts <- 0
  capexcounter_cel <- 0
  capexcounter_sat <- 0

  opexcounter_focl <- 0
  opexcounter_rts <- 0
  opexcounter_cel <- 0
  opexcounter_sat <- 0

  incomecounter_focl <- 0
  incomecounter_rts <- 0
  incomecounter_cel <- 0
  incomecounter_sat <- 0

  for (i in 1: (numberofobj-1))
  {
    for (j in (i+1):numberofobj)
    {
      val <- result [i, j]

      #Parse format
      #finr <- paste (filtered_tech [ind1,ind2], round (as.numeric(filtered [ind1,ind2]), digits = 2) , sep = " ")

      if (!is.na(val))
      {
        onecell <- unlist(strsplit(val, " "))
        if (!is.na (onecell))
        {
          #components_number <- ncol (onecell)
          # if (components_number >= 2)
          # {
            tech <- onecell [1]
            value <- onecell [2]
            capex <- 0
            opex <- 0
            income <- 0

            # if (components_number > 2)
            # {
            #   capex <- onecell [3]
            #   opex <- onecell [4]
            #   income <- onecell [5]
            #
            # }

            totalnumberoflinks <- totalnumberoflinks + 1

            switch (tech,
                    'Fiber' = {
                      techcounter_focl <- techcounter_focl + 1
                      moneycounter_focl <- moneycounter_focl +  as.numeric(value)
                      capexcounter_focl <- capexcounter_focl +  as.numeric(capex)
                      opexcounter_focl <- opexcounter_focl +  as.numeric(opex)
                      incomecounter_focl <- incomecounter_focl +  as.numeric(income)
                    },
                    'Microwave' = {
                      techcounter_rts <- techcounter_rts  + 1
                      moneycounter_rts <- moneycounter_rts + as.numeric(value)
                      capexcounter_rts <- capexcounter_rts +  as.numeric(capex)
                      opexcounter_rts <- opexcounter_rts +  as.numeric(opex)
                      incomecounter_rts <- incomecounter_rts +  as.numeric(income)

                    },
                    'Cellular' = {
                      techcounter_cel <- techcounter_cel + 1
                      moneycounter_cel <- moneycounter_cel + as.numeric(value)
                      capexcounter_cel <- capexcounter_cel +  as.numeric(capex)
                      opexcounter_cel <- opexcounter_cel +  as.numeric(opex)

                    },
                    'Satellite' = {
                      techcounter_sat <- techcounter_sat + 1
                      moneycounter_sat <- moneycounter_sat + as.numeric(value)
                      capexcounter_sat <- capexcounter_sat +  as.numeric(capex)
                      opexcounter_sat <- opexcounter_sat +  as.numeric(opex)

                    }
            );

          #}

        }
      }

    }
  }

  moneycounter_total <- moneycounter_focl + moneycounter_rts + moneycounter_cel + moneycounter_sat
  capexcounter_total <- capexcounter_focl +  capexcounter_rts + capexcounter_cel + capexcounter_sat
  opexcounter_total <- opexcounter_focl +  opexcounter_rts + opexcounter_cel + opexcounter_sat
  incomecounter_total <- incomecounter_focl +  incomecounter_rts

  techcounter_focl <- round (techcounter_focl, digits = 0)
  techcounter_rts <-  round (techcounter_rts, digits = 0)
  techcounter_cel <-  round (techcounter_cel, digits = 0)
  techcounter_sat <-  round (techcounter_sat, digits = 0)

#  .GlobalEnv$top_summary [2,1] = i18n$t("Total number of links of whole network")
  .GlobalEnv$top_summary [2,2] = totalnumberoflinks

  if (totalnumberoflinks > 0)
  {
    #.GlobalEnv$top_summary [3,1] = i18n$t("Number of links where FOCL is recommended for deployment")
    .GlobalEnv$top_summary [3,2] <- paste (techcounter_focl, " (", round ((techcounter_focl/totalnumberoflinks)*100, digits = 2) , "%)", sep = "")

    #.GlobalEnv$top_summary [4,1] = i18n$t("Number of links where Microwave is recommended for deployment")
    .GlobalEnv$top_summary [4,2] <- paste (techcounter_rts, " (", round ((techcounter_rts/totalnumberoflinks)*100, digits = 2) , "%)", sep = "")

    #.GlobalEnv$top_summary [5,1] = i18n$t("Number of links where Satellite is recommended for deployment")
    .GlobalEnv$top_summary [5,2] <- paste (techcounter_sat, " (", round ((techcounter_sat/totalnumberoflinks)*100, digits = 2) , "%)", sep = "")

    #.GlobalEnv$top_summary [6,1] = i18n$t("Number of links where Cellular is recommended for deployment")
    .GlobalEnv$top_summary [6,2] <- paste (techcounter_cel, " (", round ((techcounter_cel/totalnumberoflinks)*100, digits = 2) , "%)", sep = "")
  }

  if (moneycounter_total != 0)
  {
    #  .GlobalEnv$top_summary [1,1] = i18n$t("Total Cost of ownership of whole network, USD")
    #  .GlobalEnv$top_summary [1,1] = i18n$t("Total NPV of whole network , USD")
    .GlobalEnv$top_summary [1,2] <- moneycounter_total

    #  .GlobalEnv$top_summary [7,1] = i18n$t("Cost of ownership of links where FOCL is recommended for deployment")
    #  .GlobalEnv$top_summary [7,1] = i18n$t("NPV of links where FOCL is recommended for deployment")
    .GlobalEnv$top_summary [7,2] <- paste (moneycounter_focl, " (", round ((moneycounter_focl/moneycounter_total)*100, digits = 2) , "%)", sep = "")

    #  .GlobalEnv$top_summary [8,1] = i18n$t("Cost of ownership of links where Microwave is recommended for deployment")
    #  .GlobalEnv$top_summary [7,1] = i18n$t("NPV of links where Microwave is recommended for deployment")
    .GlobalEnv$top_summary [8,2] <- paste (moneycounter_rts, " (", round ((moneycounter_rts/moneycounter_total)*100, digits = 2) , "%)", sep = "")

    #  .GlobalEnv$top_summary [9,1] = i18n$t("Cost of ownership of links where Satellite is recommended for deployment")
    #  .GlobalEnv$top_summary [7,1] = i18n$t("NPV of links where Satellite is recommended for deployment")
    .GlobalEnv$top_summary [9,2] <- paste (moneycounter_sat, " (", round ((moneycounter_sat/moneycounter_total)*100, digits = 2) , "%)", sep = "")

    #  .GlobalEnv$top_summary [10,1] = i18n$t("Cost of ownership of links where Cellular is recommended for deployment")
    #  .GlobalEnv$top_summary [7,1] = i18n$t("NPV of links where Cellular is recommended for deployment")
    .GlobalEnv$top_summary [10,2] <- paste (moneycounter_cel, " (", round ((moneycounter_cel/moneycounter_total)*100, digits = 2) , "%)", sep = "")

  }

  # if (capexcounter_total != 0)
  # {
  #   #.GlobalEnv$top_summary [21,1] = i18n$t("Total CAPEX of whole network, USD")
  #   .GlobalEnv$top_summary [21,2] <- capexcounter_total
  #
  #   #.GlobalEnv$top_summary [11,1] = i18n$t("CAPEX of links where FOCL is recommended for deployment")
  #   .GlobalEnv$top_summary [11,2] <- paste (capexcounter_focl, " (", round ((capexcounter_focl/capexcounter_total)*100, digits = 2) , "%)", sep = "")
  #
  #   #.GlobalEnv$top_summary [12,1] = i18n$t("CAPEX of links where Microwave is recommended for deployment")
  #   .GlobalEnv$top_summary [12,2] <- paste (capexcounter_rts, " (", round ((capexcounter_rts/capexcounter_total)*100, digits = 2) , "%)", sep = "")
  #
  #   #.GlobalEnv$top_summary [13,1] = i18n$t("CAPEX of links where Satellite is recommended for deployment")
  #   .GlobalEnv$top_summary [13,2] <- paste (capexcounter_sat, " (", round ((capexcounter_sat/capexcounter_total)*100, digits = 2) , "%)", sep = "")
  #
  #   .GlobalEnv$top_summary [14,1] = i18n$t("CAPEX of links where Cellular is recommended for deployment")
  #   .GlobalEnv$top_summary [14,2] <- paste (capexcounter_cel, " (", round ((capexcounter_cel/capexcounter_total)*100, digits = 2) , "%)", sep = "")
  # }
  #
  #
  #
  # if (opexcounter_total != 0)
  # {
  #   #.GlobalEnv$top_summary [22,1] = i18n$t("Total OPEX of whole network, USD per year")
  #   .GlobalEnv$top_summary [22,2] <- opexcounter_total
  #
  #   #.GlobalEnv$top_summary [15,1] = i18n$t("OPEX of links where FOCL is recommended for deployment")
  #   .GlobalEnv$top_summary [15,2] <- paste (opexcounter_focl, " (", round ((opexcounter_focl/opexcounter_total)*100, digits = 2) , "%)", sep = "")
  #
  #   #.GlobalEnv$top_summary [16,1] = i18n$t("OPEX of links where Microwave is recommended for deployment")
  #   .GlobalEnv$top_summary [16,2] <- paste (opexcounter_rts, " (", round ((opexcounter_rts/opexcounter_total)*100, digits = 2) , "%)", sep = "")
  #
  #   #.GlobalEnv$top_summary [17,1] = i18n$t("OPEX of links where Satellite is recommended for deployment")
  #   .GlobalEnv$top_summary [17,2] <- paste (opexcounter_sat, " (", round ((opexcounter_sat/opexcounter_total)*100, digits = 2) , "%)", sep = "")
  #
  #   #.GlobalEnv$top_summary [18,1] = i18n$t("OPEX of links where Cellular is recommended for deployment")
  #   .GlobalEnv$top_summary [18,2] <- paste (opexcounter_cel, " (", round ((opexcounter_cel/opexcounter_total)*100, digits = 2) , "%)", sep = "")
  # }
  #
  # if (incomecounter_total != 0)
  # {
  #   #.GlobalEnv$top_summary [23,1] = i18n$t("Total INCOME of whole network, USD per year")
  #   .GlobalEnv$top_summary [23,2] <- incomecounter_total
  #
  #   #.GlobalEnv$top_summary [19,1] = i18n$t("INCOME of links where FOCL is recommended for deployment")
  #   .GlobalEnv$top_summary [19,2] <- paste (incomecounter_focl, " (", round ((incomecounter_focl/incomecounter_total)*100, digits = 2) , "%)", sep = "")
  #
  #   #.GlobalEnv$top_summary [20,1] = i18n$t("INCOME of links where Microwave is recommended for deployment")
  #   .GlobalEnv$top_summary [20,2] <- paste (incomecounter_rts, " (", round ((incomecounter_rts/incomecounter_total)*100, digits = 2) , "%)", sep = "")
  # }


  return (0)
}


calc_one_claster <- function(input, objects, intermediate = NULL)
{
  bUseFocl <- F
  bUseRts <- F
  bUseSatellite <- F
  bUseCellular <- F

  if  (input$InitialData.UseFOCL == 1)
    bUseFocl <- T

  if (input$InitialData.UseRTS == 1)
    bUseRts <- T

  if (input$InitialData.UseSatellite == 1)
    bUseSatellite <- T

  if (input$InitialData.UseCellular == 1)
    bUseCellular <- T


  #Total number of objects for processing (N)
  numberofobj <- nrow(objects)

  #Empty matrix N x N for storing distances
  distancematrix <- matrix (nrow = numberofobj, ncol = numberofobj)

  #Empty matrix N x N for storing log-records about links in the network
  logmatrix <- matrix (nrow = numberofobj, ncol = numberofobj)


  for (i in 1: (numberofobj-1))
  {
    for (j in (i+1):numberofobj)
    {
      #Coordinates of the first point, f.e. i=1
      lon1 <- objects [i,2]
      lat1 <- objects [i,3]

      #Coordinates of the second point, f.e. i+1 = 2
      lon2 <- objects [j,2]
      lat2 <- objects [j,3]

      #Line of sight distance in meters
      distancem <- distm (c(as.numeric (lon1), as.numeric (lat1)),c(as.numeric (lon2), as.numeric (lat2)),fun=distHaversine)

      #Line of sight distance in km
      distancekm <- round (as.numeric (distancem/1000), digits = 2)

      #Topography coefficient (taking into account that roads are going not by the line og sight)
      #In the future versions should be changed  to the real distance by the roads received from any geomap system
      #Coefficient should be between 0 and 1
      TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

      #Distance by the roads in km
      distancekmbyroads <- round (distancekm * (1 + TopCoeff), digits = 2)

      #Symetric filling of the matrices
      distancematrix [i,j] <- distancekm
      distancematrix [j,i] <- distancekm

      logmatrix [i, j] <-  paste (i18n$t("Distance (line of sight), km: "), distancekm, i18n$t("\r\n Distance by roads (assumed), km: "), distancekmbyroads, sep = "")
      logmatrix [j, i] <-  paste (i18n$t("Distance (line of sight), km: "), distancekm, i18n$t("\r\n Distance by roads (assumed), km: "), distancekmbyroads, sep = "")

    }

    #Filling the main diagonal of the matrices
    distancematrix [i,i] <- 0
    rb <- objects [i,4]
    logmatrix [i,i] <-  paste (i18n$t("Node #: "), i, i18n$t("\r\n Required bandwidth, Mbps: "), rb, sep = "")
  }

  #Filling the last cell of the matrices
  distancematrix [numberofobj,numberofobj] <- 0
  rb <- objects [numberofobj,4]

  logmatrix [numberofobj,numberofobj] <-  paste (i18n$t("Required bandwidth, Mbps: "), rb, sep = "")


  .GlobalEnv$top_calc_status  = 101

  #currentTs <- Sys.time()
  # elapsed <- Sys.time() - currentTs
  # timestr <- paste ("Distance Matrix",elapsed, sep = " ")
  # print (timestr)
  # currentTs <- Sys.time()


  #Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
  input2 <- input

  if (is.reactivevalues (input))
    input2 <- reactiveValuesToList (input)


  input2$bWriteLog <- F

  bCalcNPV <- F
  if (input2$InitialData.UseNPV == 1)
    bCalcNPV <- T

  input2$InitialData.UseNPV <- as.character(input2$InitialData.UseNPV)

  #Step 4: NPV or Cost of ownership matrix calculation (fiber vs microwave)

  #Empty matrix N x N for storing plereminary results of calculations (digits)
  npvmatrix <- matrix (nrow = numberofobj, ncol = numberofobj)

  #Empty matrix N x N for storing plereminary results of calculations (technology name)
  techmatrix <- matrix (nrow = numberofobj, ncol = numberofobj)


  for (i in 1: (numberofobj-1))
  {
    rb <- objects [i,4]


    for (j in (i+1):numberofobj)
    {

      distancekm <- as.numeric (distancematrix [i,j])

      input2$SchoolSpecificData.Length <- distancekm

      input2$SchoolSpecificData.RequiredBandwidth <- rb

      #ALGORITHM2_3
      #Possible maximum for Cost of Ownership
      ownership_f = 1000000000000
      ownership_m = 1000000000000
      capex_f = 0
      opex_f = 0
      income_f = 0

      #Possible minimum for NPV
      npv_f = -1000000000000
      npv_m = -1000000000000
      capex_m = 0
      opex_m = 0
      income_m = 0


      if (bUseFocl)
      {

        #Coefficient should be between 0 and 1
        TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

        #Distance by the roads in km
        distancekmbyroads <- round (distancekm * (1 + TopCoeff), digits = 2)

        input2$SchoolSpecificData.Length <- distancekmbyroads

        #Algorithm for overall FOCL construction cost evaluation

        focl_capex <- NULL
        focl_capex =  algorithm2_1_impl (input2)
        if (is.null (focl_capex))
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate overall FOCL construction cost for some link"))
          .GlobalEnv$top_calc_status  = 408
          return (0)
        }

        #Overall cost of FOCL installation
        TotalCAPEXFOCL =  as.numeric (focl_capex [1,2])

        #Overall length of the FOCL construction site
        FOCLLenghtTotal = as.numeric (focl_capex [2,2])

        FOCLSectionLengthCD =   as.numeric (focl_capex [3,2])

        #Сost of equipment and materials for the construction of fiber optic lines
        CostOfEqAndMatFOCL =  as.numeric (focl_capex [4,2])


        #Algorithm for overall FOCL maintenance cost evaluation
        intermediate51 <- list (RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
        intermediate51$RequiredCapacity  <- rb
        intermediate51$FOCLSectionLengthCD <- FOCLSectionLengthCD
        intermediate51$Technology <- 0


        intermediate2 <- list (FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
        intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
        intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
        intermediate2$RequiredCapacity  <- rb

        focl_opex <- NULL
        focl_opex =  algorithm2_2_impl (input2, intermediate2)
        if (is.null (focl_opex))
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate overall FOCL maintenance cost for some link"))
          .GlobalEnv$top_calc_status  = 407
          return (0)
        }

        #Total cost for FOCL maintenance for the entire period of operation
        TotalOPEXFOCL =  as.numeric (focl_opex [1,2])


        #Cost of ownership for the period
        ownership_f = TotalCAPEXFOCL  + TotalOPEXFOCL * as.numeric (input$PVOptionSet.PaybackPeriod)
        capex_f = TotalCAPEXFOCL
        opex_f = TotalOPEXFOCL


        if (bCalcNPV)
        {
          focl_income <- NULL
          focl_income =  algorithm2_12_impl (input2, intermediate51)
          if (is.null (focl_income))
          {
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate potential income from FOCL for some link"))
            .GlobalEnv$top_calc_status  = 406
            return (0)
          }


          NetIncome <- as.numeric (focl_income [1,2])
          income_f = NetIncome

          #NPV - FOCL
          intermediate5 <- list (NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
          TotalInvest <- TotalCAPEXFOCL
          CostOfOperation  <- TotalOPEXFOCL
          CostOfEquipmentAndMaterials  <- CostOfEqAndMatFOCL
          intermediate5$TotalInvest <- TotalInvest
          intermediate5$CostOfOperation <- CostOfOperation
          intermediate5$NetIncome <- NetIncome
          intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
          intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

          npv_focl <- NULL
          npv_focl <- algorithm2_7_impl (input2, intermediate5)

          if (is.null (npv_focl))
          {
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV of FOCL for some link"))
            .GlobalEnv$top_calc_status  = 405
            return (0)
          }

          npv_f = as.numeric(npv_focl[1,2])
        }


      }


      if (bUseRts)
      {

        #Algorithm for total RTS construction cost evaluation between object and SN in locality
        rts_capex <- NULL
        rts_capex =  algorithm2_3_impl (input2, intermediate)
        if (is.null (rts_capex))
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate total RTS construction cost for some link"))
          .GlobalEnv$top_calc_status  = 508
          return (0)
        }


        TotalCAPEXRTS <-   as.numeric (rts_capex [1,2])

        NumberOfRepeaters <-   as.numeric (rts_capex [2,2])

        #Total cost of equipment and materials for the construction of the RTS
        CostOfEqAndMatRTS <-   as.numeric (rts_capex [3,2])

        WidthFrequencyChannel <- as.numeric (rts_capex [4,2])


        #Algorithm for total RTS maintenance cost evaluation between object and SN in locality

        intermediate3 <- list (NumberOfRepeaters = 0, RequiredCapacity = 0.0, NumberOfTerminals = 0.0, WidthFrequencyChannel = 0.0)
        intermediate3$NumberOfRepeaters <- NumberOfRepeaters
        intermediate3$RequiredCapacity  <- rb
        intermediate3$NumberOfTerminals <- 2
        intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel


        if (TotalCAPEXRTS == 0)
        {
          intermediate3$NumberOfTerminals <- 0
        }

        rts_opex <- NULL
        rts_opex =  algorithm2_4_impl (input2, intermediate3)

        if (is.null (rts_opex))
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate total RTS maintenance cost for some link"))
          .GlobalEnv$top_calc_status  = 507
          return (0)
        }

        TotalOPEXRTS = as.numeric (rts_opex [1,2])



        #Cost of ownership for the period
        ownership_m <- TotalCAPEXRTS  + TotalOPEXRTS * as.numeric (input$PVOptionSet.PaybackPeriod)
        capex_m = TotalCAPEXRTS
        opex_m = TotalOPEXRTS


        if (bCalcNPV)
        {
          #NPV - Microwave

          intermediate51 <- list (RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
          intermediate51$RequiredCapacity  <- rb
          intermediate51$Technology <- 1

          rts_income <- NULL
          rts_income =  algorithm2_12_impl (input2, intermediate51)
          if (is.null (rts_income))
          {
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate potential income from RTS for some link"))
            .GlobalEnv$top_calc_status  = 506
            return (0)
          }


          NetIncome <- as.numeric (rts_income [1,2])
          income_m = NetIncome

          intermediate5 <- list (NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)

          TotalInvest <- TotalCAPEXRTS
          CostOfOperation  <- TotalOPEXRTS
          CostOfEquipmentAndMaterials  <- CostOfEqAndMatRTS

          intermediate5$TotalInvest <- TotalInvest
          intermediate5$CostOfOperation <- CostOfOperation
          intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
          intermediate5$NetIncome <- NetIncome
          intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

          npv_microwave <- NULL
          npv_microwave <- algorithm2_7_impl (input2, intermediate5)
          if (is.null (npv_microwave))
          {
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV for RTS for some link"))
            .GlobalEnv$top_calc_status  = 505
            return (0)
          }

          npv_m <- as.numeric(npv_microwave[1,2])

        }

      }

      ownership <- 0
      npv <- 0
      capex <- 0
      opex <- 0
      income <- 0

      mt <- "Unknown"

      foclres <- ""
      rtsres <- ""



      switch (input2$InitialData.UseNPV,
              '0' = {#Cost of ownership
                if (bUseFocl && bUseRts)
                {
                  if (ownership_f < ownership_m)
                  {
                    ownership <- ownership_f
                    capex <- capex_f
                    opex <- opex_f
                    income <- income_f

                    mt <- "Fiber"
                  }
                  else
                  {
                    ownership <- ownership_m
                    capex <- capex_m
                    opex <- opex_m
                    income <- income_m

                    mt <- "Microwave"
                  }
                }
                else if (bUseFocl)
                {
                  ownership <- ownership_f
                  capex <- capex_f
                  opex <- opex_f
                  income <- income_f

                  mt <- "Fiber"
                }
                else if (bUseRts)
                {
                  ownership <- ownership_m
                  capex <- capex_m
                  opex <- opex_m
                  income <- income_m

                  mt <- "Microwave"
                }

                foclres <- paste ("Fiber (cost of ownership)", round (as.numeric(ownership_f), digits = 2), sep = ":")
                rtsres <- paste (i18n$t("Microwave (cost of ownership)"), round (as.numeric(ownership_m), digits = 2), sep = ":")

              },
              '1' = {#NPV

                if (bUseFocl && bUseRts)
                {
                  if (npv_f > npv_m)
                  {
                    npv <- npv_f
                    capex <- capex_f
                    opex <- opex_f
                    income <- income_f

                    mt <- "Fiber"
                  }
                  else
                  {
                    npv <- npv_m
                    capex <- capex_m
                    opex <- opex_m
                    income <- income_m

                    mt <- "Microwave"
                  }
                }
                else if (bUseFocl)
                {
                  npv <- npv_f
                  capex <- capex_f
                  opex <- opex_f
                  income <- income_f

                  mt <- "Fiber"
                }
                else if (bUseRts)
                {
                  npv <- npv_m
                  capex <- capex_m
                  opex <- opex_m
                  income <- income_m

                  mt <- "Microwave"
                }

                foclres <- paste (i18n$t("Fiber (NPV)"), round (as.numeric(npv_f), digits = 2), sep = ":")
                rtsres <- paste (i18n$t("Microwave (NPV)"), round (as.numeric(npv_m), digits = 2), sep = ":")
              }
      );



      logstringold <- logmatrix [i,j]


      if (!bUseFocl)
      {
        foclres <- "Fiber: not allowed"
      }

      if (!bUseRts)
      {
        rtsres <- i18n$t("Microwave: not allowed")
      }

      logstringnew <- paste (foclres, rtsres, i18n$t("\r\nWinner:"), mt, sep = " ")


      switch (input2$InitialData.UseNPV,
              '0' = {#Cost of ownership
                npvmatrix [i,j] <- round (as.numeric(ownership), digits = 2)
                npvmatrix [j,i] <- round (as.numeric(ownership), digits = 2)

              },
              '1' = {#NPV

                npvmatrix [i,j] <- round (as.numeric(npv), digits = 2)
                npvmatrix [j,i] <- round (as.numeric(npv), digits = 2)

              }
      )



      techmatrix [i,j] <- mt
      techmatrix [j,i] <- mt

      logmatrix [i,j] <- paste (logstringold, logstringnew, sep = "\r\n")
      logmatrix [j,i] <- paste (logstringold, logstringnew, sep = "\r\n")

    }
    npvmatrix [i,i] <- 0
    techmatrix [i,i] <- i18n$t("N/a")

  }

  npvmatrix [numberofobj,numberofobj] <- 0
  techmatrix [numberofobj,numberofobj] <- i18n$t("N/a")

  # elapsed <- Sys.time() - currentTs
  # timestr <- paste ("NPV Matrix",elapsed, sep = " ")
  # print (timestr)
  # currentTs <- Sys.time()

  .GlobalEnv$top_calc_status  = 102

  #Step 5: Filtering of result by cellular and sattelite

  #Empty matrices N x N for storing filtering results
  filtered <- matrix (nrow = numberofobj, ncol = numberofobj)
  filtered_tech <- matrix (nrow = numberofobj, ncol = numberofobj)


  for (i in 1:numberofobj)
  {
    for (j in 1:numberofobj)
    {
      val <- npvmatrix [i,j]
      filtered [i,j] <- as.numeric(val)
      val2 <- techmatrix [i,j]
      filtered_tech [i,j] <- val2
    }
  }


  #Step 5-1: Detect nodes with existing fiber connection or nearest to it
  #Empty vector of basic nodes - maximum is N nodes (all nodes are basic)
  nodeindexes <- matrix (nrow = numberofobj, ncol = 1)

  numberofnodes <- 0
  defaultnodeindex <- 1
  minval <- objects [1,5]

  for (i in 1: numberofobj)
  {
    dtf <- objects [i,5]

    if (dtf == 0)
    {
      numberofnodes <- numberofnodes + 1
      nodeindexes [numberofnodes, 1] <- i
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

  #Step 5-2: At least one node will be selected as basic (if no nodes with 0 distance to the fiber - the node with minimum distance will be selected)
  if (numberofnodes == 0)
  {
    numberofnodes <- 1
    nodeindexes [numberofnodes, 1] <- defaultnodeindex
  }

  #CLASTERING SHOULD BE ADDED ONLY IF WE HAVE MORE THAN ONE BASIC NODE

  #Step 5-3: NPV matrix filtering (by possible satellite or cellular connections)
  #Calculation cost of connectivity via satellite for each nodes (except base nodes) and cost of connectivity via cellular for each nodes where 3G or 4G  coverage exist (except base nodes)


  if (bUseSatellite || bUseCellular)
  {

    #Filtering by satellites or cellular
    numberofsats <- 0
    numberofcells <- 0
    satindexes <- matrix (nrow = numberofobj, ncol = 2)
    cellindexes <- matrix (nrow = numberofobj, ncol = 2)


    for (i in 1: numberofobj)
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

      rb <- objects [i,4]
      ownership_s <- 100000000000000
      npv_s <- -100000000000000
      capex_s = 0
      opex_s = 0



      if (bUseSatellite)
      {
        intermediate4 <- list (NumberVSATsets = 0, RequiredCapacity = 0.0)
        intermediate4$RequiredCapacity  <- rb

        sat_capex <- NULL
        sat_capex =  algorithm2_5_impl (input2, intermediate4)
        if (is.null (sat_capex))
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate CAPEX of satellite for some link"))
          .GlobalEnv$top_calc_status  = 703
          return (0)
        }


        # Required number of VSAT sets

        NumberVSATsets = as.numeric (sat_capex [2,2])

        TotalCAPEXSetellite =  as.numeric (sat_capex [1,2])

        #Total cost of VSAT equipment and installation materials
        CostOfVSATEqAndMat =  as.numeric (sat_capex [3,2])

        #Algorithm for determining the total cost of the maintenance of the satellite communication channel


        intermediate4$NumberVSATsets <- NumberVSATsets


        sat_opex <- NULL
        sat_opex =  algorithm2_6_impl (input2, intermediate4)
        if (is.null (sat_opex))
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate OPEX of satellite for some link"))
          .GlobalEnv$top_calc_status  = 702
          return (0)
        }

        TotalOPEXSatellite =  as.numeric (sat_opex [1,2])



        ownership_s <- TotalCAPEXSetellite  + TotalOPEXSatellite * as.numeric (input$PVOptionSet.PaybackPeriod)
        capex_s = TotalCAPEXSetellite
        opex_s = TotalOPEXSatellite


        if (bCalcNPV)
        {
          #NPV - Satellite
          intermediate5 <- list (NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
          TotalInvest <- TotalCAPEXSetellite
          CostOfOperation  <- TotalOPEXSatellite
          CostOfEquipmentAndMaterials  <- CostOfVSATEqAndMat

          intermediate5$NetIncome <- 0
          intermediate5$TotalInvest <- TotalInvest
          intermediate5$CostOfOperation <- CostOfOperation
          intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
          intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

          npv_satellite <- NULL
          npv_satellite <- algorithm2_7_impl (input2, intermediate5)
          if (is.null (npv_satellite))
          {
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV of satellite for some link"))
            .GlobalEnv$top_calc_status  = 701
            return (0)
          }

          npv_s <- as.numeric(npv_satellite[1,2])

        }


      }

      ownership_cellular <- 100000000000000
      npv_cellular <- -100000000000000
      capex_cellular = 0
      opex_cellular = 0


      cellstatus <- "2G"

      if (bUseCellular)
      {

        cellstatus <- objects [i,6]

        if (!is.na(cellstatus))
        {
          if ((cellstatus == "3G") || (cellstatus == "4G"))
          {
            #Algorithm for determining the total cost of the installation and configuration of the cellular communication channel

            intermediate41 <- list (NumberCellularsets = 0, RequiredCapacity = 0.0)
            intermediate41$RequiredCapacity  <- rb

            cel_capex <- NULL
            cel_capex =  algorithm2_10_impl (input, intermediate41)
            if (is.null (cel_capex))
            {
              .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
              .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate CAPEX of cellular for some link"))
              .GlobalEnv$top_calc_status  = 603
              return (0)
            }


            # Required number of Cellular sets

            NumberCellularsets = as.numeric (cel_capex [2,2])

            TotalCAPEXCellular =  as.numeric (cel_capex [1,2])

            #Total cost of Cellular equipment and installation materials
            CostOfCellularEqAndMat =  as.numeric (cel_capex [3,2])


            #Algorithm for determining the total cost of the maintenance of the Cellular communication channel


            intermediate41$NumberCellularsets <- NumberCellularsets

            cel_opex <- NULL
            cel_opex =  algorithm2_11_impl (input, intermediate41)
            if (is.null (cel_opex))
            {
              .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
              .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate OPEX of cellular for some link"))
              .GlobalEnv$top_calc_status  = 602
              return (0)
            }

            TotalOPEXCellular =  as.numeric (cel_opex [1,2])



            ownership_cellular <- TotalCAPEXCellular  + TotalOPEXCellular * as.numeric (input$PVOptionSet.PaybackPeriod)
            capex_cellular = TotalCAPEXCellular
            opex_cellular = TotalOPEXCellular

            if (bCalcNPV)
            {
              #NPV - Cellular
              intermediate5 <- list (NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
              TotalInvest <- TotalCAPEXCellular
              CostOfOperation  <- TotalOPEXCellular
              CostOfEquipmentAndMaterials  <- CostOfCellularEqAndMat

              intermediate5$NetIncome <- 0
              intermediate5$TotalInvest <- TotalInvest
              intermediate5$CostOfOperation <- CostOfOperation
              intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
              intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

              npv_cell <- NULL
              npv_cell <- algorithm2_7_impl (input, intermediate5)
              if (is.null (npv_cell))
              {
                .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
                .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV of cellular for some link"))
                .GlobalEnv$top_calc_status  = 601
                return (0)
              }

              npv_cellular <- as.numeric(npv_cell[1,2])

            }

          }
        }
        else
          cellstatus <- "2G"

      }


      #The connectivity will be marked as reasonable only if satelllite or cellular is better than any other direct connection of the node


      change_to_cell <- F

      if (bUseCellular)
      {

        for (j in 1:numberofobj)
        {
          if (i == j)
            next


          ownership <- as.numeric (npvmatrix [i,j])
          npv <- as.numeric (npvmatrix [i,j])

          switch (input2$InitialData.UseNPV,
                  '0' = {#Cost of ownership
                    if ((cellstatus == "3G") || (cellstatus == "4G"))
                    {
                      if (ownership_cellular < ownership)
                      {
                        change_to_cell <- T
                      }
                      else
                      {
                        change_to_cell <- F
                        break
                      }
                    }

                  },
                  '1' = {#NPV
                    if ((cellstatus == "3G") || (cellstatus == "4G"))
                    {
                      if (npv_cellular > npv)
                      {
                        change_to_cell <- T
                      }
                      else
                      {
                        change_to_cell <- F
                        break
                      }
                    }
                  }
          )


        }

      }



      change_to_sat <- F

      if (bUseSatellite)
      {
        for (j in 1:numberofobj)
        {
          if (i == j)
            next


          ownership <- as.numeric (npvmatrix [i,j])
          npv <- as.numeric (npvmatrix [i,j])

          switch (input2$InitialData.UseNPV,
                  '0' = {#Cost of ownership
                    if (ownership_s < ownership)
                    {
                      change_to_sat <- T
                    }
                    else
                    {
                      change_to_sat <- F
                      break
                    }

                  },
                  '1' = {#NPV
                    if (npv_s > npv)
                    {
                      change_to_sat <- T
                    }
                    else
                    {
                      change_to_sat <- F
                      break
                    }

                  }
          )

        }

      }


      #Choosing the most efficient if both are effective
      if (change_to_sat && change_to_cell)
      {

        switch (input2$InitialData.UseNPV,
                '0' = {#Cost of ownership
                  if (ownership_cellular < ownership_s)
                  {
                    change_to_sat <- T
                  }
                  else
                  {
                    change_to_cell <- F
                    break
                  }

                },
                '1' = {#NPV

                  if (npv_cellular > npv_s)
                  {
                    change_to_sat <- T
                  }
                  else
                  {
                    change_to_cell <- F
                    break
                  }

                }
        )

      }



      if (change_to_sat)
      {
        numberofsats <- numberofsats + 1
        satindexes[numberofsats, 1] <- i
        switch (input2$InitialData.UseNPV,
                '0' = {#Cost of ownership
                  satindexes[numberofsats, 2] <- round (as.numeric(ownership_s), digits = 2)
                },
                '1' = {#NPV

                  satindexes[numberofsats, 2] <- round (as.numeric(npv_s), digits = 2)
                }
        )

      }

      if (change_to_cell)
      {
        numberofcells <- numberofcells + 1
        cellindexes[numberofcells, 1] <- i
        switch (input2$InitialData.UseNPV,
                '0' = {#Cost of ownership
                  cellindexes[numberofcells, 2] <- round (as.numeric(ownership_cellular), digits = 2)
                },
                '1' = {#NPV

                  cellindexes[numberofcells, 2] <- round (as.numeric(npv_cellular), digits = 2)
                }
        )

      }
    }


  }


  if (bUseSatellite)
  {
    basenode <- nodeindexes [1,1]

    if (numberofsats > 0)
    {
      for (i in 1: numberofsats)
      {
        satindex <- satindexes [i,1]

        for (j in 1:numberofobj)
        {
          filtered [satindex,j] <- NA
          filtered [j,satindex] <- NA

          filtered_tech [satindex,j] <- NA
          filtered_tech [j,satindex] <- NA

          if (satindex == j)
          {
            filtered [j,satindex] <- 0
            filtered_tech [j,satindex] <- 0
          }
        }



        filtered [satindex,basenode] <- satindexes [i,2]
        filtered [basenode, satindex] <- satindexes [i,2]

        filtered_tech [satindex,basenode] <- i18n$t("Satellite")
        filtered_tech [basenode, satindex] <- i18n$t("Satellite")

        logstringold <- logmatrix [satindex,basenode]

        ##CHOOSE
        switch (input2$InitialData.UseNPV,
                '0' = {#Cost of ownership
                  satres <- paste (i18n$t("Satellite (cost of ownership)"), round (as.numeric(satindexes [i,2]), digits = 2), sep = ":")
                },
                '1' = {#NPV
                  satres <- paste (i18n$t("Satellite (NPV)"), round (as.numeric(satindexes [i,2]), digits = 2), sep = ":")
                }
        )



        logstringnew <- paste (satres, i18n$t("\r\nWinner:"), i18n$t("Satellite"), sep = " ")


        logmatrix [satindex,basenode] <- paste (logstringold, logstringnew, sep = "\r\n")
        logmatrix [basenode, satindex] <- paste (logstringold, logstringnew, sep = "\r\n")


      }

    }
  }

  if (bUseCellular)
  {

    basenode <- nodeindexes [1,1]

    if (numberofcells > 0)
    {

      for (i in 1: numberofcells)
      {
        cellindex <- cellindexes [i,1]

        for (j in 1:numberofobj)
        {
          filtered [cellindex,j] <- NA
          filtered [j,cellindex] <- NA

          filtered_tech [cellindex,j] <- NA
          filtered_tech [j,cellindex] <- NA

          if (cellindex == j)
          {
            filtered [j,cellindex] <- 0
            filtered_tech [j,cellindex] <- 0
          }
        }

        filtered [cellindex,basenode] <- cellindexes [i,2]
        filtered [basenode, cellindex] <- cellindexes [i,2]

        filtered_tech [cellindex,basenode] <- i18n$t("Cellular")
        filtered_tech [basenode, cellindex] <- i18n$t("Cellular")


        logstringold <- logmatrix [cellindex,basenode]

        switch (input2$InitialData.UseNPV,
                '0' = {#Cost of ownership
                  cellres <- paste (i18n$t("Cellular (cost of ownership)"), round (as.numeric(cellindexes [i,2]), digits = 2), sep = ":")
                },
                '1' = {#NPV
                  cellres <- paste (i18n$t("Cellular (NPV)"), round (as.numeric(cellindexes [i,2]), digits = 2), sep = ":")

                }
        )


        logstringnew <- paste ( cellres, i18n$t("\r\nWinner:"), i18n$t("Cellular"), sep = " ")


        logmatrix [cellindex,basenode] <- paste (logstringold, logstringnew, sep = "\r\n")
        logmatrix [basenode, cellindex] <- paste (logstringold, logstringnew, sep = "\r\n")

      }

    }

  }

  # elapsed <- Sys.time() - currentTs
  # timestr <- paste ("Filtering by Sattelite & Cellular",elapsed, sep = " ")
  # print (timestr)
  # currentTs <- Sys.time()

  .GlobalEnv$top_calc_status  = 103

  #Step 6: Identification duplication links for removing (closed triangles)
  #Empty vector of links for removing
  links_for_removing <- matrix (nrow = 0, ncol = 2)

  if (numberofobj > 3)
  {
    for (i in 1: (numberofobj-2))
    {
      for (j in (i+1):(numberofobj-1))
      {
        for (f in (j+1):numberofobj)
        {
          val_ij <- filtered [i,j]
          val_if <- filtered [i,f]
          val_jf <- filtered [j,f]

          if ((!is.na (val_ij)) && (!is.na (val_if)) && (!is.na (val_jf)))
          {
            #Closed triangle detected
            record <- NULL

            all_records <- c (val_ij, val_if, val_jf)



            switch (input2$InitialData.UseNPV,
                    '0' = {#Cost of ownership
                      indexofmax <- which.max(all_records)


                      if (!is.na (indexofmax))
                      {
                        if (indexofmax == 1)
                        {
                          #Link between i and j has maximum cost of ownership
                          record <- c (i,j)
                        }

                        if (indexofmax == 2)
                        {
                          #Link between i and f has maximum cost of ownership
                          record <- c (i,f)
                        }

                        if (indexofmax == 3)
                        {
                          #Link between j and f has maximum cost of ownership
                          record <- c (j,f)
                        }

                      }

                    },
                    '1' = {#NPV
                      indexofmin <- which.min(all_records)


                      if (!is.na (indexofmin))
                      {
                        if (indexofmin == 1)
                        {
                          #Link between i and j has minimal NPV
                          record <- c (i,j)
                        }

                        if (indexofmin == 2)
                        {
                          #Link between i and f has minimal NPV
                          record <- c (i,f)
                        }

                        if (indexofmin == 3)
                        {
                          #Link between j and f has minimal NPV
                          record <- c (j,f)
                        }

                      }


                    }
            )




            if (!is.null(record))
              links_for_removing <- rbind(links_for_removing, record)



          }
        }
      }
    }


    links_for_removing <- unique(links_for_removing)



    number_for_removing = nrow (links_for_removing)




    for (i in 1: (numberofobj-1))
    {
      for (j in (i+1):(numberofobj))
      {


        L1 <- F
        L2 <- F

        if (number_for_removing > 0)
        {
          for (f in 1:number_for_removing)
          {
            val1 <- links_for_removing[f,1]
            val2 <- links_for_removing[f,2]

            if ((i == val1) && (j == val2))
            {
              L1 <- T
              break()
            }

            if ((i == val2) && (j == val1))
            {
              L2 <- T
              break()
            }
          }
        }


        if (L1 || L2)
        {
          #Link will be removed
          filtered [i,j] <- NA
          filtered [j,i] <- NA
        }

      }

    }

  }

  # elapsed <- Sys.time() - currentTs
  # timestr <- paste ("Removing redundant links (closed triangles)",elapsed, sep = " ")
  # print (timestr)
  # currentTs <- Sys.time()

  .GlobalEnv$top_calc_status  = 104

  #Step 7: Optimization of whole network by excluding big loops

  filtered_positive <- filtered

  #Multiplication by -1 to make all digits positive, after this - less is better for both cases NPV and Cost of ownership
  switch (input2$InitialData.UseNPV,
          '0' = {#Cost of ownership
            #Do nothing
          },
          '1' = {#NPV

            filtered_positive <- filtered * -1
          }
  )

  filtered_positive[is.na(filtered)] <- 0

  g <- NULL
  g <- graph.adjacency(filtered_positive, weighted=TRUE)


  if (is.null (g))
  {
    .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot construct graph for the network"))
    .GlobalEnv$top_calc_status  = 311
    return (0)
  }


  (s.paths <- shortest.paths(g, algorithm = "dijkstra"))

  if (is.null (s.paths))
  {
    .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot optimize network by using dijkstra"))
    .GlobalEnv$top_calc_status  = 310
    return (0)
  }


  sums <- vector (length = numberofobj)

  for (i in 1:numberofobj)
  {
    sums [i] <- sum (s.paths [i,])
  }

  #Center of the network (plase where sum of paths to all other nodes are minimal)
  indexofmin <- which.min(sums)

  #Empty matrix N x N for storing final results

  result <- matrix (nrow = numberofobj, ncol = numberofobj)

  #Removing some loops if there are more than one path from the central node to the some node

  if (indexofmin > 0)
  {
    .GlobalEnv$centralpointforclaster <- indexofmin

    #shp <- get.all.shortest.paths(g, from = indexofmin, to = V(g))
    shp <- get.shortest.paths(g, from = indexofmin, to = V(g))

    count <- length(shp$vpath)


    if (count >= 1)
    {
      for (j in 1:count)
      {
        elem <- shp$vpath[[j]]


        len <- length(elem)


        if (len >= 2)
        {
          for (f in 1: (len-1))
          {
            ind1 <- elem[f]
            ind2 <- elem [f+1]

            finr <- paste (filtered_tech [ind1,ind2], round (as.numeric(filtered [ind1,ind2]), digits = 2) , sep = " ")


            result [ind1, ind2] <- finr
            result [ind2, ind1] <- finr


          }
        }

      }

    }


  }



  for (i in 1:(numberofobj-1))
  {
    for (j in (i+1):numberofobj)
    {
      if (i == j)
        next

      val <- result [i,j]
      if (is.na (val))
      {
        logstringold <- logmatrix [i,j]
        logstringnew <- "Removed as not optimal"


        logmatrix [i,j] <- paste (logstringold, logstringnew, sep = "\r\n")
        logmatrix [j,i] <- paste (logstringold, logstringnew, sep = "\r\n")
      }
    }
  }

  # elapsed <- Sys.time() - currentTs
  # timestr <- paste ("Removing loops in the network (including shortest paths indentification by dijkstra)",elapsed, sep = " ")
  # print (timestr)
  # currentTs <- Sys.time()

  .GlobalEnv$top_calc_status  = 105

  #Step 8: Clastering

  bDoClastering <- F

  if  (input2$TopologySettings.MakeClastering == 1)
    bDoClastering <- T

  bRemoveLinks <- F

  if  (input2$TopologySettings.RemoveInterclasterLinks == 1)
    bRemoveLinks <- T


  if ((numberofnodes > 1) && bDoClastering)
  {

    nodesbybasenodes <- matrix (nrow = 0, ncol = 2)

    for (i in 1:numberofobj)
    {
      isBaseNode <- F
      nearestNode <- 1
      for (j in 1:numberofnodes)
      {
        basenode <- nodeindexes [j,1]
        if (i == basenode)
        {
          isBaseNode <- T
          break
        }

        curNearest <- nodeindexes [nearestNode,1]

        nextNearest <- nodeindexes [j,1]

        #Compate distance between (i and curNearest) and (i and nextNearest)

        if (curNearest == nextNearest)
          next

        shp1 <- get.shortest.paths(g, from = i, to = c (curNearest))

        count1 <- length(shp1$vpath)

        totallen1 <- 0
        if (count1 >= 1)
        {
          for (f in 1:count1)
          {
            elem <- shp1$vpath[[f]]



            len <- length(elem)



            if (len >= 2)
            {
              for (k in 1: (len-1))
              {

                ind1 <- as.numeric (elem[k])
                ind2 <- as.numeric (elem [k+1])
                len1 <- round (as.numeric(filtered_positive [ind1,ind2]), digits = 2)

                if (len1 > 0)
                  totallen1 = as.numeric (totallen1) + len1



              }

            }
            else if (len >= 1)
            {
              print (elem)
            }
          }
        }

        shp2 <- get.shortest.paths(g, from = i, to = c (nextNearest))

        count2 <- length(shp2$vpath)

        totallen2 <- 0
        if (count2 >= 1)
        {
          for (f in 1:count2)
          {
            elem <- shp2$vpath[[f]]

            len <- length(elem)

            if (len >= 2)
            {
              for (k in 1:(len-1))
              {
                ind1 <- as.numeric (elem[k])
                ind2 <- as.numeric (elem [k+1])

                len2 <- round (as.numeric(filtered_positive [ind1,ind2]), digits = 2)

                if (len2 > 0)
                  totallen2 = as.numeric (totallen2) + len2

              }

            }
            else if (len >= 1)
            {
              print (elem)
            }

          }
        }

        if (totallen2 < totallen1)
        {
          nearestNode <- j
        }

      }
      if (isBaseNode)
        next

      record <- NULL

      idBaseNode <- nodeindexes [nearestNode,1]

      record <- c (i, idBaseNode)

      if (!is.null(record))
      {
        #Add to the distribution array combination i and nodeindexes [nearestNode,1]
        nodesbybasenodes <- rbind(nodesbybasenodes, record)

        #Add to the log logmatrix [i,j] which node is identified as nearest basenode
        logstringold <- logmatrix [i,i]
        logstringnew <- paste ("Node ", idBaseNode, " is identified as nearest basenode for this node" , sep = "")


        logmatrix [i,i] <- paste (logstringold, logstringnew, sep = "\r\n")

      }

    }


    #Removing inter-claster links. Step 1
    number_of_distributed_nodes = nrow (nodesbybasenodes)
    if (number_of_distributed_nodes >= 2)
    {

      if (numberofobj != (numberofnodes + number_of_distributed_nodes))
      {
        .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
        .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Removing inter-claster links: Clastering is wrong"))
        .GlobalEnv$top_calc_status  = 309
        return (0)

      }

      for (i in 1: (number_of_distributed_nodes-1))
      {
        node11 <- as.numeric (nodesbybasenodes[i,1])
        node12 <- as.numeric (nodesbybasenodes[i,2])

        node21 <- as.numeric (nodesbybasenodes[i+1,1])
        node22 <- as.numeric (nodesbybasenodes[i+1,2])


        if (node12 != node22)
        {
          #Nodes from the different clasters. Let's remove the link between them if exist
          val <- result [node11, node21]
          if (!is.na (val))
          {
            logstringold <- logmatrix [node11, node21]
            logstringnew <- "Removed as interclastering link"

            if (bRemoveLinks)
            {
              result [node11, node21] <- NA
              result [node21, node11] <- NA

            }
            else
            {
              logstringnew <- "Marked as interclastering link"
            }

            logmatrix [node11, node21] <- paste (logstringold, logstringnew, sep = "\r\n")
            logmatrix [node21, node11] <- paste (logstringold, logstringnew, sep = "\r\n")


          }

        }
      }
    }

    #Removing inter-claster links. Step 2 - Isolated basenodes and links between basenodes
    if (number_of_distributed_nodes >= 1)
    {
      for (f in 1:numberofnodes)
      {
        basenode1 <- nodeindexes [f,1]

        bNodeIsIsolated <- T
        for (l in 1: number_of_distributed_nodes)
        {
          basenode2 <- as.numeric (nodesbybasenodes[l,2])
          if (basenode1 == basenode2)
          {
            bNodeIsIsolated <- F
            break
          }
        }

        if (bNodeIsIsolated)
        {
          for (m in 1:numberofobj)
          {
            if (m == basenode1)
              next

            val <- result [basenode1, m]

            if (!is.na (val))
            {
              logstringold <- logmatrix [basenode1, m]
              logstringnew <- "Removed as interclastering link (isolated basenode)"

              if (bRemoveLinks)
              {
                result [basenode1, m] <- NA
                result [m, basenode1] <- NA
              }
              else
              {
                logstringnew <- "Marked as interclastering link (isolated basenode)"
              }


              logmatrix [basenode1, m] <- paste (logstringold, logstringnew, sep = "\r\n")
              logmatrix [m, basenode1] <- paste (logstringold, logstringnew, sep = "\r\n")

            }


          }
        }
        else
        {
          for (q in 1: numberofnodes)
          {
            basenode3 <- nodeindexes [f,1]
            if (basenode1 == basenode3)
              next

            val <- result [basenode1, basenode3]
            if (!is.na (val))
            {
              logstringold <- logmatrix [basenode1, basenode3]
              logstringnew <- "Removed as interclastering link (direct link between basenodes)"

              if (bRemoveLinks)
              {
                result [basenode1, basenode3] <- NA
                result [basenode3, basenode1] <- NA
              }
              else
              {
                logstringnew <- "Marked as interclastering link (direct link between basenodes)"
              }


              logmatrix [basenode1, basenode3] <- paste (logstringold, logstringnew, sep = "\r\n")
              logmatrix [basenode3, basenode1] <- paste (logstringold, logstringnew, sep = "\r\n")

            }

          }
        }
      }

    }

  }

  # elapsed <- Sys.time() - currentTs
  # timestr <- paste ("Clastering",elapsed, sep = " ")
  # print (timestr)
  # currentTs <- Sys.time()

  .GlobalEnv$top_calc_status  = 106

  #Step 9: Calculation cost of connectivity for the basenodes
  for (i in 1:numberofnodes)
  {

    basenode <- nodeindexes [i,1]
    dtf <- as.numeric (objects [basenode,5])
    rb <- as.numeric (objects [basenode,4])

    logstringold <- logmatrix [basenode,basenode]
    logstringnew <- paste ("Identified as a basenode. Distance to the fiber, km", round (as.numeric(dtf), digits = 2), sep = ":")

    logmatrix [basenode,basenode] <- paste (logstringold, logstringnew, sep = "\r\n")




    {

      input2 <- input


      if (is.reactivevalues (input))
        input2 <- reactiveValuesToList (input)

      input2$bWriteLog <- F

      input2$InitialData.UseNPV <- as.character(input2$InitialData.UseNPV)

      input2$SchoolSpecificData.Length <- dtf
      input2$SchoolSpecificData.RequiredBandwidth <- rb

      #ALGORITHM2_3

      ownership_f2 = 1000000000000
      ownership_m2 = 1000000000000

      npv_f = -1000000000000
      npv_m = -1000000000000


      if (bUseFocl)
      {

        #Coefficient should be between 0 and 1
        TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

        #Distance by the roads in km
        distancekmbyroads <- round (dtf * (1 + TopCoeff), digits = 2)

        input2$SchoolSpecificData.Length <- distancekmbyroads

        #Algorithm for overall FOCL construction cost evaluation
        focl_capex <- NULL
        focl_capex <-  algorithm2_1_impl (input2)
        if (is.null (focl_capex))
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate CAPEX for FOCL connection of basenode to PoP"))
          .GlobalEnv$top_calc_status  = 404
          return (0)
        }

        #Overall length of the FOCL construction site
        FOCLLenghtTotal = as.numeric (focl_capex [2,2])


        FOCLSectionLengthCD =   as.numeric (focl_capex [3,2])

        #Overall cost of FOCL installation
        TotalCAPEXFOCL =  as.numeric (focl_capex [1,2])



        #Algorithm for overall FOCL maintenance cost evaluation

        intermediate2 <- list (FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
        intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
        intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
        intermediate2$RequiredCapacity  <- rb

        focl_opex <- NULL
        focl_opex =  algorithm2_2_impl (input2, intermediate2)
        if (is.null (focl_opex))
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate OPEX for FOCL connection of basenode to PoP"))
          .GlobalEnv$top_calc_status  = 403
          return (0)
        }

        #Total cost for FOCL maintenance for the entire period of operation
        TotalOPEXFOCL =  as.numeric (focl_opex [1,2])



        ownership_f2 = TotalCAPEXFOCL  + TotalOPEXFOCL * as.numeric (input$PVOptionSet.PaybackPeriod)

        #NPV - FOCL

        if (bCalcNPV)
        {
          focl_income <- NULL
          focl_income =  as.matrix (algorithm2_12_impl (input2, intermediate51))
          if (is.null (focl_income))
          {
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate INCOME for FOCL connection of basenode to PoP"))
            .GlobalEnv$top_calc_status  = 402
            return (0)
          }

          NetIncome <- as.numeric (focl_income [1,2])

          intermediate5 <- list (NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
          TotalInvest <- TotalCAPEXFOCL
          CostOfOperation  <- TotalOPEXFOCL
          CostOfEquipmentAndMaterials  <- CostOfEqAndMatFOCL
          intermediate5$TotalInvest <- TotalInvest
          intermediate5$CostOfOperation <- CostOfOperation
          intermediate5$NetIncome <- NetIncome
          intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
          intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

          npv_focl <- NULL
          npv_focl <- algorithm2_7_impl (input2, intermediate5)

          if (is.null (npv_focl))
          {
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV for FOCL connection of basenode to PoP"))
            .GlobalEnv$top_calc_status  = 401
            return (0)
          }


          npv_f = as.numeric(npv_focl[1,2])

        }



      }


      if (bUseRts)
      {

        #Algorithm for total RTS construction cost evaluation between object and SN in locality
        rts_capex <- NULL
        rts_capex =  algorithm2_3_impl (input2, intermediate)
        if (is.null (rts_capex))
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate CAPEX for RTS connection of basenode to PoP"))
          .GlobalEnv$top_calc_status  = 504
          return (0)
        }


        TotalCAPEXRTS <-   as.numeric (rts_capex [1,2])

        NumberOfRepeaters <-   as.numeric (rts_capex [2,2])

        WidthFrequencyChannel <-   as.numeric (rts_capex [4,2])


        #Algorithm for total RTS maintenance cost evaluation between object and SN in locality

        intermediate3 <- list (NumberOfRepeaters = 0, RequiredCapacity = 0.0, NumberOfTerminals = 0.0, WidthFrequencyChannel = 0.0)
        intermediate3$NumberOfRepeaters <- NumberOfRepeaters
        intermediate3$RequiredCapacity  <- rb
        intermediate3$NumberOfTerminals <- 2
        intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel


        if (TotalCAPEXRTS == 0)
        {
          intermediate3$NumberOfTerminals <- 0
        }

        rts_opex <- NULL
        rts_opex =  algorithm2_4_impl (input2, intermediate3)
        if (is.null (rts_opex))
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate OPEX for RTS connection of basenode to PoP"))
          .GlobalEnv$top_calc_status  = 503
          return (0)
        }


        TotalOPEXRTS = as.numeric (rts_opex [1,2])

        ownership_m2 <- TotalCAPEXRTS  + TotalOPEXRTS * as.numeric (input$PVOptionSet.PaybackPeriod)

        if (bCalcNPV)
        {
          #NPV - Microwave
          intermediate51 <- list (RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
          intermediate51$RequiredCapacity  <- rb
          intermediate51$Technology <- 1

          rts_income <- NULL
          rts_income =  algorithm2_12_impl (input2, intermediate51)
          if (is.null (rts_income))
          {
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate INCOME for RTS connection of basenode to PoP"))
            .GlobalEnv$top_calc_status  = 502
            return (0)
          }


          NetIncome <- as.numeric (rts_income [1,2])


          intermediate5 <- list (NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)

          TotalInvest <- TotalCAPEXRTS
          CostOfOperation  <- TotalOPEXRTS
          CostOfEquipmentAndMaterials  <- CostOfEqAndMatRTS


          intermediate5$TotalInvest <- TotalInvest
          intermediate5$CostOfOperation <- CostOfOperation
          intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
          intermediate5$NetIncome <- NetIncome
          intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

          npv_microwave <- NULL
          npv_microwave <- algorithm2_7_impl (input2, intermediate5)
          if (is.null (npv_microwave))
          {
            .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
            .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV for RTS connection of basenode to PoP"))
            .GlobalEnv$top_calc_status  = 501
            return (0)
          }


          npv_m <- as.numeric(npv_microwave[1,2])


        }

      }

      ownership <- 0
      mt <- "Unknown"

      switch (input2$InitialData.UseNPV,
              '0' = {#Cost of ownership
                if (bUseFocl && bUseRts)
                {
                  if (ownership_f2 < ownership_m2)
                  {
                    ownership <- ownership_f2
                    mt <- "Fiber"
                  }
                  else
                  {
                    ownership <- ownership_m2
                    mt <- "Microwave"
                  }
                }
                else if (bUseFocl)
                {
                  ownership <- ownership_f2
                  mt <- "Fiber"
                }
                else if (bUseRts)
                {
                  ownership <- ownership_m2
                  mt <- "Microwave"
                }


              },
              '1' = {#NPV
                if (bUseFocl && bUseRts)
                {
                  if (npv_f > npv_m)
                  {
                    npv <- npv_f
                    mt <- i18n$t("Fiber")
                  }
                  else
                  {
                    npv <- npv_m
                    mt <- i18n$t("Microwave")
                  }
                }
                else if (bUseFocl)
                {
                  npv <- npv_f
                  mt <- i18n$t("Fiber")
                }
                else if (bUseRts)
                {
                  npv <- npv_m
                  mt <- i18n$t("Microwave")
                }


              }
      )



      logstringold <- logmatrix [basenode,basenode]


      foclres <- ""
      rtsres <- ""



      if (!bUseFocl)
      {
        foclres <- i18n$t("Fiber: not allowed")
      }
      else
      {
        switch (input2$InitialData.UseNPV,
                '0' = {#Cost of ownership
                  foclres <- paste (i18n$t("Fiber (cost of ownership)"), round (as.numeric(ownership_f2), digits = 2), sep = ":")
                },
                '1' = {#NPV
                  foclres <- paste (i18n$t("Fiber (NPV)"), round (as.numeric(npv_f), digits = 2), sep = ":")
                }
        )

      }

      if (!bUseRts)
      {
        rtsres <- i18n$t("Microwave: not allowed")
      }
      else
      {
        switch (input2$InitialData.UseNPV,
                '0' = {#Cost of ownership
                  rtsres <- paste (i18n$t("Microwave (cost of ownership)"), round (as.numeric(ownership_m2), digits = 2), sep = ":")
                },
                '1' = {#NPV

                  rtsres <- paste (i18n$t("Microwave (NPV)"), round (as.numeric(npv_m), digits = 2), sep = ":")
                }
        )

      }

      logstringnew <- paste (foclres, rtsres, "\r\nWinner:", mt, sep = " ")

      if (dtf == 0)
      {
        switch (input2$InitialData.UseNPV,
                '0' = {#Cost of ownership
                  logstringnew <- paste (i18n$t("Cost of ownership (Internet channel only):"), round (as.numeric(ownership), digits = 2), sep = " ")
                },
                '1' = {#NPV

                  logstringnew <- paste (i18n$t("NPV (Internet channel only):"), round (as.numeric(npv), digits = 2), sep = " ")
                }
        )

      }


      logmatrix [basenode,basenode] <- paste (logstringold, logstringnew, sep = "\r\n")

    }

  }


  # elapsed <- Sys.time() - currentTs
  # timestr <- paste ("Base nodes links calculation",elapsed, sep = " ")
  # print (timestr)



  .GlobalEnv$mylog <- logmatrix

  return (result)
}

method4_impl <- function(input, intermediate = NULL)
{

  #currentTs <- Sys.time()


  #Step 1: Check if construction of the network is impossible without choosing FOCL or Microwave


  bUseFocl <- F
  bUseRts <- F
  bUseSatellite <- F
  bUseCellular <- F

  if  (input$InitialData.UseFOCL == 1)
    bUseFocl <- T

  if (input$InitialData.UseRTS == 1)
    bUseRts <- T

  if (input$InitialData.UseSatellite == 1)
    bUseSatellite <- T

  if (input$InitialData.UseCellular == 1)
    bUseCellular <- T

  if ((!bUseFocl) && (!bUseRts))
  {
    .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Construction of the network is impossible without choosing FOCL or Microwave"))
    .GlobalEnv$top_calc_status  = 308
    return (0)
  }

  #Step 2: Distance connectivity matrix between investigated objects (localities) in estimated broadband network
  objects <- NULL
  objects <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)

  if (is.null (objects))
  {
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot read list of objects for calculation"))
    .GlobalEnv$top_calc_status  = 307
    return (0)
  }

  #Total number of objects for processing (N)
  numberofobj <- nrow(objects)

  if (numberofobj < 2)
  {
    .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate topology for less than two objects"))
    .GlobalEnv$top_calc_status  = 306
    return (0)
  }

  MaxClastersNumber <- as.numeric (input$TopologySettings.MaxClastersNumber)

  MaxItemsPerClaster <- as.numeric (input$TopologySettings.MaxItemsPerClaster)

  MaxNumberOfRecords <- MaxClastersNumber*MaxItemsPerClaster

  if (numberofobj > MaxNumberOfRecords)
  {
    .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
    .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate topology. Too many records"))
    .GlobalEnv$top_calc_status  = 305
    return (0)
  }

  bMakePreClastering <- F

  if  (input$TopologySettings.MakePreClastering == 1)
    bMakePreClastering <- T


  bCalcNPV <- F
  if (input$InitialData.UseNPV == 1)
    bCalcNPV <- T

  init_summary (bCalcNPV)

  #Check if pre-clastering possible
  if (bMakePreClastering)
  {

    clasters <- unique(objects[c("REGION")])

    numberofclasters <- nrow (clasters)

    if (numberofclasters < 1)
    {
      .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot make pre-clastering. Not enough information"))
      .GlobalEnv$top_calc_status  = 304
      return (0)
    }

    if (numberofclasters > MaxClastersNumber)
    {
      .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot make pre-clastering. Too many clasters"))
      .GlobalEnv$top_calc_status  = 303
      return (0)
    }

    for (i in 1: numberofclasters)
    {
      clastername <- as.character (clasters [i,1])

      oneclaster <- subset(objects, REGION == clastername)


      records_in_claster_number <- nrow (oneclaster)

      if (records_in_claster_number > MaxItemsPerClaster)
      {
        .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
        .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot make pre-clastering. Too large claster detected"))
        .GlobalEnv$top_calc_status  = 302
        return (0)
      }

      #result <- calc_one_claster (input, oneclaster, intermediate)
    }

    #Empty matrices N x N for storing log-records about links in the network and resulting information
    super_logmatrix <- matrix (nrow = numberofobj, ncol = numberofobj)
    super_result <- matrix (nrow = numberofobj, ncol = numberofobj)

    result <- NULL

    centralnodesindexes <- matrix (nrow = 0, ncol = 1)
    interclasterlinks <- matrix (nrow = 0, ncol = 6)

    for (i in 1: numberofclasters)
    {
      clastername <- as.character (clasters [i,1])

      clasterindexes <- matrix (nrow = 0, ncol = 1)
      clasterforcalculation <- matrix (nrow = 0, ncol = 6)


      for (j in 1:numberofobj)
      {
        objname <- as.character (objects [j,1])
        lon <- as.character (objects [j,2])
        lat <- as.character (objects [j,3])
        rb <- as.character (objects [j,4])
        dtf <- as.character (objects [j,5])
        cov <- as.character (objects [j,6])

        clastername_loc <- as.character (objects [j,7])

        if (clastername_loc == clastername)
        {
          #Record is related to current claster
          record <- NULL
          r_index <- NULL

          r_index <- c (j)

          record <- c (objname, lon, lat, rb, dtf, cov)

          clasterindexes <- rbind (clasterindexes, r_index)

          clasterforcalculation <- rbind(clasterforcalculation, record)


        }
      }

      numb_rec_in_claster <- nrow (clasterforcalculation)

      if (numb_rec_in_claster > 1)
      {
        result <- calc_one_claster (input, clasterforcalculation, intermediate)

        central_node <- .GlobalEnv$centralpointforclaster
      }
      else
      {
        if (numb_rec_in_claster == 0)
        {
          .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Unexpected error in the pre-clastering"))
          .GlobalEnv$top_calc_status  = 301
          return (0)
        }

        result <- matrix (nrow = 1, ncol = 1)
        logmatrix <- matrix (nrow = 1, ncol = 1)

        central_node <- 1


      }

      central_node <- clasterindexes [central_node]

      if (central_node > 0)
      {

        cn_index <- c (central_node)

        ic_objname <- as.character (objects [central_node,1])
        ic_lon <- as.character (objects [central_node,2])
        ic_lat <- as.character (objects [central_node,3])
        ic_rb <- as.character (objects [central_node,4])
        ic_dtf <- as.character (objects [central_node,5])
        ic_cov <- as.character (objects [central_node,6])

        centralnodesindexes <- rbind (centralnodesindexes, cn_index)

        inter_claster_record <- c (ic_objname, ic_lon, ic_lat, ic_rb, ic_dtf, ic_cov)
        interclasterlinks <- rbind(interclasterlinks, inter_claster_record)
      }



      oneclaster_size <- nrow (result)

      claster_indexes_size <- nrow (clasterindexes)



      if (oneclaster_size == claster_indexes_size)
      {
        for (z in 1:oneclaster_size)
        {

          rowindex <- clasterindexes [z]


          for (u in 1:oneclaster_size)
          {
            colindex <- clasterindexes [u]
            val <- result [z,u]
            log <- .GlobalEnv$mylog [z,u]


            super_result [rowindex, colindex] <- val
            super_logmatrix [rowindex, colindex] <- log
          }
        }
      }
      else
      {
        .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
        .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Unexpected error in the pre-clastering"))
        .GlobalEnv$top_calc_status  = 301
        return (0)

      }



    }

    input2 <- input

    if (is.reactivevalues (input))
      input2 <- reactiveValuesToList (input)

    input2$TopologySettings.MakeClastering <- 0
    input2$TopologySettings.RemoveInterclasterLinks <- 0
    input2$InitialData.UseSatellite <- 0
    input2$InitialData.UseCellular <- 0

    result <- calc_one_claster (input2, interclasterlinks, intermediate)


    oneclaster_size <- nrow (result)

    claster_indexes_size <- nrow (centralnodesindexes)



    if (oneclaster_size == claster_indexes_size)
    {
      for (z in 1:oneclaster_size)
      {


        rowindex <- centralnodesindexes [z]

        for (u in 1:oneclaster_size)
        {
          colindex <- centralnodesindexes [u]


          val <- result [z,u]
          log <- .GlobalEnv$mylog [z,u]


          super_result [rowindex, colindex] <- val
          super_logmatrix [rowindex, colindex] <- log
        }
      }
    }
    else
    {
      .GlobalEnv$mylog <- matrix("Detailed Calculation Log:")
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Unexpected error in the pre-clastering"))
      .GlobalEnv$top_calc_status  = 301
      return (0)

    }



    .GlobalEnv$mylog <- super_logmatrix

    calculate_summary (super_result)


    return (super_result)
  }
  else
  {
    result <- calc_one_claster (input, objects, intermediate)
  }


  calculate_summary (result)

  .GlobalEnv$top_calc_status  = 100

  return (result)
}




method4_inv_impl <- function(input, intermediate = NULL)
{


  result <- 0

  return (result)
}

method4 <- function(input, output)
{
  switch (input$algorithm,
          ALL = {


            req (input$Files.ListOfObjects)

            req (input$InitialData.UseFOCL)
            req (input$InitialData.UseRTS)
            req (input$InitialData.UseSatellite)
            req (input$InitialData.UseCellular)
            req (input$InitialData.UseNPV)
            req (input$TopologySettings.MakeClastering)
            req (input$TopologySettings.RemoveInterclasterLinks)
            req (input$TopologySettings.MakePreClastering)
            req (input$TopologySettings.MaxClastersNumber)
            req (input$TopologySettings.MaxItemsPerClaster)



            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))


            result <- method4_impl (input)

            output$c_names <- renderTable (.GlobalEnv$top_summary, colnames=FALSE)
            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)


          },
          ALGORITHM4_1 = {#Geodesic distance calculation between investigated objects (localities) in estimated broadband network
            algorithm4_1 (input, output)
          },
          ALGORITHM4_2 = {#NPV matrix calculations
            algorithm4_2 (input, output)
          },
          ALGORITHM4_3 = {#Optimal path selection between set of objects using NPV connectivity matrix
            algorithm4_3 (input, output)
          },
          ALGORITHM4_4 = {#Maximum distance and bandwidth based lattice clustering algorithm
            algorithm4_4 (input, output)
          },
		  ALGORITHM4_5 = {#Step-by-step topology optimization algorithm
            algorithm4_5 (input, output)
          },
          stop ("No!")
  )
}


method4_inv <- function(input, output)
{
  req (input)
  req (input$algorithm)
  req (output)

}
