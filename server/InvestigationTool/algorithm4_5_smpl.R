library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Step-by-step topology optimization algorithm

library(igraph)
library(dplyr)
library(geosphere)
library(SDMTools)
library(leaflet)
library(rjson)
#library(jsonlite)

if (exists('input')) {
  if (exists('TopologySettings.Algorithm', input)) {
	require(jsonlite)
  } else {
	require(rjson)
  }
}

library(pheatmap)

#Simplified calculations of 8-col NPV matrix (from each regular node to BN, 4 possible technologies)
calc_4x_matrix_simple <- function(input, objects, bnindex, intermediate = NULL)
{

  req(input)
  req(objects)

  numberofobj <- nrow(objects)
  npv4matrix <- matrix(nrow = numberofobj, ncol = 8, 0)
  BN <- objects[bnindex,]
  BNLon <- BN[2]
  BNLat <- BN[3]

  if (numberofobj > 0)
  {
	for (i in 1:numberofobj) # Distance and preliminary cycle
	{
	  npv4matrix[i, 2] <- as.numeric(objects[i, 4]) # 2nd-row is RBs
	  if (i != bnindex) {
		dist_to_BN <- distm(c(as.numeric(objects[i, 2]), as.numeric(objects[i, 3])), c(as.numeric(BNLon), as.numeric(BNLat)), fun = distHaversine)
		npv4matrix[i, 1] <- round(as.numeric(dist_to_BN[1] / 1000), digits = 2) # 1st-row is distance to BN
	  }
	  else {
		npv4matrix[i, 1] <- 0 # 1st-row is distance to BN
	  }
	  npv4matrix[i, 3] <- 0 # npv_focl
	  npv4matrix[i, 4] <- 0 # npv_rts
	  npv4matrix[i, 5] <- 0 # npv_Satellite
	  npv4matrix[i, 6] <- 0 # npv_cellular
	  npv4matrix[i, 7] <- as.integer(0) # best tech
	  npv4matrix[i, 8] <- 0 # best npv
	}

	#Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
	input2 <- input # Optimize by second choise in the next "if" or omit

	if (is.reactivevalues(input))
	  input2 <- reactiveValuesToList(input)

	bCalcNPV = T

	for (i in 1:numberofobj) # NPV main cycle ----------------------------------------------------------
	{
	  dist_rec <- as.numeric(npv4matrix[i, 1])
	  bw_rec <- as.numeric(npv4matrix[i, 2])

	  #input2$SchoolSpecificData.Length <- distancekm
	  #input2$SchoolSpecificData.RequiredBandwidth <- rb

	  #if (i != input$Intermediate.BNIndex) # Checks curnode for BNode -------------------------------------
	  if (i != bnindex) # Checks curnode for BNode -------------------------------------
	  {
		# FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
		#Coefficient should be between 0 and 1
		TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

		#Distance by the roads in km
		dist_rec_byroads <- round(dist_rec * (1 + TopCoeff), digits = 2) # All corrections are made inside of simplified fucntions

		#input2$SchoolSpecificData.Length <- distancekmbyroads
		#Algorithm for overall FOCL construction cost evaluation

		capex_focl <- NULL
		capex_focl <- calc_capex_simple(input2, dist_rec, bw_rec, 1)
		if (is.null(capex_focl))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate overall FOCL construction cost for some link"))
		  .GlobalEnv$top_calc_status = 408
		  return(0)
		}

		opex_focl <- NULL
		opex_focl <- calc_opex_simple(input2, dist_rec, bw_rec, 1)
		if (is.null(opex_focl))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate overall FOCL maintenance cost for some link"))
		  .GlobalEnv$top_calc_status = 407
		  return(0)
		}

		#Cost of ownership for the period
		#ownership_f = TotalCAPEXFOCL + TotalOPEXFOCL * as.numeric(input$PVOptionSet.PaybackPeriod)

		if (bCalcNPV)
		{

		  npv_focl <- NULL
		  npv_focl <- calc_npv_simple(input2, capex_focl, opex_focl, bw_rec, 1)

		  if (is.null(npv_focl))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate NPV of FOCL for some link"))
			.GlobalEnv$top_calc_status = 405
			return(0)
		  }

		  npv4matrix[i, 3] <- npv_focl
		}
		# FOCL NPV CALCs ENDS -----------------------------------------------------------------------------

		# RTS NPV CALCs STARTS ----------------------------------------------------------------------------
		#Algorithm for total RTS construction cost evaluation between object and SN in locality
		capex_rts <- NULL
		capex_rts <- calc_capex_simple(input2, dist_rec, bw_rec, 2)
		if (is.null(capex_rts))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate total RTS construction cost for some link"))
		  .GlobalEnv$top_calc_status = 508
		  return(0)
		}

		opex_rts <- NULL
		opex_rts <- calc_opex_simple(input2, dist_rec, bw_rec, 2)

		if (is.null(opex_rts))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate total RTS maintenance cost for some link"))
		  .GlobalEnv$top_calc_status = 507
		  return(0)
		}

		#Cost of ownership for the period
		#ownership_m <- TotalCAPEXRTS + TotalOPEXRTS * as.numeric(input$PVOptionSet.PaybackPeriod)

		if (bCalcNPV)
		{
		  #NPV - Microwave

		  npv_rts <- NULL
		  npv_rts <- calc_npv_simple(input2, capex_rts, opex_rts, bw_rec, 2)
		  if (is.null(npv_rts))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate NPV for RTS for some link"))
			.GlobalEnv$top_calc_status = 505
			return(0)
		  }

		  limit_rts <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity) # Add check for presense, and move this check up !
		  if (limit_rts < bw_rec)
			npv_rts <- -9999999999999

		  npv4matrix[i, 4] <- npv_rts
		}
		# RTS NPV CALCs ENDS   ----------------------------------------------------------------------------

		# SATELLITE NPV CALCs STARTS   ---------------------------------------------------------------------
		#rb <- objects [i,4]
		ownership_s <- 100000000000000
		npv_sat <- -100000000000000
		capex_sat = 0
		opex_sat = 0


		#intermediate4 <- list(NumberVSATsets = 0, RequiredCapacity = 0.0)
		#intermediate4$RequiredCapacity <- rb

		capex_sat <- NULL
		capex_sat <- calc_capex_simple(input2, 1 , bw_rec, 3)
		if (is.null(capex_sat))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate CAPEX of satellite for some link"))
		  .GlobalEnv$top_calc_status = 703
		  return(0)
		}

		opex_sat <- NULL
		opex_sat <- calc_opex_simple(input2, 1 , bw_rec, 3)
		if (is.null(opex_sat))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate OPEX of satellite for some link"))
		  .GlobalEnv$top_calc_status = 702
		  return(0)
		}

		#ownership_s <- TotalCAPEXSetellite + TotalOPEXSatellite * as.numeric(input$PVOptionSet.PaybackPeriod)

		if (bCalcNPV)
		{

		  npv_sat <- NULL
		  npv_sat <- calc_npv_simple(input2, capex_sat, opex_sat, bw_rec, 3)
		  if (is.null(npv_sat))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate NPV of satellite for some link"))
			.GlobalEnv$top_calc_status = 701
			return(0)
		  }

		  limit_sat <- as.numeric(input$InitialDataSatellite.MaximumLinkCapacity) # Add check for presense, and move this check up !
		  if (limit_sat < bw_rec)
			npv_sat <- -9999999999999

		  npv4matrix[i, 5] <- npv_sat
		}


		# SATELLITE NPV CALCs ENDS   -----------------------------------------------------------------------

		# CELLULAR NPV CALCs STARTS   ---------------------------------------------------------------------

		ownership_cellular <- 100000000000000
		npv_cell <- -100000000000000
		capex_cellular = 0
		opex_cellular = 0


		cellstatus <- "2G"

		cellstatus <- objects[i, 6]

		if (!is.na(cellstatus))
		{
		  if (cellstatus == "3G" || cellstatus == "4G")
		  {
			#Algorithm for determining the total cost of the installation and configuration of the cellular communication channel

			capex_cell <- NULL
			capex_cell <- calc_capex_simple(input2, 1 , bw_rec, 4)
			if (is.null(capex_cell))
			{
			  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate CAPEX of cellular for some link"))
			  .GlobalEnv$top_calc_status = 603
			  return(0)
			}

			opex_cell <- NULL
			opex_cell <- calc_opex_simple(input2, 1 , bw_rec, 4)
			if (is.null(opex_cell))
			{
			  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate OPEX of cellular for some link"))
			  .GlobalEnv$top_calc_status = 602
			  return(0)
			}
			#ownership_cellular <- TotalCAPEXCellular + TotalOPEXCellular * as.numeric(input$PVOptionSet.PaybackPeriod)

			if (bCalcNPV)
			{
			  #NPV - Cellular
			  npv_cell <- NULL
			  npv_cell <- calc_npv_simple(input2, capex_cell, opex_cell, bw_rec, 4)
			  if (is.null(npv_cell))
			  {
				.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
				.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate NPV of cellular for some link"))
				.GlobalEnv$top_calc_status = 601
				return(0)
			  }

			  limit_cell <- as.numeric(input$InitialDataCellular.MaximumLinkCapacity) # Add check for presense, and move this check up !
			  if (limit_cell < bw_rec)
				npv_cell <- -9999999999999

			  npv4matrix[i, 6] <- npv_cell
			}

		  }
		  else
			npv4matrix[i, 6] <- npv_cell        # for 2G
		}
		else
		  cellstatus <- "2G"


		# CELLULAR NPV CALCs ENDS   -----------------------------------------------------------------------

	  } # Checking curnode for BNode ENDS -----------------------------------------------------------------

	  best_npv_index = which(npv4matrix[i, 3:6] == max(npv4matrix[i, 3:6]))[1] # 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	  if (best_npv_index < 1 || best_npv_index > 4)
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_npv_simple: Incorrect technology index"))
		.GlobalEnv$top_calc_status = 604
		#print ("Incorrect technology index")
		#browser()
		return(0)
	  }
	  best_npv_value = npv4matrix[[i, (2 + best_npv_index)]]
	  npv4matrix[i, 7] = best_npv_index # TechIndex <- Index 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	  npv4matrix[i, 8] = best_npv_value

	} # NPV MAIN CYCLE ENDS -------------------------------------------------------------------------------


  } # numobj checking block ends


  #print(npv4matrix)
  result <- npv4matrix
  #result <- obj_plus_DTBN[1:100,]
  #result <- obj_plus_DTBN[1,]

  return(result)

}

calc_4x_matrix_costless <- function(input, objects, bnindex, intermediate = NULL)
{

  req(input)
  req(objects)

  numberofobj <- nrow(objects)
  npv4matrix <- matrix(nrow = numberofobj, ncol = 8, 0)
  BN <- objects[bnindex,]
  BNLon <- BN[2]
  BNLat <- BN[3]

  focl_link_bw <-  as.numeric(input$TopologySettings.CostlessCapacity)
  extracharge_bw <- as.numeric(input$TopologySettings.CostlessBwExtracharge)

  #print("check 111")
  #print(as.numeric(input$TopologySettings.CostlessRTSBaseMetr))
  #print(as.numeric(input$TopologySettings.CostlessSATBaseMetr))
  #print(as.numeric(input$InitialDataSatellite.MaximumLinkCapacity))
  #print("/ check 111")


  if (numberofobj > 0)
  {
	for (i in 1:numberofobj) # Distance and preliminary cycle
	{
	  npv4matrix[i, 2] <- as.numeric(objects[i, 4]) # 2nd-row is RBs
	  if (i != bnindex) {
		dist_to_BN <- distm(c(as.numeric(objects[i, 2]), as.numeric(objects[i, 3])), c(as.numeric(BNLon), as.numeric(BNLat)), fun = distHaversine)
		npv4matrix[i, 1] <- round(as.numeric(dist_to_BN[1] / 1000), digits = 2) # 1st-row is distance to BN
	  }
	  else {
		npv4matrix[i, 1] <- 0 # 1st-row is distance to BN
	  }
	  npv4matrix[i, 3] <- -100000000000000 # npv_focl
	  npv4matrix[i, 4] <- -100000000000000 # npv_rts
	  npv4matrix[i, 5] <- -100000000000000 # npv_Satellite
	  npv4matrix[i, 6] <- -100000000000000 # npv_cellular
	  npv4matrix[i, 7] <- as.integer(1) # best tech
	  npv4matrix[i, 8] <- 0 # best npv
	}

	#Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
	input2 <- input # Optimize by second choise in the next "if" or omit

	if (is.reactivevalues(input))
	  input2 <- reactiveValuesToList(input)

	bCalcNPV = T

	for (i in 1:numberofobj) # NPV main cycle ----------------------------------------------------------
	{
	  dist_rec <- as.numeric(npv4matrix[i, 1])
	  bw_rec <- as.numeric(npv4matrix[i, 2])

	  #input2$SchoolSpecificData.Length <- distancekm
	  #input2$SchoolSpecificData.RequiredBandwidth <- rb

	  #if (i != input$Intermediate.BNIndex) # Checks curnode for BNode -------------------------------------
	  if (i != bnindex) # Checks curnode for BNode -------------------------------------
	  {
		# FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
		#Coefficient should be between 0 and 1
		TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

		#Distance by the roads in km
		dist_rec_byroads <- round(dist_rec * (1 + TopCoeff), digits = 2) # All corrections are made inside of simplified fucntions

		if (bCalcNPV)
		{

		  # !!! remove to params
		  # focl_link_bw <- 10000
		  # extracharge_bw <- 0.05

		  costless_focl <- NULL # COSTLESS_FOCL
		  costless_focl <- dist_rec_byroads*(1+log2(1+floor(bw_rec/(2*focl_link_bw)))*extracharge_bw)
		  #costless_focl <- calc_npv_simple(input2, capex_focl, opex_focl, bw_rec, 1)


		  if (is.null(costless_focl))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_metric_costless: Cannot calculate costless metric of FOCL for some link"))
			.GlobalEnv$top_calc_status = 405
			return(0)
		  }

		  npv4matrix[i, 3] <- (-1)*costless_focl
		}
		# FOCL NPV CALCs ENDS -----------------------------------------------------------------------------

		# RTS NPV CALCs STARTS ----------------------------------------------------------------------------
		#Algorithm for total RTS construction cost evaluation between object and SN in locality
		#capex_rts <- NULL
		#opex_rts <- NULL
		#npv_rts <- -100000000000000
		#npv4matrix[i, 4] <- npv_rts

		# Move to Ext params
		# costless_focl_to_rts_coeff <- 0.4
		#rts_base_metric <- 1
		rts_base_metric <- as.numeric(input$TopologySettings.CostlessRTSBaseMetr)
		costless_rts_bw_lim_total <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity)
		rts_link_bw <- as.numeric(input$TopologySettings.CostlessCapacityRTS) # rename previous to FOCL
		rts_link_dist <- as.numeric(input$TopologySettings.CostlessDistRTS)
		#costless_rts_bw_lim <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity)
		if (bw_rec <= costless_rts_bw_lim_total) {   # COSTLESS_RTS
		  #costless_rts <- dist_rec * costless_focl_to_rts_coeff
		  costless_rts <- ceiling(dist_rec / rts_link_dist) *
			ceiling(bw_rec / rts_link_bw) *
			rts_base_metric
		}
		else
		  costless_rts <- 99999999999

		# costless_rts <- 99999999999 # Remove !!!

		npv4matrix[i, 4] <- (-1) * costless_rts

		# RTS CALCs ENDS   ----------------------------------------------------------------------------

		# SATELLITE METRICs CALCs STARTS   ---------------------------------------------------------------------
		#rb <- objects [i,4]
		#ownership_s <- 100000000000000
		costless_sat <- 100000000000000
		#capex_sat = 0
		#opex_sat = 0

		# Move to Ext params
		#print(as.numeric(input$InitialDataSatellite.MaximumLinkCapacity))
		#print(as.numeric(input$TopologySettings.CostlessCapacitySAT))
		# costless_focl_to_rts_coeff <- 0.4
		#sat_base_metric <- 0.05 # ( 600 / 13000)
		sat_base_metric <- as.numeric(input$TopologySettings.CostlessSATBaseMetr)
		costless_sat_bw_lim_total <- as.numeric(input$InitialDataSatellite.MaximumLinkCapacity)
		sat_link_bw <- as.numeric(input$TopologySettings.CostlessCapacitySAT) # rename
		#costless_rts_bw_lim <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity)
		#print(sat_link_bw)
		#print(costless_sat_bw_lim_total)
		#sat_link_bw <- 50
		#costless_sat_bw_lim_total <- 50
		if (bw_rec <= costless_sat_bw_lim_total) { # COSTLESS_SATELLITE
		  #costless_rts <- dist_rec * costless_focl_to_rts_coeff
		  costless_sat <- ceiling(bw_rec / sat_link_bw) * sat_base_metric
		}
		else
		  costless_sat <- 99999999999

		# costless_sat <- 99999999999 # Remove !!!

		npv4matrix[i, 5] <- (-1) * costless_sat

		# SATELLITE METRICs CALCs ENDS   -----------------------------------------------------------------------

		# CELLULAR NPV CALCs STARTS   ---------------------------------------------------------------------

		ownership_cellular <- 100000000000000
		npv_cell <- -100000000000000
		capex_cellular = 0
		opex_cellular = 0
		npv4matrix[i, 6] <- npv_cell

		# CELLULAR NPV CALCs ENDS   -----------------------------------------------------------------------

	  } # Checking curnode for BNode ENDS -----------------------------------------------------------------

	  best_npv_index = which(npv4matrix[i, 3:6] == max(npv4matrix[i, 3:6]))[1] # 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	  if (best_npv_index < 1 || best_npv_index > 4)
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_npv_simple: Incorrect technology index"))
		.GlobalEnv$top_calc_status = 604
		#print ("Incorrect technology index")
		#browser()
		return(0)
	  }
	  best_npv_value = npv4matrix[[i, (2 + best_npv_index)]]
	  npv4matrix[i, 7] = best_npv_index # TechIndex <- Index 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	  npv4matrix[i, 8] = best_npv_value

	} # NPV MAIN CYCLE ENDS -------------------------------------------------------------------------------


  } # numobj checking block ends


  #print(npv4matrix)
  result <- npv4matrix
  #result <- obj_plus_DTBN[1:100,]
  #result <- obj_plus_DTBN[1,]

  return(result)

}

calc_4x_matrix_simple_cown <- function(input, objects, bnindex, intermediate = NULL)
{

  req(input)
  req(objects)

  numberofobj <- nrow(objects)
  cown4matrix <- matrix(nrow = numberofobj, ncol = 8, 0)
  BN <- objects[bnindex,]
  BNLon <- BN[2]
  BNLat <- BN[3]

  if (numberofobj > 0)
  {
	for (i in 1:numberofobj) # Distance and preliminary cycle
	{
	  cown4matrix[i, 2] <- as.numeric(objects[i, 4]) # 2nd-row is RBs
	  if (i != bnindex) {
		dist_to_BN <- distm(c(as.numeric(objects[i, 2]), as.numeric(objects[i, 3])), c(as.numeric(BNLon), as.numeric(BNLat)), fun = distHaversine)
		cown4matrix[i, 1] <- round(as.numeric(dist_to_BN[1] / 1000), digits = 2) # 1st-row is distance to BN
	  }
	  else {
		cown4matrix[i, 1] <- 0 # 1st-row is distance to BN
	  }
	  cown4matrix[i, 3] <- 0 # npv_focl
	  cown4matrix[i, 4] <- 0 # npv_rts
	  cown4matrix[i, 5] <- 0 # npv_Satellite
	  cown4matrix[i, 6] <- 0 # npv_cellular
	  cown4matrix[i, 7] <- as.integer(0) # best tech
	  cown4matrix[i, 8] <- 0 # best npv
	}

	#Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
	input2 <- input # Optimize by second choise in the next "if" or omit

	if (is.reactivevalues(input))
	  input2 <- reactiveValuesToList(input)

	bCalcNPV = T

	for (i in 1:numberofobj) # NPV main cycle ----------------------------------------------------------
	{
	  dist_rec <- as.numeric(cown4matrix[i, 1])
	  bw_rec <- as.numeric(cown4matrix[i, 2])

	  #input2$SchoolSpecificData.Length <- distancekm
	  #input2$SchoolSpecificData.RequiredBandwidth <- rb

	  #if (i != input$Intermediate.BNIndex) # Checks curnode for BNode -------------------------------------
	  if (i != bnindex) # Checks curnode for BNode -------------------------------------
	  {
		# FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
		#Coefficient should be between 0 and 1
		TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

		#Distance by the roads in km
		dist_rec_byroads <- round(dist_rec * (1 + TopCoeff), digits = 2) # All corrections are made inside of simplified fucntions

		#input2$SchoolSpecificData.Length <- distancekmbyroads
		#Algorithm for overall FOCL construction cost evaluation

		capex_focl <- NULL
		capex_focl <- calc_capex_simple(input2, dist_rec, bw_rec, 1)
		if (is.null(capex_focl))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate overall FOCL construction cost for some link"))
		  .GlobalEnv$top_calc_status = 408
		  return(0)
		}

		opex_focl <- NULL
		opex_focl <- calc_opex_simple(input2, dist_rec, bw_rec, 1)
		if (is.null(opex_focl))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate overall FOCL maintenance cost for some link"))
		  .GlobalEnv$top_calc_status = 407
		  return(0)
		}

		#Cost of ownership for the period
		#ownership_f = TotalCAPEXFOCL + TotalOPEXFOCL * as.numeric(input$PVOptionSet.PaybackPeriod)

		if (bCalcNPV)
		{

		  tco_focl <- NULL
		  tco_focl <- calc_tco_simple(input2, capex_focl, opex_focl)

		  if (is.null(tco_focl))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate NPV of FOCL for some link"))
			.GlobalEnv$top_calc_status = 405
			return(0)
		  }

		  cown4matrix[i, 3] <- (-1)*tco_focl
		}
		# FOCL NPV CALCs ENDS -----------------------------------------------------------------------------

		# RTS NPV CALCs STARTS ----------------------------------------------------------------------------
		#Algorithm for total RTS construction cost evaluation between object and SN in locality
		capex_rts <- NULL
		capex_rts <- calc_capex_simple(input2, dist_rec, bw_rec, 2)
		if (is.null(capex_rts))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate total RTS construction cost for some link"))
		  .GlobalEnv$top_calc_status = 508
		  return(0)
		}

		opex_rts <- NULL
		opex_rts <- calc_opex_simple(input2, dist_rec, bw_rec, 2)

		if (is.null(opex_rts))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate total RTS maintenance cost for some link"))
		  .GlobalEnv$top_calc_status = 507
		  return(0)
		}

		#Cost of ownership for the period
		#ownership_m <- TotalCAPEXRTS + TotalOPEXRTS * as.numeric(input$PVOptionSet.PaybackPeriod)

		if (bCalcNPV)
		{
		  #NPV - Microwave

		  tco_rts <- NULL
		  tco_rts <- calc_tco_simple(input2, capex_rts, opex_rts)
		  if (is.null(tco_rts))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate NPV for RTS for some link"))
			.GlobalEnv$top_calc_status = 505
			return(0)
		  }

		  tco_rts_bcp <- tco_rts
		  limit_rts <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity) # Add check for presense, and move this check up !
		  if (limit_rts < bw_rec)
			tco_rts <- 9999999999997

		  cown4matrix[i, 4] <- (-1)*tco_rts
		}
		# RTS NPV CALCs ENDS   ----------------------------------------------------------------------------

		# SATELLITE NPV CALCs STARTS   ---------------------------------------------------------------------
		#rb <- objects [i,4]
		ownership_s <- 100000000000000
		tco_sat <- 100000000000000
		capex_sat = 0
		opex_sat = 0


		#intermediate4 <- list(NumberVSATsets = 0, RequiredCapacity = 0.0)
		#intermediate4$RequiredCapacity <- rb

		capex_sat <- NULL
		capex_sat <- calc_capex_simple(input2, 1 , bw_rec, 3)
		if (is.null(capex_sat))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate CAPEX of satellite for some link"))
		  .GlobalEnv$top_calc_status = 703
		  return(0)
		}

		opex_sat <- NULL
		opex_sat <- calc_opex_simple(input2, 1 , bw_rec, 3)
		if (is.null(opex_sat))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate OPEX of satellite for some link"))
		  .GlobalEnv$top_calc_status = 702
		  return(0)
		}

		#ownership_s <- TotalCAPEXSetellite + TotalOPEXSatellite * as.numeric(input$PVOptionSet.PaybackPeriod)

		if (bCalcNPV)
		{

		  tco_sat <- NULL
		  tco_sat <- calc_tco_simple(input2, capex_sat, opex_sat)
		  if (is.null(tco_sat))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate NPV of satellite for some link"))
			.GlobalEnv$top_calc_status = 701
			return(0)
		  }

		  limit_sat <- as.numeric(input$InitialDataSatellite.MaximumLinkCapacity) # Add check for presense, and move this check up !
		  if (limit_sat < bw_rec)
			tco_sat <- 9999999999999


		  cown4matrix[i, 5] <- (-1)*tco_sat
		}


		# SATELLITE NPV CALCs ENDS   -----------------------------------------------------------------------

		# CELLULAR NPV CALCs STARTS   ---------------------------------------------------------------------

		#tco_cell <- 100000000000000
		tco_cell <- 100000000000000
		capex_cellular = 0
		opex_cellular = 0


		cellstatus <- "2G"

		cellstatus <- objects[i, 6]

		if (!is.na(cellstatus))
		{
		  if (cellstatus == "3G" || cellstatus == "4G")
		  {
			#Algorithm for determining the total cost of the installation and configuration of the cellular communication channel

			capex_cell <- NULL
			capex_cell <- calc_capex_simple(input2, 1 , bw_rec, 4)
			if (is.null(capex_cell))
			{
			  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate CAPEX of cellular for some link"))
			  .GlobalEnv$top_calc_status = 603
			  return(0)
			}

			opex_cell <- NULL
			opex_cell <- calc_opex_simple(input2, 1 , bw_rec, 4)
			if (is.null(opex_cell))
			{
			  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate OPEX of cellular for some link"))
			  .GlobalEnv$top_calc_status = 602
			  return(0)
			}
			#ownership_cellular <- TotalCAPEXCellular + TotalOPEXCellular * as.numeric(input$PVOptionSet.PaybackPeriod)

			if (bCalcNPV)
			{
			  #NPV - Cellular
			  tco_cell <- NULL
			  tco_cell <- calc_tco_simple(input2, capex_cell, opex_cell)
			  if (is.null(npv_cell))
			  {
				.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
				.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp_simple: Cannot calculate NPV of cellular for some link"))
				.GlobalEnv$top_calc_status = 601
				return(0)
			  }

			  limit_cell <- as.numeric(input$InitialDataCellular.MaximumLinkCapacity) # Add check for presense, and move this check up !
			  if (limit_cell < bw_rec)
				tco_cell <- 9999999999999

			  cown4matrix[i, 6] <- (-1)*tco_cell
			}

		  }
		  else
			cown4matrix[i, 6] <- (-1)*tco_cell        # for 2G
		}
		else
		  cellstatus <- "2G"


		# CELLULAR NPV CALCs ENDS   -----------------------------------------------------------------------

	  } # Checking curnode for BNode ENDS -----------------------------------------------------------------

	  #	  best_npv_index = which(cown4matrix[i, 3:6] == max(cown4matrix[i, 3:6]))[1] # 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	  #	  if (best_npv_index < 1 || best_npv_index > 4)
	  #		{
	  #		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  #		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_npv_simple: Incorrect technology index"))
	  #		.GlobalEnv$top_calc_status = 604
	  #		#print ("Incorrect technology index")
	  #		#browser()
	  #		return(0)
	  #	  }
	  #	  best_npv_value = cown4matrix[[i, (2 + best_npv_index)]]
	  #	  cown4matrix[i, 7] = best_npv_index # TechIndex <- Index 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	  #	  cown4matrix[i, 8] = best_npv_value

	  best_cown_index = which(cown4matrix[i, 3:6] == max(cown4matrix[i, 3:6]))[1] # 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	  if (best_cown_index < 1 || best_cown_index > 4)
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_own: Incorrect technology index"))
		.GlobalEnv$top_calc_status = 604
		#print ("Incorrect technology index")
		#browser()
		return(0)
	  }
	  best_cown_value = cown4matrix[[i, (2 + best_cown_index)]]
	  cown4matrix[i, 7] = best_cown_index # TechIndex <- Index 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	  cown4matrix[i, 8] = best_cown_value
	  if (abs(best_cown_value) > 9999999999900) { # Bad situation, it is not possible to create topology with such bw restrictions
		cown4matrix[i, 7] <- 2 # TechIndex <- Index 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
		cown4matrix[i, 8] <- (-1)*tco_rts_bcp
		cown4matrix[i, 4] <- (-1)*tco_rts_bcp
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Topology correctness warning: Bandwidth restrictions are too strong !"))
		#cat("Topology correctness warning: Bandwidth restrictions are too strong !", file = 'opt-warn.txt', APPEND = TRUE)
		cat("Topology correctness warning: Bandwidth restrictions are too strong !", sep = "\n", file = 'opt-warn.txt')
	  }

	} # NPV MAIN CYCLE ENDS -------------------------------------------------------------------------------


  } # numobj checking block ends


  #print(cown4matrix)
  result <- cown4matrix
  #result <- obj_plus_DTBN[1:100,]
  #result <- obj_plus_DTBN[1,]

  return(result)

}

calc_internet_addon_simple_cown <- function(input, FullRequiredBandwidth = 0.0) # from obj1 to obj2
{
  req(input)

  if (FullRequiredBandwidth < 1000)
	l_ISP_fee <- as.numeric(input$ExtParam.AnnualISPFeeFOCL1)
  else
	l_ISP_fee <- as.numeric(input$ExtParam.AnnualISPFeeFOCL2)

  l_payback_period <- as.numeric(input$IntParam.PaybackPeriod)

  cown_addon_simple <- -1 *
	as.numeric(FullRequiredBandwidth) *
	l_ISP_fee *
	l_payback_period
  return(cown_addon_simple)
}

calc_internet_addon_simple_npv <- function(input, FullRequiredBandwidth = 0.0, intermediate = NULL) # from obj1 to obj2
{
  req(input)

  if (FullRequiredBandwidth < 1000)
	l_ISP_fee <- as.numeric(input$ExtParam.AnnualISPFeeFOCL1)
  else
	l_ISP_fee <- as.numeric(input$ExtParam.AnnualISPFeeFOCL2)

  l_payback_period <- as.numeric(input$IntParam.PaybackPeriod)
  CostOfOperation <- as.numeric(FullRequiredBandwidth) * l_ISP_fee

  npv <- 0
  npv_temp <- (as.numeric(CostOfOperation)*(1-as.numeric(input$IntParam.TAX_PROF)/100))
  for (j in 1:l_payback_period) {
	npv <- npv + npv_temp/((1+as.numeric(input$IntParam.K_DISC)/100)^j)
  }

  return(npv)
}
#Calculation of FOCL and RTS NPV values for a pair of connected objects (obj1 -> obj2)

# Simplified calc_npv_pair
calc_npv_pair_simple <- function(input, obj1, obj2, focl_pos = 1, rts_pos = 1, intermediate = NULL) # from obj1 to obj2
{
  req(input)
  req(obj1)
  req(obj2)
  req(input$InitialDataRadio.MaximumLinkCapacity)
  req(input$InitialDataSatellite.MaximumLinkCapacity)
  req(input$InitialDataCellular.MaximumLinkCapacity)
  npv2matrix <- matrix(nrow = 1, ncol = 6, 0) # row = 3 for NPV FOCL, row = 4 NPV for RTS

  bw_rec <- as.numeric(obj1[[4]]) # Required bandwidth
  npv2matrix[1, 2] <- bw_rec
  dist_pair <- distm(c(as.numeric(obj1[[2]]), as.numeric(obj1[[3]])), c(as.numeric(obj2[[2]]), as.numeric(obj2[[3]])), fun = distHaversine)
  dist_rec <- round(as.numeric(dist_pair[1] / 1000), digits = 2) # 1st-row is distance between obj1 and obj2
  npv2matrix[1, 1] <- dist_rec

  npv2matrix[1, 3] <- 0 # npv_focl
  npv2matrix[1, 4] <- 0 # npv_rts
  npv2matrix[1, 5] <- 0 # best tech
  npv2matrix[1, 6] <- 0 # best npv

  #Coefficient should be between 0 and 1
  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km, used for FOCL only
  dist_rec_byroads <- round(dist_rec * (1 + TopCoeff), digits = 2) # Now corrections are made inside of simplified fucntions

  capex_focl <- calc_capex_simple(input, dist_rec, bw_rec, 1)
  capex_rts <- calc_capex_simple(input, dist_rec, bw_rec, 2)

  opex_focl <- calc_opex_simple(input, dist_rec, bw_rec, 1)
  opex_rts <- calc_opex_simple(input, dist_rec, bw_rec, 2)

  #tco_focl <- calc_tco_simple(input_load, capex_focl, opex_focl, 1)
  #tco_rts <- calc_tco_simple(input_load, capex_rts, opex_rts, 2)

  npv_focl <- calc_npv_simple(input, capex_focl, opex_focl, bw_rec, 1)
  npv_rts <- calc_npv_simple(input, capex_rts, opex_rts, bw_rec, 2)

  npv2matrix[1, 3] = npv_focl
  npv2matrix[1, 4] = npv_rts

  # For disabled techs return prohibitive npv; Move it prior to calcs !
  if (focl_pos == 0) npv_focl = -99999999999
  if (rts_pos == 0) npv_rts = -99999999999

  limit_rts <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity) # Add check for presense
  if (limit_rts < bw_rec)
	npv_rts = -99999999999

  if (npv_focl >= npv_rts)
  {
	npv2matrix[1, 5] = 1
	npv2matrix[1, 6] = npv_focl
  }
  else
  {
	npv2matrix[1, 5] = 2
	npv2matrix[1, 6] = npv_rts
  }
  #print(npv2matrix)
  result <- npv2matrix

  return(result)
}

calc_npv_pair_costless <- function(input, obj1, obj2, focl_pos = 1, rts_pos = 1, intermediate = NULL) # from obj1 to obj2
{

  req(input)
  req(obj1)
  req(obj2)
  npv2matrix <- matrix(nrow = 1, ncol = 6, 0) # row = 3 for NPV FOCL, row = 4 NPV for RTS

  bw_rec <- as.numeric(obj1[[4]]) # Required bandwidth
  npv2matrix[1, 2] <- bw_rec
  dist_pair <- distm(c(as.numeric(obj1[[2]]), as.numeric(obj1[[3]])), c(as.numeric(obj2[[2]]), as.numeric(obj2[[3]])), fun = distHaversine)
  dist_rec <- round(as.numeric(dist_pair[1] / 1000), digits = 2) # 1st-row is distance between obj1 and obj2
  npv2matrix[1, 1] <- dist_rec

  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km, used for FOCL only
  dist_rec_byroads <- round(dist_rec * (1 + TopCoeff), digits = 2)

  # !!! remove to params
  #focl_link_bw <- 10000
  #extracharge_bw <- 0.05

  focl_link_bw <-  as.numeric(input$TopologySettings.CostlessCapacity)
  focl_extracharge_bw <- as.numeric(input$TopologySettings.CostlessBwExtracharge)

  if (focl_pos == 1) # COSTLESS_FOCL
	costless_focl <- dist_rec_byroads * (1 + log2(1 + floor(bw_rec / (2 * focl_link_bw))) * focl_extracharge_bw)
  else
	costless_focl <- 99999999999

  # Move to Ext params
  # costless_focl_to_rts_coeff <- 0.4
  # rts_base_metric <- 2
  rts_base_metric <- as.numeric(input$TopologySettings.CostlessRTSBaseMetr)
  costless_rts_bw_lim_total <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity)
  rts_link_bw <-  as.numeric(input$TopologySettings.CostlessCapacityRTS) # rename previous to FOCL
  rts_link_dist <- as.numeric(input$TopologySettings.CostlessDistRTS)
  #costless_rts_bw_lim <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity)
  if (bw_rec <= costless_rts_bw_lim_total && rts_pos == 1) { #COSTLESS_RTS
	#costless_rts <- dist_rec * costless_focl_to_rts_coeff
	costless_rts <- ceiling(dist_rec / rts_link_dist) * ceiling(bw_rec / rts_link_bw) * rts_base_metric
  }
  else
	costless_rts <- 99999999999

  # costless_rts <- 99999999999 # Remove

  if (costless_rts < costless_focl) {
	npv2matrix[1, 3] = (-1) * costless_focl  # lesser abs(metric) should be better
	npv2matrix[1, 4] = (-1) * costless_rts
	npv2matrix[1, 5] = 2
	npv2matrix[1, 6] = (-1) * costless_rts
  } else {
	npv2matrix[1, 3] = (-1) * costless_focl  # lesser abs(metric) should be better
	npv2matrix[1, 4] = (-1) * costless_rts
	npv2matrix[1, 5] = 1
	npv2matrix[1, 6] = (-1) * costless_focl
  }



  #print(sprintf('Calc npv_pair costless: %d %f',npv2matrix[1, 5], npv2matrix[1, 6]))

  result <- npv2matrix
  return(result)

}

calc_cown_pair_simple <- function(input, obj1, obj2, focl_pos = 1, rts_pos = 1, intermediate = NULL) # from obj1 to obj2
{
  req(input)
  req(obj1)
  req(obj2)
  req(input$InitialDataRadio.MaximumLinkCapacity)
  req(input$InitialDataSatellite.MaximumLinkCapacity)
  req(input$InitialDataCellular.MaximumLinkCapacity)
  cown2matrix <- matrix(nrow = 1, ncol = 6, 0) # row = 3 for NPV FOCL, row = 4 NPV for RTS

  bw_rec <- as.numeric(obj1[[4]]) # Required bandwidth
  cown2matrix[1, 2] <- bw_rec
  dist_pair <- distm(c(as.numeric(obj1[[2]]), as.numeric(obj1[[3]])), c(as.numeric(obj2[[2]]), as.numeric(obj2[[3]])), fun = distHaversine)
  dist_rec <- round(as.numeric(dist_pair[1] / 1000), digits = 2) # 1st-row is distance between obj1 and obj2
  cown2matrix[1, 1] <- dist_rec

  cown2matrix[1, 3] <- 0 # npv_focl
  cown2matrix[1, 4] <- 0 # npv_rts
  cown2matrix[1, 5] <- 0 # best tech
  cown2matrix[1, 6] <- 0 # best npv

  #Coefficient should be between 0 and 1
  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km, used for FOCL only
  dist_rec_byroads <- round(dist_rec * (1 + TopCoeff), digits = 2) # Now corrections are made inside of simplified fucntions

  capex_focl <- calc_capex_simple(input, dist_rec, bw_rec, 1)
  capex_rts <- calc_capex_simple(input, dist_rec, bw_rec, 2)

  opex_focl <- calc_opex_simple(input, dist_rec, bw_rec, 1)
  opex_rts <- calc_opex_simple(input, dist_rec, bw_rec, 2)

  #tco_focl <- calc_tco_simple(input_load, capex_focl, opex_focl, 1)
  #tco_rts <- calc_tco_simple(input_load, capex_rts, opex_rts, 2)

  # For disabled techs return prohibitive tco
  if (focl_pos == 0)
	tco_focl = 9999999999999
  else
	tco_focl <- calc_tco_simple(input, capex_focl, opex_focl)

  if (rts_pos == 0)
	tco_rts = 9999999999999
  else
	tco_rts <- calc_tco_simple(input, capex_rts, opex_rts)

  tco_rts_bcp <- tco_rts
  limit_rts <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity) # Add check for presense, and move this check up !
  if (limit_rts < bw_rec)
	tco_rts <- 9999999999999

  cown2matrix[1, 3] <- (-1)*tco_focl
  cown2matrix[1, 4] <- (-1)*tco_rts

  if (abs(tco_focl) < abs(tco_rts))
  {
	cown2matrix[1, 5] = 1
	cown2matrix[1, 6] = (-1)*tco_focl
  }
  else
  {
	cown2matrix[1, 5] = 2
	cown2matrix[1, 6] = (-1)*tco_rts
  }

  if (abs(cown2matrix[1, 6]) > 9999999999900) { # Bad situation, it is not possible to create topology with such bw restrictions
	cown2matrix[1, 5] <- 2 # TechIndex <- Index 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	cown2matrix[1, 6] <- (-1) * tco_rts_bcp
	cown2matrix[1, 4] <- (-1) * tco_rts_bcp
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Topology correctness warning: Bandwidth restrictions are too strong !"))
	#cat("Topology correctness warning: Bandwidth restrictions are too strong !", file = 'opt-warn.txt', APPEND = TRUE)
	cat("Topology correctness warning: Bandwidth restrictions are too strong !", sep = "\n", file = 'opt-warn.txt', APPEND = TRUE)
	# Add test ouptut to file or console
  }
  #print(cown2matrix)
  result <- cown2matrix

  return(result)
}

calc_capex_simple <- function(input, dist, bw, tech, intermediate = NULL) # using simplified formulas to calculate OPEX
{

  req(input)
  req(dist)
  req(bw)
  req(tech)

  #Other int reqs
  #req(input$IntParam.B_C_T)
  #req(input$IntParam.B_C)
  #req(input$IntParam.LC1)
  #req(input$IntParam.LC3)
  #req(input$IntParam.BPB)

  #Other ext reqs
  #req(input$ExtParam.SpectrumLicenseCostOne)
  #req(input$ExtParam.ServiceLicenseFeeOne)
  #req(input$ExtParam.ConstructionCostFOCL)
  #req(input$ExtParam.ConstructionCostRTS)
  #req(input$ExtParam.EquipSATL)
  #req(input$ExtParam.EquipCELL)

  #CAPEX (dist, bw, tech) = BC(bw, tech) * Construction_cost * dist + LC1(tech) * One time spectrum license fee * (bw / bits_per_baud_coef)
  # + LC3(tech) * One time service license fee

  # Check tech in [1..4]
  #print('t1')

  #input$IntParam.B_C <- c(2,1,1,1) #depends from tech
  trsh <- input$IntParam.B_C_T[[tech]]
  if (bw > trsh) {
	l_BC <- as.numeric(input$IntParam.B_C[[tech]])
  } else {
	l_BC <- 1
  }
  # to delete !
  #print('*******')
  #print(trsh)
  #print(l_BC)

  l_upscale_factor <- 1 # Scaling equipment and expeditures to upscale capacity

  #input$IntParam.LC1 <- c(0,1,0,0)
  if (input$IntParam.LC1[[tech]] != 0) {
	l_License_spectrum_onetime <- as.numeric(input$ExtParam.SpectrumLicenseCostOne)
  } else {
	l_License_spectrum_onetime <- 0
  }

  if (input$IntParam.LC3[[tech]] != 0) {
	l_license_service_onetime <- as.numeric(input$ExtParam.ServiceLicenseFeeOne)
  } else {
	l_license_service_onetime <- 0
  }

  if (tech == 1) {
	l_ConstrCost <- as.numeric(input$ExtParam.ConstructionCostFOCL)
	l_act_dist <- dist * (1 + as.numeric(input$IntParam.RoadTC))
  }

  if (tech == 2) {
	if (bw > as.numeric(input$ExtParam.RTS_1_MaxBw)) {
	  l_ConstrCost <- as.numeric(input$ExtParam.ConstructionCostRTS_2)
	  l_dist_factor <- ceiling(dist/as.numeric(input$ExtParam.ConstructionDistLimitRTS_2))
	  l_License_spectrum_onetime <- as.numeric(input$ExtParam.SpectrumLicenseCostOne_2)
	}
	else {
	  l_ConstrCost <- as.numeric(input$ExtParam.ConstructionCostRTS_1)
	  l_dist_factor <- ceiling(dist/as.numeric(input$ExtParam.ConstructionDistLimitRTS_1))
	  l_License_spectrum_onetime <- as.numeric(input$ExtParam.SpectrumLicenseCostOne_1)
	}

	l_act_dist <- l_dist_factor
  }
  if (tech == 3) {
	l_ConstrCost <- as.numeric(input$ExtParam.EquipSAT)

	if (bw > as.numeric(input$ExtParam.ScaleSAT_MaxBw))
	  l_upscale_factor <- ceiling(bw/as.numeric(input$ExtParam.ScaleSAT_MaxBw))
	else
	  l_upscale_factor <- 1

	l_act_dist <- 1
  }

  if (tech == 4) {
	l_ConstrCost <- as.numeric(input$ExtParam.EquipCELL)
	l_act_dist <- 1
  }

  l_BPB <- as.numeric(input$IntParam.BPB)

  #CAPEX (dist, bw, tech) = BC(bw, tech) * Construction_cost * dist + LC1(tech) * One time spectrum license fee * (bw / bits_per_baud_coef)
  # + LC3(tech) * One time service license fee

  #print('t2')
  #l_act_dist <- dist * (1 + as.numeric(input$IntParam.RoadTC))  # Road Tolography coef - FOCL only ! Accounted externaly
  #l_act_dist <- dist

  #if (tech == 2) {
  #print(sprintf("Capex Tech = 2 -> l_BC = %f l_ConstrCost = %f", l_BC, l_ConstrCost))
  #}

  capex <- as.numeric(l_BC * l_ConstrCost * l_act_dist * l_upscale_factor) +
	as.numeric(l_License_spectrum_onetime * (bw / l_BPB)) +
	as.numeric(l_license_service_onetime)
  return(capex)


}

calc_opex_simple <- function(input, dist, bw, tech, intermediate = NULL) # using simplified formulas to calculate OPEX
{

  req(input)
  req(dist)
  req(bw)
  req(tech)

  #Other int reqs
  #req(input$IntParam.B_C_T)
  #req(input$IntParam.B_C)
  req(input$IntParam.LC2)
  req(input$IntParam.PaybackPeriod)
  req(input$IntParam.BPB)
  req(input$IntParam.ISP_C)

  if (exists("bIgnoreInternetCost", input))
	l_ignore_int_cost <- input$bIgnoreInternetCost
  else
	l_ignore_int_cost <- F

  #Other ext reqs
  #req(input$ExtParam.SpectrumLicenseCostOne)
  #req(input$ExtParam.ServiceLicenseFeeOne)
  #req(input$ExtParam.ConstructionCostFOCL)
  #req(input$ExtParam.ConstructionCostRTS)
  #req(input$ExtParam.EquipSATL)
  #req(input$ExtParam.EquipCELL)

  #OPEX (dist, bw, opex) = (BC(bw, tech) * Operation_and_maintanance * dist + LC2(tech)*Annual_recurring_license_fee * (bw / bits_per_baud_coef)
  # + ISPC(tech) * Annual_ISP_fee(tech) * bw ) * PaybackPeriod

  # Check tech in [1..4]

  #input$IntParam.B_C <- c(2,1,1,1) #depends from tech

  trsh <- input$IntParam.B_C_T[[tech]]
  if (bw > trsh) {
	l_BC <- as.numeric(input$IntParam.B_C[[tech]])
  } else {
	l_BC <- 1
  }

  #input$IntParam.LC1 <- c(0,1,0,0)
  if (input$IntParam.LC2[[tech]] != 0) {
	l_License_spectrum_reccur <- as.numeric(input$ExtParam.SpectrumLicenseCostReccur)
  } else {
	l_License_spectrum_reccur <- 0
  }

  if (input$IntParam.ISP_C[[tech]] != 0) {
	l_ISPC <- as.numeric(1)
  } else {
	l_ISPC <- as.numeric(0)
  }

  l_upscale_factor <- 1

  if (tech == 1) {
	l_act_dist <- dist * (1 + as.numeric(input$IntParam.RoadTC))
	#l_OpMnCost <- as.numeric(input$ExtParam.ConstructionCostFOCL)
	l_OpMnCost <- as.numeric(input$ExtParam.AnnualMaintenanceCostFOCL)
	if (bw < 1024)
	  l_ISP_fee <- as.numeric(input$ExtParam.AnnualISPFeeFOCL1)
	else
	  l_ISP_fee <- as.numeric(input$ExtParam.AnnualISPFeeFOCL2)
	if (l_ignore_int_cost) # Ignore internet cost to avoid double accounting in case of cumulation
	  l_ISP_fee <- 0
  }

  if (tech == 2) {
	if (bw > as.numeric(input$ExtParam.RTS_1_MaxBw)) {
	  l_OpMnCost <- as.numeric(input$ExtParam.AnnualMaintenanceCostRTS_2)
	  l_dist_factor <- ceiling(dist/as.numeric(input$ExtParam.ConstructionDistLimitRTS_2))
	  l_License_spectrum_reccur <- as.numeric(input$ExtParam.SpectrumLicenseCostReccur_2)
	}
	else {
	  l_OpMnCost <- as.numeric(input$ExtParam.AnnualMaintenanceCostRTS_1)
	  l_dist_factor <- ceiling(dist/as.numeric(input$ExtParam.ConstructionDistLimitRTS_1))
	  l_License_spectrum_reccur <- as.numeric(input$ExtParam.SpectrumLicenseCostReccur_1)
	}
	#l_OpMnCost <- as.numeric(input$ExtParam.ConstructionCostRTS)
	#l_OpMnCost <- as.numeric(input$ExtParam.AnnualMaintenanceCostRTS)
	l_ISP_fee <- as.numeric(0)
	l_act_dist <- l_dist_factor
	if (l_ignore_int_cost) # Ignore internet cost to avoid double accounting in case of cumulation
	  l_ISP_fee <- 0
  }

  if (tech == 3) {
	l_OpMnCost <- as.numeric(input$ExtParam.EquipSAT)
	l_ISP_fee <- as.numeric(input$ExtParam.AnnualISPFeeSAT)

	if (bw > as.numeric(input$ExtParam.ScaleSAT_MaxBw))
	  l_upscale_factor <- ceiling(bw / as.numeric(input$ExtParam.ScaleSAT_MaxBw))
	else
	  l_upscale_factor <- 1

	l_act_dist <- 1
  }

  if (tech == 4) {
	l_OpMnCost <- as.numeric(input$ExtParam.EquipCELL)
	l_ISP_fee <- as.numeric(input$ExtParam.AnnualISPFeeCELL)
	l_act_dist <- 1
  }

  l_BPB <- as.numeric(input$IntParam.BPB)
  #l_act_dist <- dist * (1 + as.numeric(input$IntParam.RoadTC))
  #l_act_dist <- dist

  #OPEX (dist, bw, opex) = (BC(bw, tech) * Operation_and_maintanance * dist + LC2(tech)*Annual_recurring_license_fee * (bw / bits_per_baud_coef)
  # + ISPC(tech) * Annual_ISP_fee(tech) * bw ) * PaybackPeriod

  #if (tech == 2) {
  #print(sprintf("Opex Tech = 2 -> l_BC = %f l_OpMnCost = %f l_ISPC = %f, l_ISP_fee = %f", l_BC, l_OpMnCost, l_ISPC, l_ISP_fee))
  #}

  opex <- as.numeric(l_BC * l_OpMnCost * l_act_dist * l_upscale_factor) +
	as.numeric(l_License_spectrum_reccur * (bw / l_BPB)) +
	as.numeric(l_ISPC * l_ISP_fee * bw * l_upscale_factor)
  return(opex)

}

calc_tco_simple <- function(input, capex, opex,  intermediate = NULL) # using simplified formulas to calculate OPEX
{

  req(input)
  req(capex)
  req(opex)

  req(input$IntParam.PaybackPeriod)

  l_payback_period <- as.numeric(input$IntParam.PaybackPeriod)

  #print(l_payback_period)

  tco <- as.numeric(capex) + as.numeric(opex) * l_payback_period
  return(tco)

}

calc_npv_simple <- function(input, capex, opex,  bw, tech, techintermediate = NULL) # using simplified formulas to calculate NPV
{

  req(input)
  req(capex)
  req(opex)
  req(bw)
  req(tech)

  req(input$IntParam.K_DISC)
  req(input$IntParam.TAX_VAT)
  req(input$IntParam.TAX_PROF)
  req(input$IntParam.AMRT_C)
  req(input$IntParam.AFFORD_C)
  req(input$IntParam.FOCL_SPD_LIM)
  req(input$IntParam.BW_DM_C)


  req(input$IntParam.PaybackPeriod)
  req(input$ExtParam.AverageIncome)

  #Remove
  #print('Coefs:')
  #print(input$IntParam.K_DISC)
  #print(input$IntParam.TAX_VAT)
  #print(input$IntParam.TAX_PROF)
  #print(input$IntParam.AFFORD_C)
  #print(input$IntParam.FOCL_SPD_LIM)
  #print(input$IntParam.BW_DM_C)
  #print('/ Coefs')



  #Net Income  = AFFORD_C * avg_wage_year*  (FOCL_SPD_LIM - bw)*(1- Tax_VAT/100)
  #or AFFORD_C * avg_wage_year* bw*(1- Tax_VAT/100)
  l_mul <- as.numeric(input$IntParam.FOCL_SPD_LIM) - bw

  if (tech == 0) { # Income generation is for FOCL only
	l_NI <- (as.numeric(input$IntParam.AFFORD_C) / 100) *
	  as.numeric(input$ExtParam.AverageIncome) *
	  l_mul *
	  (1 - as.numeric(input$IntParam.TAX_VAT) / 100)
  }
  else {
	l_NI <- 0
  }

  npv <- (-1) * as.numeric(capex)
  l_payback_period <- input$IntParam.PaybackPeriod
  npv_temp <- ((l_NI -  as.numeric(opex))*(1-as.numeric(input$IntParam.TAX_PROF)/100) +  as.numeric(capex) * as.numeric(input$IntParam.AMRT_C)/100)
  for (j in 1:l_payback_period) {
	npv <- npv + npv_temp/((1+as.numeric(input$IntParam.K_DISC)/100)^j)
	#npv_temp <- npv_temp + ((l_NI -  as.numeric(opex))*(1-as.numeric(input$IntParam.TAX_PROF)/100) +  as.numeric(capex) * as.numeric(input$IntParam.AMRT_C)/100)
	#npv_temp <- npv_temp + (((l_NI - opex)*(1-as.numeric(input$IntParam.TAX_PROF)/100) + capex * as.numeric(input$IntParam.AMRT_C))/((1+as.numeric(input$IntParam.K_DISC))^j))
  }

  #npv <- npv_temp/((1+as.numeric(input$IntParam.K_DISC)/100)^j) - as.numeric(capex)
  ##npv <- npv_temp/((1+as.numeric(input$IntParam.K_DISC))^j) - as.numeric(capex)
  return(npv)

}


calc_npv_simple_upop <- function(input, capex, opex,  bw, unc_pop, tech, techintermediate = NULL) # using simplified formulas to calculate NPV. Previous version was using unc_pop
{

  req(input)
  req(capex)
  req(opex)
  req(bw)
  req(unc_pop)
  req(tech)

  req(input$IntParam.K_DISC)
  req(input$IntParam.TAX_VAT)
  req(input$IntParam.TAX_PROF)
  req(input$IntParam.AMRT_C)
  req(input$IntParam.AFFORD_C)
  req(input$IntParam.FOCL_SPD_LIM)
  req(input$IntParam.BW_DM_C)


  req(input$IntParam.PaybackPeriod)
  req(input$ExtParam.AverageIncome)

  #Net Income  = AFFORD_C * avg_wage* 12 * Min((FOCL_SPD_LIM - bw), unc_pop * BW_DM_C)*(1- Tax_VAT/100)

  if ((as.numeric(input$IntParam.FOCL_SPD_LIM) - bw) < (unc_pop * as.numeric(input$IntParam.BW_DM_C)))
	l_mul <- (as.numeric(input$IntParam.FOCL_SPD_LIM) - bw)
  else
	l_mul <- (unc_pop * as.numeric(input$IntParam.BW_DM_C))

  if (tech == 0) { # Income generation is for FOCL only
	l_NI <- (as.numeric(input$IntParam.AFFORD_C) / 100) *
	  as.numeric(input$ExtParam.AverageIncome) *
	  l_mul *
	  (1 - as.numeric(input$IntParam.TAX_VAT) / 100)
  }
  else {
	l_NI <- 0
  }

  npv_temp <- 0
  l_payback_period <- input$IntParam.PaybackPeriod
  for (j in 1:l_payback_period) {
	npv_temp <- npv_temp + ((l_NI - opex)*(1-as.numeric(input$IntParam.TAX_PROF)/100) + capex * as.numeric(input$IntParam.AMRT_C)/100)
	#npv_temp <- npv_temp + (((l_NI - opex)*(1-as.numeric(input$IntParam.TAX_PROF)/100) + capex * as.numeric(input$IntParam.AMRT_C))/((1+as.numeric(input$IntParam.K_DISC))^j))
  }

  npv <- npv_temp/((1+as.numeric(input$IntParam.K_DISC)/100)^j) - as.numeric(capex)
  #npv <- npv_temp/((1+as.numeric(input$IntParam.K_DISC))^j) - as.numeric(capex)
  return(npv)

}

calc_tco_simple_lm <- function(input, upop, tech = 2, road_dist = 0, intermediate = NULL) # using simplified formulas to calculate tco for the last mile
{

  req(input)
  #req(dist)
  req(upop)
  #req(tech)

  req(input$ExtParam.LmApCapacity)
  req(input$ExtParam.LmApConEqCost)
  req(input$ExtParam.LmApMntCost)
  req(input$IntParam.PaybackPeriod)
  req(input$ExtParam.LmApElectrCons)
  req(input$ExtParam.LmElectrCost)
  req(input$ExtParam.LmHouseholdSize)
  req(input$ExtParam.LmObjSize)
  req(input$ExtParam.LmApCoverage)


  #print("req")

  lm_tco <- 0
  l_payback_period <- as.numeric(input$IntParam.PaybackPeriod)

  if (tech == 1) { # FOCL

	#print("check_tech_1")

	#lm_road_to_focl <- as.numeric(input$ExtParam.LmRoadsToFOCL)
	obj_square <- as.numeric(input$ExtParam.LmObjSize) # in sq.km
	road_coef <- sqrt(2*obj_square/(3*sqrt(3)))/2
	lm_households_to_focl <- (road_coef*upop)/as.numeric(input$ExtParam.LmHouseholdSize)
	lm_constr <- as.numeric(input$ExtParam.ConstructionCostFOCL)
	lm_mnt <- as.numeric(input$ExtParam.AnnualMaintenanceCostFOCL)

	if (road_dist != 0)
	  lm_focl_dist <- road_dist
	else
	  lm_focl_dist <- lm_households_to_focl

	lm_capex <- lm_constr * lm_focl_dist
	lm_opex <- lm_ap_mnt * lm_focl_dist

	lm_tco <- as.numeric(lm_capex) + as.numeric(lm_opex) * l_payback_period
  }

  if (tech == 2) { # WiFI Based

	lm_ap_cap <- as.numeric(input$ExtParam.LmApCapacity)
	lm_ap_coneq <- as.numeric(input$ExtParam.LmApConEqCost)
	lm_ap_mnt <- as.numeric(input$ExtParam.LmApMntCost)
	lm_ap_el_cons <- as.numeric(input$ExtParam.LmApElectrCons)
	lm_el_cost <- as.numeric(input$ExtParam.LmElectrCost)

	#lm_ap_cov <- 0.4 # Move to LmParams !!!!
	lm_ap_cov <- as.numeric(input$ExtParam.LmApCoverage)
	obj_square <- as.numeric(input$ExtParam.LmObjSize) # in sq.km

	lm_ap_num <- 1
	if (upop > lm_ap_cap)
	  lm_ap_num_1 <- ceiling(upop / lm_ap_cap)
	else
	  lm_ap_num_1 <- 1

	if (obj_square > lm_ap_cov)
	  lm_ap_num_2 <- ceiling(obj_square / lm_ap_cov)
	else
	  lm_ap_num_2 <- 1

	lm_ap_num <- max(lm_ap_num_1,lm_ap_num_2)

	lm_capex <- lm_ap_coneq * lm_ap_num
	lm_opex <- (lm_ap_mnt + lm_ap_el_cons * lm_el_cost) * lm_ap_num

	lm_tco <- as.numeric(lm_capex) + as.numeric(lm_opex)  * l_payback_period
  }

  if (tech == 3) { # Cell Based

	lm_bs_cap <- as.numeric(input$ExtParam.LmBsCapacity)
	lm_bs_coneq <- as.numeric(input$ExtParam.LmBsConEqCost)
	lm_bs_mnt <- as.numeric(input$ExtParam.LmBsMntCost)
	lm_bs_el_cons <- as.numeric(input$ExtParam.LmBsElectrCons)
	lm_el_cost <- as.numeric(input$ExtParam.LmElectrCost)
	lm_el_cover <- as.numeric(input$ExtParam.LmBsCoverage)
	lm_obj_size <- as.numeric(input$ExtParam.LmObjSize)

	lm_bs_num_1 <- 1
	if (upop > lm_bs_cap)
	  lm_bs_num_1 <- ceiling(upop / lm_bs_cap)

	lm_bs_num_2 <- ceiling(lm_obj_size / lm_el_cover)
	if (lm_bs_num_1 >= lm_bs_num_2)
	  lm_bs_num <- lm_bs_num_1
	else
	  lm_bs_num <- lm_bs_num_2

	lm_capex <- lm_bs_coneq * lm_bs_num
	lm_opex <- (lm_bs_mnt + lm_bs_el_cons * lm_el_cost) * lm_bs_num

	lm_tco <- as.numeric(lm_capex) + as.numeric(lm_opex)  * l_payback_period
  }

  return(lm_tco)


}

calc_bn_focl_costless_addon <- function(input, dtf, bn_rb, intermediate = NULL) # from BN to the Internet via focl
{


  req(input)
  req(dtf)
  req(bn_rb)

  dist_rec <- as.numeric(dtf)
  bw_rec <- as.numeric(bn_rb)

  #Coefficient should be between 0 and 1
  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km, used for FOCL only
  dist_rec_byroads <- round(dist_rec * (1 + TopCoeff), digits = 2) # Now corrections are made inside of simplified fucntions

  focl_link_bw <-  as.numeric(input$TopologySettings.CostlessCapacity)
  focl_extracharge_bw <- as.numeric(input$TopologySettings.CostlessBwExtracharge)

  costless_focl <- dist_rec_byroads * (1 + log2(1 + floor(bw_rec / (2 * focl_link_bw))) * focl_extracharge_bw)

  result_costless <- as.numeric(costless_focl)

  return(result_costless)




  # !!! remove to params
  #focl_link_bw <- 10000
  #extracharge_bw <- 0.05


  if (focl_pos == 1) # COSTLESS_FOCL
	costless_focl <- dist_rec_byroads * (1 + log2(1 + floor(bw_rec / (2 * focl_link_bw))) * focl_extracharge_bw)
  else
	costless_focl <- 99999999999
}

calc_bn_focl_simple_cown_addon <- function(input, dtf, bn_rb, intermediate = NULL) # from BN to the Internet via focl
{


  req(input)
  req(dtf)
  req(bn_rb)

  dist_rec <- as.numeric(dtf)
  bw_rec <- as.numeric(bn_rb)

  #Coefficient should be between 0 and 1
  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km, used for FOCL only
  dist_rec_byroads <- round(dist_rec * (1 + TopCoeff), digits = 2) # Now corrections are made inside of simplified fucntions

  capex_focl <- calc_capex_simple(input, dist_rec_byroads, bw_rec, 1)
  opex_focl <- calc_opex_simple(input, dist_rec_byroads, bw_rec, 1)
  tco_focl <- calc_tco_simple(input, capex_focl, opex_focl)

  result_cown <- -as.numeric(tco_focl)

  return(result_cown)
}


calc_bn_focl_simple_npv_addon <- function(input, dtf, bn_rb, intermediate = NULL) # from BN to the Internet via focl
{

  req(input)
  req(dtf)
  req(bn_rb)

  dist_rec <- as.numeric(dtf)
  bw_rec <- as.numeric(bn_rb)

  #Coefficient should be between 0 and 1
  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km, used for FOCL only
  dist_rec_byroads <- round(dist_rec * (1 + TopCoeff), digits = 2) # Now corrections are made inside of simplified fucntions

  capex_focl <- calc_capex_simple(input, dist_rec_byroads, bw_rec, 1)
  opex_focl <- calc_opex_simple(input, dist_rec_byroads, bw_rec, 1)
  npv_focl <- calc_npv_simple(input, capex_focl, opex_focl, bw_rec, 1)

  result_npv <- as.numeric(npv_focl)

  return(result_npv)
}
