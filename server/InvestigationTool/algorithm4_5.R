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
	library(mapview)
  }
}

library(pheatmap)


#Determination of the Basenode

formula_4_5_1 <- function(input, output, intermediate = NULL)
{
  req(input)
  req(input$Files.ListOfObjects)
  req(input$TopologySettings.SNDTFThreshold)

  objects <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE) # Fix decimals

  req(objects)

  result <- find_basenode(input, objects)
  return(result)
}

#Demo for calculation of 6-row NPV matrix (from each regular node to BN, 4 possible technologies)

formula_4_5_2 <- function(input, output, intermediate = NULL)
{
  req(input)
  req(input$Files.ListOfObjects)
  req(input$Intermediate.BNIndex)
  # Add additional reqs !

  objects <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)

  req(objects)

  # Load NPV variables
  input_load <- input

  if (is.reactivevalues(input))
	input_load <- reactiveValuesToList(input)

  npv_set <- rjson::fromJSON(file = "ivars.json") # enable usage file name from widget
  npv_set_nrow <- length(result["shared_vars"][[1]])
  for (i in 1:npv_set_nrow) {
	text_line = paste("input_load$", str_trim(npv_set["shared_vars"][[1]][[i]][["name"]], side = "both"), " <- ", str_trim(npv_set["shared_vars"][[1]][[i]][["value"]]))
	# print(text_line)
	eval(parse(text = text_line))
	#eval(parse(text= paste("input_load$",str_trim(npv_set["shared_vars"][[1]][[i]][["name"]], side = "both"), " <- ",str_trim(npv_set["shared_vars"][[1]][[i]][["value"]] ))))
  }

  npv4matrix <- calc_4x_matrix_router(input_load, objects, input$Intermediate.BNIndex, use_metric) # was input

  #print(npv4matrix)
  result <- npv4matrix

  return(result)
}


#Sorting all regular nodes by distance, then by bandwidth requirement

formula_4_5_3 <- function(input, output, intermediate = NULL)
{
  req(input)
  req(input$Files.ListOfObjects)
  req(input$Intermediate.BNLon)
  req(input$Intermediate.BNLat)


  objects <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)

  req(objects)

  numberofobj <- nrow(objects)
  obj_plus_DTBN <- objects
  obj_plus_DTBN$DTBN = 9999999
  dist_to_BN <- 0

  if (numberofobj > 0)
  {
	for (i in 1:numberofobj)
	{
	  dist_to_BN <- distm(c(as.numeric(objects[i, 2]), as.numeric(objects[i, 3])), c(as.numeric(input$Intermediate.BNLon), as.numeric(input$Intermediate.BNLat)), fun = distHaversine)
	  obj_plus_DTBN[i,]$DTBN = dist_to_BN[1]
	  #obj_plus_DTBN [i,]$DTBN = 100
	  obj_plus_DTBN[i, 1] = iconv(as.character(obj_plus_DTBN[i, 1]), "UTF-8", "UTF-8", sub = '')
	  obj_plus_DTBN[i, 7] = iconv(as.character(obj_plus_DTBN[i, 7]), "UTF-8", "UTF-8", sub = '')

	}

  }


  result <- print(obj_plus_DTBN[order(obj_plus_DTBN$DTBN, -obj_plus_DTBN$RB),])
  print(obj_plus_DTBN[order(obj_plus_DTBN$DTBN, -obj_plus_DTBN$RB),])
  #result <- obj_plus_DTBN[1:100,]
  #result <- obj_plus_DTBN[1,]
  return(result)
}

#Visualization of the Basenode

formula_4_5_5 <- function(input, output, intermediate = NULL)
{
  req(input)
  req(input$Files.ListOfObjects)
  req(input$TopologySettings.SNDTFThreshold)

  objects <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)

  req(objects)

  numberofobj <- nrow(objects)

  snodes_candidates_ind = c()
  #min_dtf = 99999999
  if (numberofobj > 0)
  {
	for (i in 1:numberofobj)
	{
	  dtf <- objects[i, 5]

	  if (dtf <= as.integer(input$TopologySettings.SNDTFThreshold))
		snodes_candidates_ind <- append(snodes_candidates_ind, i)
	  #if min_dtf > dtf
	  #{
	  #	append(snodes_candidates_ind,i)
	  #	min_dtf = dtf
	  #}

	}
	#print(paste("base nodes candidates: ",snodes_candidates_ind,sep=","))
	#print(paste(snodes_candidates_ind))
  }


  longs <- pull(objects[, 2])
  lats <- pull(objects[, 3])
  #loc_names <- pull(objects[,1])
  loc_names = iconv(pull(objects[, 1]), "UTF-8", "UTF-8", sub = '')
  weight_bandw <- pull(objects[, 4])
  cog_result <- COGravity(longs, lats, , weight_bandw)

  cog_result_long <- cog_result[1]
  cog_result_lat <- cog_result[3]

  if (numberofobj > 5000) {
	v1 <- data.frame(ids = 1:numberofobj, name = as.character(1:numberofobj), x = as.numeric(longs), y = as.numeric(lats))
  }
  else {
	v1 <- data.frame(ids = 1:numberofobj, name = loc_names, x = as.numeric(longs), y = as.numeric(lats))
	#print(v1$name)
  }

  if (length(snodes_candidates_ind) == 1)
  {
	#BaseNode = objects [i,]
	BaseNode = objects[snodes_candidates_ind[1],]
	BaseNode$ind = snodes_candidates_ind[1]
	#print (BaseNode)

	#return (BaseNode)
	# result_map <- renderLeaflet({leaflet(data=v1[1:numberofobj,]) %>% addTiles() %>%
	# addMarkers(lng=v1$x, lat=v1$y, popup=v1$name) %>%
	# addAwesomeMarkers(lng=as.numeric(BaseNode[,2]), lat=as.numeric(BaseNode[,3]),
	# icon = awesomeIcons(markerColor = "red"),
	# popup=paste(iconv(BaseNode[,1], "UTF-8", "UTF-8",sub=''),"- BaseNode of the cluster"))
	# })
	result <- list(v1, BaseNode)
	return(result)
  }

  if (length(snodes_candidates_ind) == 0)
  {
	min_dist_m = 99999999
	BaseNode_ind <- 0
	if (numberofobj > 0)
	{
	  for (i in 1:numberofobj)
	  {
		cur_dist_m <- distm(c(as.numeric(objects[i, 2]), as.numeric(objects[i, 3])), c(as.numeric(cog_result_long), as.numeric(cog_result_lat)), fun = distHaversine)[1]
		if (cur_dist_m < min_dist_m)
		{
		  BaseNode_ind <- i
		  min_dist_m <- cur_dist_m
		}

	  }

	  BaseNode = objects[BaseNode_ind,]
	  BaseNode$ind = BaseNode_ind
	  #print (BaseNode)

	  # result_map <- renderLeaflet({leaflet(data=v1[1:numberofobj,]) %>% addTiles() %>%
	  # addMarkers(lng=v1$x, lat=v1$y, popup=v1$name) %>%
	  # addAwesomeMarkers(lng=as.numeric(BaseNode[,2]), lat=as.numeric(BaseNode[,3]),
	  # icon = awesomeIcons(markerColor = "red"),
	  # popup=paste(iconv(BaseNode[,1], "UTF-8", "UTF-8",sub=''),"- BaseNode of the cluster"))
	  # })
	  result <- list(v1, BaseNode)
	  return(result)
	}
  }
  else
  {
	min_dist_m <- 99999999
	BaseNode_ind <- 0
	for (i in 1:length(snodes_candidates_ind))
	{
	  cur_dist_m <- as.double(distm(c(as.numeric(objects[snodes_candidates_ind[i], 2]), as.numeric(objects[snodes_candidates_ind[i], 3])), c(as.numeric(cog_result_long), as.numeric(cog_result_lat)), fun = distHaversine))[1]
	  #print(min_dist_m)
	  #print(cur_dist_m)
	  if (cur_dist_m < min_dist_m)
	  {
		#BaseNode_ind <- snodes_candidates_ind[i]
		BaseNode_ind <- as.integer(snodes_candidates_ind[i])
		min_dist_m <- cur_dist_m
	  }
	}
	BaseNode = objects[BaseNode_ind,]
	BaseNode$ind = as.integer(BaseNode_ind)
	#BaseNode$ind = as.integer(snodes_candidates_ind[BaseNode_ind])
	print(BaseNode)
	#print(BaseNode_ind)
	#print(BaseNode$ind)
	#print(v1$name)

	# result_map <- renderLeaflet({leaflet(data=v1[1:numberofobj,]) %>% addTiles() %>%
	# addMarkers(lng=v1$x, lat=v1$y, popup=v1$name) %>%
	# #addMarkers(lng=v1$x, lat=v1$y) %>%
	# #addProviderTiles(providers$Stamen.TonerLite) %>%
	# addAwesomeMarkers(lng=as.numeric(BaseNode[,2]), lat=as.numeric(BaseNode[,3]),
	# icon = awesomeIcons(markerColor = "red"),
	# popup=paste(iconv(BaseNode[,1], "UTF-8", "UTF-8",sub=''),"- BaseNode of the cluster"))
	# })
	result <- list(v1, BaseNode)
	return(result)

	#return (BaseNode)
	#}

  }
}

formula_4_5_6 <- function(input_load, output, objects, intermediate = NULL)
{

  req(input_load)
  req(objects)

  # Load NPV variables
  #input_load <- input
  tech_dic = c("FOCL", "RTS", "SAT", "CELL")

  #if (is.reactivevalues(input))
  #input_load <- reactiveValuesToList(input)
  #else
  #input_load <- input

  # Enabling stub mode for fast computation
  input_load$use_stub <- F

  use_cown <- F # using tco as metric by default otherwise using npv
  if (input_load$TopologySettings.UseCOWN == 1)
	use_cown <- T

  use_metric <- 0 # using classic npv or tco as metric by default
  if (input_load$TopologySettings.UseMetric == 1) # Use simplified metric
	use_metric <- 1
  if (input_load$TopologySettings.UseMetric == 2) # Use costless metric
	use_metric <- 2


  global_log_enabled <- F # Set in F to run in production
  if (exists("bWriteLog", input_load)) global_log_enabled <- T   # Enabling global logging in case of production

  #if (exists("bIgnoreInternetCost", input_load)) input_load$bIgnoreInternetCost <- globlaIgnoreInternetCost   # Setup usage of InternetCost for calculation process
  globlaIgnoreInternetCost <- F
  input_load$bIgnoreInternetCost <- globlaIgnoreInternetCost

  if (exists("TopologySettings.EnableGL", input_load)) {   # Enabling global logging if running as webapp
	if (input_load$TopologySettings.EnableGL == 1) {
	  global_log_enabled <- T
	}
  }

  log_to_files <- F
  if (exists("TopologySettings.LogToFile", input_load)) {
	if (input_load$TopologySettings.LogToFile == 1)
	  log_to_files <- T
  }

  do_tace_postopt <- 0 # Provides additioanl tracing of postoptimization phase

  #npv_set <- fromJSON(file = "ivars.json")
  #if (exists("Files.NPVSettings", input_load)) {
  #if (global_log_enabled) {
  #  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_6: Trying to load additional parameters from json.file"), input_load$Files.NPVSettings, sep = ": "))
  #}
  #npv_set <- fromJSON(file = input_load$Files.NPVSettings)
  #npv_set_nrow <- length(npv_set["shared_vars"][[1]])
  #for (i in 1:npv_set_nrow) {
  #  text_line = paste("input_load$", str_trim(npv_set["shared_vars"][[1]][[i]][["name"]], side = "both"), " <- ", str_trim(npv_set["shared_vars"][[1]][[i]][["value"]]))
  #  # print(text_line)
  #  eval(parse(text = text_line))
  #  #eval(parse(text= paste("input_load$",str_trim(npv_set["shared_vars"][[1]][[i]][["name"]], side = "both"), " <- ",str_trim(npv_set["shared_vars"][[1]][[i]][["value"]] ))))
  #}
  #}
  # Add all other requirements

  #objects <- vroom::vroom(input_load$Files.ListOfObjects, altrep = FALSE) # Fix decimals

  # Initialize log files
  if (log_to_files)
	cat("Step-by-step construction process:", file = "steps-log.txt", sep = "\n", append = FALSE)

  #cat("Postoptimization log:", file = "opt-log.txt", sep = "\n", append = FALSE)
  cat("New cluster:", file = "opt-log.txt", sep = "\n", append = TRUE)
  cat("New cluster:", file = "opt-warn.txt", sep = "\n", append = TRUE)


  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Step-by-step construction process initiated:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Using cluster data, contains %d objects", nrow(objects))))
	if (use_cown)
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Using Cost-of-Ownership as optimization metric"))
	else
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Using NPV as optimization metric"))
  }

  is_cell_excluded <- 1
  is_dc_nodes <- 0

  #print(sprintf("1 Radio MaximumLinkCapacity %f", input_load$InitialDataRadio.MaximumLinkCapacity))
  #print(sprintf("1 Satelite MaximumLinkCapacity %f", input_load$InitialDataSatellite.MaximumLinkCapacity))
  #print(sprintf("1 Cellular MaximumLinkCapacity %f", input_load$InitialDataCellular.MaximumLinkCapacity))

  PostoptEnabled <- input_load$TopologySettings.EnablePostopt
  #FOCL_Thrld <- input_load$TopologySettings.FOCLBottleTrh # Check as rudiment
  #RTS_Thrld <- input_load$TopologySettings.RTSBottleTrh # Check as rudiment
  RTS_Thrld <- as.numeric(input_load$InitialDataRadio.MaximumLinkCapacity)
  isFOCLBased <- input_load$TopologySettings.FOCLBased
  isRTSBased <- input_load$TopologySettings.RTSBased
  is_cell_excluded <- input_load$TopologySettings.ExcludeCellular
  is_dc_nodes <- input_load$TopologySettings.DCNodes

  #rts_disabled <- _loadTopologySettings.DisableRTS
  #if (rts_disabled == 1)
  #rts_possible_global <- 0
  #else
  #rts_possible_global <- 1

  #if (input_load$InitialData.UseRTS == 0 && input_load$InitialData.UseFOCL == 0) { # Commented after sat. only topology was introduced
  #.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
  #.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Both of FOCL and RTS should not be disabled"))
  #.GlobalEnv$top_calc_status = 604
  #return(0)
  #}

  if (input_load$InitialData.UseRTS == 0 &&
	input_load$InitialData.UseFOCL == 0 &&
	input_load$InitialData.UseSatellite == 0) { # Too many restrictions
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: FOCL,RTS and SAT should not be disabled simultaneously"))
	.GlobalEnv$top_calc_status = 604
	return(0)
  }


  if (input_load$InitialData.UseRTS == 0 && input_load$InitialData.UseFOCL == 0) {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: As FOCL and RTS are disabled Sattelite only topology enabled"))
	#.GlobalEnv$top_calc_status = 604
	result <- formula_4_5_6_satellite(input_load, output, objects)
	return(result)
  }

  #if (PostoptEnabled) { To decide a bit later
  if (input_load$InitialData.UseRTS == 0 && isRTSBased) {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Incompatible parameters, RTSBased and disabled RTS"))
	.GlobalEnv$top_calc_status = 605
	return(0)
  }

  if (input_load$InitialData.UseFOCL == 0 && isFOCLBased) {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Incompatible parameters, FOCLBased and disabled FOCL"))
	.GlobalEnv$top_calc_status = 606
	return(0)
  }

  if (isRTSBased && isFOCLBased) {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Incompatible parameters, FOCLBased and RTSBased"))
	.GlobalEnv$top_calc_status = 607
	return(0)
  }

  #}

  if (!PostoptEnabled && (isRTSBased || isFOCLBased)) {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Incompatible parameters, Postopt and FOCL/RTSBased"))
	.GlobalEnv$top_calc_status = 608
	return(0)
  }

  rts_possible_global <- 1
  if (input_load$InitialData.UseRTS == 0) {
	rts_possible_global <- 0
	if (global_log_enabled)
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: RTS technology disabled "))
  }

  focl_possible_global <- 1
  if (input_load$InitialData.UseFOCL == 0) {
	focl_possible_global <- 0
	if (global_log_enabled)
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: FOCL technology disabled "))
  }

  #celsat_disabled <- input_load$TopologySettings.DisCelSat
  cells_disabled <- 0
  if (input_load$InitialData.UseCellular == 0) {
	cells_disabled <- 1
	if (global_log_enabled) {
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: CELLULAR technology disabled "))
	}
  }

  if (cells_disabled == 1 && is_cell_excluded == 1) {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Incompatible parameters, Excluded 4G and disabled CELLULAR"))
	.GlobalEnv$top_calc_status = 605
	return(0)
  }

  sat_disabled <- 0
  if (input_load$InitialData.UseSatellite == 0) {
	sat_disabled <- 1
	if (global_log_enabled) {
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: SAT technology disabled "))
	}
  }

  #print(sprintf("rts_possible_global %d ------------------- ",rts_possible_global))
  #print(sprintf("celsat_disabled %d ------------------- ",celsat_disabled))

  #print(sprintf("PostoptEnabled %d ------------------- ",PostoptEnabled))

  # 1 --- Finding BaseNode ----------------

  print(nrow(objects)) # Debug number of objects in cluster
  if (nrow(objects) == 1) {
	#print(objects)
  }
  BaseNode <- find_basenode(input_load, objects)
  #return (result)
  if (global_log_enabled)
	#.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_6: BaseNode found "), i18n$t(paste(BaseNode$ind, " ", BaseNode[1])), sep = ": "))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: BaseNode found "))
  # 1 -------------------------------------

  # 2 --- Sorting nodes by distance to BN, then by RB

  numberofobj <- nrow(objects)
  obj_plus_DTBN <- objects
  obj_plus_DTBN$DTBN = 9999999
  dist_to_BN <- 0

  if (numberofobj > 0) {
	for (i in 1:numberofobj) {
	  dist_to_BN <- distm(c(as.numeric(objects[i, 2]), as.numeric(objects[i, 3])), c(as.numeric(BaseNode[[2]]), as.numeric(BaseNode[[3]])), fun = distHaversine)
	  obj_plus_DTBN[i,]$DTBN = as.numeric(dist_to_BN[1] / 1000)
	  obj_plus_DTBN[i, 1] = iconv(as.character(obj_plus_DTBN[i, 1]), "UTF-8", "UTF-8", sub = '')
	  obj_plus_DTBN[i, 7] = iconv(as.character(obj_plus_DTBN[i, 7]), "UTF-8", "UTF-8", sub = '')
	}
  }

  interim_sorted_nodes <- obj_plus_DTBN[order(obj_plus_DTBN$DTBN, -obj_plus_DTBN$RB),]
  #print(obj_plus_DTBN[order(obj_plus_DTBN$DTBN,-obj_plus_DTBN$RB),])
  #browser()

  # 2 -----------------------------------------------

  # 2A --- Preparing main node_table ----------------

  # 2B --- Preparing adhoc data for map visualization ---------------

  numberofnodes <- nrow(interim_sorted_nodes)
  longs <- pull(interim_sorted_nodes[, 2])
  lats <- pull(interim_sorted_nodes[, 3])
  loc_names = iconv(pull(interim_sorted_nodes[, 1]), "UTF-8", "UTF-8", sub = '')

  v1 <- data.frame(ids = 1:numberofnodes, name = loc_names, x = as.numeric(longs), y = as.numeric(lats)) # visualization data
  #print(v1$name)


  # 2B --------------------------------------------------------

  if (numberofobj < 1)
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Insufficeient number of nodes"))
	#print ("Insufficeient number of nodes")
	return(0)
  }

  #OBJECTNAME    LON   LAT    RB   DTF COV   REGION  DTBN HopsToBN NextNode   FRB   NPV  Tech  isBN isLeaf NPVC

  # Initialize main lookup table
  main_table <- interim_sorted_nodes[2:numberofobj,] # Main table should not contain the BN
  main_table$HopsToBN <- as.integer(0)
  main_table$NextNode <- as.integer(-1) # -1 - disconnected / 0 - Connected to BN
  main_table$FRB <- as.numeric(0)
  main_table$NPV <- as.numeric(0)
  main_table$Tech <- as.integer(0)
  main_table$isBN <- as.integer(0)
  main_table$isLeaf <- as.integer(0)
  main_table$NPVC <- as.numeric(0)
  main_table$Lm <- as.numeric(0)
  main_table$DistToNext <- as.numeric(0)

  # Initialize BaseNode table
  main_BN <- interim_sorted_nodes[1,]
  main_BN$HopsToBN <- as.integer(0)
  main_BN$NextNode <- as.integer(0)
  main_BN$FRB <- as.numeric(main_BN$RB)
  main_BN$NPV <- as.numeric(0)
  main_BN$Tech <- as.integer(0)
  main_BN$isBN <- as.integer(1)
  main_BN$isLeaf <- as.integer(0)
  main_BN$NPVC <- as.numeric(0)

  #print(main_table)
  if (numberofobj > 1) { # to support 1-node clusters !!!

	# 2A ----------------------------------------------

	# Just for testing
	# for (i in 2: numberofobj)
	# {
	# temp_npv_matrix <- calc_npv_pair(input, objects[i,], BaseNode, intermediate = NULL)
	# print(temp_npv_matrix)
	# }

	# 3 --- Preparing 4-tech 6-rows matrix

	#bnindex = BaseNode[[8]]
	# bnindex = BaseNode[["ind"]]
	# npv4matrix <- calc_4x_matrix(input, objects, bnindex)
	#print("before 4x")
	#print(sprintf("PVOptionSet.PaybackPeriod %f", as.numeric(input_load$PVOptionSet.PaybackPeriod)))

	bnindex = 1
	if (use_cown) {
	  npv4matrix <- calc_4x_matrix_router_cown(input_load, interim_sorted_nodes, bnindex, use_metric) # use COWN
	} else {
	  npv4matrix <- calc_4x_matrix_router(input_load, interim_sorted_nodes, bnindex, use_metric) # use NPV
	}

	#print("after 4x")

	if (log_to_files) {
	  write.table(npv4matrix, file = "npv4matrix.txt", row.names = TRUE, col.names = TRUE, append = FALSE)
	  write.table(npv4matrix, file = "main_table.txt", row.names = TRUE, col.names = TRUE, append = FALSE)
	}

	# print(npv4matrix)
	#result <- npv4matrix

	npv4matrix_bcp <- npv4matrix # Backup for enabling
	if (isFOCLBased) {
	  if (is_cell_excluded == 0)
		npv4matrix <- recalc_4x(npv4matrix, 1, 0, 1, 1) # ? sat 0 cell 0
	  else
		npv4matrix <- recalc_4x(npv4matrix, 1, 0, 1, 0) # ? sat 0 cell 0
	}

	if (isRTSBased) {
	  if (is_cell_excluded == 0)
		npv4matrix <- recalc_4x(npv4matrix, 0, 1, 1, 1) # ? sat 0 cell 0
	  else
		npv4matrix <- recalc_4x(npv4matrix, 0, 1, 1, 0) # ? sat 0 cell 0
	}

	if (!isFOCLBased && !isRTSBased)
	  npv4matrix <- recalc_4x(npv4matrix, focl_possible_global, rts_possible_global, sat_disabled, cells_disabled)

	npv4rs <- nrow(npv4matrix)

	# removed to recalc_4x
	# 3 ----------------------------------

	# --- Main Loop ----------------------
	mrs <- nrow(main_table)

	if (mrs < 1) {
	  result <- list(v1, BaseNode, main_BN)
	  return(result)
	}


	# Calculating first row, correspoding to 1st node connected directly to BN
	main_table$HopsToBN[1] = 1
	main_table$NextNode[1] = 0
	main_table$FRB[1] = main_table$RB[1]
	main_table$isLeaf[1] = 1
	main_table$NPVC[1] = npv4matrix[2, 8]
	main_table$Tech[1] = npv4matrix[2, 7] # Check 2-line
	main_table$NPV[1] = npv4matrix[2, 8]


	main_BN$FRB <- main_BN$RB + main_table$RB[1]
	main_BN$isLeaf <- 0

	# npv4matrix[which(npv4matrix$RB[1,] == min(m1$RB))[1],]

	if (mrs > 1) # more than 1 row in main lookup table
	{
	  for (c_line in 2:mrs) #c_line - current line in main lookup table
	  {

		# Question ?! Priority of 4G-exclusion vs DCNode own FOCL connection

		if (is_cell_excluded == 1 && (main_table$COV[c_line] == '4G' || main_table$COV[c_line] == '3G')) {
		  if (global_log_enabled) {
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Excluding 4G/3G enabled node %d name %s", c_line, main_table$OBJECTNAME[c_line])))
		  }

		  main_table$NextNode[c_line] = 0 # Connected directly to BN
		  main_table$HopsToBN[c_line] = 1

		  main_table$isLeaf[c_line] = 1
		  main_BN$isLeaf <- 0

		  #main_BN$FRB <- main_BN$FRB + main_table$RB[c_line]
		  main_table$FRB[c_line] <- main_table$RB[c_line]

		  # No bandwidth limits checking is required for BN
		  main_table$Tech[c_line] <- 4 # Cell is set to be best Tech
		  main_table$NPV[c_line] <- npv4matrix[c_line + 1, 6]  # Cell's NPV/COWN is set to be best NPV/COWN
		  main_table$NPVC[c_line] <- main_table$NPV[c_line]

		  next
		}

		if (is_dc_nodes == 1 && main_table$DTF[c_line] == 0) {
		  if (global_log_enabled) {
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: DCNode was found ! %d name %s", c_line, main_table$OBJECTNAME[c_line])))
		  }

		  main_table$NextNode[c_line] = 0 # Connected by own FOCL //1st idea -> c_line
		  main_table$HopsToBN[c_line] = 0 #1st idea -> -1

		  main_table$isLeaf[c_line] = 1
		  main_BN$isLeaf <- 0   #1st idea do not alter #main_BN$isLeaf <- 0

		  main_BN$FRB <- main_BN$FRB + main_table$RB[c_line] # update Full RB for BN anyway
		  main_table$FRB[c_line] <- main_table$RB[c_line] # ? -> 0

		  # No bandwidth limits checking is required for BN
		  main_table$Tech[c_line] <- 1 # Own FOCL !!!
		  main_table$NPV[c_line] <- 0 # Own FOCL !!!
		  main_table$NPVC[c_line] <- 0 # Own FOCL !!!

		  next
		}

		best_tech_BN <- npv4matrix[c_line + 1, 7]
		best_npv_BN <- npv4matrix[c_line + 1, 8]
		distance_to_BN <- npv4matrix[c_line + 1, 1]
		RB_to_BN <- npv4matrix[c_line + 1, 2]
		#print(sprintf("c_line: %d toBN - best tech is %f , npv is %f ------------------- ",c_line, best_tech_BN, best_npv_BN))

		if (log_to_files)
		  cat(sprintf("c_line: %d Dist to BN: %f, RB to BN %f, toBN - best tech is %f , npv is %f ------------------- ", c_line, distance_to_BN, RB_to_BN, best_tech_BN, best_npv_BN), file = "steps-log.txt", sep = "\n", append = TRUE)

		#if (global_log_enabled) {
		#.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Processing node (%d) %s Dist to BN: %f, RB  %f, BestTech to BN: %s, Metric (npv/cown) to BN is %f ", c_line, main_table$OBJECTNAME[c_line], distance_to_BN, RB_to_BN, tech_dic[best_tech_BN], best_npv_BN)))
		#}

		temp_npv_vec <- c()
		temp_tech_vec <- c()
		max_node_npv <- -99999999999
		t_j <- 1 #index for temp vectors
		temp_saved_index <- c()
		for (i in 1:(c_line - 1)) # iterate through  previously connected nodes
		{
		  candidate <- main_table[i,]
		  #browser()

		  #If CELL or SAT and no-postopt skip candidate
		  if (PostoptEnabled == 0 && (candidate$Tech == 3 || candidate$Tech == 4)) {
			next
		  }
		  # Also skip candidate even in case of post opt, this is a try to make results better
		  if (PostoptEnabled == 1 && (candidate$Tech == 3 || candidate$Tech == 4)) {
			next
		  }
		  # If 4G excluded skip 4G
		  if (is_cell_excluded == 1 && candidate$Tech == 4) {
			next
		  }


		  #Implement later: Peliminary bandwidth bottleneck checking (start_node, add_rb, thrld)
		  rts_possible <- 1
		  if (rts_possible_global == 0) {
			rts_possible <- 0
		  } # Disable RTS from widget
		  else {
			if (PostoptEnabled == 0) {
			  #print("checking RTS ...")
			  #rts_possible <- frb_check_rts(main_table, i, main_table$RB[c_line], RTS_Thrld)
			  rts_possible <- 1
			  #print(rts_possible)
			}
			  #rts_possible <- 1 # Correct this
			else
			  #rts_possible <- frb_check_postopt(main_table, i, main_table$RB[c_line], RTS_Thrld)
			  rts_possible <- 1  # Correct this
		  }

		  if (focl_possible_global == 0) {
			focl_possible <- 0
		  } # Disable RTS from widget
		  else {
			if (PostoptEnabled == 0)
			  #focl_possible <- frb_check(main_table, i, main_table$RB[c_line], FOCL_Thrld)
			  focl_possible <- 1
			else
			  #focl_possible <- frb_check_postopt(main_table, i, main_table$RB[c_line], FOCL_Thrld)
			  focl_possible <- 1
		  }

		  if (isFOCLBased) {
			focl_possible <- 1
			rts_possible <- 0
		  }

		  if (isRTSBased) {
			focl_possible <- 0
			rts_possible <- 1
		  }

		  #if (PostoptEnabled == 0) {
		  #  focl_possible <- frb_check(main_table, i, main_table$RB[c_line], FOCL_Thrld)
		  #  rts_possible <- frb_check(main_table, i, main_table$RB[c_line], RTS_Thrld)
		  #}
		  #else {
		  #  focl_possible <- frb_check_postopt(main_table, i, main_table$RB[c_line], FOCL_Thrld)
		  #  if (rts_possible_global == 1) { # Disable RTS from widget
		  #	rts_possible <- frb_check_postopt(main_table, i, main_table$RB[c_line], RTS_Thrld)
		  #  }
		  #  else {
		  #	rts_possible <- 0
		  #  }
		  #}
		  # print(sprintf("Checking bottlenecks for candidate %d -> %s ... FOCL_Thrld %f focl_possible %d RTS_Thrld %f rts_possible %d", i, candidate[[1]], FOCL_Thrld, focl_possible, RTS_Thrld, rts_possible))

		  # toRemove #if ((PostoptEnabled == 0) && (rts_possible == 1)) { # If no postopt, then we need to check RTS limits for possible bandwidth (focl - no limits applied, cell and sat are checked on the first stage)
		  #	rts_possible <- frb_check(main_table, i, main_table$RB[c_line], RTS_Thrld)
		  #}

		  bottle_chk <- 1
		  bottle_chk <- frb_check_rts(main_table, i, main_table$RB[c_line], RTS_Thrld) # 1 - is OK
		  if (bottle_chk == 0) next
		  if (focl_possible == 0 && rts_possible == 0) next # It's not possible to connect to candidate not via focl nor rts, skip it

		  #Preliminary distance checking as next step
		  tmp_dist <- distm(c(as.numeric(main_table[c_line, 2]), as.numeric(main_table[c_line, 3])), c(as.numeric(candidate[[2]]), as.numeric(candidate[[3]])), fun = distHaversine) # Replace index to named-values
		  dist_to_candidate <- round(as.numeric(tmp_dist / 1000), digits = 4)

		  # print(sprintf("Distance to candidate %f dist_to_BN %f", dist_to_candidate, distance_to_BN))
		  if (log_to_files) {
			cat(sprintf("Distance to candidate %f dist_to_BN %f", dist_to_candidate, distance_to_BN), file = "steps-log.txt", sep = "\n", append = TRUE)
			cat(sprintf("Candidate tech is %d ", candidate$Tech), file = "steps-log.txt", sep = "\n", append = TRUE)
		  }

		  if (dist_to_candidate < distance_to_BN) {
			#temp_res_matrix <- calc_npv_pair(input, main_table[c_line,1:8], main_table[i,1:8]) # from obj1 to obj2
			if (!use_cown)
			  temp_res_matrix <- calc_npv_pair_router(input_load, main_table[c_line, 1:8], candidate[1:8], focl_possible, rts_possible, use_metric) # from obj1 to obj2 , was input
			else
			  temp_res_matrix <- calc_cown_pair_router(input_load, main_table[c_line, 1:8], candidate[1:8], focl_possible, rts_possible, use_metric) # from obj1 to obj2 , was input

			temp_npv_vec[t_j] <- temp_res_matrix[6]
			temp_tech_vec[t_j] <- temp_res_matrix[5]
			temp_saved_index[t_j] <- i # saving index of the outter loop, required for distance filtering
			# print(sprintf("processing : %d row of main t., c.temp index is, %d ,res_matrix is %f %f %f %f %f %f------------------- ",i,t_j, temp_res_matrix[1],temp_res_matrix[2],temp_res_matrix[3],temp_res_matrix[4],temp_res_matrix[5],temp_res_matrix[6]))
			if (log_to_files)
			  cat(sprintf("processing candidate: %d row of main t., c.temp index is, %d ,res_matrix is %f %f %f %f %f %f------------------- ", i, t_j, temp_res_matrix[1], temp_res_matrix[2], temp_res_matrix[3], temp_res_matrix[4], temp_res_matrix[5], temp_res_matrix[6]), file = "steps-log.txt", sep = "\n", append = TRUE)
			#if (global_log_enabled) {
			#.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Processing candidate for the link (%d) %s Metrics - FOCL: %f, RTS  %f, Best Tech: %s, Best Metric: %f ", i, main_table$OBJECTNAME[i], temp_res_matrix[3], temp_res_matrix[4], tech_dic[temp_res_matrix[5]], temp_res_matrix[6])))
			#}
			t_j <- t_j + 1
		  }
		}

		#OBJECTNAME    LON   LAT    RB   DTF COV   REGION  DTBN HopsToBN NextNode   FRB   NPV  Tech  isBN isLeaf ifConnPosible

		if (length(temp_npv_vec) > 0) max_node_npv = max(temp_npv_vec) # check possibility of an empty set

		# print(sprintf("len of temp_npv_vec : %d max_node_npv, %f ******************* ",length(temp_npv_vec), max_node_npv))
		if (log_to_files)
		  cat(sprintf("len of temp_npv_vec : %d max_node_npv, %f ******************* ", length(temp_npv_vec), max_node_npv), file = "steps-log.txt", sep = "\n", append = TRUE)

		if (length(temp_npv_vec) > 0 && max_node_npv > best_npv_BN)
		{
		  max_node_index = which(temp_npv_vec == max_node_npv)[1]
		  ntc_index = temp_saved_index[max_node_index] # node to connect index
		  main_table$NextNode[c_line] = ntc_index

		  main_table$Tech[c_line] = temp_tech_vec[max_node_index]
		  main_table$NPV[c_line] = max_node_npv

		  main_table$FRB[c_line] = main_table$RB[c_line]
		  main_BN$FRB <- main_BN$FRB + main_table$RB[c_line] # update Full RB for BN anyway

		  main_table$isLeaf[c_line] <- 1
		  main_table$isLeaf[ntc_index] <- 0

		  main_table$HopsToBN[c_line] <- main_table$HopsToBN[ntc_index] + 1

		  # Update FRB for all subsequent nodes in the chain / from main_table ntc_index
		  # chain_length <- frb_update(ntc_index, main_table$RB[c_line])

		  chain_length <- 1
		  node_i <- ntc_index
		  #browser()

		  #if (global_log_enabled) {
		  #.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Connecting (%d) %s  to the %d %s using: %s", c_line, main_table$OBJECTNAME[c_line], ntc_index, main_table$OBJECTNAME[ntc_index], tech_dic[main_table$Tech[c_line]])))
		  #}

		  while (node_i != 0)
		  {
			if (chain_length > 50000) # Remove in production
			{
			  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Looping is too long"))
			  #print("Looping is too long")
			  break
			}
			main_table$FRB[node_i] <- (main_table$FRB[node_i] + main_table$RB[c_line])
			if (main_table$NextNode[node_i] != 0)
			{
			  node_i = main_table$NextNode[node_i]
			  chain_length = chain_length + 1
			}
			else break

		  }

		  # print(sprintf("Updating FRBs ... NextNodeIndex %d HopsToBN %d chain_length %d",ntc_index, main_table$HopsToBN[c_line], chain_length))
		}
		else
		{
		  main_table$NextNode[c_line] = 0 # Connected directly to BN
		  main_table$HopsToBN[c_line] = 1

		  main_table$isLeaf[c_line] = 1
		  main_BN$isLeaf <- 0

		  main_BN$FRB <- main_BN$FRB + main_table$RB[c_line]
		  main_table$FRB[c_line] <- main_table$RB[c_line]

		  # No bandwidth limits checking is required for BN
		  main_table$Tech[c_line] = npv4matrix[c_line + 1, 7] # Check 2-line
		  main_table$NPV[c_line] = npv4matrix[c_line + 1, 8]

		  #if (global_log_enabled) {
		  #.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Connecting (%d) %s to the BaseNode directly, using %s", c_line, main_table$OBJECTNAME[c_line], tech_dic[main_table$Tech[c_line]])))
		  #}
		}
	  }
	}
	# ------------------------------------
	# browser()

	#Calculate some stats
	npv_total <- 0
	npv_cum_total <- 0
	num_focl <- 0
	num_rts <- 0
	num_sat <- 0
	num_cell <- 0
	lm_total <- 0
	len_focl <- 0
	len_rts <- 0
	len_cell <- 0
	len_sat <- 0
	rb_focl <- 0
	rb_rts <- 0
	rb_sat <- 0
	rb_cell <- 0
	frb_focl <- 0
	frb_rts <- 0
	frb_cell <- 0
	frb_sat <- 0
	metric_focl <- 0
	metric_rts <- 0
	metric_sat <- 0
	metric_cell <- 0

	if (isFOCLBased || isRTSBased) {
	  npv4matrix <- npv4matrix_bcp
	  npv4matrix <- recalc_4x(npv4matrix, focl_possible_global, rts_possible_global, sat_disabled, cells_disabled)
	}

	globlaIgnoreInternetCost <- T # disable internet accounting for FOCL anf RTS links when calculating cummulative values
	input_load$bIgnoreInternetCost <- globlaIgnoreInternetCost

	if (sat_disabled == 0 || cells_disabled == 0) {
	  # Recalculate LeafNodes for possible SAT|CELLs
	  if (global_log_enabled) {
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Recalculating leaf nodes")))
	  }
	  for (leaf_line in 1:mrs) {
		if (main_table$isLeaf[leaf_line] == 1) { # Recalculate best tech if it's a Leaf node and CELL/SAT is possible

		  if (is_cell_excluded == 1 && main_table$Tech[leaf_line] == 4) {
			next
		  }

		  sat_npv_BN <- npv4matrix[leaf_line + 1, 5]
		  cell_npv_BN <- npv4matrix[leaf_line + 1, 6]

		  if (sat_npv_BN > cell_npv_BN) {
			best_tech_BN <- 3
			best_npv_BN <- sat_npv_BN
		  }
		  else {
			best_tech_BN <- 4
			best_npv_BN <- cell_npv_BN
		  }

		  #distance_to_BN <- npv4matrix[leaf_line + 1, 1]
		  #RB_to_BN <- npv4matrix[leaf_line + 1, 2]

		  cur_node_npv <- main_table$NPV[leaf_line]
		  cur_node_tech <- main_table$Tech[leaf_line]
		  if (best_npv_BN > cur_node_npv) { # >=
			if (global_log_enabled) {
			  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Leaf node %d %s technology %d was changed to %d, npv prev %f -> new %f ", leaf_line, main_table$OBJECTNAME[leaf_line], cur_node_tech, best_tech_BN, cur_node_npv, best_npv_BN)))
			}

			# NextNode isLeaf <- 1
			next_node <- main_table$NextNode[leaf_line]
			main_table$isLeaf[next_node] <- 1

			main_table$Tech[leaf_line] <- best_tech_BN
			main_table$NPV[leaf_line] <- best_npv_BN
			main_table$NPVC[leaf_line] <- best_npv_BN
			main_table$FRB[leaf_line] <- main_table$RB[leaf_line] #?
			main_table$HopsToBN[leaf_line] <- 1

			main_table$NextNode[leaf_line] <- 0 # Connect directly to BN
		  }
		}
	  }
	}

	for (stat_line in 1:mrs)
	{
	  #bcp_table <- main_table
	  #npv_total <- npv_total + main_table$NPV[stat_line]
	  next_node <- main_table$NextNode[stat_line]
	  bcp_rb <- main_table$RB[stat_line]
	  main_table$RB[stat_line] <- main_table$FRB[stat_line]

	  if (PostoptEnabled == 0) {
		# Bind cum_npv to already selected technology
		if (main_table$Tech[stat_line] == 1)
		{
		  cur_tech_npv_index <- 3
		}
		else
		{
		  cur_tech_npv_index <- 4
		}

		if (next_node != 0)
		{
		  if (!use_cown)
			temp <- calc_npv_pair_router(input_load, main_table[stat_line, 1:8], main_table[next_node, 1:8], focl_possible_global, rts_possible_global, use_metric)
		  else
			temp <- calc_cown_pair_router(input_load, main_table[stat_line, 1:8], main_table[next_node, 1:8], focl_possible_global, rts_possible_global, use_metric)
		  npv_cum <- temp[[cur_tech_npv_index]]
		}
		else
		{
		  if (main_table$Tech[stat_line] < 3) #SAT or CELL is not applicable to FPVC
		  {
			if (!use_cown)
			  temp <- calc_npv_pair_router(input_load, main_table[stat_line, 1:8], main_BN[1, 1:8], focl_possible_global, rts_possible_global, use_metric)
			else
			  temp <- calc_cown_pair_router(input_load, main_table[stat_line, 1:8], main_BN[1, 1:8], focl_possible_global, rts_possible_global, use_metric)
			npv_cum <- temp[[cur_tech_npv_index]]
		  }
		  else
		  {
			npv_cum <- main_table$NPV[stat_line]
		  }
		}
	  }
	  else { # Case of Postopimization with replacement of transit 4g and Sats to FOCL or RTS
		# if (main_table$isLeaf[stat_line] == 0) { # Recalculate FNPV and best tech if it's NOT a Leaf node
		#if ((main_table$isLeaf[stat_line] == 1) && (main_table$Tech[stat_line] > 2))
		# next
		if (next_node != 0)
		{
		  if (!use_cown)
			temp <- calc_npv_pair_router(input_load, main_table[stat_line, 1:8], main_table[next_node, 1:8], focl_possible_global, rts_possible_global, use_metric)
		  else
			temp <- calc_cown_pair_router(input_load, main_table[stat_line, 1:8], main_table[next_node, 1:8], focl_possible_global, rts_possible_global, use_metric)

		  #cat(sprintf("PostOpt c_line: %d Old tech: %d RB: %f FRB: %f focl_pos: %d rts_pos: %d ", stat_line, as.integer(main_table$Tech[stat_line]), bcp_rb, main_table$RB[stat_line], as.integer(focl_possible_global), as.integer(rts_possible_global)), file = "opt-log.txt", sep = "\n", append = TRUE)
		  #cat(sprintf("PostOpt c_line: %d Old NPV : %f Old NPVC: %f", stat_line, main_table$NPV[stat_line], main_table$NPVC[stat_line]), file = "opt-log.txt", sep = "\n", append = TRUE)
		  old_tech <- main_table$Tech[stat_line]
		  new_tech <- temp[[5]]
		  if (do_tace_postopt == 1 && old_tech != new_tech) {
			print(sprintf("PostOpt c_line: %d Tech is changed !", stat_line))
			print(sprintf("PostOpt c_line: %d Old tech: %d RB: %f FRB: %f focl_pos: %d rts_pos: %d ", stat_line, as.integer(main_table$Tech[stat_line]), bcp_rb, main_table$FRB[stat_line], as.integer(focl_possible_global), as.integer(rts_possible_global)))
			print(sprintf("PostOpt c_line: %d Old NPV : %f Old NPVC: %f", stat_line, main_table$NPV[stat_line], main_table$NPVC[stat_line]))
			print(sprintf("PostOpt c_line: %d FOCL NPVC : %f RTS NPVC: %f Best: %d %f", stat_line, temp[[3]], temp[[4]], temp[[5]], temp[[6]]))
			cat(sprintf("PostOpt c_line: %d Tech is changed !", stat_line), file = "opt-log.txt", sep = "\n", append = TRUE)
			cat(sprintf("PostOpt c_line: %d Old tech: %d RB: %f FRB: %f focl_pos: %d rts_pos: %d ", stat_line, as.integer(main_table$Tech[stat_line]), bcp_rb, main_table$RB[stat_line], as.integer(focl_possible_global), as.integer(rts_possible_global)), file = "opt-log.txt", sep = "\n", append = TRUE)
			cat(sprintf("PostOpt c_line: %d Old NPV : %f Old NPVC: %f", stat_line, main_table$NPV[stat_line], main_table$NPVC[stat_line]), file = "opt-log.txt", sep = "\n", append = TRUE)
			cat(sprintf("PostOpt c_line: %d FOCL NPVC : %f RTS NPVC: %f Best: %d %f", stat_line, temp[[3]], temp[[4]], temp[[5]], temp[[6]]), file = "opt-log.txt", sep = "\n", append = TRUE)
		  }
		  npv_cum <- temp[[6]]
		  main_table$Tech[stat_line] <- temp[[5]] #new_tech
		  main_table$RB[stat_line] <- bcp_rb
		  if (!use_cown)
			temp_after <- calc_npv_pair_router(input_load, main_table[stat_line, 1:8], main_table[next_node, 1:8], focl_possible_global, rts_possible_global, use_metric)
		  else
			temp_after <- calc_cown_pair_router(input_load, main_table[stat_line, 1:8], main_table[next_node, 1:8], focl_possible_global, rts_possible_global, use_metric)
		  main_table$NPV[stat_line] <- temp_after[[6]]

		  if (do_tace_postopt == 1 && old_tech != new_tech) {
			print(sprintf("PostOpt c_line: %d New Tech %d New NPV: %f New NPVC: %f ", stat_line, as.integer(main_table$Tech[stat_line]), main_table$NPV[stat_line], npv_cum))
			cat(sprintf("PostOpt c_line: %d New Tech %d New NPV: %f New NPVC: %f ", stat_line, as.integer(main_table$Tech[stat_line]), main_table$NPV[stat_line], npv_cum), file = "opt-log.txt", sep = "\n", append = TRUE)
		  }
		}
		else {
		  # if (is_cell_excluded == 0 || (is_cell_excluded == 1 && main_table$Tech[stat_line] != 4)) {
		  if (is_dc_nodes == 0 || (is_dc_nodes == 1 && main_table$DTF[stat_line] != 0)) {
			if (is_cell_excluded == 0 || (is_cell_excluded == 1 && main_table$Tech[stat_line] != 4))
			  if (main_table$isLeaf[stat_line] == 0 || (main_table$isLeaf[stat_line] == 1 &&
				main_table$Tech[stat_line] != 3 &&
				main_table$Tech[stat_line] != 4)) {
				if (!use_cown)
				  temp <- calc_npv_pair_router(input_load, main_table[stat_line, 1:8], main_BN[1, 1:8], focl_possible_global, rts_possible_global, use_metric)
				else
				  temp <- calc_cown_pair_router(input_load, main_table[stat_line, 1:8], main_BN[1, 1:8], focl_possible_global, rts_possible_global, use_metric)

				#cat(sprintf("PostOpt c_line: %d Old tech: %d RB: %f FRB: %f focl_pos: %d rts_pos: %d ", stat_line, as.integer(main_table$Tech[stat_line]), bcp_rb, main_table$RB[stat_line], as.integer(focl_possible_global), as.integer(rts_possible_global)), file = "opt-log.txt", sep = "\n", append = TRUE)
				#cat(sprintf("PostOpt c_line: %d Old NPV : %f Old NPVC: %f", stat_line, main_table$NPV[stat_line], main_table$NPVC[stat_line]), file = "opt-log.txt", sep = "\n", append = TRUE)

				npv_cum <- temp[[6]]
				main_table$Tech[stat_line] <- temp[[5]]
				main_table$RB[stat_line] <- bcp_rb
				if (!use_cown)
				  temp_after <- calc_npv_pair_router(input_load, main_table[stat_line, 1:8], main_BN[1, 1:8], focl_possible_global, rts_possible_global, use_metric)
				else
				  temp_after <- calc_cown_pair_router(input_load, main_table[stat_line, 1:8], main_BN[1, 1:8], focl_possible_global, rts_possible_global, use_metric)
				main_table$NPV[stat_line] <- temp_after[[6]]

				#cat(sprintf("PostOpt c_line: %d New Tech %d New NPV: %f New NPVC: %f ", stat_line, as.integer(main_table$Tech[stat_line]), main_table$NPV[stat_line], npv_cum), file = "opt-log.txt", sep = "\n", append = TRUE)
			  } else { # if leaf connected with SAT or CELL there is no to optimize
				npv_cum <- main_table$NPV[stat_line]
				#main_table$RB[stat_line] <- bcp_rb # already done later
			  }
		  }
		}
	  }

	  if (is_cell_excluded == 1 && main_table$Tech[stat_line] == 4)
		npv_cum <- main_table$NPV[stat_line]

	  if (is_dc_nodes == 1 && main_table$DTF[stat_line] == 0)
		npv_cum <- 0

	  main_table$NPVC[stat_line] <- npv_cum
	  npv_cum_total <- npv_cum_total + npv_cum
	  npv_total <- npv_total + main_table$NPV[stat_line]
	  main_table$RB[stat_line] <- bcp_rb

	  temp_next <- main_table$NextNode[stat_line]
	  if (temp_next != 0)
		temp_dist <- calc_final_dist(input_load, main_table[stat_line, 1:8], main_table[temp_next, 1:8])
	  else
		temp_dist <- calc_final_dist(input_load, main_table[stat_line, 1:8], main_BN[1, 1:8])
	  main_table$DistToNext[stat_line] <- temp_dist

	  if (main_table$Tech[stat_line] == 1) {
		num_focl <- num_focl + 1
		len_focl <- len_focl + main_table$DistToNext[stat_line]
		rb_focl <- rb_focl + main_table$RB[stat_line]
		frb_focl <- frb_focl + main_table$FRB[stat_line]
		metric_focl <- metric_focl + main_table$NPVC[stat_line]
	  }
	  if (main_table$Tech[stat_line] == 2) {
		num_rts <- num_rts + 1
		len_rts <- len_rts + main_table$DistToNext[stat_line]
		rb_rts <- rb_rts + main_table$RB[stat_line]
		frb_rts <- frb_rts + main_table$FRB[stat_line]
		metric_rts <- metric_rts + main_table$NPVC[stat_line]
	  }

	  if (main_table$Tech[stat_line] == 3) {
		num_sat <- num_sat + 1
		len_sat <- len_sat + main_table$DistToNext[stat_line]
		rb_sat <- rb_sat + main_table$RB[stat_line]
		frb_sat <- frb_sat + main_table$RB[stat_line]
		metric_sat <- metric_sat + main_table$NPVC[stat_line]
	  }

	  if (main_table$Tech[stat_line] == 4) {
		num_cell <- num_cell + 1
		len_cell <- len_cell + main_table$DistToNext[stat_line]
		rb_cell <- rb_cell + main_table$RB[stat_line]
		frb_cell <- frb_cell + main_table$RB[stat_line]
		metric_cell <- metric_cell + main_table$NPVC[stat_line]
	  }

	  if (as.numeric(input_load$TopologySettings.LastMile) == 1) {
		lm_tech <- as.numeric(input_load$TopologySettings.LMTech)
		# IntParam.BW_DM_C
		# l_BW_DM_C <- 3
		if (input_load$TopologySettings.DataFileFormat == 0) {
		  l_BW_DM_C <- input_load$IntParam.BW_DM_C
		  l_upop <- main_table$RB[stat_line] / l_BW_DM_C
		  lm_roads <- 0
		}
		else {
		  l_upop <- main_table$UPOP[stat_line]
		  lm_roads <- main_table$ROADS[stat_line]
		}
		#print("check1")
		lm_temp <- calc_tco_simple_lm(input_load, l_upop, lm_tech, lm_roads)
		#print("check_last")
		main_table$Lm[stat_line] <- lm_temp
		lm_total <- lm_total + lm_temp
		#print(lm_temp)

	  }
	  else {
		main_table$Lm[stat_line] <- 0
	  }
	}
  } else {
	total_links <- 0
	num_focl <- 0
	num_rts <- 0
	num_sat <- 0
	num_cell <- 0
	len_focl <- 0
	frb_focl <- 0
	metric_focl <- 0
	npv_total <- 0
	npv_cum_total <- 0
	npv_cum_addon <- 0
	npv_dtf_addon <- 0
	len_rts <- 0
	frb_rts <- 0
	metric_rts <- 0
	len_sat <- 0
	frb_sat <- 0
	metric_sat <- 0
	len_cell <- 0
	frb_cell <- 0
	metric_cell <- 0
	capex_cum_total <- 0
	opex_cum_total <- 0
	lm_total <- 0
	capex_cum_total <- 0
	opex_cum_total <- 0
	mrs <- 0
	npv4matrix <- matrix(nrow = numberofobj, ncol = 8, 0)
  }
  globlaIgnoreInternetCost <- F
  input_load$bIgnoreInternetCost <- globlaIgnoreInternetCost # Setting back policy of InternetCost usage

  npv_cum_addon <- 0 # Calculate additional internet costs for FOCl and RTS
  if (use_metric == 0) { # Not simplified methods for npv and tco calculations
	if (!use_cown)
	  npv_cum_addon <- calc_internet_addon_npv(input_load, main_BN$FRB)
	else
	  npv_cum_addon <- calc_internet_addon_cown(input_load, main_BN$FRB)
  }
  if (use_metric == 1) { # Simplified methods for npv and tco calculations
	if (!use_cown)
	  npv_cum_addon <- calc_internet_addon_simple_npv(input_load, main_BN$FRB)
	else
	  npv_cum_addon <- calc_internet_addon_simple_cown(input_load, main_BN$FRB)
  }

  npv_dtf_addon <- 0 # Calculate addititonal cost for the case when BaseNode is not yet connected to the Internet directly via fiber
  if (use_metric == 0) {
	if (BaseNode$DTF != 0) {
	  if (!use_cown)
		npv_dtf_addon <- calc_bn_focl_npv_addon(input_load, BaseNode$DTF, main_BN$FRB)
	  else
		npv_dtf_addon <- calc_bn_focl_cown_addon(input_load, BaseNode$DTF, main_BN$FRB)
	}
  }

  if (use_metric == 1) {
	if (BaseNode$DTF != 0) {
	  if (!use_cown)
		npv_dtf_addon <- calc_bn_focl_simple_npv_addon(input_load, BaseNode$DTF, main_BN$FRB)
	  else
		npv_dtf_addon <- calc_bn_focl_simple_cown_addon(input_load, BaseNode$DTF, main_BN$FRB)
	}
  }

  if (use_metric == 2) {  # case of simplified and over-simplified
	if (BaseNode$DTF != 0) {
	  costless_dtf_addon <- calc_bn_focl_costless_addon(input_load, BaseNode$DTF, main_BN$FRB)
	  #npv_dtf_addon <- -1 * as.numeric(costless_dtf_addon)
	  npv_dtf_addon <- 0
	} else {
	  npv_dtf_addon <- 0
	  costless_dtf_addon <- 0
	}
  }

  if (use_metric > 2) {  # case of some future simplified methods
	if (BaseNode$DTF != 0) {
	  if (!use_cown)
		npv_dtf_addon <- 0
	  else
		npv_dtf_addon <- 0
	}
  }

  capex_cum_total <- 0 # Add Required Calculations
  opex_cum_total <- 0

  npv_cum_total <- npv_cum_total + npv_cum_addon + npv_dtf_addon # Show last addon

  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Caclulating  NPV/COWN addon for InternetCost - %f", npv_cum_addon)))
  }

  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Caclulating  NPV/COWN addon for connection BN to the Internet (DTF>0) - %f", npv_dtf_addon)))
  }

  total_links = mrs
  if (!use_cown)
	# add mpv calculation for lm_total and 1 for default number of clusters
	stats_v <- c(total_links, num_focl, num_rts, num_sat, num_cell, round(npv_total, 2), round(npv_cum_total, 2), round(npv_cum_addon, 2), round(npv_dtf_addon, 2), 0, 1, len_focl, frb_focl, metric_focl, len_rts, frb_rts, metric_rts, len_sat, frb_sat, metric_sat, len_cell, frb_cell, metric_cell, capex_cum_total, opex_cum_total)
  else
	stats_v <- c(total_links, num_focl, num_rts, num_sat, num_cell, -1 * round(npv_total, 2), -1 * round(npv_cum_total, 2), -1 * round(npv_cum_addon, 2), -1 * round(npv_dtf_addon, 2), round(lm_total, 2), 1, len_focl, frb_focl, -1 * metric_focl, len_rts, frb_rts, -1 * metric_rts, len_sat, frb_sat, -1 * metric_sat, len_cell, frb_cell, -1 * metric_cell, capex_cum_total, opex_cum_total)

  if (as.numeric(input_load$TopologySettings.UseMetric) == 2) {
	unit_price <- as.numeric(input_load$TopologySettings.CostlessUnitPrice)
	unit_price_mnt <- as.numeric(input_load$TopologySettings.CostlessUnitPriceMnt) # Imply an additional component of stat_v [24]
	stats_v <- c(total_links, num_focl, num_rts, num_sat, num_cell, -1 * round(npv_total, 2), -1 * round(npv_cum_total, 2), -1 * round(npv_cum_total * unit_price, 2), round(costless_dtf_addon, 2), 0, 1, len_focl, frb_focl, -1 * metric_focl, len_rts, frb_rts, -1 * metric_rts, len_sat, frb_sat, -1 * metric_sat, len_cell, frb_cell, -1 * metric_cell, -1 * round(npv_cum_total * unit_price_mnt, 2))
	#stats_v <- c(total_links, num_focl, num_rts, num_sat, num_cell, -1 * round(npv_total, 2), -1 * round(npv_cum_total, 2), -1 * round(npv_cum_total * unit_price, 2), -1 * round(npv_dtf_addon, 2), 0, 1, len_focl, frb_focl, -1 * metric_focl, len_rts, frb_rts, -1 * metric_rts, len_sat, frb_sat, -1 * metric_sat, len_cell, frb_cell, -1 * metric_cell, -1 * round(npv_cum_total * unit_price_mnt, 2))
  }
  #result <- list(v1,main_BN,main_table)
  temp_lables <- c()
  #iconv(pull(interim_sorted_nodes[, 1]), "UTF-8", "UTF-8", sub = '')
  temp_lables[1] <- sprintf("%s RB: %f FRB: %f M: %f CM: %f", iconv(as.character(main_BN$OBJECTNAME[1]), "UTF-8", "UTF-8", sub = ''), main_BN$RB[1], main_BN$FRB[1], main_BN$NPV[1], main_BN$NPVC[1])
  #temp_lables[1] <- v1$name[1]
  if (numberofobj > 1)
	for (i in 2:nrow(v1)) {
	  #temp_lables[i] <- sprintf("%s <br> RB: %f FRB: %f <br> M: %f CM: %f LM: %f", iconv(main_table$OBJECTNAME[i - 1], "UTF-8", "UTF-8", sub = ''), main_table$RB[i - 1], main_table$FRB[i - 1], main_table$NPV[i - 1], main_table$NPVC[i - 1], main_table$Lm[i-1])
	  temp_lables[i] <- sprintf("%s <br> RB: %f FRB: %f <br> M: %f CM: %f", iconv(as.character(main_table$OBJECTNAME[i - 1]), "UTF-8", "UTF-8", sub = ''), main_table$RB[i - 1], main_table$FRB[i - 1], main_table$NPV[i - 1], main_table$NPVC[i - 1])
	}
  v1$name <- temp_lables
  result <- list(v1, main_BN, main_table, stats_v, npv4matrix)
  return(result)
}

formula_4_5_7 <- function(input_load, output, intermediate = NULL)
{

  req(input_load)

  # Load NPV variables
  #input_load <- input
  tech_dic = c("FOCL", "RTS", "SAT", "CELL")

  # Enabling stub mode for fast computation
  input_load$use_stub <- F

  use_cown <- F
  if (input_load$TopologySettings.UseCOWN == 1)
	use_cown <- T

  global_log_enabled <- F # Set in F to run in production
  if (exists("bWriteLog", input_load)) global_log_enabled <- T   # Enabling global logging in case of production

  #if (exists("bIgnoreInternetCost", input_load)) input_load$bIgnoreInternetCost <- globlaIgnoreInternetCost   # Setup usage of InternetCost for calculation process
  globlaIgnoreInternetCost <- F
  input_load$bIgnoreInternetCost <- globlaIgnoreInternetCost

  if (exists("TopologySettings.EnableGL", input_load)) {   # Enabling global logging if running as webapp
	if (input_load$TopologySettings.EnableGL == 1) {
	  global_log_enabled <- T
	}
  }

  log_to_files <- F
  if (exists("TopologySettings.LogToFile", input_load)) {
	if (input_load$TopologySettings.LogToFile == 1)
	  log_to_files <- T
  }

  if (global_log_enabled) {
	if (use_cown)
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_7: Using Cost-of-Ownership as optimization metric"))
	else
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_7: Using NPV as optimization metric"))
  }

  dist_values <- c(1, 2, 5, 10, 25, 50, 100, 250, 500, 1000)
  bandw_values <- c(1, 5, 10, 25, 50, 100, 250, 500, 1000, 5000)

  result_matrix_focl <- matrix(nrow = length(dist_values), ncol = length(bandw_values), 0)
  result_matrix_rts <- matrix(nrow = length(dist_values), ncol = length(bandw_values), 0)
  result_matrix_sat <- matrix(nrow = length(dist_values), ncol = length(bandw_values), 0)
  result_matrix_cell <- matrix(nrow = length(dist_values), ncol = length(bandw_values), 0)
  result_matrix_best <- matrix(nrow = length(dist_values), ncol = length(bandw_values), 0)

  caption_string <- paste('x,', paste(bandw_values, collapse = ",", sep = ","))
  cat(caption_string, file = "tech_focl_data.txt", sep = "\n", append = FALSE)
  cat(caption_string, file = "tech_rts_data.txt", sep = "\n", append = FALSE)
  cat(caption_string, file = "tech_sat_data.txt", sep = "\n", append = FALSE)
  cat(caption_string, file = "tech_cell_data.txt", sep = "\n", append = FALSE)
  cat(caption_string, file = "tech_best_data.txt", sep = "\n", append = FALSE)

  for (i in 1:length(dist_values)) { # Distance cycle
	for (j in 1:length(bandw_values)) # Bandwidth cycle
	{
	  temp_matix <- calc_npv_est(input_load, dist_values[i], bandw_values[j])
	  result_matrix_focl[i, j] <- temp_matix[1, 3]
	  result_matrix_rts[i, j] <- temp_matix[1, 4]
	  result_matrix_sat[i, j] <- temp_matix[1, 5]
	  result_matrix_cell[i, j] <- temp_matix[1, 6]
	  result_matrix_best[i, j] <- temp_matix[1, 7]

	  #npv4matrix[i, 3] <- 0 # npv_focl
	  #npv4matrix[i, 4] <- 0 # npv_rts
	  #npv4matrix[i, 5] <- 0 # npv_satelite
	  #npv4matrix[i, 6] <- 0 # npv_cellular
	  #npv4matrix[i, 7] <- as.integer(0) # best tech
	  #npv4matrix[i, 8] <- 0 # best npv

	}
	cat(paste(as.character(dist_values[i]), paste(result_matrix_focl[i,], collapse = ",", sep = ","), sep = ","), file = "tech_focl_data.txt", sep = "\n", append = TRUE)
	cat(paste(as.character(dist_values[i]), paste(result_matrix_rts[i,], collapse = ",", sep = ","), sep = ","), file = "tech_rts_data.txt", sep = "\n", append = TRUE)
	cat(paste(as.character(dist_values[i]), paste(result_matrix_sat[i,], collapse = ",", sep = ","), sep = ","), file = "tech_sat_data.txt", sep = "\n", append = TRUE)
	cat(paste(as.character(dist_values[i]), paste(result_matrix_cell[i,], collapse = ",", sep = ","), sep = ","), file = "tech_cell_data.txt", sep = "\n", append = TRUE)
	cat(paste(as.character(dist_values[i]), paste(result_matrix_best[i,], collapse = ",", sep = ","), sep = ","), file = "tech_best_data.txt", sep = "\n", append = TRUE)

  }

  print(result_matrix_focl)
  print(result_matrix_rts)
  print(result_matrix_sat)
  print(result_matrix_cell)
  print(result_matrix_best)

  return(result_matrix_best)
}

recalc_4x <- function(npv4matrix, focl_possible_global = 1, rts_possible_global = 1, sat_disabled = 0, cells_disabled = 0)
{
  npv4rs <- nrow(npv4matrix)

  # Apply disabling for FOCL and CELLULAR if necessary
  if (focl_possible_global == 0) {
	#npv4rs <- nrow(npv4matrix)
	for (i in 2:npv4rs) {
	  if (npv4matrix[i, 3] > -9999999999990) { # Make sure that FOCL and RTS are not disabled simultaneously
		npv4matrix[i, 3] <- -100000000000000 # Making FOCL impossible
	  }
	  else {
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Topology correctness warning: RTS Bandwidth restrictions are too strong !"))
		#cat("Topology correctness warning: Bandwidth restrictions are too strong !", file = 'opt-warn.txt', APPEND = TRUE)
		cat("Topology correctness warning: Bandwidth restrictions are too strong !", sep = "\n", file = 'opt-warn.txt', APPEND = TRUE)
	  }
	}
  }

  if (rts_possible_global == 0) {
	#npv4rs <- nrow(npv4matrix)
	for (i in 2:npv4rs) {
	  npv4matrix[i, 4] <- -100000000000000 # Making RTS impossible
	}
  }

  # Disable SAT and CELLULAR if necessary
  if (sat_disabled == 1) {
	#npv4rs <- nrow(npv4matrix)
	for (i in 2:npv4rs) {
	  npv4matrix[i, 5] <- -100000000000000 # Making SAT impossible
	}
  }

  if (cells_disabled == 1) {
	#npv4rs <- nrow(npv4matrix)
	for (i in 2:npv4rs) {
	  npv4matrix[i, 6] <- -100000000000000 # Making CELL impossible
	}
  }

  # Recalculate 4x_matrix's the best tech and the metric
  for (i in 2:npv4rs) {
	best_npv_index <- which(npv4matrix[i, 3:6] == max(npv4matrix[i, 3:6]))[1] # 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	if (best_npv_index < 1 || best_npv_index > 4)
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_npv: Incorrect technology index"))
	  .GlobalEnv$top_calc_status = 604
	  #print ("Incorrect technology index")
	  #browser()
	  return(0)
	}
	best_npv_value <- npv4matrix[[i, (2 + best_npv_index)]]
	npv4matrix[i, 7] <- best_npv_index # TechIndex <- Index 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	npv4matrix[i, 8] <- best_npv_value
  }

  return(npv4matrix)
}

frb_update <- function(start_node, add_rb) # updates FRB field in main_table for chain started at start_node
{
  #OBJECTNAME    LON   LAT    RB   DTF COV   REGION  DTBN HopsToBN NextNode   FRB   NPV  Tech  isBN isLeaf NPVC
  #Aghavnavank	45.09	40.72	122.12	100.00	2G	AM-TV	18.75	1.00	0.00	122.12	-348756.32	1.00	0	1.00	1.00
  counter <- 1
  node_i <- start_node
  while (node_i != 0) {
	if (counter > 1000) # Remove in production
	{
	  print("Looping is too long")
	  return(-1)
	}
	main_table$FRB[node_i] <<- (main_table$FRB[node_i] + add_rb)
	if (main_table$NextNode[node_i] != 0)
	{
	  node_i = main_table$NextNode[node_i]
	  counter = counter + 1
	}
	else break
  }
  return(counter)
}

#frb_check <- function(start_node, add_rb, thrld_focl, thrld_rts) # Check FRB for bottleneck, focl and rts separately. Implement later ...

frb_check <- function(node_table, start_node, add_rb, thrld) # Check FRB for bottleneck, unified, returns 1 if no bottleneck, 0 - otherwise
{
  counter <- 1
  node_i <- start_node
  new_FRB <- 0

  while (node_i != 0) {
	if (counter > 1000) # Remove in production
	{
	  print("Looping is too long")
	  return(-1)
	}
	new_FRB <- node_table$FRB[node_i] + add_rb
	if (new_FRB > thrld)
	{
	  return(0)
	}
	interim_tech <- node_table$Tech[node_i]
	if (interim_tech > 2) # avoid connect FOCL and RTS to SAT and CELL
	{
	  return(0)
	}
	if (node_table$NextNode[node_i] != 0)
	{
	  node_i = node_table$NextNode[node_i]
	}
	else
	{
	  break
	}
  }
  return(1)
}

frb_check_rts <- function(node_table, start_node, add_rb, thrld) # Check FRB for bottleneck, unified, returns 1 if no bottleneck, 0 - otherwise
{
  counter <- 1
  node_i <- start_node
  new_FRB <- 0

  while (node_i != 0) {
	if (counter > 1000) # Remove in production
	{
	  print("Looping is too long")
	  return(-1)
	}
	interim_tech <- node_table$Tech[node_i]
	if (interim_tech == 2) {
	  new_FRB <- node_table$FRB[node_i] + add_rb
	  if (new_FRB > thrld)
		return(0)
	}

	if (node_table$NextNode[node_i] != 0)
	  node_i <- node_table$NextNode[node_i]
	else
	  break
  }
  return(1)
}

frb_check_postopt <- function(node_table, start_node, add_rb, thrld) # Check FRB for bottleneck, unified, returns 1 if no bottleneck, 0 - otherwise
{
  counter <- 1
  node_i <- start_node
  new_FRB <- 0

  while (node_i != 0) {
	if (counter > 1000) # Remove in production
	{
	  print("Looping is too long")
	  return(-1)
	}
	new_FRB <- node_table$FRB[node_i] + add_rb
	if (new_FRB > thrld)
	{
	  return(0)
	}
	#interim_tech <- node_table$Tech[node_i]
	#if (interim_tech > 2) # avoid connect FOCL and RTS to SAT and CELL
	#{
	#return(0)
	#}
	if (node_table$NextNode[node_i] != 0)
	{
	  node_i = node_table$NextNode[node_i]
	}
	else
	{
	  break
	}
  }
  return(1)
}

algorithm4_5_impl <- function(input, intermediate = NULL) {

  if (is.reactivevalues(input))
	input_load <- reactiveValuesToList(input)
  else
	input_load <- as.list(input)

  print('****************')
  print(typeof(input_load))

  global_log_enabled <- F # Set in F to run in production
  if (exists("bWriteLog", input_load)) global_log_enabled <- T   # Enabling global logging in case of production

  if (exists("TopologySettings.EnableGL", input_load)) {   # Enabling global logging if running as webapp
	if (input_load$TopologySettings.EnableGL == 1) {
	  global_log_enabled <- T
	}
  }

  use_cown <- F
  if (input_load$TopologySettings.UseCOWN == 1)
	use_cown <- T

  use_clustering <- F
  if (input_load$TopologySettings.EnableClustering == 1)
	use_clustering <- T

  cat("Postoptimization log:", file = "opt-log.txt", sep = "\n", append = FALSE)
  cat("Postoptimization warnings:", file = "opt-warn.txt", sep = "\n", append = FALSE)

  ts_total_start <- Sys.time()

  #npv_set <- fromJSON(file = "ivars.json")
  if (exists("Files.NPVSettings", input_load)) {
	ts_load_start <- Sys.time()
	if (global_log_enabled) {
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Algorithm_4_5_impl: Trying to load additional parameters from json.file"), input_load$Files.NPVSettings, sep = ": "))
	}
	npv_set <- rjson::fromJSON(file = input_load$Files.NPVSettings)
	npv_set_nrow <- length(npv_set["shared_vars"][[1]])
	for (i in 1:npv_set_nrow) {
	  text_line <- paste("input_load$", str_trim(npv_set["shared_vars"][[1]][[i]][["name"]], side = "both"), " <- ", str_trim(npv_set["shared_vars"][[1]][[i]][["value"]]))
	  # print(text_line)
	  eval(parse(text = text_line))
	  #eval(parse(text= paste("input_load$",str_trim(npv_set["shared_vars"][[1]][[i]][["name"]], side = "both"), " <- ",str_trim(npv_set["shared_vars"][[1]][[i]][["value"]] ))))
	}
	time_config_load <- Sys.time() - ts_load_start
	if (global_log_enabled) {
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Algorithm_4_5_impl: Time consumed by loading of json.file is "), format(time_config_load), sep = ": "))
	}
  }

  # Manually added parameters
  #input_load$InitialDataRadio.MaximumLinkCapacity <- 750 # Radwin 2000 https://www.radwin.com/ptp-radwin-2000/
  #input_load$InitialDataSatellite.MaximumLinkCapacity <- 50 # https://www.ookla.com/articles/starlink-hughesnet-viasat-performance-q4-2021 (avg download speed is 49,33 Mbps)
  #input_load$InitialDataCellular.MaximumLinkCapacity <- 30 # https://www.speedtest.net/awards/brazil/ (avg download speed is 28.75 Mbps)
  limit_t1 <- as.numeric(input_load$InitialDataRadio.MaximumLinkCapacity)
  limit_t2 <- as.numeric(input_load$InitialDataSatellite.MaximumLinkCapacity)
  limit_t3 <- as.numeric(input_load$InitialDataCellular.MaximumLinkCapacity)

  input_load$InitialDataRadio.MaximumLinkCapacity <- limit_t1
  input_load$InitialDataSatellite.MaximumLinkCapacity <- limit_t2
  input_load$InitialDataCellular.MaximumLinkCapacity <- limit_t3

  #print(sprintf("Radio MaximumLinkCapacity %f", as.numeric(input_load$InitialDataRadio.MaximumLinkCapacity)))
  #print(sprintf("Satellite MaximumLinkCapacity %f", as.numeric(input_load$InitialDataSatellite.MaximumLinkCapacity)))
  #print(sprintf("Cellular MaximumLinkCapacity %f", as.numeric(input_load$InitialDataCellular.MaximumLinkCapacity)))

  print(sprintf("Radio MaximumLinkCapacity %f", input_load$InitialDataRadio.MaximumLinkCapacity))
  print(sprintf("Satellite MaximumLinkCapacity %f", input_load$InitialDataSatellite.MaximumLinkCapacity))
  print(sprintf("Cellular MaximumLinkCapacity %f", input_load$InitialDataCellular.MaximumLinkCapacity))

  pb_temp <- as.numeric(input_load$IntParam.PaybackPeriod)
  input_load$IntParam.PaybackPeriod <- pb_temp
  input_load$PVOptionSet.PaybackPeriod <- pb_temp
  print(sprintf("INT PaybackPeriod %f", input_load$IntParam.PaybackPeriod))
  print(sprintf("PV PaybackPeriod %f", input_load$IntParam.PaybackPeriod))

  # Additional parameters for simplified calculations (temporary added manually)
  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Algorithm_4_5_impl: Loading params for simplified calculations"))
  }
  else
	print("Loading params for simplified calculations ... ")

  if (exists("Files.IntParamsSmpl", input_load)) {
	ts_load_start <- Sys.time()

	print("Loading int params from file ... 1562 ")

	o_i_vars <- vroom::vroom(input_load$Files.IntParamsSmpl, altrep = FALSE)
	req(o_i_vars)
	num_vars <- nrow(o_i_vars)

	print(typeof(o_i_vars))
	print(typeof(input_load))

	attach(input_load)
	print(exists("IntParam.CC_C"))
	detach(input_load)

	for (i in 1:num_vars) {
	  text_line <- paste("input_load$", str_trim(o_i_vars$PARAM[i], side = "both"), " <- ", str_trim(o_i_vars$VALUE[i], side = "both"))
	  #print(text_line)
	  eval(parse(text = text_line))
	}

	attach(input_load)
	print(exists("IntParam.CC_C"))
	detach(input_load)

	time_config_load <- Sys.time() - ts_load_start
	if (global_log_enabled) {
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Algorithm_4_5_impl: Time consumed by loading of internal vars is "), format(time_config_load), sep = ": "))
	}
  }
  else { # else using the following default values

	print("Using built-in int params  ... 1585")
	#Construction cost coef
	input_load$IntParam.CC_C <- list(0, 0, 1, 1) #depends from tech

	#Bandwidth coefficient to take into account additional expenditures for very extensive bandwidth requirements
	input_load$IntParam.B_C <- list(2, 1, 1, 1) #depends from tech

	input_load$IntParam.B_C_T <- list(40 * 1024, 1, 1, 1) #Threshold depends from tech, Mb/s

	#License requirement coefficient for one time regulation license fee
	input_load$IntParam.LC1 <- list(0, 1, 0, 0)

	#License requirement coefficient for recurring license fee
	input_load$IntParam.LC2 <- list(0, 1, 0, 0)

	#License requirement coefficient for one time ISP/mainantance license fee
	input_load$IntParam.LC3 <- list(0, 0, 0, 0)

	#Binary coefficient for enabling annual ISP fee
	input_load$IntParam.ISP_C <- list(1, 0, 1, 1)

	#Income coefficient for NPV calculations ( in % of annual ISP fee)
	input_load$IntParam.INC_C <- list(50, 0, 0, 0)

	#Payback period was moved to interactive parameters
	input_load$IntParam.PaybackPeriod <- 5 # root of i.

	# Bits per baud coef
	input_load$IntParam.BPB <- 8

	# Road topography coef
	input_load$IntParam.RoadTC <- 0.02

	# Scale factor for input pop data
	input_load$IntParam.PopUnits <- 1

  }


  if (exists("Files.ExtParamsSmpl", input_load)) {
	ts_load_start <- Sys.time()

	print("loading ext params ...")

	o_e_vars <- vroom::vroom(input_load$Files.ExtParamsSmpl, altrep = FALSE)
	req(o_e_vars)
	num_vars <- nrow(o_e_vars)

	for (i in 1:num_vars) {
	  text_line <- paste("input_load$", str_trim(o_e_vars$PARAM[i], side = "both"), " <- ", str_trim(o_e_vars$VALUE[i], side = "both"))
	  #print(text_line)
	  eval(parse(text = text_line))
	}
	time_config_load <- Sys.time() - ts_load_start
	if (global_log_enabled) {
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Algorithm_4_5_impl: Time consumed by loading of external vars is "), format(time_config_load), sep = ": "))
	}
  }
  else {

	print("Using hardcoded ext params ...")
	#Fiber optic cable line construction cost (aerial / buried / over existing cable ducts or poles)	 13 636.36 	USD per km
	input_load$ExtParam.ConstructionCostFOCL <- 13636.3

	#Annual fiber optic cable line operation and maintenance cost (aerial / buried / over existing cable ducts or poles)	 163.64 	USD per km per year
	input_load$ExtParam.AnnualMaintenanceCostFOCL <- 163.64

	#Annual ISP fee for 1 Mbps of dedicated internet access channel over a fiber optic cable line  (For total troughput < 1 Gbps)	 17.21 	USD per Mbps per year
	input_load$ExtParam.AnnualISPFeeFOCL1 <- 17.21

	#Annual ISP fee for 1 Mbps of dedicated internet access channel over a fiber optic cable line  (For total troughput >= 1 Gbps)	 10.30 	USD per Mbps per year
	input_load$ExtParam.AnnualISPFeeFOCL2 <- 10.30

	#Microwave point-to-point channel construction cost 		USD per km
	input_load$ExtParam.ConstructionCostRTS <- 5000

	#Annual microwave point-to-point channel operation and maintenance cost		USD per km per year
	input_load$ExtParam.AnnualMaintenanceCostRTS <- 300

	#One time spectrum license fee for 1MHz		USD per MHz
	input_load$ExtParam.SpectrumLicenseCostOne <- 611 # was 0 for Brazil

	#Annual recurring spectrum license fee for 1MHz		USD per MHz per year
	input_load$ExtParam.SpectrumLicenseCostReccur <- 0

	#Satellite terminal equipment purchase and installation cost		USD per Mbps
	input_load$ExtParam.EquipSAT <- 599

	#Annual satellite terminal and ad-hoc equipment maintenance cost		USD per Mbps per year
	input_load$ExtParam.EquipMainantanceSAT <- 10

	#Annual ISP fee for 1 Mbps of dedicated internet access channel over a satellite link		USD per Mbps per year
	input_load$ExtParam.AnnualISPFeeSAT <- 12

	#Cellular terminal equipment purchase and installation cost		USD per Mbps
	input_load$ExtParam.EquipCELL <- 400

	#Annual fee for 1 Mbps of dedicated internet access over cellular link		USD per Mbps per year
	input_load$ExtParam.AnnualISPFeeCELL <- 25

	# Average wage per year
	input_load$ExtParam.AverageIncome <- 7850

	#One time service license fee 		USD
	input_load$ExtParam.ServiceLicenseFeeOne <- 100000

	# Params for last mile calculations
	input_load$ExtParam.LmApCapacity <- 20
	input_load$ExtParam.LmApConEqCost <- 300
	input_load$ExtParam.LmApMntCost <- 50
  }
  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Algorithm_4_5_impl: Loading params done ..."))
  }
  else
	print("Loading params done ...")

  # Add all other requirements

  objects <- vroom::vroom(input_load$Files.ListOfObjects, altrep = FALSE, col_types = vroom::cols(
	OBJECTNAME = vroom::col_character(),
	LON = vroom::col_double(),
	LAT = vroom::col_double(),
	RB = vroom::col_double(),
	DTF = vroom::col_double(),
	COV = vroom::col_character(),
	REGION = vroom::col_character(),
	POC = vroom::col_character(),
	POCLON = vroom::col_double(),
	POCLAT = vroom::col_double(),
	.delim = ";"
  )) # Fix decimals
  req(objects)
  #vroom::spec(objects)
  numberofobj <- nrow(objects)
  #for (k in 1:numberofobj)
  #{
  #objects[k, 1] <- as.character(objects[k, 1])
  #objects[k, 7] <- as.character(objects[k, 7])
  #}

  if (numberofobj < 1)
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Algorithm_4_5_impl: No objects to process "))
	#print ("Insufficeient number of nodes")
	return(0)
  }

  if (global_log_enabled) {
	#.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Step-by-step construction process initiated:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Algorithm_4_5_impl: Using data in file: %s, contains %d objects", input_load$Files.ListOfObjects, nrow(objects))))
  }


  if (use_clustering) {

	ts_clustering_start <- Sys.time()

	cl_result <- formula_4_4_1(input, output, objects)

	ts_clustering_time <- Sys.time() - ts_clustering_start

	if (global_log_enabled) {
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Algorithm_4_5_impl: Clustering is done, time for clustering is %s", format(ts_clustering_time))))
	}

	v1 <- cl_result[[1]]

	map_4_5_6_clust <- leaflet(data = v1[,]) %>%
	  addTiles() #%>%
	#addMarkers(lng = v1$x, lat = v1$y, popup = v1$name)

	#exp_markers <- data.frame(lng = v1$x, lat = v1$y, popup = v1$name)

	#print(v1)
	cur_area <- cl_result[[2]]
	clust_list <- cl_result[[3]]
	#area_stats <- result[[4]]
	clust_num <- as.integer(length(clust_list) / 2)

	clust_nodes_list <- list()
	clust_coord_list <- list()

	#exp_poly_lon <- c()
	#exp_poly_lat <- c()
	#exp_poly_label <- c()
	exp_poly_line <- list()
	exp_poly_line_csv <- list()

	stat_vec <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
	for (cl_i in seq(from = 1, to = length(clust_list), by = 2)) { # Iterating clusters
	  lc_area <- clust_list[[cl_i]]
	  rc_area <- clust_list[[cl_i + 1]]

	  cur_cluster_nodes <- filter(objects, LON >= lc_area[1] &  # Double acc  issue for >= <= ... if cluster has topright coord equals to border then apply <=
		LON <= rc_area[1] &  # <
		LAT <= lc_area[2] &  # <
		LAT >= rc_area[2])

	  if (nrow(cur_cluster_nodes) == 0) next # Avoid processing of empty clusters
	  clust_nodes_list <- append(clust_nodes_list, cur_cluster_nodes)
	  clust_coord_list <- append(clust_coord_list, c(lc_area, rc_area))
	  #for (obj_i in 1:numberofobj) {
	  #objects[obj_i]$LON
	  #
	  #}

	  if (global_log_enabled) {
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Algorithm_4_5_impl: Processing cluster: %d, contains %d objects", as.integer(floor(cl_i / 2) + 1), length(cur_cluster_nodes))))
	  }
	  result_clust <- formula_4_5_6(input_load, output, cur_cluster_nodes)
	  #result <- list(v1, main_BN, main_table, stats_v, npv4matrix)
	  v1_clust <- result_clust[[1]]
	  BaseNode_clust <- result_clust[[2]]

	  node_table_clust <- result_clust[[3]]
	  stat_vec_clust <- result_clust[[4]]
	  npv4matrix_clust <- result_clust[[5]]

	  stat_vec <- stat_vec + stat_vec_clust


	  # Adding feature: different markers for diffeent techs

	  reg_ind <- c()
	  sat_ind <- c()
	  cell_ind <- c()

	  for (i in 1:nrow(node_table_clust)) { # Visualize connections inside of a cluster
		#for (i in 1:nrow(v1_clust)) { # Visualize connections inside of a cluster
		tech <- node_table_clust$Tech[i]

		if (tech == 1 || tech == 2)
		  reg_ind <- append(reg_ind, i + 1)

		if (tech == 3)
		  sat_ind <- append(sat_ind, i + 1)

		if (tech == 4)
		  cell_ind <- append(cell_ind, i + 1)

		#print(paste('----- ',i,' -----'))
		#print(node_table_clust[i,])
		#print(v1_clust[i,])

	  }

	  v1_reg <- v1_clust[reg_ind,]
	  v1_sat <- v1_clust[sat_ind,]
	  v1_cell <- v1_clust[cell_ind,]

	  #print("V1 rows:")
	  #print(nrow(v1_clust))
	  #print("clust_table rows:")
	  #print(nrow(node_table_clust))

	  #print("REGs:")
	  #print(reg_ind)

	  #print("SATs:")
	  #print(sat_ind)

	  #print(v1_reg)
	  #print(v1_sat)

	  #print(' -------- BaseNodeClust ------------ ')
	  #print(BaseNode_clust)
	  #print(' -------- node_table_clust ----------- ')
	  #print(node_table_clust)
	  #print(' -------- v1_clust ----------- ')
	  #print(v1_clust)

	  # Discuss exp_markers for different cases

	  # / new feature

	  if (cl_i == 1) {
		BaseNode_Total <- BaseNode_clust
		node_table_total <- node_table_clust
		npv4matrix_total <- npv4matrix_clust
		exp_awesome_markers <- data.frame(lng = as.numeric(BaseNode_clust[, 2]), lat = as.numeric(BaseNode_clust[, 3]), popup = paste(iconv(as.character(BaseNode_clust[, 1]), "UTF-8", "UTF-8", sub = ''), sprintf("<br> RB: %s FRB: %s", BaseNode_clust$RB, BaseNode_clust$FRB), "<br> BaseNode of the cluster"))
		exp_awesome_markers_csv <- data.frame(lon = as.numeric(BaseNode_clust[, 2]), lat = as.numeric(BaseNode_clust[, 3]), base_node_name = as.character(iconv(BaseNode_clust[, 1], "UTF-8", "UTF-8", sub = '')), required_bw = BaseNode_clust$RB, required_cumul_bw = BaseNode_clust$FRB, stringsAsFactors = F)
		v1_total <- v1_clust
		exp_markers <- data.frame(lng = as.numeric(v1_clust$x), lat = as.numeric(v1_clust$y), popup = as.character(v1_clust$name)) # first marker for export
		exp_markers_csv <- data.frame(lon = as.numeric(BaseNode_clust[, 2]), lat = as.numeric(BaseNode_clust[, 3]), base_node_name = as.character(BaseNode_clust[, 1]), required_bw = BaseNode_clust$RB, required_cumul_bw = BaseNode_clust$FRB, cluster = as.integer(floor(cl_i / 2) + 1), stringsAsFactors = F)
		#exp_markers_csv <- data.frame(lng = as.numeric(v1_clust$x), lat = as.numeric(v1_clust$y))
		#print('node')
		#print(node_table_clust[1,])
		#print(node_table_clust[2,])
		#print('clust')
		#print(v1_clust[1,])
		#print(v1_clust[2,])
	  }
	  else {
		BaseNode_Total <- rbind(BaseNode_Total, BaseNode_clust)
		node_table_total <- rbind(node_table_total, node_table_clust)
		npv4matrix_total <- rbind(npv4matrix_total, npv4matrix_clust)
		exp_awesome_markers <- union(exp_awesome_markers, data.frame(lng = as.numeric(BaseNode_clust[, 2]), lat = as.numeric(BaseNode_clust[, 3]), popup = paste(iconv(as.character(BaseNode_clust[, 1]), "UTF-8", "UTF-8", sub = ''), sprintf("<br> RB: %s FRB: %s", BaseNode_clust$RB, BaseNode_clust$FRB), "<br> BaseNode of the cluster")))
		exp_awesome_markers_csv <- rbind(exp_awesome_markers_csv, c(lon = as.numeric(BaseNode_clust[, 2]), lat = as.numeric(BaseNode_clust[, 3]), base_node_name = as.character(iconv(BaseNode_clust[, 1], "UTF-8", "UTF-8", sub = '')), required_bw = BaseNode_clust$RB, required_cumul_bw = BaseNode_clust$FRB))
		v1_total <- rbind(v1_total, v1_clust)
		exp_markers <- union(exp_markers, data.frame(lng = as.numeric(v1_clust$x), lat = as.numeric(v1_clust$y), popup = as.character(v1_clust$name))) # subsequent markers for export to Ilya
		exp_markers_csv <- rbind(exp_markers_csv, c(lon = as.numeric(BaseNode_clust[, 2]), lat = as.numeric(BaseNode_clust[, 3]), base_node_name = as.character(BaseNode_clust[, 1]), required_bw = BaseNode_clust$RB, required_cumul_bw = BaseNode_clust$FRB, cluster = as.integer(floor(cl_i / 2) + 1)))
		#exp_markers_csv <- union(exp_markers_csv, data.frame(lng = as.numeric(v1_clust$x), lat = as.numeric(v1_clust$y))) # subsequent markers for export to Steve

	  }


	  map_4_5_6_clust <- addMarkers(map_4_5_6_clust, lng = v1_clust$x, lat = v1_clust$y, popup = v1_clust$name)
	  #map_4_5_6_clust <- addMarkers(map_4_5_6_clust, lng = v1_reg$x, lat = v1_reg$y, popup = v1_reg$name)

	  if (nrow(v1_sat) > 0)
		map_4_5_6_clust <- addCircleMarkers(map_4_5_6_clust, lng = v1_sat$x, lat = v1_sat$y, popup = v1_sat$name, fillColor = "#D02", fillOpacity = 1, stroke = F) # SAT nodes

	  if (nrow(v1_cell) > 0)
		map_4_5_6_clust <- addCircleMarkers(map_4_5_6_clust, lng = v1_cell$x, lat = v1_cell$y, popup = v1_cell$name, fillColor = "#D2F", fillOpacity = 1, stroke = F) # CELL nodes

	  map_4_5_6_clust <- addAwesomeMarkers(map_4_5_6_clust, lng = as.numeric(BaseNode_clust[, 2]), lat = as.numeric(BaseNode_clust[, 3]),
										   icon = awesomeIcons(markerColor = "red"),
										   popup = paste(iconv(BaseNode_clust[, 1], "UTF-8", "UTF-8", sub = ''), sprintf("<br> RB: %s FRB: %s", BaseNode_clust$RB, BaseNode_clust$FRB), "<br> BaseNode of the cluster"))


	  tech_list <- c('FOCL', 'RTS', 'SAT', 'CELL')

	  exp_poly_line_clust <- list()
	  exp_poly_line_clust_csv <- list()
	  index_csv <- 1
	  for (i in 1:nrow(node_table_clust)) { # Visualize connections inside of a cluster
		tech <- node_table_clust$Tech[i]
		next_node <- node_table_clust$NextNode[i]
		rb <- node_table_clust$RB[i]
		frb <- node_table_clust$FRB[i]
		link_metric <- node_table_clust$NPV[i]
		link_metric_cum <- node_table_clust$NPVC[i]
		link_dist <- node_table_clust$DistToNext[i]
		node_name_from <- as.character(node_table_clust$OBJECTNAME[i])

		# Add colorization by tech
		tech_color_list <- switch(tech, "#02F", "#0F2", "#D02", "#D2F") # Rename

		node_lat_from <- as.numeric(node_table_clust$LAT[i])
		node_lon_from <- as.numeric(node_table_clust$LON[i])


		if (next_node == 0)
		{
		  lon_precalc <- c(as.numeric(node_table_clust$LON[i]), as.numeric(BaseNode_clust[, 2]))
		  lat_precalc <- c(as.numeric(node_table_clust$LAT[i]), as.numeric(BaseNode_clust[, 3]))
		  node_name_to <- as.character(BaseNode_clust[, 1])
		  node_lon_to <- as.numeric(BaseNode_clust[, 2])
		  node_lat_to <- as.numeric(BaseNode_clust[, 3])
		}
		else
		{
		  lon_precalc <- c(as.numeric(node_table_clust$LON[i]), as.numeric(node_table_clust$LON[next_node]))
		  lat_precalc <- c(as.numeric(node_table_clust$LAT[i]), as.numeric(node_table_clust$LAT[next_node]))
		  node_name_to <- as.character(node_table_clust$OBJECTNAME[next_node])
		  node_lon_to <- as.numeric(node_table_clust$LON[next_node])
		  node_lat_to <- as.numeric(node_table_clust$LAT[next_node])
		}
		exp_markers_csv <- rbind(exp_markers_csv, c(lon = node_lon_from, lat = node_lat_from, node_name = node_name_from, required_bw = rb, required_cumul_bw = frb, cluster = as.integer(floor(cl_i / 2) + 1)))
		exp_poly_line_clust_csv[[i]] <- c(node_name_from, node_lon_from, node_lat_from, node_name_to, node_lon_to, node_lat_to, i18n$t(tech_list[tech]), frb, link_dist, link_metric, link_metric_cum) # now exporting all connections

		if (tech == 3 || tech == 4) next # Display connections only for FOCL and RTS
		#if (tech == 1 || tech == 2) # Display connections only for FOCL and RTS
		map_4_5_6_clust <- addPolylines(map_4_5_6_clust, lng = lon_precalc,
										lat = lat_precalc,
										label = paste(i18n$t(tech_list[tech]), 'FRB', frb, 'DIST', link_dist, sep = ':'),
										#color = "#02F",
										color = tech_color_list,
										weight = 2,
										opacity = 0.9)
		# mapshot(map_4_5_6, file = sprintf("pic-step-%d.png",i))

		# Discuss export of  poly lines
		exp_poly_line_clust[[index_csv]] <- c(lon_precalc, lat_precalc, paste(i18n$t(tech_list[tech]), 'FRB', frb, 'DIST', link_dist, sep = ':')) # index i was changed to avoid gaps
		#exp_poly_line_clust_csv[[index_csv]] <- c(lon_precalc, lat_precalc, i18n$t(tech_list[tech]), frb, link_dist, link_metric, link_metric_cum)
		index_csv <- index_csv + 1
	  } # Visualize connections inside of a cluster
	  exp_poly_line <- append(exp_poly_line, exp_poly_line_clust)
	  exp_poly_line_csv <- append(exp_poly_line_csv, exp_poly_line_clust_csv)


	} # Iterating clusters

	# Calculate modified DTF for costless
	costless_dtf_addon_m <- as.numeric(0)
	if (as.numeric(input_load$TopologySettings.UseMetric) == 2) {
	  for (j in 1:nrow(BaseNode_Total)) {
		#print(j)
		#print(BaseNode_Total[j,])
		newpoc <- as.character(BaseNode_Total$OBJECTNAME[j])
		newpoc_lon <- as.numeric(BaseNode_Total$LON[j])
		newpoc_lat <- as.numeric(BaseNode_Total$LAT[j])
		if (BaseNode_Total$DTF[j] == 0) next
		dtf_1 <- BaseNode_Total$DTF[j]
		lon_1 <- BaseNode_Total$LON[j]
		lat_1 <- BaseNode_Total$LAT[j]
		frb_1 <- BaseNode_Total$FRB[j]
		if (j < nrow(BaseNode_Total)) { # last node in list should be connected by default (preliminary sort ?)
		  for (k in (j + 1):nrow(BaseNode_Total)) {
			# if (k == j) next
			dist_to_BN <- distm(c(lon_1, lat_1), c(as.numeric(BaseNode_Total$LON[k]), as.numeric(BaseNode_Total$LAT[k])), fun = distHaversine)
			dtf_2 <- round(as.numeric(dist_to_BN[1] / 1000), digits = 4)
			if (dtf_2 < dtf_1) {
			  dtf_1 <- dtf_2
			  newpoc <- as.character(BaseNode_Total$OBJECTNAME[k])
			  newpoc_lon <- as.numeric(BaseNode_Total$LON[k])
			  newpoc_lat <- as.numeric(BaseNode_Total$LAT[k])
			}
		  }
		  BaseNode_Total$POC[j] <- newpoc
		  BaseNode_Total$POCLON[j] <- newpoc_lon
		  BaseNode_Total$POCLAT[j] <- newpoc_lat
		}
		costless_dtf_addon_m <- costless_dtf_addon_m + calc_bn_focl_costless_addon(input_load, dtf_1, frb_1)
		#print(sprintf("BN Opt> J: %d NPOC: %s NLOC: %f NLAT: %f DTF1: %f DTF2: %f FRB %f", j, newpoc, newpoc_lon, newpoc_lat, BaseNode_Total$DTF[j], dtf_1, frb_1))
	  }
	}
	#stat_vec <- append(stat_vec, clust_num)
	stat_vec[11] <- clust_num
	if (!use_cown)
	  stat_table = data.frame(Name = c("Total links", "FOCL (N/L/B/M) ", "RTS (N/L/B/M) ", "SAT (N/L/B/M) ", "MOB (N/L/B/M)", "NPV Total", "NPV/CAPEX/OPEX cumulative total", "Internet addon", "BN-to-fiber addon", "LMC TCO", "Number of Clusters"), Value = stat_vec[1:11])
	else
	  stat_table = data.frame(Name = c("Total links", "FOCL (N/L/B/M)", "RTS (N/L/B/M)", "SAT (N/L/B/M)", "MOB (N/L/B/M)", "Cost-of-Ownership Total", "Cost-of-Ownership/CAPEX/OPEX cumulative total", "Internet addon", "BN-to-fiber addon", "LM TCO", "Number of Clusters"), Value = stat_vec[1:11])

	if (as.numeric(input_load$TopologySettings.UseMetric) == 2)
	  stat_table = data.frame(Name = c("Total links", "FOCL (N/L/B/M) ", "RTS (N/L/B/M) ", "SAT (N/L/B/M) ", "MOB (N/L/B/M)", "Costless Total", "Costless cumulative total", "Costless to CAPEX/OPEX", "BN-to-fiber addon", "LM TCO", "Number of Clusters"), Value = stat_vec[1:11])


	stat_table[2, 2] <- sprintf("%d / %.2f / %.1f / %.1f ", stat_vec[2], stat_vec[12], stat_vec[13], stat_vec[14]) # add info about overall length, frb, metric(tco or npv) for every tech
	stat_table[3, 2] <- sprintf("%d / %.2f / %.1f / %.1f ", stat_vec[3], stat_vec[15], stat_vec[16], stat_vec[17]) #
	stat_table[4, 2] <- sprintf("%d / %.2f / %.1f / %.1f ", stat_vec[4], stat_vec[18], stat_vec[19], stat_vec[20]) #
	stat_table[5, 2] <- sprintf("%d / %.2f / %.1f / %.1f ", stat_vec[5], stat_vec[21], stat_vec[22], stat_vec[23]) #

	if (as.numeric(input_load$TopologySettings.UseMetric) != 2)
	  stat_table[7, 2] <- sprintf("%.2f / %.2f / %.2f ", stat_vec[7], stat_vec[24], stat_vec[25]) # add info for CAPEX and OPEX separately

	if (as.numeric(input_load$TopologySettings.UseMetric) == 2) {
	  stat_table[8, 2] <- sprintf("%.2f / %.2f ", stat_vec[8], stat_vec[24]) # add info for costless to CAPEX/OPEX separately
	  stat_table[9, 2] <- sprintf("%.2f / %.2f ", stat_vec[9], costless_dtf_addon_m) # add info for costless to CAPEX/OPEX separately
	}
	#exp_poly_line_df <- data.frame(id = 1:length(exp_poly_lon), lon = exp_poly_lon, lat = exp_poly_lat, label = exp_poly_label)
	#exp_poly_line_df <- data.frame(exp_poly_line_exp)
	#print(exp_poly_line_csv)
	#print('---------- df ---------')
	#exp_poly_line_df <- data.frame(Name = c("LON1", "LON2", "LAT1", "LAT2", "TECH", "RB_CUMULATIVE"), exp_poly_line_csv)
	exp_poly_line_df <- as.data.frame(do.call(rbind, exp_poly_line_csv))
	colnames(exp_poly_line_df) <- c("NAME_FROM", "LON1", "LAT1", "NAME_TO", "LON2", "LAT2", "TECH", "RB_CUMULATIVE", "DISTANCE", "METRIC", "CUMULATIVE_METRIC")
	#print(exp_poly_line_df)

	mymap <- renderLeaflet(map_4_5_6_clust)
	mydata <- renderTable(stat_table, colnames = FALSE)
	mydata2 <- renderTable(node_table_total, colnames = TRUE) # Integral table with Nodes
	mydata3 <- renderTable(npv4matrix_total, colnames = TRUE) # Integral table with BNs / Integral BN NPV table
	mylog <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
	mydata_raw <- stat_table

	my_map_lines <- exp_poly_line
	my_markers <- exp_markers
	my_awe_markers <- exp_awesome_markers


	result <- list(mymap, mydata, mydata2, mydata3, mydata_raw, mylog, my_map_lines, my_markers, my_awe_markers)

	save_topology_pics <- F
	if (exists("TopologySettings.SaveTopologyPics", input_load)) {
	  if (input_load$TopologySettings.SaveTopologyPics == 1)
		save_topology_pics <- T   # Enabling global logging in case of production
	}
	if (save_topology_pics == T) { # Change resolution
	  mapshot(map_4_5_6_clust, file = "exp_topology.png", remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar", "drawToolbar", "easyButton"), selfcontained = FALSE, vwidth = 5000, vheight = 5000)
	  #mapshot(map_4_5_6_clust, file = "topology_exp.png", remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar", "drawToolbar", "easyButton"), selfcontained = FALSE)
	}

	save_topology_file <- F
	if (exists("TopologySettings.SaveToFile", input_load)) {
	  if (input_load$TopologySettings.SaveToFile == 1)
		save_topology_file <- T   # Enabling saving topology and results to file
	}
	if (save_topology_file == T) { #
	  write.csv(stat_table, "exp_results.csv", row.names = F, col.names = F)
	  write.csv(exp_poly_line_df, file = "exp_topology_lines.csv", row.names = F, quote = F)
	  #print(paste("lons:", nrow(exp_markers_csv[1]), "lats:", nrow(exp_markers_csv[2]), "names:", nrow(exp_markers_csv[3])))
	  #print(paste("lons:", nrow(exp_markers_csv[[1]]), "lats:", nrow(exp_markers_csv[[2]])))
	  #print(exp_markers)
	  #print('--- awe ---')
	  #print(paste("lons:", nrow(exp_awesome_markers_csv[1]), "lats:", nrow(exp_awesome_markers_csv[2]), "names:", nrow(exp_awesome_markers_csv[3])))
	  #print(exp_awesome_markers_csv)
	  write.csv(exp_awesome_markers_csv, file = "exp_a_markers.csv", col.names = T, row.names = F, quote = F)
	  write.csv(exp_markers_csv, file = "exp_markers_csv.csv", col.names = T, row.names = F, quote = F)
	  write.csv(BaseNode_Total, file = "exp_base_hexes.csv", col.names = T, row.names = F, quote = F)
	}


  }
  else { # No clustering

	#result_456 <- formula_4_5_6(input_load, output)
	result_456 <- formula_4_5_6(input_load, output, objects)
	v1 <- result_456[[1]]
	#print(v1)
	BaseNode <- result_456[[2]]
	#print(BaseNode)
	#browser()


	map_4_5_6 <- leaflet(data = v1[,]) %>%
	  addTiles() %>%
	  addMarkers(lng = v1$x, lat = v1$y, popup = v1$name) %>%
	  #addMarkers(lng=v1$x, lat=v1$y) %>%
	  #addProviderTiles(providers$Stamen.TonerLite) %>%
	  addAwesomeMarkers(lng = as.numeric(BaseNode[, 2]), lat = as.numeric(BaseNode[, 3]),
						icon = awesomeIcons(markerColor = "red"),
						popup = paste(iconv(BaseNode[, 1], "UTF-8", "UTF-8", sub = ''), sprintf("<br> RB: %s FRB: %s", BaseNode$RB, BaseNode$FRB), "<br> BaseNode of the cluster"))

	node_table <- result_456[[3]]
	stat_vec <- result_456[[4]]
	npv4matrix <- result_456[[5]]

	if (!use_cown)
	  stat_table = data.frame(Name = c("Total links", "FOCL (N/L/B/M)", "RTS (N/L/B/M)", "SAT (N/L/B/M)", "MOB (N/L/B/M)", "NPV Total", "NPV cumulative total", "Internet addon", "Dtf addon", "LM", "Number of Clusters"), Value = stat_vec[1:11])
	else
	  stat_table = data.frame(Name = c("Total links", "FOCL (N/L/B/M)", "RTS (N/L/B/M)", "SAT (N/L/B/M)", "MOB (N/L/B/M)", "Cost-of-Ownership Total", "Cost-of-Ownership cumulative total", "Internet addon", "BN-to-fiber addon", "LM TCO", "Number of Clusters"), Value = stat_vec[1:11])

	stat_table[2, 2] <- sprintf("%d / %.2f / %.1f / %.1f ", stat_vec[2], stat_vec[12], stat_vec[13], stat_vec[14]) # add info about overall length, frb, metric(tco or npv) for every tech
	stat_table[3, 2] <- sprintf("%d / %.2f / %.1f / %.1f ", stat_vec[3], stat_vec[15], stat_vec[16], stat_vec[17]) #
	stat_table[4, 2] <- sprintf("%d / %.2f / %.1f / %.1f ", stat_vec[4], stat_vec[18], stat_vec[19], stat_vec[20]) #
	stat_table[5, 2] <- sprintf("%d / %.2f / %.1f / %.1f ", stat_vec[5], stat_vec[21], stat_vec[22], stat_vec[23]) #

	#OBJECTNAME    LON   LAT    RB   DTF COV   REGION  DTBN HopsToBN NextNode   FRB   NPV  Tech  isBN isLeaf NPVC

	tech_list <- c('FOCL', 'RTS', 'SAT', 'CELL')

	exp_poly_line <- list()

	for (i in 1:nrow(node_table)) {
	  next_node <- node_table$NextNode[i]
	  tech <- node_table$Tech[i]
	  frb <- node_table$FRB[i]
	  tech_color_list <- switch(tech, "#02F", "#0F2", "#D02", "#D2F")

	  if (next_node == 0)
	  {
		lon_precalc = c(as.numeric(node_table$LON[i]), as.numeric(BaseNode[, 2]))
		lat_precalc = c(as.numeric(node_table$LAT[i]), as.numeric(BaseNode[, 3]))
	  }
	  else
	  {
		lon_precalc = c(as.numeric(node_table$LON[i]), as.numeric(node_table$LON[next_node]))
		lat_precalc = c(as.numeric(node_table$LAT[i]), as.numeric(node_table$LAT[next_node]))
	  }
	  # Add colorization by tech
	  map_4_5_6 <- addPolylines(map_4_5_6, lng = lon_precalc,
								lat = lat_precalc,
								label = paste(i18n$t(tech_list[tech]), frb, sep = ':'),
								#color = "#02F",
								color = tech_color_list,
								weight = 2,
								opacity = 0.9)
	  # mapshot(map_4_5_6, file = sprintf("pic-step-%d.png",i))
	  exp_poly_line[[i]] <- c(lon_precalc, lat_precalc, paste(i18n$t(tech_list[tech]), frb, sep = ':'))
	}

	exp_markers <- data.frame(lng = v1$x, lat = v1$y, popup = v1$name)
	exp_awesome_markers <- data.frame(lng = as.numeric(BaseNode[, 2]), lat = as.numeric(BaseNode[, 3]), popup = paste(iconv(BaseNode[, 1], "UTF-8", "UTF-8", sub = ''), sprintf("<br> RB: %s FRB: %s", BaseNode$RB, BaseNode$FRB), "<br> BaseNode of the cluster"))

	#output$mymap <- renderLeaflet(map_4_5_6)
	#
	#
	#output$data <- renderTable(stat_table, colnames = FALSE)
	#output$data2 <- renderTable(node_table, colnames = TRUE)
	#output$data3 <- renderTable(npv4matrix, colnames = TRUE)
	#
	#
	#output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

	mymap <- renderLeaflet(map_4_5_6)
	mydata <- renderTable(stat_table, colnames = FALSE)
	mydata2 <- renderTable(node_table, colnames = TRUE)
	mydata3 <- renderTable(npv4matrix, colnames = TRUE)
	mylog <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
	mydata_raw <- stat_table
	#mymap_src <- map_4_5_6

	my_map_lines <- exp_poly_line
	my_markers <- exp_markers
	my_awe_markers <- exp_awesome_markers

	#result <- list(mymap, mymap_src, mymap_json, mydata, mydata2, mydata3, mydata_raw, mylog)
	#result <- list(mymap, mydata, mydata2, mydata3, mydata_raw, mylog, mymap_json, mymap_src)
	#result <- list(mymap, mydata, mydata2, mydata3, mydata_raw, mylog, mymap_json, my_map_lines, my_markers, my_awe_markers) # Iliya
	result <- list(mymap, mydata, mydata2, mydata3, mydata_raw, mylog, my_map_lines, my_markers, my_awe_markers)
  }
  ts_total_time <- Sys.time() - ts_total_start
  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Algorithm_4_5_impl: Done, total calculation time is %s", format(ts_total_time))))
  }
  return(result)


}

algorithm4_5 <- function(input, output) # Main routing
{
  req(input)
  req(input$formula)
  req(output)

  switch(input$formula,
		 ALL = { # ------------ ALL --------------

		   req(input$Files.ListOfObjects)
		   req(input$TopologySettings.SNDTFThreshold)
		   req(input$TopologySettings.AdminBN)
		   #req (input$TopologySettings.EnableCore)
		   #req (input$TopologySettings.CoreSize)
		   req(input$TopologySettings.EnablePostopt)
		   req(input$TopologySettings.UseCOWN)
		   req(input$InitialData.UseFOCL)
		   req(input$InitialData.UseRTS)
		   req(input$InitialData.UseSatellite)
		   req(input$InitialData.UseCellular)
		   req(input$TopologySettings.FOCLBottleTrh)
		   req(input$TopologySettings.RTSBottleTrh)
		   req(input$TopologySettings.EnableGL)

		   if (input$TopologySettings.EnableGL == 1) {
			 global_log_enabled <- T
			 .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		   }

		   result <- algorithm4_5_impl(input)

		   output$mymap <- result[[1]]

		   output$data <- result[[2]]
		   output$data2 <- result[[3]]
		   output$data3 <- result[[4]]

		   output$log <- result[[6]]

		   #output$data <- renderTable(result$BN, colnames=FALSE)
		   #output$myplot <- renderPlot(plot(result, edge.arrow.size=.4,vertex.label=NA))


		 }, # ------------ ALL --------------
		 FORMULA_4_5_1 = { #Determination of the BaseNode (BN)

		   req(input$Files.ListOfObjects)
		   req(input$TopologySettings.SNDTFThreshold)

		   .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

		   result <- formula_4_5_1(input, output)

		   #output$data <- renderTable(result, colnames=FALSE)

		   output$data <- renderTable(result, colnames = TRUE)

		   BaseNode <- result
		   output$log <- renderTable(paste(i18n$t("Formula_4_5_1: BaseNode found "), paste(BaseNode$ind, " ", BaseNode[1]), sep = ": "), colnames = TRUE)
		   #output$myplot <- renderPlot(plot(result, edge.arrow.size=.4,vertex.label=NA))

		 },
		 FORMULA_4_5_2 = { #Calculation of 4-row NPV matrix (from each regular node to BN, 4 possible technologies)

		   req(input$Files.ListOfObjects)
		   req(input$Intermediate.BNIndex)
		   #req (input$Intermediate.NodeList)

		   .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

		   result <- formula_4_5_2(input, output)

		   output$data <- renderTable(result, colnames = FALSE)
		 },
		 FORMULA_4_5_3 = { #Sorting all regular nodes by distance, then by bandwidth requirement


		   #req (input$Intermediate.NodeList)
		   req(input$Files.ListOfObjects)
		   req(input$Intermediate.BNLon)
		   req(input$Intermediate.BNLat)


		   .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

		   result <- formula_4_5_3(input, output)

		   output$data <- renderTable(result, colnames = TRUE)
		 },
		 FORMULA_4_5_4 = { #Detemination of the Node nearest to the "Center of Mass" of bandwidth requirements

		   req(input$Files.ListOfObjects)

		   .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

		   result <- formula_4_5_4(input, output)

		   output$data <- renderTable(result, colnames = FALSE)
		 },

		 FORMULA_4_5_5 = { #Visualization of the BaseNode (BN)

		   req(input$Files.ListOfObjects)
		   req(input$TopologySettings.SNDTFThreshold)

		   .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

		   result <- formula_4_5_5(input, output)
		   v1 <- result[[1]]
		   #print(v1)
		   BaseNode <- result[[2]]
		   #print(BaseNode)
		   #browser()

		   output$mymap <- renderLeaflet({ leaflet(data = v1[,]) %>%
			 addTiles() %>%
			 #addMarkers(lng = v1$x, lat = v1$y, popup = paste(v1$name,v1$adds,sep = "<br>") %>%
			 addMarkers(lng = v1$x, lat = v1$y, popup = v1$name) %>%
			 #addMarkers(lng=v1$x, lat=v1$y) %>%
			 #addProviderTiles(providers$Stamen.TonerLite) %>%
			 addAwesomeMarkers(lng = as.numeric(BaseNode[2]), lat = as.numeric(BaseNode[3]),
							   icon = awesomeIcons(markerColor = "red"),
							   popup = paste(iconv(BaseNode[, 1], "UTF-8", "UTF-8", sub = ''), sprintf("<br> RB: %s FRB: %s", BaseNode$RB, BaseNode$FRB), "<br> BaseNode of the cluster"))
		   })

		   output$data <- renderTable(result[[2]], colnames = FALSE)

		   #output$data <- renderTable(result$BN, colnames=FALSE)
		   #output$myplot <- renderPlot(plot(result, edge.arrow.size=.4,vertex.label=NA))


		 },
		 FORMULA_4_5_6 = { #Step-By-Step topology construction
		   req(input$Files.ListOfObjects)
		   req(input$TopologySettings.SNDTFThreshold)
		   req(input$TopologySettings.AdminBN)
		   #req (input$TopologySettings.EnableCore)
		   #req (input$TopologySettings.CoreSize)
		   req(input$TopologySettings.EnablePostopt)
		   req(input$TopologySettings.UseCOWN)
		   req(input$InitialData.UseFOCL)
		   req(input$InitialData.UseRTS)
		   req(input$InitialData.UseSatellite)
		   req(input$InitialData.UseCellular)
		   req(input$TopologySettings.FOCLBottleTrh)
		   req(input$TopologySettings.RTSBottleTrh)
		   req(input$TopologySettings.EnableGL)
		   req(input$TopologySettings.SaveTopologyPics)

		   if (input$TopologySettings.EnableGL == 1) {
			 global_log_enabled <- T
			 .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		   }

		   result <- algorithm4_5_impl(input)

		   output$mymap <- result[[1]]

		   output$data <- result[[2]]
		   output$data2 <- result[[3]]
		   output$data3 <- result[[4]]

		   output$log <- result[[6]]
		 },
		 FORMULA_4_5_7 = {

		   req(input$Files.ListOfObjects)
		   req(input$TopologySettings.SNDTFThreshold)
		   req(input$TopologySettings.AdminBN)
		   #req (input$TopologySettings.EnableCore)
		   #req (input$TopologySettings.CoreSize)
		   req(input$TopologySettings.EnablePostopt)
		   req(input$TopologySettings.UseCOWN)
		   req(input$InitialData.UseFOCL)
		   req(input$InitialData.UseRTS)
		   req(input$InitialData.UseSatellite)
		   req(input$InitialData.UseCellular)
		   req(input$TopologySettings.FOCLBottleTrh)
		   req(input$TopologySettings.RTSBottleTrh)
		   req(input$TopologySettings.EnableGL)


		   if (input$TopologySettings.EnableGL == 1) {
			 global_log_enabled <- T
			 .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		   }

		   result <- algorithm4_5_prep(input)

		   #output$mymap <- result[[1]]

		   #output$data <- result[[2]]
		   output$data2 <- result[[1]]
		   output$data3 <- result[[1]]
		   output$myplot <- result[[3]]
		   output$log <- result[[2]]

		 },
		 FORMULA_4_5_8 = { # calls algorithm4_5_simple

		   req(input$Files.ListOfObjects)
		   req(input$InitialData.NodeFrom)
		   req(input$InitialData.NodeTo)
		   req(input$IntParam.FOCL_SPD_LIM)
		   req(input$IntParam.BW_DM_C)
		   req(input$IntParam.AMRT_C)
		   req(input$IntParam.OVS_C)
		   req(input$IntParam.AFFORD_C)
		   #req(input$TopologySettings.SNDTFThreshold)
		   #req(input$TopologySettings.AdminBN)
		   #req (input$TopologySettings.EnableCore)
		   #req (input$TopologySettings.CoreSize)
		   #req(input$TopologySettings.EnablePostopt)
		   #req(input$TopologySettings.UseCOWN)
		   #req(input$InitialData.UseFOCL)
		   #req(input$InitialData.UseRTS)
		   #req(input$InitialData.UseSatellite)
		   #req(input$InitialData.UseCellular)
		   #req(input$TopologySettings.FOCLBottleTrh)
		   #req(input$TopologySettings.RTSBottleTrh)
		   #req(input$TopologySettings.EnableGL)

		   if (input$TopologySettings.EnableGL == 1) {
			 global_log_enabled <- T
			 .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		   }

		   result <- algorithm4_5_simple(input)

		   #output$mymap <- result[[1]]

		   #output$data <- result[[2]]
		   output$data2 <- result[[1]]
		   output$data3 <- result[[3]]
		   #output$myplot <- result[[3]]
		   output$log <- result[[2]]

		 },

		 stop("No!")

  )
}


algorithm4_5_prep <- function(input, intermediate = NULL) { #Load params for NPV/Cown evaluation in formula_4_5_7

  if (is.reactivevalues(input))
	input_load <- reactiveValuesToList(input)
  else
	input_load <- input

  global_log_enabled <- F # Set in F to run in production
  if (exists("bWriteLog", input_load)) global_log_enabled <- T   # Enabling global logging in case of production

  if (exists("TopologySettings.EnableGL", input_load)) {   # Enabling global logging if running as webapp
	if (input_load$TopologySettings.EnableGL == 1) {
	  global_log_enabled <- T
	}
  }

  ts_total_start <- Sys.time()

  #npv_set <- fromJSON(file = "ivars.json")
  if (exists("Files.NPVSettings", input_load)) {
	ts_load_start <- Sys.time()
	if (global_log_enabled) {
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Algorithm_4_5_prep: Trying to load additional parameters from json.file"), input_load$Files.NPVSettings, sep = ": "))
	}
	npv_set <- rjson::fromJSON(file = input_load$Files.NPVSettings)
	npv_set_nrow <- length(npv_set["shared_vars"][[1]])
	for (i in 1:npv_set_nrow) {
	  text_line <- paste("input_load$", str_trim(npv_set["shared_vars"][[1]][[i]][["name"]], side = "both"), " <- ", str_trim(npv_set["shared_vars"][[1]][[i]][["value"]]))
	  # print(text_line)
	  eval(parse(text = text_line))
	  #eval(parse(text= paste("input_load$",str_trim(npv_set["shared_vars"][[1]][[i]][["name"]], side = "both"), " <- ",str_trim(npv_set["shared_vars"][[1]][[i]][["value"]] ))))
	}
	time_config_load <- Sys.time() - ts_load_start
	if (global_log_enabled) {
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Algorithm_4_5_prep: Time consumed by loading of json.file is "), format(time_config_load), sep = ": "))
	}
  }

  # Manually added parameters
  #input_load$InitialDataRadio.MaximumLinkCapacity <- 750 # Radwin 2000 https://www.radwin.com/ptp-radwin-2000/
  #input_load$InitialDataSatellite.MaximumLinkCapacity <- 50 # https://www.ookla.com/articles/starlink-hughesnet-viasat-performance-q4-2021 (avg download speed is 49,33 Mbps)
  #input_load$InitialDataCellular.MaximumLinkCapacity <- 30 # https://www.speedtest.net/awards/brazil/ (avg download speed is 28.75 Mbps)
  print(sprintf("Radio MaximumLinkCapacity %s", input_load$InitialDataRadio.MaximumLinkCapacity))
  print(sprintf("Satellite MaximumLinkCapacity %s", input_load$InitialDataSatellite.MaximumLinkCapacity))
  print(sprintf("Cellular MaximumLinkCapacity %s", input_load$InitialDataCellular.MaximumLinkCapacity))

  if (global_log_enabled) {
	#.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Step-by-step construction process initiated:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Algorithm_4_5_prep: Calculating NPV/COWN dependencies ")))
  }

  best_table <- formula_4_5_7(input_load, output)

  dist_values <- c(1, 2, 5, 10, 25, 50, 100, 250, 500, 1000)
  bandw_values <- c(1, 5, 10, 25, 50, 100, 250, 500, 1000, 5000)
  rownames(best_table) <- bandw_values # Move to 4_5_7 itself
  colnames(best_table) <- dist_values

  #i_length <- length(input_load)
  #for (i in 1:i_length) {

  #}
  #param_table <- data.frame()

  mydata3 <- renderPlot(pheatmap(best_table, cutree_rows = NA, cutree_cols = NA, treeheight_row = 0, treeheight_col = 0, cluster_rows = FALSE, cluster_cols = FALSE,
								 color = c('#0505FF', '#05FF05', '#FF0505', '#DD05DD'), breaks = c(0, 1, 2, 3, 4), legend_breaks = c(0, 1, 2, 3),
								 legend_labels = c('FOCL', 'RTS', 'SAT', 'CELL'), show_colnames = T, show_rownames = T))
  mydata2 <- renderTable(best_table, colnames = TRUE)
  mylog <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
  result <- list(mydata2, mylog, mydata3)

  return(result)

}

#Load params for NPV/Cown evaluation in formula_4_5_7 (8 ?)

algorithm4_5_simple <- function(input, intermediate = NULL) {

  if (is.reactivevalues(input))
	input_load <- reactiveValuesToList(input)
  else
	input_load <- input

  print("loading hadcoded int params ...")
  #Construction cost coef
  input_load$IntParam.CC_C <- list(0, 0, 1, 1) #depends from tech

  #Bandwidth coefficient to take into account additional expenditures for very extensive bandwidth requirements
  input_load$IntParam.B_C <- list(2, 2, 1, 1) #depends from tech

  input_load$IntParam.B_C_T <- list(40 * 1024, 1, 1, 1) #Threshold depends from tech, Mb/s

  #License requirement coefficient for one time regulation license fee
  input_load$IntParam.LC1 <- list(0, 1, 0, 0)

  #License requirement coefficient for recurring license fee
  input_load$IntParam.LC2 <- list(0, 1, 0, 0)

  #License requirement coefficient for one time ISP/mainantance license fee
  input_load$IntParam.LC3 <- list(0, 0, 0, 0)

  #Binary coefficient for enabling annual ISP fee
  input_load$IntParam.ISP_C <- list(1, 0, 1, 1)

  #Income coefficient for NPV calculations ( in % of annual ISP fee)
  input_load$IntParam.INC_C <- list(50, 0, 0, 0)

  #Payback period move to shiny
  input_load$IntParam.PaybackPeriod <- 5

  # Bits per baud coef
  input_load$IntParam.BPB <- 8

  # Road topography coef
  input_load$IntParam.RoadTC <- 0.02

  # Scale factor for input pop data
  input_load$IntParam.PopUnits <- 1

  print("loading hardcoded ext params ...")

  #Fiber optic cable line construction cost (aerial / buried / over existing cable ducts or poles)	 13 636.36 	USD per km
  input_load$ExtParam.ConstructionCostFOCL <- 13636.3

  #Fiber optic cable line construction cost (aerial / buried / over existing cable ducts or poles) USD per km
  input_load$ExtParam.EquipCostFOCL <- 0;

  #Annual fiber optic cable line operation and maintenance cost (aerial / buried / over existing cable ducts or poles)	 163.64 	USD per km per year
  input_load$ExtParam.AnnualMaintenanceCostFOCL <- 163.64

  #Annual ISP fee for 1 Mbps of dedicated internet access channel over a fiber optic cable line  (For total troughput < 1 Gbps)	 17.21 	USD per Mbps per year
  input_load$ExtParam.AnnualISPFeeFOCL1 <- 17.21

  #Annual ISP fee for 1 Mbps of dedicated internet access channel over a fiber optic cable line  (For total troughput >= 1 Gbps)	 10.30 	USD per Mbps per year
  input_load$ExtParam.AnnualISPFeeFOCL2 <- 10.30

  #Microwave type 1 maximum link speed in Mbps
  input_load$ExtParam.RTS_1_MaxBw <- 750

  #Microwave type 2 maximum link speed in Mbps
  input_load$ExtParam.RTS_2_MaxBw <- 9750

  #Microwave point-to-point channel construction cost USD per installation
  input_load$ExtParam.ConstructionCostRTS_1 <- 2000

  #Microwave point-to-point channel construction cost USD per installation
  input_load$ExtParam.ConstructionCostRTS_1 <- 124000

  #Annual microwave point-to-point channel operation and maintenance cost		USD per km per year
  #input_load$ExtParam.AnnualMaintenanceCostRTS <- 300

  #RTS_1 maximum per hop distance in km
  input_load$ExtParam.ConstructionDistLimitRTS_1 <- 50

  #RTS_2 maximum per hop distance in km
  input_load$ExtParam.ConstructionDistLimitRTS_2 <- 18

  #Annual microwave point-to-point channel operation and maintenance cost USD per km per year
  input_load$ExtParam.AnnualMaintenanceCostRTS_1 <- 430

  #Annual microwave point-to-point channel operation and maintenance cost USD per km per year
  input_load$ExtParam.AnnualMaintenanceCostRTS_2 <- 430

  #One time spectrum license fee for 1MHz USD per MHz
  #input_load$ExtParam.SpectrumLicenseCostOne <- 0
  input_load$ExtParam.SpectrumLicenseCostOne_1 <- 611
  input_load$ExtParam.SpectrumLicenseCostOne_2 <- 0

  #Annual recurring spectrum license fee for 1MHz		USD per MHz per year
  #input_load$ExtParam.SpectrumLicenseCostReccur <- 0
  input_load$ExtParam.SpectrumLicenseCostReccur_1 <- 0
  input_load$ExtParam.SpectrumLicenseCostReccur_2 <- 75

  #Satellite terminal equipment purchase and installation cost		USD per Mbps
  input_load$ExtParam.EquipSAT <- 599

  #Annual satellite terminal and ad-hoc equipment maintenance cost		USD per Mbps per year
  input_load$ExtParam.EquipMainantanceSAT <- 10

  #Annual ISP fee for 1 Mbps of dedicated internet access channel over a satellite link		USD per Mbps per year
  input_load$ExtParam.AnnualISPFeeSAT <- 12

  #Satellite link speed that can be upscaled by increasing number of terminals
  input_load$ExtParam.ScaleSAT_MaxBw <- 50;

  #Cellular terminal equipment purchase and installation cost		USD per Mbps
  input_load$ExtParam.EquipCELL <- 400

  #Annual fee for 1 Mbps of dedicated internet access over cellular link		USD per Mbps per year
  input_load$ExtParam.AnnualISPFeeCELL <- 25

  # Average wage per year
  input_load$ExtParam.AverageIncome <- 7850

  #One time service license fee 		USD
  input_load$ExtParam.ServiceLicenseFeeOne <- 100000

  print("params loaded...")

  # Manually added parameters (Simple)
  #input_load$InitialDataRadio.MaximumLinkCapacity <- 750 # Radwin 2000 https://www.radwin.com/ptp-radwin-2000/
  #input_load$InitialDataSatellite.MaximumLinkCapacity <- 50 # https://www.ookla.com/articles/starlink-hughesnet-viasat-performance-q4-2021 (avg download speed is 49,33 Mbps)
  #input_load$InitialDataCellular.MaximumLinkCapacity <- 30 # https://www.speedtest.net/awards/brazil/ (avg download speed is 28.75 Mbps)
  print(sprintf("Radio MaximumLinkCapacity %s", input_load$InitialDataRadio.MaximumLinkCapacity))
  print(sprintf("Satellite MaximumLinkCapacity %s", input_load$InitialDataSatellite.MaximumLinkCapacity))
  print(sprintf("Cellular MaximumLinkCapacity %s", input_load$InitialDataCellular.MaximumLinkCapacity))

  global_log_enabled <- F # Set in F to run in production
  if (exists("bWriteLog", input_load)) global_log_enabled <- T   # Enabling global logging in case of production

  if (exists("TopologySettings.EnableGL", input_load)) {   # Enabling global logging if running as webapp
	if (input_load$TopologySettings.EnableGL == 1) {
	  global_log_enabled <- T
	}
  }

  pop_units <- as.numeric(input_load$IntParam.PopUnits)
  hRes <- as.numeric(input_load$IntParam.HexRes)

  ts_total_start <- Sys.time()

  # index,hex_id,pops,site_fiber,fiber_m,site_coverage,uncovered,uncov_pops,lat,lon

  objects <- vroom::vroom(input_load$Files.ListOfObjects, altrep = FALSE) #
  req(objects)
  numberofobj <- nrow(objects)

  if (numberofobj < 1)
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Algorithm_4_5_simpl: No objects to process "))
	#print ("Insufficeient number of nodes")
	return(0)
  }

  if (global_log_enabled) {
	#.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Step-by-step construction process initiated:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Algorithm_4_5_simple: Using data in file: %s, contains %d objects", input_load$Files.ListOfObjects, nrow(objects))))
  }
  # Manually added parameters
  #input_load$InitialDataRadio.MaximumLinkCapacity <- 2000
  #input_load$InitialDataSatellite.MaximumLinkCapacity <- 50
  #input_load$InitialDataCellular.MaximumLinkCapacity <- 100


  if (global_log_enabled) {
	#.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Step-by-step construction process initiated:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Algorithm_4_5_simple: Calculating BW requirements ")))
  }

  # index,hex_id,pops,site_fiber,fiber_m,site_coverage,uncovered,uncov_pops,lat,lon,+rb
  # was OBJECTNAME;LON;LAT;RB;DTF;COV;REGION
  initial_data_table <- objects
  initial_data_table$RB <- as.numeric(0)
  initial_data_table$DTF <- as.numeric(0)
  initial_data_table$ROADS <- as.numeric(0)
  initial_data_table$UPOP <- as.numeric(0)
  initial_data_table$site_coverage <- as.numeric(0) # Stub to determine 2G only
  initial_data_table$COV <- as.character("2G")
  initial_data_table$REGION <- as.character("")
  initial_data_table$VALID <- T
  numberofobj <- nrow(initial_data_table)

  print(sprintf("BW_DM_C %f ------------------- ", input_load$IntParam.BW_DM_C))
  print(sprintf("OVS_C %f ------------------- ", input_load$IntParam.OVS_C))
  print(sprintf("HexRes %d ------------------- ", input_load$IntParam.HexRes))

  # Mitigate empty cells issue
  initial_data_table$std_network_coverage <- ifelse(is.na(initial_data_table$std_network_coverage), 0, initial_data_table$std_network_coverage)
  initial_data_table$primary_km <- ifelse(is.na(initial_data_table$primary_km), 0, initial_data_table$primary_km)
  initial_data_table$secondary_km <- ifelse(is.na(initial_data_table$secondary_km), 0, initial_data_table$secondary_km)
  initial_data_table$uncov_pops <- ifelse(is.na(initial_data_table$uncov_pops), 0, initial_data_table$uncov_pops)
  #initial_data_table$uncov_pops<-ifelse(is.na(initial_data_table$uncov_pops),0,initial_data_table$uncov_pops)
  #OBJECTNAME;LON;LAT;RB;DTF;COV;REGION
  #i <- 1 # To make filtering of hexagons that are already covered
  filter_crt <- 3 # Criteria for filtering 0 - by std_network_coverage, 1 - by uncovered population
  filtering_trsh_upop <- 2000 # filter hexagons by uncovered populations
  filtering_trsh_focl <- 20000 # filter hexagons by distnce to the fiber, m

  #filtering_trsh_cov <- 0.25 # threshold for std_network_coverage do determine if hexagon is treated as covered or not
  filtering_trsh_cov <- 0.8 # threshold for std_network_coverage do determine if hexagon is treated as covered or not
  dtf_trsh_vector <- c(8.54, 3.23, 1.22) # km
  dtf_trsh <- as.numeric(dtf_trsh_vector[hRes]) * 1000 # threshold to determine if hexagon is directly connected to the fiber, depends from hexagon square 5.16 -> 1.22 / 36.13 -> 3.23 / 252.9 -> 8.54 km
  print(sprintf("Filtering criteria: %d ------------------- ", filter_crt))
  print(sprintf("STD thrsh: %f ------------------- ", filtering_trsh_cov))
  print(sprintf("UPOP thrsh: %f ------------------- ", filtering_trsh_upop))
  print(sprintf("Fiber_m thrsh: %f ------------------- ", filtering_trsh_focl))
  print(sprintf("DTF thrsh: %f ------------------- ", dtf_trsh))
  dc_nodes <- 0 # numer of directly connected nodes
  for (i in 1:numberofobj)
  {

	if (filter_crt == 1) {
	  if (as.numeric(initial_data_table$uncov_pops[i]) < filtering_trsh_upop) { # Mark all hexagons with upop less then the threshold
		initial_data_table$VALID[i] <- F
		#next
	  }
	}

	if (filter_crt == 0) {
	  if (as.numeric(initial_data_table$std_network_coverage[i]) > filtering_trsh_cov) {
		initial_data_table$VALID[i] <- F
		#next
	  }
	}

	if (filter_crt == 3) {
	  if (as.numeric(initial_data_table$fiber_m[i]) > filtering_trsh_focl) {
		initial_data_table$VALID[i] <- F
		#next
	  }
	}

	#print('1')
	if (as.numeric(initial_data_table$primary_km[i]) > 0)
	  c_prim_road <- as.numeric(initial_data_table$primary_km[i])
	else
	  c_prim_road <- 0
	#print('2')
	if (as.numeric(initial_data_table$secondary_km[i]) > 0)
	  c_sec_road <- as.numeric(initial_data_table$secondary_km[i])
	else
	  c_sec_road <- 0
	#print('3')
	initial_data_table$UPOP[i] <- initial_data_table$uncov_pops[i]
	initial_data_table$ROADS[i] <- c_prim_road + c_sec_road
	initial_data_table$RB[i] <- round(as.numeric(input_load$IntParam.BW_DM_C *
												   initial_data_table$uncov_pops[i] *
												   pop_units / input_load$IntParam.OVS_C), 2)

	#print(sprintf("BW_DM_C %f ------------------- ", input_load$IntParam.BW_DM_C))
	#print(sprintf("OVS_C %f ------------------- ", input_load$IntParam.OVS_C))
	#print(sprintf("uncov_pops %f ------------------- ", initial_data_table$uncov_pops[i]))
	#print(sprintf("RB %f ------------------- ", initial_data_table$RB[i]))
	#test_capex_focl <- calc_capex_simple(iput_load, 100, initial_data_table$RB[i], 1)
	#print(sprintf("Capex focl %f ------------------- ", test_capex_focl))
	#test_capex_rts <- calc_capex_simple(input_load, 100, initial_data_table$RB[i], 2)
	#print(sprintf("Capex rts %f ------------------- ", test_capex_rts))
	#test_capex_sat <- calc_capex_simple(input_load, 100, initial_data_table$RB[i], 3)
	#print(sprintf("Capex sat %f ------------------- ", test_capex_sat))
	#test_capex_cell <- calc_capex_simple(input_load, 100, initial_data_table$RB[i], 4)
	#print(sprintf("Capex cell %f ------------------- ", test_capex_cell))

	#df_convert$OBJECTNAME[i] <- initial_data_table$hex_id[i]
	#df_convert$LON[i] <- initial_data_table$lon[i]
	#df_convert$LAT[i] <- initial_data_table$lat[i]
	#df_convert$RB[i] <- initial_data_table$RB[i]
	#print('4')
	if (initial_data_table$fiber_m[i] <= dtf_trsh) { # Check if fiber is located inside the hex
	  initial_data_table$DTF[i] <- as.numeric(0)
	  dc_nodes <- dc_nodes + 1
	}
	else {
	  initial_data_table$DTF[i] <- as.numeric(initial_data_table$fiber_m[i] / 1000)
	}
	#print(sprintf("-- fiber_m %f dtf_trsh %f RES: %f ---  ", initial_data_table$fiber_m[i], dtf_trsh, initial_data_table$DTF[i]))

	if (as.numeric(initial_data_table$site_coverage[i]) == 0)
	  initial_data_table$COV[i] <- "2G"
	else
	  initial_data_table$COV[i] <- "3G"
	initial_data_table$REGION[i] <- initial_data_table$hex_id[i]
	#i <- i + 1
	if (initial_data_table$VALID == F)
	  print(sprintf("-- Phase 2 node %s %f %f ---  ", initial_data_table$hex_id[i], initial_data_table$UPOP[i], initial_data_table$uncov_pops[i]))
  }

  print(sprintf("-- Directly connected nodes %d ---  ", dc_nodes))
  residual_data_table <- initial_data_table[(initial_data_table$VALID == F) & (initial_data_table$UPOP > filtering_trsh_upop),] # Filtering hexagons for phase 2
  #initial_data_table <- initial_data_table[initial_data_table$VALID == T,] # Filtering hexagons for the phase 1 criteria
  initial_data_table <- initial_data_table[initial_data_table$VALID == T,] # Change to filter_crt = 4

  #initial_data_table$POC <- as.character(initial_data_table$hex_id)
  #initial_data_table$POCLON <- as.numeric(initial_data_table$lon)
  #initial_data_table$POCLAT <- as.numeric(initial_data_table$lat)

  df_convert <- data.frame(as.character(initial_data_table$hex_id), initial_data_table$lon, initial_data_table$lat, initial_data_table$RB, initial_data_table$DTF, initial_data_table$COV, as.character(initial_data_table$REGION), as.character(initial_data_table$hex_id), initial_data_table$lon, initial_data_table$lat)
  names(df_convert) <- c("OBJECTNAME", "LON", "LAT", "RB", "DTF", "COV", "REGION", "POC", "POCLON", "POCLAT")
  result_file <- "coord_hex.csv"
  #vroom::vroom_write(df_convert, result_file)
  vroom::vroom_write(df_convert, result_file, delim = ";")

  if (filter_crt == 3) {
	#residual_data_table <- initial_data_table[initial_data_table$VALID == F,] # Filtering hexagons for phase 2
	residual_data_table$POC <- as.character("")
	residual_data_table$POCLON <- as.numeric(0)
	residual_data_table$POCLAT <- as.numeric(0)
	numberofobj_main <- nrow(initial_data_table)
	numberofobj_res <- nrow(residual_data_table)
	#print(numberofobj_res)
	if (numberofobj_res > 0) {
	  for (res_i in 1:numberofobj_res) { # updating dtf's in phase 2 nodes regarding new best possibilities
		cur_dtf <- as.numeric(residual_data_table$fiber_m[res_i])
		cur_lon <- as.numeric(residual_data_table$lon[res_i])
		cur_lat <- as.numeric(residual_data_table$lat[res_i])
		cur_conn_point <- "NATIVE"
		cur_conn_point_lon <- cur_lon
		cur_conn_point_lat <- cur_lat

		for (in_i in 1:numberofobj_main) {
		  new_dist <- distm(c(as.numeric(initial_data_table$lon[in_i]), as.numeric(initial_data_table$lat[in_i])), c(cur_lon, cur_lat), fun = distHaversine)
		  new_dtf <- round(as.numeric(new_dist[1] / 1000), digits = 4) # 1st-row is distance to BN
		  #print(sprintf("Coordinates %f %f %f %f ------------------- ", as.numeric(initial_data_table$lon[in_i]), as.numeric(initial_data_table$lat[in_i]), cur_lon, cur_lat))
		  #print(sprintf("Cur_dtf vs new_dtf %f %f %f %f ------------------- ", cur_dtf, new_dtf, cur_lon, cur_lat))
		  if (new_dtf < cur_dtf) {
			cur_dtf <- new_dtf
			cur_conn_point <- ifelse(is.na(initial_data_table$hex_id[in_i]), "Unknown", as.character(initial_data_table$hex_id[in_i]))
			cur_conn_point_lon <- as.numeric(initial_data_table$lon[in_i])
			cur_conn_point_lat <- as.numeric(initial_data_table$lat[in_i])
			#print(as.character(initial_data_table$hex_id[in_i]))
		  }
		}
		#print(sprintf("Connection %f %s ------------------- ", cur_dtf, as.character(cur_conn_point)))
		residual_data_table$fiber_m[res_i] <- cur_dtf
		residual_data_table$POC[res_i] <- as.character(cur_conn_point)
		residual_data_table$POCLON[res_i] <- cur_conn_point_lon
		residual_data_table$POCLAT[res_i] <- cur_conn_point_lat
	  }
	}

	df_convert <- data.frame(as.character(residual_data_table$hex_id), residual_data_table$lon, residual_data_table$lat, residual_data_table$RB, residual_data_table$DTF, residual_data_table$COV, as.character(residual_data_table$REGION), residual_data_table$POC, residual_data_table$POCLON, residual_data_table$POCLAT)
	names(df_convert) <- c("OBJECTNAME", "LON", "LAT", "RB", "DTF", "COV", "REGION", "POC", "POCLON", "POCLAT")
	result_file <- "coord_hex_residual.csv"
	#vroom::vroom_write(df_convert, result_file)
	vroom::vroom_write(df_convert, result_file, delim = ";")
  }

  print("Check 1")

  from_ind <- as.numeric(input_load$InitialData.NodeFrom)
  to_ind <- as.numeric(input_load$InitialData.NodeTo)
  bw_rec <- initial_data_table$RB[from_ind]
  unc_pop <- initial_data_table$uncov_pops[from_ind]
  from_name <- initial_data_table$hex_id[from_ind]
  to_name <- initial_data_table$hex_id[to_ind]
  dist_rec <- round(distm(c(as.numeric(initial_data_table$lon[from_ind]), as.numeric(initial_data_table$lat[from_ind])), c(as.numeric(initial_data_table$lon[to_ind]), as.numeric(initial_data_table$lat[to_ind])), fun = distHaversine) / 1000, 4)

  print("Check 2")
  print(sprintf("FROM IDX %d TO IDX %d ", from_ind, to_ind))
  print(sprintf("FROM %s TO %s DIST %f BW %f ", from_name, to_name, dist_rec, bw_rec))

  test_capex_focl <- calc_capex_simple(input_load, dist_rec, bw_rec, 1)
  print(sprintf("Capex focl %f ------------------- ", test_capex_focl))
  test_capex_rts <- calc_capex_simple(input_load, dist_rec, bw_rec, 2)
  print(sprintf("Capex rts %f ------------------- ", test_capex_rts))
  test_capex_sat <- calc_capex_simple(input_load, dist_rec, bw_rec, 3)
  print(sprintf("Capex sat %f ------------------- ", test_capex_sat))
  test_capex_cell <- calc_capex_simple(input_load, dist_rec, bw_rec, 4)
  print(sprintf("Capex cell %f ------------------- ", test_capex_cell))

  print("Check 3")

  print(" -------------------------------- ")
  print(sprintf("FROM %s TO %s DIST %f BW %f ", from_name, to_name, dist_rec, bw_rec))
  print(sprintf("CAPEX FOCL %f ", test_capex_focl))
  print(sprintf("CAPEX RTS %f ", test_capex_rts))
  print(sprintf("CAPEX SAT %f ", test_capex_sat))
  print(sprintf("CAPEX CELL %f ", test_capex_cell))
  print(" -------------------------------- ")

  test_opex_focl <- calc_opex_simple(input_load, dist_rec, bw_rec, 1)
  #print(sprintf("Capex focl %f ------------------- ", test_capex_focl))
  test_opex_rts <- calc_opex_simple(input_load, dist_rec, bw_rec, 2)
  #print(sprintf("Capex rts %f ------------------- ", test_capex_rts))
  test_opex_sat <- calc_opex_simple(input_load, dist_rec, bw_rec, 3)
  #print(sprintf("Capex sat %f ------------------- ", test_capex_sat))
  test_opex_cell <- calc_opex_simple(input_load, dist_rec, bw_rec, 4)

  print("Check 4")

  print(" -------------------------------- ")
  print(sprintf("FROM %s TO %s DIST %f BW %f ", from_name, to_name, dist_rec, bw_rec))
  print(sprintf("OPEX FOCL %f ", test_opex_focl))
  print(sprintf("OPEX RTS %f ", test_opex_rts))
  print(sprintf("OPEX SAT %f ", test_opex_sat))
  print(sprintf("OPEX CELL %f ", test_opex_cell))
  print(" -------------------------------- ")

  test_tco_focl <- calc_tco_simple(input_load, test_capex_focl, test_opex_focl, 1)
  #print(sprintf("Capex focl %f ------------------- ", test_capex_focl))
  test_tco_rts <- calc_tco_simple(input_load, test_capex_rts, test_opex_rts, 2)
  #print(sprintf("Capex rts %f ------------------- ", test_capex_rts))
  test_tco_sat <- calc_tco_simple(input_load, test_capex_sat, test_opex_sat, 3)
  #print(sprintf("Capex sat %f ------------------- ", test_capex_sat))
  test_tco_cell <- calc_tco_simple(input_load, test_capex_cell, test_opex_cell, 4)

  print("Check 5")

  print(" -------------------------------- ")
  print(sprintf("FROM %s TO %s DIST %f BW %f ", from_name, to_name, dist_rec, bw_rec))
  print(sprintf("OPEX FOCL %f ", test_tco_focl))
  print(sprintf("OPEX RTS %f ", test_tco_rts))
  print(sprintf("OPEX SAT %f ", test_tco_sat))
  print(sprintf("OPEX CELL %f ", test_tco_cell))
  print(" -------------------------------- ")

  test_npv_focl <- calc_npv_simple(input_load, test_capex_focl, test_opex_focl, bw_rec, 1)
  #test_npv_focl <- calc_npv_simple(input_load, test_capex_focl, test_opex_focl, bw_rec, unc_pop,1)
  #print(sprintf("Capex focl %f ------------------- ", test_capex_focl))
  test_npv_rts <- calc_npv_simple(input_load, test_capex_rts, test_opex_rts, bw_rec, 2)
  #test_npv_rts <- calc_npv_simple(input_load, test_capex_rts, test_opex_rts, bw_rec, unc_pop,2)
  #print(sprintf("Capex rts %f ------------------- ", test_capex_rts))
  test_npv_sat <- calc_npv_simple(input_load, test_capex_sat, test_opex_sat, bw_rec, 3)
  #test_npv_sat <- calc_npv_simple(input_load, test_capex_sat, test_opex_sat, bw_rec, unc_pop,3)
  #print(sprintf("Capex sat %f ------------------- ", test_capex_sat))
  #test_npv_cell <- calc_npv_simple(input_load, test_capex_cell, test_opex_cell, bw_rec, unc_pop,4)
  test_npv_cell <- calc_npv_simple(input_load, test_capex_cell, test_opex_cell, bw_rec, 4)

  print("Check 6")

  print(" -------------------------------- ")
  print(sprintf("FROM %s TO %s DIST %f BW %f ", from_name, to_name, dist_rec, bw_rec))
  print(sprintf("OPEX FOCL %f ", test_npv_focl))
  print(sprintf("OPEX RTS %f ", test_npv_rts))
  print(sprintf("OPEX SAT %f ", test_npv_sat))
  print(sprintf("OPEX CELL %f ", test_npv_cell))
  print(" -------------------------------- ")

  #param_table <- data.frame()
  row_names <- c("FOCL", "RTS", "SAT", "MOB")
  capex_res <- c(test_capex_focl, test_capex_rts, test_capex_sat, test_capex_cell)
  opex_res <- c(test_opex_focl, test_opex_rts, test_opex_sat, test_opex_cell)
  tco_res <- c(test_tco_focl, test_tco_rts, test_tco_sat, test_tco_cell)
  #tco_res <- c(test_capex_focl + test_opex_focl, test_opex_rts + test_opex_rts, test_opex_sat + test_opex_sat, test_opex_cell + test_opex_cell)
  npv_res <- c(test_npv_focl, test_npv_rts, test_npv_sat, test_npv_cell)
  result_simple_table <- data.frame(row_names, capex_res, opex_res, tco_res, npv_res)
  print(result_simple_table)

  #rownames(result_simple_table) <- c("CAPEX", "OPEX", "TCO", "NPV")
  mydata2 <- renderTable(initial_data_table, colnames = TRUE)
  mylog <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
  mydata3 <- renderTable(result_simple_table, colnames = TRUE)
  result <- list(mydata2, mylog, mydata3)
  #result <- list(mydata2, mydata3, mylog)

  return(result)

}


#Calculation of 8-col NPV matrix (from each regular node to BN, 4 possible technologies)

calc_4x_matrix <- function(input, objects, bnindex, intermediate = NULL)
{

  req(input)
  #req (input$Files.ListOfObjects)
  #req (input$Intermediate.BNIndex)
  # Add additional reqs !
  #objects <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)

  req(objects)

  numberofobj <- nrow(objects)
  npv4matrix <- matrix(nrow = numberofobj, ncol = 8, 0)
  #BN <- objects[input$Intermediate.BNIndex,]
  BN <- objects[bnindex,]
  BNLon = BN[2]
  BNLat = BN[3]

  if (numberofobj > 0)
  {
	for (i in 1:numberofobj) # Distance and preliminary cycle
	{
	  npv4matrix[i, 2] = as.numeric(objects[i, 4]) # 2nd-row is RBs
	  #if (i != input$Intermediate.BNIndex) {
	  if (i != bnindex) {
		dist_to_BN <- distm(c(as.numeric(objects[i, 2]), as.numeric(objects[i, 3])), c(as.numeric(BNLon), as.numeric(BNLat)), fun = distHaversine)
		#distancekm <- round (as.numeric (distancem/1000), digits = 2)
		npv4matrix[i, 1] = round(as.numeric(dist_to_BN[1] / 1000), digits = 2) # 1st-row is distance to BN
		#obj_plus_DTBN [i,]$DTBN = 100
		#obj_plus_DTBN[i,1] = iconv(obj_plus_DTBN[i,1], "UTF-8", "UTF-8",sub='')
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

	  # Stub starts --------- !!!!!!!
	  if (input$use_stub) {
		npv4matrix[i, 3] <- -100 * npv4matrix[i, 1] # npv_focl
		npv4matrix[i, 4] <- -200 * npv4matrix[i, 1] # npv_rts
		npv4matrix[i, 5] <- -300 * npv4matrix[i, 1] # npv_satellite
		npv4matrix[i, 6] <- -400 * npv4matrix[i, 1] # npv_cellular
		npv4matrix[i, 7] <- 1 # best tech
		npv4matrix[i, 8] <- npv4matrix[i, 3] # best npv
	  }
	  # Stub ends --------- !!!!!!!

	}

	# Stub starts --------- !!!!!!!
	if (input$use_stub) {
	  return(npv4matrix)
	}

	#Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
	input2 <- input

	if (is.reactivevalues(input))
	  input2 <- reactiveValuesToList(input)

	bCalcNPV = T

	for (i in 1:numberofobj) # NPV main cycle ----------------------------------------------------------
	{
	  distancekm <- as.numeric(npv4matrix[i, 1])
	  rb <- as.numeric(npv4matrix[i, 2])

	  input2$SchoolSpecificData.Length <- distancekm
	  input2$SchoolSpecificData.RequiredBandwidth <- rb

	  #if (i != input$Intermediate.BNIndex) # Checks curnode for BNode -------------------------------------
	  if (i != bnindex) # Checks curnode for BNode -------------------------------------
	  {


		# FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
		#Coefficient should be between 0 and 1
		TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

		#Distance by the roads in km
		distancekmbyroads <- round(distancekm * (1 + TopCoeff), digits = 2)

		input2$SchoolSpecificData.Length <- distancekmbyroads

		#Algorithm for overall FOCL construction cost evaluation

		focl_capex <- NULL
		focl_capex = algorithm2_1_impl(input2)
		if (is.null(focl_capex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate overall FOCL construction cost for some link"))
		  .GlobalEnv$top_calc_status = 408
		  return(0)
		}

		#Overall cost of FOCL installation
		TotalCAPEXFOCL = as.numeric(focl_capex[1, 2])

		#Overall length of the FOCL construction site
		FOCLLenghtTotal = as.numeric(focl_capex[2, 2])

		FOCLSectionLengthCD = as.numeric(focl_capex[3, 2])

		#Cost of equipment and materials for the construction of fiber optic lines
		CostOfEqAndMatFOCL = as.numeric(focl_capex[4, 2])


		#Algorithm for overall FOCL maintenance cost evaluation
		intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
		intermediate51$RequiredCapacity <- rb
		intermediate51$FOCLSectionLengthCD <- FOCLSectionLengthCD
		intermediate51$Technology <- 0


		intermediate2 <- list(FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
		intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
		intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
		intermediate2$RequiredCapacity <- rb

		focl_opex <- NULL
		focl_opex = algorithm2_2_impl(input2, intermediate2)
		if (is.null(focl_opex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate overall FOCL maintenance cost for some link"))
		  .GlobalEnv$top_calc_status = 407
		  return(0)
		}

		#Total cost for FOCL maintenance for the entire period of operation
		TotalOPEXFOCL = as.numeric(focl_opex[1, 2])


		#Cost of ownership for the period
		ownership_f = TotalCAPEXFOCL + TotalOPEXFOCL * as.numeric(input$PVOptionSet.PaybackPeriod)
		capex_f = TotalCAPEXFOCL
		opex_f = TotalOPEXFOCL


		if (bCalcNPV)
		{
		  focl_income <- NULL
		  focl_income = algorithm2_12_impl(input2, intermediate51)
		  if (is.null(focl_income))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate potential income from FOCL for some link"))
			.GlobalEnv$top_calc_status = 406
			return(0)
		  }


		  NetIncome <- as.numeric(focl_income[1, 2])
		  income_f = NetIncome

		  #NPV - FOCL
		  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
		  TotalInvest <- TotalCAPEXFOCL
		  CostOfOperation <- TotalOPEXFOCL
		  CostOfEquipmentAndMaterials <- CostOfEqAndMatFOCL
		  intermediate5$TotalInvest <- TotalInvest
		  intermediate5$CostOfOperation <- CostOfOperation
		  intermediate5$NetIncome <- NetIncome
		  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
		  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

		  npv_focl <- NULL
		  npv_focl <- algorithm2_7_impl(input2, intermediate5)

		  if (is.null(npv_focl))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV of FOCL for some link"))
			.GlobalEnv$top_calc_status = 405
			return(0)
		  }

		  npv_f = as.numeric(npv_focl[1, 2])
		  npv4matrix[i, 3] = npv_f
		}
		# FOCL NPV CALCs ENDS -----------------------------------------------------------------------------

		# RTS NPV CALCs STARTS ----------------------------------------------------------------------------
		input2$SchoolSpecificData.Length <- distancekm #Back to the regular distance

		#Algorithm for total RTS construction cost evaluation between object and SN in locality
		rts_capex <- NULL
		rts_capex = algorithm2_3_impl(input2, intermediate)
		if (is.null(rts_capex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate total RTS construction cost for some link"))
		  .GlobalEnv$top_calc_status = 508
		  return(0)
		}


		TotalCAPEXRTS <- as.numeric(rts_capex[1, 2])

		NumberOfRepeaters <- as.numeric(rts_capex[2, 2])

		#Total cost of equipment and materials for the construction of the RTS
		CostOfEqAndMatRTS <- as.numeric(rts_capex[3, 2])

		WidthFrequencyChannel <- as.numeric(rts_capex[4, 2])


		#Algorithm for total RTS maintenance cost evaluation between object and SN in locality

		intermediate3 <- list(NumberOfRepeaters = 0, RequiredCapacity = 0.0, NumberOfTerminals = 0.0, WidthFrequencyChannel = 0.0)
		intermediate3$NumberOfRepeaters <- NumberOfRepeaters
		intermediate3$RequiredCapacity <- rb
		intermediate3$NumberOfTerminals <- 2
		intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel


		if (TotalCAPEXRTS == 0)
		{
		  intermediate3$NumberOfTerminals <- 0
		}

		rts_opex <- NULL
		rts_opex = algorithm2_4_impl(input2, intermediate3)

		if (is.null(rts_opex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate total RTS maintenance cost for some link"))
		  .GlobalEnv$top_calc_status = 507
		  return(0)
		}

		TotalOPEXRTS = as.numeric(rts_opex[1, 2])


		#Cost of ownership for the period
		ownership_m <- TotalCAPEXRTS + TotalOPEXRTS * as.numeric(input$PVOptionSet.PaybackPeriod)
		capex_m = TotalCAPEXRTS
		opex_m = TotalOPEXRTS


		if (bCalcNPV)
		{
		  #NPV - Microwave

		  intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
		  intermediate51$RequiredCapacity <- rb
		  intermediate51$Technology <- 1

		  rts_income <- NULL
		  rts_income = algorithm2_12_impl(input2, intermediate51)
		  if (is.null(rts_income))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate potential income from RTS for some link"))
			.GlobalEnv$top_calc_status = 506
			return(0)
		  }


		  NetIncome <- as.numeric(rts_income[1, 2])
		  income_m = NetIncome

		  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)

		  TotalInvest <- TotalCAPEXRTS
		  CostOfOperation <- TotalOPEXRTS
		  CostOfEquipmentAndMaterials <- CostOfEqAndMatRTS

		  intermediate5$TotalInvest <- TotalInvest
		  intermediate5$CostOfOperation <- CostOfOperation
		  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
		  intermediate5$NetIncome <- NetIncome
		  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

		  npv_microwave <- NULL
		  npv_microwave <- algorithm2_7_impl(input2, intermediate5)
		  if (is.null(npv_microwave))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV for RTS for some link"))
			.GlobalEnv$top_calc_status = 505
			return(0)
		  }

		  npv_m <- as.numeric(npv_microwave[1, 2])
		  npv4matrix[i, 4] = npv_m

		}
		# RTS NPV CALCs ENDS   ----------------------------------------------------------------------------

		# SATELLITE NPV CALCs STARTS   ---------------------------------------------------------------------
		#rb <- objects [i,4]
		ownership_s <- 100000000000000
		npv_s <- -100000000000000
		capex_s = 0
		opex_s = 0


		intermediate4 <- list(NumberVSATsets = 0, RequiredCapacity = 0.0)
		intermediate4$RequiredCapacity <- rb

		sat_capex <- NULL
		sat_capex = algorithm2_5_impl(input2, intermediate4)
		if (is.null(sat_capex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate CAPEX of satellite for some link"))
		  .GlobalEnv$top_calc_status = 703
		  return(0)
		}


		# Required number of VSAT sets

		NumberVSATsets = as.numeric(sat_capex[2, 2])

		TotalCAPEXSetellite = as.numeric(sat_capex[1, 2])

		#Total cost of VSAT equipment and installation materials
		CostOfVSATEqAndMat = as.numeric(sat_capex[3, 2])

		#Algorithm for determining the total cost of the maintenance of the satellite communication channel


		intermediate4$NumberVSATsets <- NumberVSATsets


		sat_opex <- NULL
		sat_opex = algorithm2_6_impl(input2, intermediate4)
		if (is.null(sat_opex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate OPEX of satellite for some link"))
		  .GlobalEnv$top_calc_status = 702
		  return(0)
		}

		TotalOPEXSatellite = as.numeric(sat_opex[1, 2])


		ownership_s <- TotalCAPEXSetellite + TotalOPEXSatellite * as.numeric(input$PVOptionSet.PaybackPeriod)
		capex_s = TotalCAPEXSetellite
		opex_s = TotalOPEXSatellite


		if (bCalcNPV)
		{
		  #NPV - Satellite
		  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
		  TotalInvest <- TotalCAPEXSetellite
		  CostOfOperation <- TotalOPEXSatellite
		  CostOfEquipmentAndMaterials <- CostOfVSATEqAndMat

		  intermediate5$NetIncome <- 0
		  intermediate5$TotalInvest <- TotalInvest
		  intermediate5$CostOfOperation <- CostOfOperation
		  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
		  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

		  npv_satellite <- NULL
		  npv_satellite <- algorithm2_7_impl(input2, intermediate5)
		  if (is.null(npv_satellite))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV of satellite for some link"))
			.GlobalEnv$top_calc_status = 701
			return(0)
		  }

		  npv_s <- as.numeric(npv_satellite[1, 2])
		  npv4matrix[i, 5] = npv_s
		}


		# SATELLITE NPV CALCs ENDS   -----------------------------------------------------------------------

		# CELLULAR NPV CALCs STARTS   ---------------------------------------------------------------------

		ownership_cellular <- 100000000000000
		npv_cellular <- -100000000000000
		capex_cellular = 0
		opex_cellular = 0


		cellstatus <- "2G"

		cellstatus <- objects[i, 6]

		if (!is.na(cellstatus))
		{
		  if (cellstatus == "3G" || cellstatus == "4G")
		  {
			#Algorithm for determining the total cost of the installation and configuration of the cellular communication channel

			intermediate41 <- list(NumberCellularsets = 0, RequiredCapacity = 0.0)
			intermediate41$RequiredCapacity <- rb

			cel_capex <- NULL
			cel_capex = algorithm2_10_impl(input, intermediate41) # input2 ?
			if (is.null(cel_capex))
			{
			  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate CAPEX of cellular for some link"))
			  .GlobalEnv$top_calc_status = 603
			  return(0)
			}


			# Required number of Cellular sets

			NumberCellularsets = as.numeric(cel_capex[2, 2])

			TotalCAPEXCellular = as.numeric(cel_capex[1, 2])

			#Total cost of Cellular equipment and installation materials
			CostOfCellularEqAndMat = as.numeric(cel_capex[3, 2])


			#Algorithm for determining the total cost of the maintenance of the Cellular communication channel


			intermediate41$NumberCellularsets <- NumberCellularsets

			cel_opex <- NULL
			cel_opex = algorithm2_11_impl(input, intermediate41)
			if (is.null(cel_opex))
			{
			  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate OPEX of cellular for some link"))
			  .GlobalEnv$top_calc_status = 602
			  return(0)
			}

			TotalOPEXCellular = as.numeric(cel_opex[1, 2])


			ownership_cellular <- TotalCAPEXCellular + TotalOPEXCellular * as.numeric(input$PVOptionSet.PaybackPeriod)
			capex_cellular = TotalCAPEXCellular
			opex_cellular = TotalOPEXCellular

			if (bCalcNPV)
			{
			  #NPV - Cellular
			  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
			  TotalInvest <- TotalCAPEXCellular
			  CostOfOperation <- TotalOPEXCellular
			  CostOfEquipmentAndMaterials <- CostOfCellularEqAndMat

			  intermediate5$NetIncome <- 0
			  intermediate5$TotalInvest <- TotalInvest
			  intermediate5$CostOfOperation <- CostOfOperation
			  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
			  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

			  npv_cell <- NULL
			  npv_cell <- algorithm2_7_impl(input, intermediate5)
			  if (is.null(npv_cell))
			  {
				.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
				.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV of cellular for some link"))
				.GlobalEnv$top_calc_status = 601
				return(0)
			  }

			  npv_cellular <- as.numeric(npv_cell[1, 2])
			  npv4matrix[i, 6] = npv_cellular
			}

		  }
		  else
			npv4matrix[i, 6] = npv_cellular        # for 2G
		}
		else
		  cellstatus <- "2G"


		# CELLULAR NPV CALCs ENDS   -----------------------------------------------------------------------

	  } # Checking curnode for BNode ENDS -----------------------------------------------------------------

	  best_npv_index = which(npv4matrix[i, 3:6] == max(npv4matrix[i, 3:6]))[1] # 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	  if (best_npv_index < 1 || best_npv_index > 4)
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_npv: Incorrect technology index"))
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

#Calculation of 8-col Cost-of-Ownership matrix (from each regular node to BN, 4 possible technologies)

calc_4x_matrix_cown <- function(input, objects, bnindex, intermediate = NULL)
{

  req(input)
  #req (input$Files.ListOfObjects)
  #req (input$Intermediate.BNIndex)
  # Add additional reqs !
  #objects <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)

  req(objects)

  numberofobj <- nrow(objects)
  cown4matrix <- matrix(nrow = numberofobj, ncol = 8, 0)
  #BN <- objects[input$Intermediate.BNIndex,]
  BN <- objects[bnindex,]
  BNLon = BN[2]
  BNLat = BN[3]

  if (numberofobj > 0)
  {
	for (i in 1:numberofobj) # Distance and preliminary cycle
	{
	  cown4matrix[i, 2] = as.numeric(objects[i, 4]) # 2nd-row is RBs
	  #if (i != input$Intermediate.BNIndex) {
	  if (i != bnindex) {
		dist_to_BN <- distm(c(as.numeric(objects[i, 2]), as.numeric(objects[i, 3])), c(as.numeric(BNLon), as.numeric(BNLat)), fun = distHaversine)
		#distancekm <- round (as.numeric (distancem/1000), digits = 2)
		cown4matrix[i, 1] = round(as.numeric(dist_to_BN[1] / 1000), digits = 2) # 1st-row is distance to BN
		#obj_plus_DTBN [i,]$DTBN = 100
		#obj_plus_DTBN[i,1] = iconv(obj_plus_DTBN[i,1], "UTF-8", "UTF-8",sub='')
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

	  # Stub starts --------- !!!!!!!
	  if (input$use_stub) {
		cown4matrix[i, 3] <- -100 * cown4matrix[i, 1] # npv_focl
		cown4matrix[i, 4] <- -200 * cown4matrix[i, 1] # npv_rts
		cown4matrix[i, 5] <- -300 * cown4matrix[i, 1] # npv_Satellite
		cown4matrix[i, 6] <- -400 * cown4matrix[i, 1] # npv_cellular
		cown4matrix[i, 7] <- 1 # best tech
		cown4matrix[i, 8] <- cown4matrix[i, 3] # best npv
	  }
	  # Stub ends --------- !!!!!!!

	}

	# Stub starts --------- !!!!!!!
	if (input$use_stub) {
	  return(cown4matrix)
	}
	# Stub ends --------- !!!!!!!

	#Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
	input2 <- input

	if (is.reactivevalues(input))
	  input2 <- reactiveValuesToList(input)

	bCalcNPV = T

	for (i in 1:numberofobj) # NPV main cycle ----------------------------------------------------------
	{
	  distancekm <- as.numeric(cown4matrix[i, 1])
	  rb <- as.numeric(cown4matrix[i, 2])

	  input2$SchoolSpecificData.Length <- distancekm
	  input2$SchoolSpecificData.RequiredBandwidth <- rb

	  #if (i != input$Intermediate.BNIndex) # Checks curnode for BNode -------------------------------------
	  if (i != bnindex) # Checks curnode for BNode -------------------------------------
	  {


		# FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
		#Coefficient should be between 0 and 1
		TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

		#Distance by the roads in km
		distancekmbyroads <- round(distancekm * (1 + TopCoeff), digits = 2)

		input2$SchoolSpecificData.Length <- distancekmbyroads

		#Algorithm for overall FOCL construction cost evaluation

		focl_capex <- NULL
		focl_capex = algorithm2_1_impl(input2)
		if (is.null(focl_capex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_own Cannot calculate overall FOCL construction cost for some link"))
		  .GlobalEnv$top_calc_status = 408
		  return(0)
		}

		#Overall cost of FOCL installation
		TotalCAPEXFOCL = as.numeric(focl_capex[1, 2])

		#Overall length of the FOCL construction site
		FOCLLenghtTotal = as.numeric(focl_capex[2, 2])

		FOCLSectionLengthCD = as.numeric(focl_capex[3, 2])

		#Cost of equipment and materials for the construction of fiber optic lines
		CostOfEqAndMatFOCL = as.numeric(focl_capex[4, 2])


		#Algorithm for overall FOCL maintenance cost evaluation
		intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
		intermediate51$RequiredCapacity <- rb
		intermediate51$FOCLSectionLengthCD <- FOCLSectionLengthCD
		intermediate51$Technology <- 0


		intermediate2 <- list(FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
		intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
		intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
		intermediate2$RequiredCapacity <- rb

		focl_opex <- NULL
		focl_opex = algorithm2_2_impl(input2, intermediate2)
		if (is.null(focl_opex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_own: Cannot calculate overall FOCL maintenance cost for some link"))
		  .GlobalEnv$top_calc_status = 407
		  return(0)
		}

		#Total cost for FOCL maintenance for the entire period of operation
		TotalOPEXFOCL = as.numeric(focl_opex[1, 2])


		#Cost of ownership for the period
		#print(1)
		ownership_f = TotalCAPEXFOCL + TotalOPEXFOCL * as.numeric(input$PVOptionSet.PaybackPeriod)
		capex_f = TotalCAPEXFOCL
		opex_f = TotalOPEXFOCL
		#print(sprintf("tco1 %f", ownership_f))

		# NPV deleted
		cown4matrix[i, 3] = -as.numeric(ownership_f)
		# FOCL NPV CALCs ENDS -----------------------------------------------------------------------------

		# RTS NPV CALCs STARTS ----------------------------------------------------------------------------
		input2$SchoolSpecificData.Length <- distancekm #Back to the regular distance

		#Algorithm for total RTS construction cost evaluation between object and SN in locality
		rts_capex <- NULL
		rts_capex = algorithm2_3_impl(input2, intermediate)
		if (is.null(rts_capex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_own: Cannot calculate total RTS construction cost for some link"))
		  .GlobalEnv$top_calc_status = 508
		  return(0)
		}


		TotalCAPEXRTS <- as.numeric(rts_capex[1, 2])

		NumberOfRepeaters <- as.numeric(rts_capex[2, 2])

		#Total cost of equipment and materials for the construction of the RTS
		CostOfEqAndMatRTS <- as.numeric(rts_capex[3, 2])

		WidthFrequencyChannel <- as.numeric(rts_capex[4, 2])


		#Algorithm for total RTS maintenance cost evaluation between object and SN in locality

		intermediate3 <- list(NumberOfRepeaters = 0, RequiredCapacity = 0.0, NumberOfTerminals = 0.0, WidthFrequencyChannel = 0.0)
		intermediate3$NumberOfRepeaters <- NumberOfRepeaters
		intermediate3$RequiredCapacity <- rb
		intermediate3$NumberOfTerminals <- 2
		intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel


		if (TotalCAPEXRTS == 0)
		{
		  intermediate3$NumberOfTerminals <- 0
		}

		rts_opex <- NULL
		rts_opex = algorithm2_4_impl(input2, intermediate3)

		if (is.null(rts_opex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_own: Cannot calculate total RTS maintenance cost for some link"))
		  .GlobalEnv$top_calc_status = 507
		  return(0)
		}

		TotalOPEXRTS = as.numeric(rts_opex[1, 2])

		#print(2)
		#Cost of ownership for the period
		ownership_m <- TotalCAPEXRTS + TotalOPEXRTS * as.numeric(input$PVOptionSet.PaybackPeriod)
		capex_m = TotalCAPEXRTS
		opex_m = TotalOPEXRTS
		#print(sprintf("tco2 %f", ownership_m))

		limit_rts <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity) # Checking RTS bandwidth limit
		if (limit_rts < rb)
		  ownership_m <- 9999999999999

		# NPV deleted
		cown4matrix[i, 4] = -as.numeric(ownership_m)
		# RTS NPV CALCs ENDS   ----------------------------------------------------------------------------

		# SATELLITE NPV CALCs STARTS   ---------------------------------------------------------------------
		#rb <- objects [i,4]
		ownership_s <- 100000000000000
		npv_s <- -100000000000000
		capex_s = 0
		opex_s = 0


		intermediate4 <- list(NumberVSATsets = 0, RequiredCapacity = 0.0)
		intermediate4$RequiredCapacity <- rb

		#print('2_5_impl')
		#print(rb)
		sat_capex <- NULL
		sat_capex = algorithm2_5_impl(input2, intermediate4)
		if (is.null(sat_capex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_own: Cannot calculate CAPEX of satellite for some link"))
		  .GlobalEnv$top_calc_status = 703
		  return(0)
		}


		# Required number of VSAT sets

		NumberVSATsets = as.numeric(sat_capex[2, 2])

		TotalCAPEXSetellite = as.numeric(sat_capex[1, 2])

		#Total cost of VSAT equipment and installation materials
		CostOfVSATEqAndMat = as.numeric(sat_capex[3, 2])

		#Algorithm for determining the total cost of the maintenance of the satellite communication channel


		intermediate4$NumberVSATsets <- NumberVSATsets

		#print('2_6_impl')
		sat_opex <- NULL
		sat_opex = algorithm2_6_impl(input2, intermediate4)
		if (is.null(sat_opex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_own: Cannot calculate OPEX of satellite for some link"))
		  .GlobalEnv$top_calc_status = 702
		  return(0)
		}

		TotalOPEXSatellite = as.numeric(sat_opex[1, 2])

		#print(3)
		ownership_s <- TotalCAPEXSetellite + TotalOPEXSatellite * as.numeric(input$PVOptionSet.PaybackPeriod)
		capex_s = TotalCAPEXSetellite
		opex_s = TotalOPEXSatellite
		#print(sprintf("tco3 %f", ownership_s))

		limit_sat <- as.numeric(input$InitialDataSatellite.MaximumLinkCapacity) # Add check for presense, and move this check up !
		if (limit_sat < rb)
		  ownership_s <- 9999999999999

		cown4matrix[i, 5] = -as.numeric(ownership_s)


		# SATELLITE NPV CALCs ENDS   -----------------------------------------------------------------------

		# CELLULAR NPV CALCs STARTS   ---------------------------------------------------------------------

		ownership_cellular <- 100000000000000
		npv_cellular <- -100000000000000
		capex_cellular = 0
		opex_cellular = 0


		cellstatus <- "2G"

		cellstatus <- objects[i, 6]

		if (!is.na(cellstatus))
		{
		  if (cellstatus == "3G" || cellstatus == "4G")
		  {
			#Algorithm for determining the total cost of the installation and configuration of the cellular communication channel

			intermediate41 <- list(NumberCellularsets = 0, RequiredCapacity = 0.0)
			intermediate41$RequiredCapacity <- rb

			cel_capex <- NULL
			cel_capex = algorithm2_10_impl(input, intermediate41)
			if (is.null(cel_capex))
			{
			  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_own: Cannot calculate CAPEX of cellular for some link"))
			  .GlobalEnv$top_calc_status = 603
			  return(0)
			}


			# Required number of Cellular sets

			NumberCellularsets = as.numeric(cel_capex[2, 2])

			TotalCAPEXCellular = as.numeric(cel_capex[1, 2])

			#Total cost of Cellular equipment and installation materials
			CostOfCellularEqAndMat = as.numeric(cel_capex[3, 2])


			#Algorithm for determining the total cost of the maintenance of the Cellular communication channel


			intermediate41$NumberCellularsets <- NumberCellularsets

			cel_opex <- NULL
			cel_opex = algorithm2_11_impl(input, intermediate41)
			if (is.null(cel_opex))
			{
			  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_own: Cannot calculate OPEX of cellular for some link"))
			  .GlobalEnv$top_calc_status = 602
			  return(0)
			}

			TotalOPEXCellular = as.numeric(cel_opex[1, 2])

			#print(4)
			ownership_cellular <- TotalCAPEXCellular + TotalOPEXCellular * as.numeric(input$PVOptionSet.PaybackPeriod)
			capex_cellular = TotalCAPEXCellular
			opex_cellular = TotalOPEXCellular
			#print(sprintf("tco4 %f", ownership_cellular))

			# deleted NPV
			limit_cell <- as.numeric(input$InitialDataCellular.MaximumLinkCapacity) # Checking bw-limit for Cellular
			if (limit_cell < rb)
			  ownership_cellular <- 9999999999999

			cown4matrix[i, 6] = -as.numeric(ownership_cellular) # Tricky part, checked -100000000000000000
		  }
		  else
			cown4matrix[i, 6] = -as.numeric(ownership_cellular)        # for 2G
		}
		else
		  cellstatus <- "2G"


		# CELLULAR NPV CALCs ENDS   -----------------------------------------------------------------------

	  } # Checking curnode for BNode ENDS -----------------------------------------------------------------

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

	} # NPV MAIN CYCLE ENDS -------------------------------------------------------------------------------


  } # numobj checking block ends


  #print(cown4matrix)
  result <- cown4matrix
  #result <- obj_plus_DTBN[1:100,]
  #result <- obj_plus_DTBN[1,]

  return(result)
}

# End of calc_4x_matrix_cown

calc_4x_matrix_router <- function(input, objects, bnindex, metric, intermediate = NULL)
{

  #cat(sprintf('Routing calc_4x_matrix to: %d',metric))

  if (metric == 0)
	result <- calc_4x_matrix(input, objects, bnindex, intermediate)

  if (metric == 1)
	result <- calc_4x_matrix_simple(input, objects, bnindex, intermediate)

  if (metric == 2)
	result <- calc_4x_matrix_costless(input, objects, bnindex, intermediate)

  return(result)
}

calc_4x_matrix_router_cown <- function(input, objects, bnindex, metric, intermediate = NULL)
{

  #cat(sprintf('Routing calc_4x_matrix to: %d',metric))

  if (metric == 0)
	result <- calc_4x_matrix_cown(input, objects, bnindex, intermediate)

  if (metric == 1)
	result <- calc_4x_matrix_simple_cown(input, objects, bnindex, intermediate)

  if (metric == 2)
	result <- calc_4x_matrix_costless(input, objects, bnindex, intermediate)

  return(result)
}

# Calculate addon for internet as serice at BaseNode

calc_internet_addon_cown <- function(input, FullRequiredBandwidth = 0.0) # from obj1 to obj2
{
  req(input)

  cown_addon <- -1 *
	as.numeric(FullRequiredBandwidth) *
	as.numeric(input$InitialDataFOCL.AnnualRentBand) *
	as.numeric(input$PVOptionSet.PaybackPeriod)
  return(cown_addon)
}


calc_internet_addon_npv <- function(input, FullRequiredBandwidth = 0.0, intermediate = NULL) # from obj1 to obj2
{
  req(input)
  input2 <- input

  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)

  TotalInvest <- 0.0
  CostOfOperation <- FullRequiredBandwidth *
	as.numeric(input$InitialDataFOCL.AnnualRentBand)
  CostOfEquipmentAndMaterials <- 0.0

  intermediate5$TotalInvest <- TotalInvest
  intermediate5$CostOfOperation <- CostOfOperation
  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
  intermediate5$NetIncome <- 0.0
  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

  npv_addon <- NULL
  npv_addon <- algorithm2_7_impl(input2, intermediate5)

  if (is.null(npv_addon))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate NPV addon"))
	.GlobalEnv$top_calc_status = 506
	return(0)
  }
  return(as.numeric(npv_addon[1, 2]))
}


#Calculation of FOCL and RTS NPV values for a pair of connected objects (obj1 -> obj2)

calc_npv_pair <- function(input, obj1, obj2, focl_pos = 1, rts_pos = 1, intermediate = NULL) # from obj1 to obj2
{

  req(input)
  #req (objects)
  req(obj1)
  req(obj2)


  # numberofobj <- nrow(objects)
  # if (numberofobj != 2)
  # {
  # .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
  # .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV for pair. Incorrect number of objects."))
  # .GlobalEnv$top_calc_status  = 901
  # return (0)
  # }

  #browser()
  npv2matrix <- matrix(nrow = 1, ncol = 6, 0) # row = 3 for NPV FOCL, row = 4 NPV for RTS
  #BN <- objects[input$Intermediate.BNIndex,]
  #BNLon = BN[2]
  #BNLat = BN[3]

  npv2matrix[1, 2] = as.numeric(obj1[[4]]) # 2nd-row is RBs

  dist_pair <- distm(c(as.numeric(obj1[[2]]), as.numeric(obj1[[3]])), c(as.numeric(obj2[[2]]), as.numeric(obj2[[3]])), fun = distHaversine)
  npv2matrix[1, 1] <- round(as.numeric(dist_pair[1] / 1000), digits = 2) # 1st-row is distance to BN

  if (input$use_stub) {
	npv2matrix[1, 3] <- -100 * npv2matrix[1, 1] # npv_focl
	npv2matrix[1, 4] <- -200 * npv2matrix[1, 1] # npv_rts
	npv2matrix[1, 5] <- 1 # best tech
	npv2matrix[1, 6] <- -100 * npv2matrix[1, 1] # best npv

	return(npv2matrix)
  }

  npv2matrix[1, 3] <- 0 # npv_focl
  npv2matrix[1, 4] <- 0 # npv_rts
  npv2matrix[1, 5] <- 0 # best tech
  npv2matrix[1, 6] <- 0 # best npv

  # npv2matrix [5,1] <- 100000000000000 # npv_Satellite
  # npv2matrix [6,1] <- 100000000000000 # npv_cellular

  #Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
  input2 <- input

  if (is.reactivevalues(input))
	input2 <- reactiveValuesToList(input)

  bCalcNPV = T

  distancekm <- as.numeric(npv2matrix[1, 1])
  rb <- as.numeric(npv2matrix[1, 2])

  input2$SchoolSpecificData.Length <- distancekm
  input2$SchoolSpecificData.RequiredBandwidth <- rb

  # FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
  #Coefficient should be between 0 and 1
  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km
  distancekmbyroads <- round(distancekm * (1 + TopCoeff), digits = 2)

  input2$SchoolSpecificData.Length <- distancekmbyroads

  #Algorithm for overall FOCL construction cost evaluation

  focl_capex <- NULL
  focl_capex = algorithm2_1_impl(input2)
  if (is.null(focl_capex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate overall FOCL construction cost for some link"))
	.GlobalEnv$top_calc_status = 408
	return(0)
  }

  #Overall cost of FOCL installation
  TotalCAPEXFOCL = as.numeric(focl_capex[1, 2])

  #Overall length of the FOCL construction site
  FOCLLenghtTotal = as.numeric(focl_capex[2, 2])

  FOCLSectionLengthCD = as.numeric(focl_capex[3, 2])

  #�ost of equipment and materials for the construction of fiber optic lines
  CostOfEqAndMatFOCL = as.numeric(focl_capex[4, 2])


  #Algorithm for overall FOCL maintenance cost evaluation
  intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
  intermediate51$RequiredCapacity <- rb
  intermediate51$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate51$Technology <- 0


  intermediate2 <- list(FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
  intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
  intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate2$RequiredCapacity <- rb

  focl_opex <- NULL
  focl_opex = algorithm2_2_impl(input2, intermediate2)
  if (is.null(focl_opex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate overall FOCL maintenance cost for some link"))
	.GlobalEnv$top_calc_status = 407
	return(0)
  }

  #Total cost for FOCL maintenance for the entire period of operation
  TotalOPEXFOCL = as.numeric(focl_opex[1, 2])


  #Cost of ownership for the period
  ownership_f = TotalCAPEXFOCL + TotalOPEXFOCL * as.numeric(input$PVOptionSet.PaybackPeriod)
  capex_f = TotalCAPEXFOCL
  opex_f = TotalOPEXFOCL


  if (bCalcNPV)
  {
	focl_income <- NULL
	focl_income = algorithm2_12_impl(input2, intermediate51)
	if (is.null(focl_income))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate potential income from FOCL for some link"))
	  .GlobalEnv$top_calc_status = 406
	  return(0)
	}


	NetIncome <- as.numeric(focl_income[1, 2])
	income_f = NetIncome

	#NPV - FOCL
	intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
	TotalInvest <- TotalCAPEXFOCL
	CostOfOperation <- TotalOPEXFOCL
	CostOfEquipmentAndMaterials <- CostOfEqAndMatFOCL
	intermediate5$TotalInvest <- TotalInvest
	intermediate5$CostOfOperation <- CostOfOperation
	intermediate5$NetIncome <- NetIncome
	intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
	intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

	npv_focl <- NULL
	npv_focl <- algorithm2_7_impl(input2, intermediate5)

	if (is.null(npv_focl))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate NPV of FOCL for some link"))
	  .GlobalEnv$top_calc_status = 405
	  return(0)
	}

	npv_f = as.numeric(npv_focl[1, 2])
	npv2matrix[1, 3] = npv_f
  }
  # FOCL NPV CALCs ENDS -----------------------------------------------------------------------------

  # RTS NPV CALCs STARTS ----------------------------------------------------------------------------
  input2$SchoolSpecificData.Length <- distancekm #Back to the regular distance

  #Algorithm for total RTS construction cost evaluation between object and SN in locality
  rts_capex <- NULL
  rts_capex = algorithm2_3_impl(input2, intermediate)
  if (is.null(rts_capex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate total RTS construction cost for some link"))
	.GlobalEnv$top_calc_status = 508
	return(0)
  }


  TotalCAPEXRTS <- as.numeric(rts_capex[1, 2])

  NumberOfRepeaters <- as.numeric(rts_capex[2, 2])

  #Total cost of equipment and materials for the construction of the RTS
  CostOfEqAndMatRTS <- as.numeric(rts_capex[3, 2])

  WidthFrequencyChannel <- as.numeric(rts_capex[4, 2])


  #Algorithm for total RTS maintenance cost evaluation between object and SN in locality

  intermediate3 <- list(NumberOfRepeaters = 0, RequiredCapacity = 0.0, NumberOfTerminals = 0.0, WidthFrequencyChannel = 0.0)
  intermediate3$NumberOfRepeaters <- NumberOfRepeaters
  intermediate3$RequiredCapacity <- rb
  intermediate3$NumberOfTerminals <- 2
  intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel


  if (TotalCAPEXRTS == 0)
  {
	intermediate3$NumberOfTerminals <- 0
  }

  rts_opex <- NULL
  rts_opex = algorithm2_4_impl(input2, intermediate3)

  if (is.null(rts_opex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate total RTS maintenance cost for some link"))
	.GlobalEnv$top_calc_status = 507
	return(0)
  }

  TotalOPEXRTS = as.numeric(rts_opex[1, 2])


  #Cost of ownership for the period
  ownership_m <- TotalCAPEXRTS + TotalOPEXRTS * as.numeric(input$PVOptionSet.PaybackPeriod)
  capex_m = TotalCAPEXRTS
  opex_m = TotalOPEXRTS


  if (bCalcNPV)
  {
	#NPV - Microwave

	intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
	intermediate51$RequiredCapacity <- rb
	intermediate51$Technology <- 1

	rts_income <- NULL
	rts_income = algorithm2_12_impl(input2, intermediate51)
	if (is.null(rts_income))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate potential income from RTS for some link"))
	  .GlobalEnv$top_calc_status = 506
	  return(0)
	}


	NetIncome <- as.numeric(rts_income[1, 2])
	income_m = NetIncome

	intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)

	TotalInvest <- TotalCAPEXRTS
	CostOfOperation <- TotalOPEXRTS
	CostOfEquipmentAndMaterials <- CostOfEqAndMatRTS

	intermediate5$TotalInvest <- TotalInvest
	intermediate5$CostOfOperation <- CostOfOperation
	intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
	intermediate5$NetIncome <- NetIncome
	intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

	npv_microwave <- NULL
	npv_microwave <- algorithm2_7_impl(input2, intermediate5)
	if (is.null(npv_microwave))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate NPV for RTS for some link"))
	  .GlobalEnv$top_calc_status = 505
	  return(0)
	}

	npv_m <- as.numeric(npv_microwave[1, 2])
	npv2matrix[1, 4] = npv_m

  }
  # RTS NPV CALCs ENDS   ----------------------------------------------------------------------------

  # Change this approach for production
  if (focl_pos == 0) npv_f = -99999999999
  if (rts_pos == 0) npv_m = -99999999999

  if (npv_f >= npv_m)
  {
	npv2matrix[1, 5] = 1
	npv2matrix[1, 6] = npv_f
  }
  else
  {
	npv2matrix[1, 5] = 2
	npv2matrix[1, 6] = npv_m
  }
  #print(npv2matrix)
  result <- npv2matrix

  return(result)
}

calc_final_dist <- function(input, obj1, obj2, intermediate = NULL) # from obj1 to obj2
{

  req(input)
  #req (objects)
  req(obj1)
  req(obj2)

  dist_pair <- distm(c(as.numeric(obj1[[2]]), as.numeric(obj1[[3]])), c(as.numeric(obj2[[2]]), as.numeric(obj2[[3]])), fun = distHaversine) # distance in meters
  dist_pair_km <- round(as.numeric(dist_pair[1] / 1000), digits = 2) # distance in km

  return(dist_pair_km)

}

calc_cown_pair <- function(input, obj1, obj2, focl_pos = 1, rts_pos = 1, intermediate = NULL) # from obj1 to obj2
{

  req(input)
  #req (objects)
  req(obj1)
  req(obj2)


  # numberofobj <- nrow(objects)
  # if (numberofobj != 2)
  # {
  # .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
  # .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV for pair. Incorrect number of objects."))
  # .GlobalEnv$top_calc_status  = 901
  # return (0)
  # }

  #browser()
  cown2matrix <- matrix(nrow = 1, ncol = 6, 0) # row = 3 for NPV FOCL, row = 4 NPV for RTS
  #BN <- objects[input$Intermediate.BNIndex,]
  #BNLon = BN[2]
  #BNLat = BN[3]

  cown2matrix[1, 2] = as.numeric(obj1[[4]]) # 2nd-row is RBs

  dist_pair <- distm(c(as.numeric(obj1[[2]]), as.numeric(obj1[[3]])), c(as.numeric(obj2[[2]]), as.numeric(obj2[[3]])), fun = distHaversine)
  cown2matrix[1, 1] <- round(as.numeric(dist_pair[1] / 1000), digits = 2) # 1st-row is distance to BN

  if (input$use_stub) {
	cown2matrix[1, 3] <- -100 * cown2matrix[1, 1] # npv_focl
	cown2matrix[1, 4] <- -200 * cown2matrix[1, 1] # npv_rts
	cown2matrix[1, 5] <- 1 # best tech
	cown2matrix[1, 6] <- -100 * cown2matrix[1, 1] # best cown

	return(npv2matrix)
  }

  cown2matrix[1, 3] <- 0 # npv_focl
  cown2matrix[1, 4] <- 0 # npv_rts
  cown2matrix[1, 5] <- 0 # best tech
  cown2matrix[1, 6] <- 0 # best npv

  # cown2matrix [5,1] <- 100000000000000 # npv_Satellite
  # cown2matrix [6,1] <- 100000000000000 # npv_cellular

  #Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
  input2 <- input

  if (is.reactivevalues(input))
	input2 <- reactiveValuesToList(input)

  bCalcNPV = T

  distancekm <- as.numeric(cown2matrix[1, 1])
  rb <- as.numeric(cown2matrix[1, 2])

  input2$SchoolSpecificData.Length <- distancekm
  input2$SchoolSpecificData.RequiredBandwidth <- rb

  # FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
  #Coefficient should be between 0 and 1
  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km
  distancekmbyroads <- round(distancekm * (1 + TopCoeff), digits = 2)

  input2$SchoolSpecificData.Length <- distancekmbyroads

  #Algorithm for overall FOCL construction cost evaluation

  focl_capex <- NULL
  focl_capex = algorithm2_1_impl(input2)
  if (is.null(focl_capex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_cown_pair: Cannot calculate overall FOCL construction cost for some link"))
	.GlobalEnv$top_calc_status = 408
	return(0)
  }

  #Overall cost of FOCL installation
  TotalCAPEXFOCL = as.numeric(focl_capex[1, 2])

  #Overall length of the FOCL construction site
  FOCLLenghtTotal = as.numeric(focl_capex[2, 2])

  FOCLSectionLengthCD = as.numeric(focl_capex[3, 2])

  #Сost of equipment and materials for the construction of fiber optic lines
  CostOfEqAndMatFOCL = as.numeric(focl_capex[4, 2])


  #Algorithm for overall FOCL maintenance cost evaluation
  intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
  intermediate51$RequiredCapacity <- rb
  intermediate51$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate51$Technology <- 0


  intermediate2 <- list(FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
  intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
  intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate2$RequiredCapacity <- rb

  focl_opex <- NULL
  focl_opex = algorithm2_2_impl(input2, intermediate2)
  if (is.null(focl_opex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_cown_pair: Cannot calculate overall FOCL maintenance cost for some link"))
	.GlobalEnv$top_calc_status = 407
	return(0)
  }

  #Total cost for FOCL maintenance for the entire period of operation
  TotalOPEXFOCL = as.numeric(focl_opex[1, 2])


  #Cost of ownership for the period
  ownership_f = TotalCAPEXFOCL + TotalOPEXFOCL * as.numeric(input$PVOptionSet.PaybackPeriod)
  capex_f = TotalCAPEXFOCL
  opex_f = TotalOPEXFOCL

  cown2matrix[1, 3] = -as.numeric(ownership_f)
  # FOCL NPV CALCs ENDS -----------------------------------------------------------------------------

  # RTS NPV CALCs STARTS ----------------------------------------------------------------------------
  input2$SchoolSpecificData.Length <- distancekm #Back to the regular distance

  #Algorithm for total RTS construction cost evaluation between object and SN in locality
  rts_capex <- NULL
  rts_capex = algorithm2_3_impl(input2, intermediate)
  if (is.null(rts_capex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_cown_pair: Cannot calculate total RTS construction cost for some link"))
	.GlobalEnv$top_calc_status = 508
	return(0)
  }


  TotalCAPEXRTS <- as.numeric(rts_capex[1, 2])

  NumberOfRepeaters <- as.numeric(rts_capex[2, 2])

  #Total cost of equipment and materials for the construction of the RTS
  CostOfEqAndMatRTS <- as.numeric(rts_capex[3, 2])

  WidthFrequencyChannel <- as.numeric(rts_capex[4, 2])


  #Algorithm for total RTS maintenance cost evaluation between object and SN in locality

  intermediate3 <- list(NumberOfRepeaters = 0, RequiredCapacity = 0.0, NumberOfTerminals = 0.0, WidthFrequencyChannel = 0.0)
  intermediate3$NumberOfRepeaters <- NumberOfRepeaters
  intermediate3$RequiredCapacity <- rb
  intermediate3$NumberOfTerminals <- 2
  intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel


  if (TotalCAPEXRTS == 0)
  {
	intermediate3$NumberOfTerminals <- 0
  }

  rts_opex <- NULL
  rts_opex = algorithm2_4_impl(input2, intermediate3)

  if (is.null(rts_opex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_cown_pair: Cannot calculate total RTS maintenance cost for some link"))
	.GlobalEnv$top_calc_status = 507
	return(0)
  }

  TotalOPEXRTS = as.numeric(rts_opex[1, 2])


  #Cost of ownership for the period
  ownership_m <- TotalCAPEXRTS + TotalOPEXRTS * as.numeric(input$PVOptionSet.PaybackPeriod)
  capex_m = TotalCAPEXRTS
  opex_m = TotalOPEXRTS

  cown2matrix[1, 4] = -as.numeric(ownership_m)
  # RTS NPV CALCs ENDS   ----------------------------------------------------------------------------

  # Change this approach for production
  if (focl_pos == 0) {
	ownership_f <- 99999999999
	cown2matrix[1, 3] <- -as.numeric(ownership_f)
  }
  if (rts_pos == 0) {
	ownership_m <- 99999999999
	cown2matrix[1, 4] <- -as.numeric(ownership_m)
  }

  # New check for RTS limit
  #tco_rts_bcp <- tco_rts
  limit_rts <- as.numeric(input$InitialDataRadio.MaximumLinkCapacity) # Check RTS bandwidth limit
  if (limit_rts < rb) {
	if (focl_pos == 1) {
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Topology correctness warning: RTS Bandwidth restrictions are too strong !"))
	  #cat("Topology correctness warning: Bandwidth restrictions are too strong !", file = 'opt-warn.txt', APPEND = TRUE)
	  cat("Topology correctness warning: Bandwidth restrictions are too strong !", sep = "\n", file = 'opt-warn.txt', APPEND = TRUE)

	} else {
	  ownership_m <- 9999999999999
	}
  }

  if (ownership_f < ownership_m)
  {
	cown2matrix[1, 5] = 1
	cown2matrix[1, 6] = -as.numeric(ownership_f)
  }
  else
  {
	cown2matrix[1, 5] = 2
	cown2matrix[1, 6] = -as.numeric(ownership_m)
  }
  #print(cown2matrix)
  result <- cown2matrix

  return(result)
}

# Router for different metrics for calc_npv_pair

calc_npv_pair_router <- function(input, obj1, obj2, focl_pos = 1, rts_pos = 1, metric, intermediate = NULL) # from obj1 to obj2
{

  #print(sprintf('Routing npv_pair to: %d',metric))

  if (metric == 0)
	result <- calc_npv_pair(input, obj1, obj2, focl_pos, rts_pos, intermediate)

  if (metric == 1)
	result <- calc_npv_pair_simple(input, obj1, obj2, focl_pos, rts_pos, intermediate)

  if (metric == 2)
	result <- calc_npv_pair_costless(input, obj1, obj2, focl_pos, rts_pos, intermediate)

  return(result)
}

# Router for different metrics for calc_cown_pair

calc_cown_pair_router <- function(input, obj1, obj2, focl_pos = 1, rts_pos = 1, metric, intermediate = NULL) # from obj1 to obj2
{

  # print(sprintf('Routing cown_pair to: %d',metric))

  if (metric == 0)
	result <- calc_cown_pair(input, obj1, obj2, focl_pos, rts_pos, intermediate)

  if (metric == 1)
	result <- calc_cown_pair_simple(input, obj1, obj2, focl_pos, rts_pos, intermediate)

  if (metric == 2)
	#result <- calc_cown_pair_costless(input, obj1, obj2, focl_pos, rts_pos, intermediate)
	result <- calc_npv_pair_costless(input, obj1, obj2, focl_pos, rts_pos, intermediate)

  return(result)
}

#BaseNode Determination

find_basenode <- function(input, objects, intermediate = NULL)
{
  numberofobj <- nrow(objects)

  snodes_candidates_ind <- c()
  #min_dtf = 99999999
  if (numberofobj == 1) {
	snodes_candidates_ind <- append(snodes_candidates_ind, 1)
	BaseNode = objects[snodes_candidates_ind[1],]
	BaseNode$ind = as.integer(snodes_candidates_ind[1])
	return(BaseNode)
  }

  if (numberofobj == 2) {
	dtf_1 <- objects[1, 5]
	dtf_2 <- objects[2, 5]

	if (dtf_1 < dtf_2) # modify to account bw, nearest to center
	  snodes_candidates_ind <- append(1, 1)
	else
	  snodes_candidates_ind <- append(2, 1)

	BaseNode = objects[snodes_candidates_ind[1],]
	BaseNode$ind = as.integer(snodes_candidates_ind[1])
	return(BaseNode)
  }


  if (numberofobj > 1)
  {
	for (i in 1:numberofobj)
	{
	  dtf <- objects[i, 5]

	  if (dtf <= input$TopologySettings.SNDTFThreshold)
		snodes_candidates_ind <- append(snodes_candidates_ind, i)

	}

  }

  #print(length(snodes_candidates_ind))

  if (length(snodes_candidates_ind) == 1)
  {
	#BaseNode = objects [i,]
	BaseNode = objects[snodes_candidates_ind[1],]
	BaseNode$ind = as.integer(snodes_candidates_ind[1])
	#print (BaseNode)
	return(BaseNode)
  }

  longs <- pull(objects[, 2])
  lats <- pull(objects[, 3])
  weight_bandw <- pull(objects[, 4])
  cog_result <- COGravity(longs, lats, , weight_bandw)

  cog_result_long <- cog_result[1]
  cog_result_lat <- cog_result[3]

  if (length(snodes_candidates_ind) == 0)
  {
	min_dist_m = 99999999
	BaseNode_ind <- 0
	if (numberofobj > 0)
	{
	  for (i in 1:numberofobj)
	  {
		cur_dist_m <- distm(c(as.numeric(objects[i, 2]), as.numeric(objects[i, 3])), c(as.numeric(cog_result_long), as.numeric(cog_result_lat)), fun = distHaversine)[1]
		if (cur_dist_m < min_dist_m)
		{
		  BaseNode_ind <- i
		  min_dist_m <- cur_dist_m
		}

	  }

	  BaseNode = objects[BaseNode_ind,]
	  BaseNode$ind = as.integer(BaseNode_ind)
	  #print (BaseNode)
	  return(BaseNode)
	}
  }
  else {
	min_dist_m = 99999999
	BaseNode_ind <- 0
	for (i in 1:length(snodes_candidates_ind))
	{
	  cur_dist_m <- distm(c(as.numeric(objects[snodes_candidates_ind[i], 2]), as.numeric(objects[snodes_candidates_ind[i], 3])), c(as.numeric(cog_result_long), as.numeric(cog_result_lat)), fun = distHaversine)
	  if (cur_dist_m < min_dist_m)
	  {
		BaseNode_ind <- as.integer(snodes_candidates_ind[i])
		min_dist_m <- cur_dist_m
	  }
	}

	BaseNode = objects[BaseNode_ind,]
	BaseNode$ind = as.integer(BaseNode_ind)
	#print (BaseNode)
	return(BaseNode)

  }
}

calc_npv_tech <- function(input, obj1, obj2, focl_pos = 1, rts_pos = 1, intermediate = NULL) # from obj1 to obj2
{

  req(input)
  #req (objects)
  req(obj1)
  req(obj2)


  # numberofobj <- nrow(objects)
  # if (numberofobj != 2)
  # {
  # .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
  # .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV for pair. Incorrect number of objects."))
  # .GlobalEnv$top_calc_status  = 901
  # return (0)
  # }

  #browser()
  npv2matrix <- matrix(nrow = 1, ncol = 6, 0) # row = 3 for FOCL, row = 4 for RTS
  #BN <- objects[input$Intermediate.BNIndex,]
  #BNLon = BN[2]
  #BNLat = BN[3]

  npv2matrix[1, 2] = as.numeric(obj1[[4]]) # 2nd-row is RBs

  dist_pair <- distm(c(as.numeric(obj1[[2]]), as.numeric(obj1[[3]])), c(as.numeric(obj2[[2]]), as.numeric(obj2[[3]])), fun = distHaversine)
  npv2matrix[1, 1] <- round(as.numeric(dist_pair[1] / 1000), digits = 2) # 1st-row is distance to BN

  npv2matrix[1, 3] <- 0 # npv_focl
  npv2matrix[1, 4] <- 0 # npv_rts
  npv2matrix[1, 5] <- 0 # best tech
  npv2matrix[1, 6] <- 0 # best npv

  # npv2matrix [5,1] <- 100000000000000 # npv_Satellite
  # npv2matrix [6,1] <- 100000000000000 # npv_cellular

  #Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
  input2 <- input

  if (is.reactivevalues(input))
	input2 <- reactiveValuesToList(input)

  bCalcNPV = T

  distancekm <- as.numeric(npv2matrix[1, 1])
  rb <- as.numeric(npv2matrix[1, 2])

  input2$SchoolSpecificData.Length <- distancekm
  input2$SchoolSpecificData.RequiredBandwidth <- rb

  # FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
  #Coefficient should be between 0 and 1
  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km
  distancekmbyroads <- round(distancekm * (1 + TopCoeff), digits = 2)

  input2$SchoolSpecificData.Length <- distancekmbyroads

  #Algorithm for overall FOCL construction cost evaluation

  focl_capex <- NULL
  focl_capex = algorithm2_1_impl(input2)
  if (is.null(focl_capex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate overall FOCL construction cost for some link"))
	.GlobalEnv$top_calc_status = 408
	return(0)
  }

  #Overall cost of FOCL installation
  TotalCAPEXFOCL = as.numeric(focl_capex[1, 2])

  #Overall length of the FOCL construction site
  FOCLLenghtTotal = as.numeric(focl_capex[2, 2])

  FOCLSectionLengthCD = as.numeric(focl_capex[3, 2])

  #�ost of equipment and materials for the construction of fiber optic lines
  CostOfEqAndMatFOCL = as.numeric(focl_capex[4, 2])


  #Algorithm for overall FOCL maintenance cost evaluation
  intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
  intermediate51$RequiredCapacity <- rb
  intermediate51$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate51$Technology <- 0


  intermediate2 <- list(FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
  intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
  intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate2$RequiredCapacity <- rb

  focl_opex <- NULL
  focl_opex = algorithm2_2_impl(input2, intermediate2)
  if (is.null(focl_opex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate overall FOCL maintenance cost for some link"))
	.GlobalEnv$top_calc_status = 407
	return(0)
  }

  #Total cost for FOCL maintenance for the entire period of operation
  TotalOPEXFOCL = as.numeric(focl_opex[1, 2])


  #Cost of ownership for the period
  ownership_f = TotalCAPEXFOCL + TotalOPEXFOCL * as.numeric(input$PVOptionSet.PaybackPeriod)
  capex_f = TotalCAPEXFOCL
  opex_f = TotalOPEXFOCL


  if (bCalcNPV)
  {
	focl_income <- NULL
	focl_income = algorithm2_12_impl(input2, intermediate51)
	if (is.null(focl_income))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate potential income from FOCL for some link"))
	  .GlobalEnv$top_calc_status = 406
	  return(0)
	}


	NetIncome <- as.numeric(focl_income[1, 2])
	income_f = NetIncome

	#NPV - FOCL
	intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
	TotalInvest <- TotalCAPEXFOCL
	CostOfOperation <- TotalOPEXFOCL
	CostOfEquipmentAndMaterials <- CostOfEqAndMatFOCL
	intermediate5$TotalInvest <- TotalInvest
	intermediate5$CostOfOperation <- CostOfOperation
	intermediate5$NetIncome <- NetIncome
	intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
	intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

	npv_focl <- NULL
	npv_focl <- algorithm2_7_impl(input2, intermediate5)

	if (is.null(npv_focl))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV of FOCL for some link"))
	  .GlobalEnv$top_calc_status = 405
	  return(0)
	}

	npv_f = as.numeric(npv_focl[1, 2])
	npv2matrix[1, 3] = npv_f
  }
  # FOCL NPV CALCs ENDS -----------------------------------------------------------------------------

  # RTS NPV CALCs STARTS ----------------------------------------------------------------------------
  input2$SchoolSpecificData.Length <- distancekm #Back to the regular distance

  #Algorithm for total RTS construction cost evaluation between object and SN in locality
  rts_capex <- NULL
  rts_capex = algorithm2_3_impl(input2, intermediate)
  if (is.null(rts_capex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate total RTS construction cost for some link"))
	.GlobalEnv$top_calc_status = 508
	return(0)
  }


  TotalCAPEXRTS <- as.numeric(rts_capex[1, 2])

  NumberOfRepeaters <- as.numeric(rts_capex[2, 2])

  #Total cost of equipment and materials for the construction of the RTS
  CostOfEqAndMatRTS <- as.numeric(rts_capex[3, 2])

  WidthFrequencyChannel <- as.numeric(rts_capex[4, 2])


  #Algorithm for total RTS maintenance cost evaluation between object and SN in locality

  intermediate3 <- list(NumberOfRepeaters = 0, RequiredCapacity = 0.0, NumberOfTerminals = 0.0, WidthFrequencyChannel = 0.0)
  intermediate3$NumberOfRepeaters <- NumberOfRepeaters
  intermediate3$RequiredCapacity <- rb
  intermediate3$NumberOfTerminals <- 2
  intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel


  if (TotalCAPEXRTS == 0)
  {
	intermediate3$NumberOfTerminals <- 0
  }

  rts_opex <- NULL
  rts_opex = algorithm2_4_impl(input2, intermediate3)

  if (is.null(rts_opex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate total RTS maintenance cost for some link"))
	.GlobalEnv$top_calc_status = 507
	return(0)
  }

  TotalOPEXRTS = as.numeric(rts_opex[1, 2])


  #Cost of ownership for the period
  ownership_m <- TotalCAPEXRTS + TotalOPEXRTS * as.numeric(input$PVOptionSet.PaybackPeriod)
  capex_m = TotalCAPEXRTS
  opex_m = TotalOPEXRTS


  if (bCalcNPV)
  {
	#NPV - Microwave

	intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
	intermediate51$RequiredCapacity <- rb
	intermediate51$Technology <- 1

	rts_income <- NULL
	rts_income = algorithm2_12_impl(input2, intermediate51)
	if (is.null(rts_income))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate potential income from RTS for some link"))
	  .GlobalEnv$top_calc_status = 506
	  return(0)
	}


	NetIncome <- as.numeric(rts_income[1, 2])
	income_m = NetIncome

	intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)

	TotalInvest <- TotalCAPEXRTS
	CostOfOperation <- TotalOPEXRTS
	CostOfEquipmentAndMaterials <- CostOfEqAndMatRTS

	intermediate5$TotalInvest <- TotalInvest
	intermediate5$CostOfOperation <- CostOfOperation
	intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
	intermediate5$NetIncome <- NetIncome
	intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

	npv_microwave <- NULL
	npv_microwave <- algorithm2_7_impl(input2, intermediate5)
	if (is.null(npv_microwave))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Cannot calculate NPV for RTS for some link"))
	  .GlobalEnv$top_calc_status = 505
	  return(0)
	}

	npv_m <- as.numeric(npv_microwave[1, 2])
	npv2matrix[1, 4] = npv_m

  }
  # RTS NPV CALCs ENDS   ----------------------------------------------------------------------------

  # Change this approach for production
  if (focl_pos == 0) npv_f = -99999999999
  if (rts_pos == 0) npv_m = -99999999999

  if (npv_f >= npv_m)
  {
	npv2matrix[1, 5] = 1
	npv2matrix[1, 6] = npv_f
  }
  else
  {
	npv2matrix[1, 5] = 2
	npv2matrix[1, 6] = npv_m
  }
  #print(npv2matrix)
  result <- npv2matrix

  return(result)
}


calc_npv_est <- function(input, distancekm, rb, intermediate = NULL) {


  #copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
  input2 <- input

  if (is.reactivevalues(input))
	input2 <- reactiveValuesToList(input)

  npv4matrix <- matrix(nrow = 1, ncol = 8, 0)

  npv4matrix[1, 1] <- distancekm
  npv4matrix[1, 2] <- rb

  bCalcNPV = T

  input2$SchoolSpecificData.Length <- distancekm
  input2$SchoolSpecificData.RequiredBandwidth <- rb

  if (T) {

	# FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
	#Coefficient should be between 0 and 1
	TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

	#Distance by the roads in km
	distancekmbyroads <- round(distancekm * (1 + TopCoeff), digits = 2)

	input2$SchoolSpecificData.Length <- distancekmbyroads

	#Algorithm for overall FOCL construction cost evaluation

	focl_capex <- NULL
	focl_capex = algorithm2_1_impl(input2)
	if (is.null(focl_capex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate overall FOCL construction cost for some link"))
	  .GlobalEnv$top_calc_status = 408
	  return(0)
	}

	#Overall cost of FOCL installation
	TotalCAPEXFOCL = as.numeric(focl_capex[1, 2])

	#Overall length of the FOCL construction site
	FOCLLenghtTotal = as.numeric(focl_capex[2, 2])

	FOCLSectionLengthCD = as.numeric(focl_capex[3, 2])

	#�ost of equipment and materials for the construction of fiber optic lines
	CostOfEqAndMatFOCL = as.numeric(focl_capex[4, 2])


	#Algorithm for overall FOCL maintenance cost evaluation
	intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
	intermediate51$RequiredCapacity <- rb
	intermediate51$FOCLSectionLengthCD <- FOCLSectionLengthCD
	intermediate51$Technology <- 0


	intermediate2 <- list(FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
	intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
	intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
	intermediate2$RequiredCapacity <- rb

	focl_opex <- NULL
	focl_opex = algorithm2_2_impl(input2, intermediate2)
	if (is.null(focl_opex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate overall FOCL maintenance cost for some link"))
	  .GlobalEnv$top_calc_status = 407
	  return(0)
	}

	#Total cost for FOCL maintenance for the entire period of operation
	TotalOPEXFOCL = as.numeric(focl_opex[1, 2])


	#Cost of ownership for the period
	ownership_f = TotalCAPEXFOCL + TotalOPEXFOCL * as.numeric(input$PVOptionSet.PaybackPeriod)
	capex_f = TotalCAPEXFOCL
	opex_f = TotalOPEXFOCL


	if (bCalcNPV)
	{
	  focl_income <- NULL
	  focl_income = algorithm2_12_impl(input2, intermediate51)
	  if (is.null(focl_income))
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate potential income from FOCL for some link"))
		.GlobalEnv$top_calc_status = 406
		return(0)
	  }


	  NetIncome <- as.numeric(focl_income[1, 2])
	  income_f = NetIncome

	  #NPV - FOCL
	  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
	  TotalInvest <- TotalCAPEXFOCL
	  CostOfOperation <- TotalOPEXFOCL
	  CostOfEquipmentAndMaterials <- CostOfEqAndMatFOCL
	  intermediate5$TotalInvest <- TotalInvest
	  intermediate5$CostOfOperation <- CostOfOperation
	  intermediate5$NetIncome <- NetIncome
	  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
	  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

	  npv_focl <- NULL
	  npv_focl <- algorithm2_7_impl(input2, intermediate5)

	  if (is.null(npv_focl))
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV of FOCL for some link"))
		.GlobalEnv$top_calc_status = 405
		return(0)
	  }

	  npv_f = as.numeric(npv_focl[1, 2])
	  npv4matrix[1, 3] = npv_f
	}
	# FOCL NPV CALCs ENDS -----------------------------------------------------------------------------

	# RTS NPV CALCs STARTS ----------------------------------------------------------------------------
	input2$SchoolSpecificData.Length <- distancekm #Back to the regular distance

	#Algorithm for total RTS construction cost evaluation between object and SN in locality
	rts_capex <- NULL
	rts_capex = algorithm2_3_impl(input2, intermediate)
	if (is.null(rts_capex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate total RTS construction cost for some link"))
	  .GlobalEnv$top_calc_status = 508
	  return(0)
	}


	TotalCAPEXRTS <- as.numeric(rts_capex[1, 2])

	NumberOfRepeaters <- as.numeric(rts_capex[2, 2])

	#Total cost of equipment and materials for the construction of the RTS
	CostOfEqAndMatRTS <- as.numeric(rts_capex[3, 2])

	WidthFrequencyChannel <- as.numeric(rts_capex[4, 2])


	#Algorithm for total RTS maintenance cost evaluation between object and SN in locality

	intermediate3 <- list(NumberOfRepeaters = 0, RequiredCapacity = 0.0, NumberOfTerminals = 0.0, WidthFrequencyChannel = 0.0)
	intermediate3$NumberOfRepeaters <- NumberOfRepeaters
	intermediate3$RequiredCapacity <- rb
	intermediate3$NumberOfTerminals <- 2
	intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel


	if (TotalCAPEXRTS == 0)
	{
	  intermediate3$NumberOfTerminals <- 0
	}

	rts_opex <- NULL
	rts_opex = algorithm2_4_impl(input2, intermediate3)

	if (is.null(rts_opex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate total RTS maintenance cost for some link"))
	  .GlobalEnv$top_calc_status = 507
	  return(0)
	}

	TotalOPEXRTS = as.numeric(rts_opex[1, 2])


	#Cost of ownership for the period
	ownership_m <- TotalCAPEXRTS + TotalOPEXRTS * as.numeric(input$PVOptionSet.PaybackPeriod)
	capex_m = TotalCAPEXRTS
	opex_m = TotalOPEXRTS


	if (bCalcNPV)
	{
	  #NPV - Microwave

	  intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
	  intermediate51$RequiredCapacity <- rb
	  intermediate51$Technology <- 1

	  rts_income <- NULL
	  rts_income = algorithm2_12_impl(input2, intermediate51)
	  if (is.null(rts_income))
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate potential income from RTS for some link"))
		.GlobalEnv$top_calc_status = 506
		return(0)
	  }


	  NetIncome <- as.numeric(rts_income[1, 2])
	  income_m = NetIncome

	  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)

	  TotalInvest <- TotalCAPEXRTS
	  CostOfOperation <- TotalOPEXRTS
	  CostOfEquipmentAndMaterials <- CostOfEqAndMatRTS

	  intermediate5$TotalInvest <- TotalInvest
	  intermediate5$CostOfOperation <- CostOfOperation
	  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
	  intermediate5$NetIncome <- NetIncome
	  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

	  npv_microwave <- NULL
	  npv_microwave <- algorithm2_7_impl(input2, intermediate5)
	  if (is.null(npv_microwave))
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV for RTS for some link"))
		.GlobalEnv$top_calc_status = 505
		return(0)
	  }

	  npv_m <- as.numeric(npv_microwave[1, 2])
	  npv4matrix[1, 4] = npv_m

	}
	# RTS NPV CALCs ENDS   ----------------------------------------------------------------------------

	# SATELLITE NPV CALCs STARTS   ---------------------------------------------------------------------
	#rb <- objects [i,4]
	ownership_s <- 100000000000000
	npv_s <- -100000000000000
	capex_s = 0
	opex_s = 0


	intermediate4 <- list(NumberVSATsets = 0, RequiredCapacity = 0.0)
	intermediate4$RequiredCapacity <- rb

	sat_capex <- NULL
	sat_capex = algorithm2_5_impl(input2, intermediate4)
	if (is.null(sat_capex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate CAPEX of satellite for some link"))
	  .GlobalEnv$top_calc_status = 703
	  return(0)
	}


	# Required number of VSAT sets

	NumberVSATsets = as.numeric(sat_capex[2, 2])

	TotalCAPEXSetellite = as.numeric(sat_capex[1, 2])

	#Total cost of VSAT equipment and installation materials
	CostOfVSATEqAndMat = as.numeric(sat_capex[3, 2])

	#Algorithm for determining the total cost of the maintenance of the satellite communication channel


	intermediate4$NumberVSATsets <- NumberVSATsets


	sat_opex <- NULL
	sat_opex = algorithm2_6_impl(input2, intermediate4)
	if (is.null(sat_opex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate OPEX of satellite for some link"))
	  .GlobalEnv$top_calc_status = 702
	  return(0)
	}

	TotalOPEXSatellite = as.numeric(sat_opex[1, 2])


	ownership_s <- TotalCAPEXSetellite + TotalOPEXSatellite * as.numeric(input$PVOptionSet.PaybackPeriod)
	capex_s = TotalCAPEXSetellite
	opex_s = TotalOPEXSatellite


	if (bCalcNPV)
	{
	  #NPV - Satellite
	  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
	  TotalInvest <- TotalCAPEXSetellite
	  CostOfOperation <- TotalOPEXSatellite
	  CostOfEquipmentAndMaterials <- CostOfVSATEqAndMat

	  intermediate5$NetIncome <- 0
	  intermediate5$TotalInvest <- TotalInvest
	  intermediate5$CostOfOperation <- CostOfOperation
	  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
	  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

	  npv_satellite <- NULL
	  npv_satellite <- algorithm2_7_impl(input2, intermediate5)
	  if (is.null(npv_satellite))
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV of satellite for some link"))
		.GlobalEnv$top_calc_status = 701
		return(0)
	  }

	  npv_s <- as.numeric(npv_satellite[1, 2])
	  npv4matrix[1, 5] = npv_s
	}


	# SATELLITE NPV CALCs ENDS   -----------------------------------------------------------------------

	# CELLULAR NPV CALCs STARTS   ---------------------------------------------------------------------

	ownership_cellular <- 100000000000000
	npv_cellular <- -100000000000000
	capex_cellular = 0
	opex_cellular = 0


	cellstatus <- "4G"

	#cellstatus <- objects[1, 6]

	if (!is.na(cellstatus))
	{
	  if (cellstatus == "3G" || cellstatus == "4G")
	  {
		#Algorithm for determining the total cost of the installation and configuration of the cellular communication channel

		intermediate41 <- list(NumberCellularsets = 0, RequiredCapacity = 0.0)
		intermediate41$RequiredCapacity <- rb

		cel_capex <- NULL
		cel_capex = algorithm2_10_impl(input, intermediate41)
		if (is.null(cel_capex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate CAPEX of cellular for some link"))
		  .GlobalEnv$top_calc_status = 603
		  return(0)
		}


		# Required number of Cellular sets

		NumberCellularsets = as.numeric(cel_capex[2, 2])

		TotalCAPEXCellular = as.numeric(cel_capex[1, 2])

		#Total cost of Cellular equipment and installation materials
		CostOfCellularEqAndMat = as.numeric(cel_capex[3, 2])


		#Algorithm for determining the total cost of the maintenance of the Cellular communication channel


		intermediate41$NumberCellularsets <- NumberCellularsets

		cel_opex <- NULL
		cel_opex = algorithm2_11_impl(input, intermediate41)
		if (is.null(cel_opex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate OPEX of cellular for some link"))
		  .GlobalEnv$top_calc_status = 602
		  return(0)
		}

		TotalOPEXCellular = as.numeric(cel_opex[1, 2])


		ownership_cellular <- TotalCAPEXCellular + TotalOPEXCellular * as.numeric(input$PVOptionSet.PaybackPeriod)
		capex_cellular = TotalCAPEXCellular
		opex_cellular = TotalOPEXCellular

		if (bCalcNPV)
		{
		  #NPV - Cellular
		  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
		  TotalInvest <- TotalCAPEXCellular
		  CostOfOperation <- TotalOPEXCellular
		  CostOfEquipmentAndMaterials <- CostOfCellularEqAndMat

		  intermediate5$NetIncome <- 0
		  intermediate5$TotalInvest <- TotalInvest
		  intermediate5$CostOfOperation <- CostOfOperation
		  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
		  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

		  npv_cell <- NULL
		  npv_cell <- algorithm2_7_impl(input, intermediate5)
		  if (is.null(npv_cell))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV of cellular for some link"))
			.GlobalEnv$top_calc_status = 601
			return(0)
		  }

		  npv_cellular <- as.numeric(npv_cell[1, 2])
		  npv4matrix[1, 6] = npv_cellular
		}

	  }
	  else
		npv4matrix[1, 6] = npv_cellular        # for 2G
	}
	else
	  cellstatus <- "2G"


	# CELLULAR NPV CALCs ENDS   -----------------------------------------------------------------------

  } # Checking curnode for BNode ENDS -----------------------------------------------------------------

  best_npv_index = which(npv4matrix[1, 3:6] == max(npv4matrix[1, 3:6]))[1] # 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
  if (best_npv_index < 1 || best_npv_index > 4)
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_npv: Incorrect technology index"))
	.GlobalEnv$top_calc_status = 604
	#print ("Incorrect technology index")
	#browser()
	return(0)
  }
  best_npv_value = npv4matrix[[1, (2 + best_npv_index)]]
  npv4matrix[1, 7] = best_npv_index # TechIndex <- Index 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
  npv4matrix[1, 8] = best_npv_value

  return(npv4matrix)
}

calc_capex_est <- function(input, distancekm, rb, intermediate = NULL) {  # Calculate estimation of capex for different tech, initial methodology


  #copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
  input2 <- input

  if (is.reactivevalues(input))
	input2 <- reactiveValuesToList(input)

  npv4matrix <- matrix(nrow = 1, ncol = 8, 0)

  npv4matrix[1, 1] <- distancekm
  npv4matrix[1, 2] <- rb

  bCalcNPV = T

  input2$SchoolSpecificData.Length <- distancekm
  input2$SchoolSpecificData.RequiredBandwidth <- rb

  if (T) {

	# FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
	#Coefficient should be between 0 and 1
	TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

	#Distance by the roads in km
	distancekmbyroads <- round(distancekm * (1 + TopCoeff), digits = 2)

	input2$SchoolSpecificData.Length <- distancekmbyroads

	#Algorithm for overall FOCL construction cost evaluation

	focl_capex <- NULL
	focl_capex = algorithm2_1_impl(input2)
	if (is.null(focl_capex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate overall FOCL construction cost for some link"))
	  .GlobalEnv$top_calc_status = 408
	  return(0)
	}

	#Overall cost of FOCL installation
	TotalCAPEXFOCL = as.numeric(focl_capex[1, 2])

	#Overall length of the FOCL construction site
	FOCLLenghtTotal = as.numeric(focl_capex[2, 2])
	FOCLSectionLengthCD = as.numeric(focl_capex[3, 2])
	#Cost of equipment and materials for the construction of fiber optic lines
	CostOfEqAndMatFOCL = as.numeric(focl_capex[4, 2])

	capex_f = TotalCAPEXFOCL

	# RTS NPV CALCs STARTS ----------------------------------------------------------------------------
	input2$SchoolSpecificData.Length <- distancekm #Back to the regular distance

	#Algorithm for total RTS construction cost evaluation between object and SN in locality
	rts_capex <- NULL
	rts_capex = algorithm2_3_impl(input2, intermediate)
	if (is.null(rts_capex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate total RTS construction cost for some link"))
	  .GlobalEnv$top_calc_status = 508
	  return(0)
	}


	TotalCAPEXRTS <- as.numeric(rts_capex[1, 2])

	NumberOfRepeaters <- as.numeric(rts_capex[2, 2])

	#Total cost of equipment and materials for the construction of the RTS
	CostOfEqAndMatRTS <- as.numeric(rts_capex[3, 2])

	WidthFrequencyChannel <- as.numeric(rts_capex[4, 2])


	#Algorithm for total RTS maintenance cost evaluation between object and SN in locality

	intermediate3 <- list(NumberOfRepeaters = 0, RequiredCapacity = 0.0, NumberOfTerminals = 0.0, WidthFrequencyChannel = 0.0)
	intermediate3$NumberOfRepeaters <- NumberOfRepeaters
	intermediate3$RequiredCapacity <- rb
	intermediate3$NumberOfTerminals <- 2
	intermediate3$WidthFrequencyChannel <- WidthFrequencyChannel


	if (TotalCAPEXRTS == 0)
	{
	  intermediate3$NumberOfTerminals <- 0
	}

	rts_opex <- NULL
	rts_opex = algorithm2_4_impl(input2, intermediate3)

	if (is.null(rts_opex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate total RTS maintenance cost for some link"))
	  .GlobalEnv$top_calc_status = 507
	  return(0)
	}

	TotalOPEXRTS = as.numeric(rts_opex[1, 2])


	#Cost of ownership for the period
	ownership_m <- TotalCAPEXRTS + TotalOPEXRTS * as.numeric(input$PVOptionSet.PaybackPeriod)
	capex_m = TotalCAPEXRTS
	opex_m = TotalOPEXRTS


	if (bCalcNPV)
	{
	  #NPV - Microwave

	  intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
	  intermediate51$RequiredCapacity <- rb
	  intermediate51$Technology <- 1

	  rts_income <- NULL
	  rts_income = algorithm2_12_impl(input2, intermediate51)
	  if (is.null(rts_income))
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate potential income from RTS for some link"))
		.GlobalEnv$top_calc_status = 506
		return(0)
	  }


	  NetIncome <- as.numeric(rts_income[1, 2])
	  income_m = NetIncome

	  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)

	  TotalInvest <- TotalCAPEXRTS
	  CostOfOperation <- TotalOPEXRTS
	  CostOfEquipmentAndMaterials <- CostOfEqAndMatRTS

	  intermediate5$TotalInvest <- TotalInvest
	  intermediate5$CostOfOperation <- CostOfOperation
	  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
	  intermediate5$NetIncome <- NetIncome
	  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

	  npv_microwave <- NULL
	  npv_microwave <- algorithm2_7_impl(input2, intermediate5)
	  if (is.null(npv_microwave))
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV for RTS for some link"))
		.GlobalEnv$top_calc_status = 505
		return(0)
	  }

	  npv_m <- as.numeric(npv_microwave[1, 2])
	  npv4matrix[1, 4] = npv_m

	}
	# RTS NPV CALCs ENDS   ----------------------------------------------------------------------------

	# SATELLITE NPV CALCs STARTS   ---------------------------------------------------------------------
	#rb <- objects [i,4]
	ownership_s <- 100000000000000
	npv_s <- -100000000000000
	capex_s = 0
	opex_s = 0


	intermediate4 <- list(NumberVSATsets = 0, RequiredCapacity = 0.0)
	intermediate4$RequiredCapacity <- rb

	sat_capex <- NULL
	sat_capex = algorithm2_5_impl(input2, intermediate4)
	if (is.null(sat_capex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate CAPEX of satellite for some link"))
	  .GlobalEnv$top_calc_status = 703
	  return(0)
	}


	# Required number of VSAT sets

	NumberVSATsets = as.numeric(sat_capex[2, 2])

	TotalCAPEXSetellite = as.numeric(sat_capex[1, 2])

	#Total cost of VSAT equipment and installation materials
	CostOfVSATEqAndMat = as.numeric(sat_capex[3, 2])

	#Algorithm for determining the total cost of the maintenance of the satellite communication channel


	intermediate4$NumberVSATsets <- NumberVSATsets


	sat_opex <- NULL
	sat_opex = algorithm2_6_impl(input2, intermediate4)
	if (is.null(sat_opex))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate OPEX of satellite for some link"))
	  .GlobalEnv$top_calc_status = 702
	  return(0)
	}

	TotalOPEXSatellite = as.numeric(sat_opex[1, 2])


	ownership_s <- TotalCAPEXSetellite + TotalOPEXSatellite * as.numeric(input$PVOptionSet.PaybackPeriod)
	capex_s = TotalCAPEXSetellite
	opex_s = TotalOPEXSatellite


	if (bCalcNPV)
	{
	  #NPV - Satellite
	  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
	  TotalInvest <- TotalCAPEXSetellite
	  CostOfOperation <- TotalOPEXSatellite
	  CostOfEquipmentAndMaterials <- CostOfVSATEqAndMat

	  intermediate5$NetIncome <- 0
	  intermediate5$TotalInvest <- TotalInvest
	  intermediate5$CostOfOperation <- CostOfOperation
	  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
	  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

	  npv_satellite <- NULL
	  npv_satellite <- algorithm2_7_impl(input2, intermediate5)
	  if (is.null(npv_satellite))
	  {
		.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV of satellite for some link"))
		.GlobalEnv$top_calc_status = 701
		return(0)
	  }

	  npv_s <- as.numeric(npv_satellite[1, 2])
	  npv4matrix[1, 5] = npv_s
	}


	# SATELLITE NPV CALCs ENDS   -----------------------------------------------------------------------

	# CELLULAR NPV CALCs STARTS   ---------------------------------------------------------------------

	ownership_cellular <- 100000000000000
	npv_cellular <- -100000000000000
	capex_cellular = 0
	opex_cellular = 0


	cellstatus <- "4G"

	#cellstatus <- objects[1, 6]

	if (!is.na(cellstatus))
	{
	  if (cellstatus == "3G" || cellstatus == "4G")
	  {
		#Algorithm for determining the total cost of the installation and configuration of the cellular communication channel

		intermediate41 <- list(NumberCellularsets = 0, RequiredCapacity = 0.0)
		intermediate41$RequiredCapacity <- rb

		cel_capex <- NULL
		cel_capex = algorithm2_10_impl(input, intermediate41)
		if (is.null(cel_capex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate CAPEX of cellular for some link"))
		  .GlobalEnv$top_calc_status = 603
		  return(0)
		}


		# Required number of Cellular sets

		NumberCellularsets = as.numeric(cel_capex[2, 2])

		TotalCAPEXCellular = as.numeric(cel_capex[1, 2])

		#Total cost of Cellular equipment and installation materials
		CostOfCellularEqAndMat = as.numeric(cel_capex[3, 2])


		#Algorithm for determining the total cost of the maintenance of the Cellular communication channel


		intermediate41$NumberCellularsets <- NumberCellularsets

		cel_opex <- NULL
		cel_opex = algorithm2_11_impl(input, intermediate41)
		if (is.null(cel_opex))
		{
		  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
		  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate OPEX of cellular for some link"))
		  .GlobalEnv$top_calc_status = 602
		  return(0)
		}

		TotalOPEXCellular = as.numeric(cel_opex[1, 2])


		ownership_cellular <- TotalCAPEXCellular + TotalOPEXCellular * as.numeric(input$PVOptionSet.PaybackPeriod)
		capex_cellular = TotalCAPEXCellular
		opex_cellular = TotalOPEXCellular

		if (bCalcNPV)
		{
		  #NPV - Cellular
		  intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
		  TotalInvest <- TotalCAPEXCellular
		  CostOfOperation <- TotalOPEXCellular
		  CostOfEquipmentAndMaterials <- CostOfCellularEqAndMat

		  intermediate5$NetIncome <- 0
		  intermediate5$TotalInvest <- TotalInvest
		  intermediate5$CostOfOperation <- CostOfOperation
		  intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
		  intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

		  npv_cell <- NULL
		  npv_cell <- algorithm2_7_impl(input, intermediate5)
		  if (is.null(npv_cell))
		  {
			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
			.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_nvp: Cannot calculate NPV of cellular for some link"))
			.GlobalEnv$top_calc_status = 601
			return(0)
		  }

		  npv_cellular <- as.numeric(npv_cell[1, 2])
		  npv4matrix[1, 6] = npv_cellular
		}

	  }
	  else
		npv4matrix[1, 6] = npv_cellular        # for 2G
	}
	else
	  cellstatus <- "2G"


	# CELLULAR NPV CALCs ENDS   -----------------------------------------------------------------------

  } # Checking curnode for BNode ENDS -----------------------------------------------------------------

  best_npv_index = which(npv4matrix[1, 3:6] == max(npv4matrix[1, 3:6]))[1] # 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
  if (best_npv_index < 1 || best_npv_index > 4)
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_4x_npv: Incorrect technology index"))
	.GlobalEnv$top_calc_status = 604
	#print ("Incorrect technology index")
	#browser()
	return(0)
  }
  best_npv_value = npv4matrix[[1, (2 + best_npv_index)]]
  npv4matrix[1, 7] = best_npv_index # TechIndex <- Index 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
  npv4matrix[1, 8] = best_npv_value

  return(npv4matrix)
}


calc_bn_focl_npv_addon <- function(input, dtf, bn_rb, intermediate = NULL) # from BN to the Internet via focl
{

  req(input)
  req(dtf)
  req(bn_rb)

  #browser()

  if (input$use_stub) {
	result_npv <- -100 * as.numberic(dtf) # npv_focl
	return(result_npv)
  }

  # npv2matrix [5,1] <- 100000000000000 # npv_Satellite
  # npv2matrix [6,1] <- 100000000000000 # npv_cellular

  #Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
  input2 <- input

  if (is.reactivevalues(input))
	input2 <- reactiveValuesToList(input)

  bCalcNPV = T

  distancekm <- as.numeric(dtf)
  rb <- as.numeric(bn_rb)

  input2$SchoolSpecificData.Length <- distancekm
  input2$SchoolSpecificData.RequiredBandwidth <- rb

  # FOCL NPV CALCs BEGINS -----------------------------------------------------------------------------
  #Coefficient should be between 0 and 1
  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km
  distancekmbyroads <- round(distancekm * (1 + TopCoeff), digits = 2)

  input2$SchoolSpecificData.Length <- distancekmbyroads

  #Algorithm for overall FOCL construction cost evaluation

  focl_capex <- NULL
  focl_capex = algorithm2_1_impl(input2)
  if (is.null(focl_capex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate overall FOCL construction cost for some link"))
	.GlobalEnv$top_calc_status = 408
	return(0)
  }

  #Overall cost of FOCL installation
  TotalCAPEXFOCL = as.numeric(focl_capex[1, 2])

  #Overall length of the FOCL construction site
  FOCLLenghtTotal = as.numeric(focl_capex[2, 2])

  FOCLSectionLengthCD = as.numeric(focl_capex[3, 2])

  #?ost of equipment and materials for the construction of fiber optic lines
  CostOfEqAndMatFOCL = as.numeric(focl_capex[4, 2])


  #Algorithm for overall FOCL maintenance cost evaluation
  intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
  intermediate51$RequiredCapacity <- rb
  intermediate51$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate51$Technology <- 0


  intermediate2 <- list(FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
  intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
  intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate2$RequiredCapacity <- rb

  focl_opex <- NULL
  focl_opex = algorithm2_2_impl(input2, intermediate2)
  if (is.null(focl_opex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate overall FOCL maintenance cost for some link"))
	.GlobalEnv$top_calc_status = 407
	return(0)
  }

  #Total cost for FOCL maintenance for the entire period of operation
  TotalOPEXFOCL = as.numeric(focl_opex[1, 2])


  #Cost of ownership for the period
  ownership_f = TotalCAPEXFOCL + TotalOPEXFOCL * as.numeric(input$PVOptionSet.PaybackPeriod)
  capex_f = TotalCAPEXFOCL
  opex_f = TotalOPEXFOCL


  if (bCalcNPV)
  {
	focl_income <- NULL
	focl_income = algorithm2_12_impl(input2, intermediate51)
	if (is.null(focl_income))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate potential income from FOCL for some link"))
	  .GlobalEnv$top_calc_status = 406
	  return(0)
	}


	NetIncome <- as.numeric(focl_income[1, 2])
	income_f = NetIncome

	#NPV - FOCL
	intermediate5 <- list(NetIncome = 0.0, TotalInvest = 0.0, CostOfOperation = 0.0, CostOfEquipmentAndMaterials = 0.0, PaybackPeriod = 5)
	TotalInvest <- TotalCAPEXFOCL
	CostOfOperation <- TotalOPEXFOCL
	CostOfEquipmentAndMaterials <- CostOfEqAndMatFOCL
	intermediate5$TotalInvest <- TotalInvest
	intermediate5$CostOfOperation <- CostOfOperation
	intermediate5$NetIncome <- NetIncome
	intermediate5$CostOfEquipmentAndMaterials <- CostOfEquipmentAndMaterials
	intermediate5$PaybackPeriod <- input2$PVOptionSet.PaybackPeriod

	npv_focl <- NULL
	npv_focl <- algorithm2_7_impl(input2, intermediate5)

	if (is.null(npv_focl))
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_npv_pair: Cannot calculate NPV of FOCL for some link"))
	  .GlobalEnv$top_calc_status = 405
	  return(0)
	}

	npv_f = as.numeric(npv_focl[1, 2])
	result_npv <- npv_f
  }
  # FOCL NPV CALCs ENDS -----------------------------------------------------------------------------

  #print(result_npv)

  return(result_npv)
}

calc_bn_focl_cown_addon <- function(input, dtf, bn_rb, intermediate = NULL) # from BN to the Internet via focl
{

  req(input)
  req(dtf)
  req(bn_rb)


  #browser()

  if (input$use_stub) {
	result_cown <- -100 * as.numberic(dtf) # npv_focl
	return(result_cown)
  }

  # cown2matrix [5,1] <- 100000000000000 # npv_Satellite
  # cown2matrix [6,1] <- 100000000000000 # npv_cellular

  #Step 3: Copy input variables (necessary to avoid problems with reactive content of Investigation Tool)
  input2 <- input

  if (is.reactivevalues(input))
	input2 <- reactiveValuesToList(input)

  distancekm <- as.numeric(dtf)
  rb <- as.numeric(bn_rb)

  input2$SchoolSpecificData.Length <- distancekm
  input2$SchoolSpecificData.RequiredBandwidth <- rb

  # FOCL COWN CALCs BEGINS -----------------------------------------------------------------------------
  #Coefficient should be between 0 and 1
  TopCoeff <- as.numeric(input$InitialDataFOCL.TopographyCoeff)

  #Distance by the roads in km
  distancekmbyroads <- round(distancekm * (1 + TopCoeff), digits = 2)

  input2$SchoolSpecificData.Length <- distancekmbyroads

  #Algorithm for overall FOCL construction cost evaluation

  focl_capex <- NULL
  focl_capex = algorithm2_1_impl(input2)
  if (is.null(focl_capex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_cown_pair: Cannot calculate overall FOCL construction cost for some link"))
	.GlobalEnv$top_calc_status = 408
	return(0)
  }

  #Overall cost of FOCL installation
  TotalCAPEXFOCL = as.numeric(focl_capex[1, 2])

  #Overall length of the FOCL construction site
  FOCLLenghtTotal = as.numeric(focl_capex[2, 2])

  FOCLSectionLengthCD = as.numeric(focl_capex[3, 2])

  #Сost of equipment and materials for the construction of fiber optic lines
  CostOfEqAndMatFOCL = as.numeric(focl_capex[4, 2])


  #Algorithm for overall FOCL maintenance cost evaluation
  intermediate51 <- list(RequiredCapacity = 0.0, FOCLSectionLengthCD = 0.0, Technology = 0.0)
  intermediate51$RequiredCapacity <- rb
  intermediate51$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate51$Technology <- 0


  intermediate2 <- list(FOCLLenghtTotal = 0.0, FOCLSectionLengthCD = 0.0, RequiredCapacity = 0.0)
  intermediate2$FOCLLenghtTotal <- FOCLLenghtTotal
  intermediate2$FOCLSectionLengthCD <- FOCLSectionLengthCD
  intermediate2$RequiredCapacity <- rb

  focl_opex <- NULL
  focl_opex = algorithm2_2_impl(input2, intermediate2)
  if (is.null(focl_opex))
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Calc_cown_pair: Cannot calculate overall FOCL maintenance cost for some link"))
	.GlobalEnv$top_calc_status = 407
	return(0)
  }

  #Total cost for FOCL maintenance for the entire period of operation
  TotalOPEXFOCL = as.numeric(focl_opex[1, 2])


  #Cost of ownership for the period
  ownership_f = TotalCAPEXFOCL + TotalOPEXFOCL * as.numeric(input$PVOptionSet.PaybackPeriod)
  capex_f = TotalCAPEXFOCL
  opex_f = TotalOPEXFOCL

  result_cown <- -as.numeric(ownership_f)
  # FOCL COWN CALCs ENDS -----------------------------------------------------------------------------

  return(result_cown)
}

recalc_4x_satellite <- function(npv4matrix, focl_possible_global = 0, rts_possible_global = 0, sat_disabled = 0, cells_disabled = 1)
{
  npv4rs <- nrow(npv4matrix)


  if (rts_possible_global == 0) {
	#npv4rs <- nrow(npv4matrix)
	for (i in 1:npv4rs) {
	  npv4matrix[i, 3] <- -100000000000000 # Making FOCL impossible
	  npv4matrix[i, 4] <- -100000000000000 # Making RTS impossible
	  npv4matrix[i, 6] <- -100000000000000 # Making CELL impossible
	}
  }

  #print('In recalc_sat !')

  # Recalculate 4x_matrix's the best tech and the metric
  for (i in 1:npv4rs) {
	# TechIndex <- Index 1 - FOCL 2 - RTS 3 - SATELLITE  4 - CELLULAR
	best_npv_index <- 3
	best_npv_value <- npv4matrix[[i, (2 + best_npv_index)]]
	npv4matrix[i, 7] <- best_npv_index
	npv4matrix[i, 8] <- best_npv_value
  }

  #print('In recalc_sat: After index ...')

  return(npv4matrix)
}

formula_4_5_6_satellite <- function(input_load, output, objects, intermediate = NULL) # To process the special case of star topology, based only on sattelite
{

  req(input_load)
  req(objects)

  # Enabling stub mode for fast computation
  input_load$use_stub <- F

  use_cown <- F # using tco as metric by default otherwise using npv
  if (input_load$TopologySettings.UseCOWN == 1)
	use_cown <- T

  use_metric <- 0 # using classic npv or tco as metric by default
  if (input_load$TopologySettings.UseMetric == 1) # Use simplified metric
	use_metric <- 1
  if (input_load$TopologySettings.UseMetric == 2) # Use costless metric
	use_metric <- 2


  global_log_enabled <- F # Set in F to run in production
  if (exists("bWriteLog", input_load)) global_log_enabled <- T   # Enabling global logging in case of production

  #if (exists("bIgnoreInternetCost", input_load)) input_load$bIgnoreInternetCost <- globlaIgnoreInternetCost   # Setup usage of InternetCost for calculation process
  globlaIgnoreInternetCost <- F
  input_load$bIgnoreInternetCost <- globlaIgnoreInternetCost

  if (exists("TopologySettings.EnableGL", input_load)) {   # Enabling global logging if running as webapp
	if (input_load$TopologySettings.EnableGL == 1) {
	  global_log_enabled <- T
	}
  }

  log_to_files <- F
  if (exists("TopologySettings.LogToFile", input_load)) {
	if (input_load$TopologySettings.LogToFile == 1)
	  log_to_files <- T
  }

  #objects <- vroom::vroom(input_load$Files.ListOfObjects, altrep = FALSE) # Fix decimals

  # Initialize log files
  if (log_to_files)
	cat("Step-by-step construction process:", file = "steps-log.txt", sep = "\n", append = FALSE)

  #cat("Postoptimization log:", file = "opt-log.txt", sep = "\n", append = FALSE)
  cat("New cluster:", file = "opt-log.txt", sep = "\n", append = TRUE)
  cat("New cluster:", file = "opt-warn.txt", sep = "\n", append = TRUE)


  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Step-by-step construction process initiated:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Using cluster data, contains %d objects", nrow(objects))))
	if (use_cown)
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Using Cost-of-Ownership as optimization metric"))
	else
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6: Using NPV as optimization metric"))
  }

  is_cell_excluded <- 1
  is_dc_nodes <- 0

  #print(sprintf("1 Radio MaximumLinkCapacity %f", input_load$InitialDataRadio.MaximumLinkCapacity))
  #print(sprintf("1 Satelite MaximumLinkCapacity %f", input_load$InitialDataSatellite.MaximumLinkCapacity))
  #print(sprintf("1 Cellular MaximumLinkCapacity %f", input_load$InitialDataCellular.MaximumLinkCapacity))

  PostoptEnabled <- input_load$TopologySettings.EnablePostopt
  #FOCL_Thrld <- input_load$TopologySettings.FOCLBottleTrh # Check as rudiment
  #RTS_Thrld <- input_load$TopologySettings.RTSBottleTrh # Check as rudiment
  RTS_Thrld <- as.numeric(input_load$InitialDataRadio.MaximumLinkCapacity)
  isFOCLBased <- input_load$TopologySettings.FOCLBased
  isRTSBased <- input_load$TopologySettings.RTSBased
  is_cell_excluded <- input_load$TopologySettings.ExcludeCellular
  is_dc_nodes <- input_load$TopologySettings.DCNodes


  rts_possible_global <- 0
  focl_possible_global <- 0
  cells_disabled <- 1
  sat_disabled <- 0

  # 1 --- Finding BaseNode ----------------

  #print('SAT: Before BN')

  BaseNode <- find_basenode(input_load, objects)
  #return (result)
  if (global_log_enabled)
	#.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_6: BaseNode found "), i18n$t(paste(BaseNode$ind, " ", BaseNode[1])), sep = ": "))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6_satellite: BaseNode found "))
  # 1 -------------------------------------

  # 2 --- Sorting nodes by distance to BN, then by RB

  numberofobj <- nrow(objects)
  obj_plus_DTBN <- objects
  obj_plus_DTBN$DTBN = 9999999
  dist_to_BN <- 0

  if (numberofobj > 0)
  {
	for (i in 1:numberofobj)
	{
	  dist_to_BN <- distm(c(as.numeric(objects[i, 2]), as.numeric(objects[i, 3])), c(as.numeric(BaseNode[[2]]), as.numeric(BaseNode[[3]])), fun = distHaversine)
	  obj_plus_DTBN[i,]$DTBN = as.numeric(dist_to_BN[1] / 1000)
	  obj_plus_DTBN[i, 1] = iconv(as.character(obj_plus_DTBN[i, 1]), "UTF-8", "UTF-8", sub = '')
	  obj_plus_DTBN[i, 7] = iconv(as.character(obj_plus_DTBN[i, 7]), "UTF-8", "UTF-8", sub = '')
	}

  }

  interim_sorted_nodes <- obj_plus_DTBN[order(obj_plus_DTBN$DTBN, -obj_plus_DTBN$RB),]

  #print('SAT: After BN')
  # 2 -----------------------------------------------

  # 2A --- Preparing main node_table ----------------

  # 2B --- Preparing adhoc data for map visualization ---------------

  numberofnodes <- nrow(interim_sorted_nodes)
  longs <- pull(interim_sorted_nodes[, 2])
  lats <- pull(interim_sorted_nodes[, 3])
  loc_names = iconv(pull(interim_sorted_nodes[, 1]), "UTF-8", "UTF-8", sub = '')

  v1 <- data.frame(ids = 1:numberofnodes, name = loc_names, x = as.numeric(longs), y = as.numeric(lats)) # visualization data
  #print(v1$name)


  # 2B --------------------------------------------------------

  if (numberofobj < 1)
  {
	.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6_satellite: Insufficeient number of nodes"))
	#print ("Insufficeient number of nodes")
	return(0)
  }

  #OBJECTNAME    LON   LAT    RB   DTF COV   REGION  DTBN HopsToBN NextNode   FRB   NPV  Tech  isBN isLeaf NPVC

  # Initialize main lookup table
  main_table <- interim_sorted_nodes[2:numberofobj,] # Main table should not contain the BN
  main_table$HopsToBN <- as.integer(0)
  main_table$NextNode <- as.integer(-1) # -1 - disconnected / 0 - Connected to BN
  main_table$FRB <- as.numeric(0)
  main_table$NPV <- as.numeric(0)
  main_table$Tech <- as.integer(0)
  main_table$isBN <- as.integer(0)
  main_table$isLeaf <- as.integer(0)
  main_table$NPVC <- as.numeric(0)
  main_table$Lm <- as.numeric(0)
  main_table$DistToNext <- as.numeric(0)

  # Initialize BaseNode table
  main_BN <- interim_sorted_nodes[1,]
  main_BN$HopsToBN <- as.integer(0)
  main_BN$NextNode <- as.integer(0)
  main_BN$FRB <- as.numeric(main_BN$RB)
  main_BN$NPV <- as.numeric(0)
  main_BN$Tech <- as.integer(0)
  main_BN$isBN <- as.integer(1)
  main_BN$isLeaf <- as.integer(0)
  main_BN$NPVC <- as.numeric(0)

  # print(main_table)

  # 2A ----------------------------------------------

  # 3 --- Preparing 4-tech 6-rows matrix

  bnindex = 1
  if (use_cown) {
	npv4matrix <- calc_4x_matrix_router_cown(input_load, interim_sorted_nodes, bnindex, use_metric) # use COWN
  } else {
	npv4matrix <- calc_4x_matrix_router(input_load, interim_sorted_nodes, bnindex, use_metric) # use NPV
  }

  #print("after 4x")

  if (log_to_files) {
	write.table(npv4matrix, file = "npv4matrix.txt", row.names = TRUE, col.names = TRUE, append = FALSE)
	write.table(npv4matrix, file = "main_table.txt", row.names = TRUE, col.names = TRUE, append = FALSE)
  }

  # print(npv4matrix)
  #result <- npv4matrix

  npv4matrix_bcp <- npv4matrix # Backup (remove later)
  npv4matrix <- recalc_4x_satellite(npv4matrix, 0, 0, 0, 1) #
  npv4rs <- nrow(npv4matrix)

  # 3 ----------------------------------

  # --- Main Loop ----------------------
  mrs <- nrow(main_table)

  if (mrs < 1) {
	result <- list(v1, BaseNode, main_BN)
	return(result)
  }


  # Calculating first row, correspoding to 1st node connected directly to BN

  main_BN$FRB <- main_BN$RB
  main_BN$isLeaf <- 0

  # npv4matrix[which(npv4matrix$RB[1,] == min(m1$RB))[1],]

  #print('Check point 1')

  for (c_line in 1:mrs) #c_line - current line in main lookup table
  {

	main_table$HopsToBN[c_line] = 1
	main_table$NextNode[c_line] = 0
	main_table$FRB[c_line] = main_table$RB[c_line]
	main_table$isLeaf[c_line] = 1
	main_table$NPVC[c_line] = npv4matrix[2, 8]
	main_table$Tech[c_line] = npv4matrix[2, 7]
	main_table$NPV[c_line] = npv4matrix[2, 8]

	limit_sat <- as.numeric(input_load$InitialDataSatellite.MaximumLinkCapacity) # Check possibility of connection
	if (limit_sat < main_table$RB[c_line])
	{
	  .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))
	  .GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t("Formula_4_5_6_satellite: Cannot build satellite only topology, bandwidth limits exceeded !"))
	  print("Cannot build satellite only topology, bandwidth limits exceeded !")
	  return(0)
	}

  }

  #print('Check point 2')
  # ------------------------------------
  # browser()

  #Calculate some stats
  npv_total <- 0
  npv_cum_total <- 0
  num_focl <- 0
  num_rts <- 0
  num_sat <- 0
  num_cell <- 0
  lm_total <- 0
  len_focl <- 0
  len_rts <- 0
  len_cell <- 0
  len_sat <- 0
  rb_focl <- 0
  rb_rts <- 0
  rb_sat <- 0
  rb_cell <- 0
  frb_focl <- 0
  frb_rts <- 0
  frb_cell <- 0
  frb_sat <- 0
  metric_focl <- 0
  metric_rts <- 0
  metric_sat <- 0
  metric_cell <- 0

  for (stat_line in 1:mrs)    # Also accounting all possible tech despite of all sat... for verification
  {

	npv_cum_total <- npv_cum_total + main_table$NPVC[stat_line]
	npv_total <- npv_total + main_table$NPV[stat_line]

	temp_next <- main_table$NextNode[stat_line]
	if (temp_next != 0)
	  temp_dist <- calc_final_dist(input_load, main_table[stat_line, 1:8], main_table[temp_next, 1:8])
	else
	  temp_dist <- calc_final_dist(input_load, main_table[stat_line, 1:8], main_BN[1, 1:8])
	main_table$DistToNext[stat_line] <- temp_dist

	if (main_table$Tech[stat_line] == 1) {
	  num_focl <- num_focl + 1
	  len_focl <- len_focl + main_table$DistToNext[stat_line]
	  rb_focl <- rb_focl + main_table$RB[stat_line]
	  frb_focl <- frb_focl + main_table$FRB[stat_line]
	  metric_focl <- metric_focl + main_table$NPVC[stat_line]
	}

	if (main_table$Tech[stat_line] == 2) {
	  num_rts <- num_rts + 1
	  len_rts <- len_rts + main_table$DistToNext[stat_line]
	  rb_rts <- rb_rts + main_table$RB[stat_line]
	  frb_rts <- frb_rts + main_table$FRB[stat_line]
	  metric_rts <- metric_rts + main_table$NPVC[stat_line]
	}

	if (main_table$Tech[stat_line] == 3) {
	  num_sat <- num_sat + 1
	  len_sat <- len_sat + main_table$DistToNext[stat_line]
	  rb_sat <- rb_sat + main_table$RB[stat_line]
	  frb_sat <- frb_sat + main_table$RB[stat_line]
	  metric_sat <- metric_sat + main_table$NPVC[stat_line]
	}

	if (main_table$Tech[stat_line] == 4) {
	  num_cell <- num_cell + 1
	  len_cell <- len_cell + main_table$DistToNext[stat_line]
	  rb_cell <- rb_cell + main_table$RB[stat_line]
	  frb_cell <- frb_cell + main_table$RB[stat_line]
	  metric_cell <- metric_cell + main_table$NPVC[stat_line]
	}

	if (as.numeric(input_load$TopologySettings.LastMile) == 1) {
	  lm_tech <- as.numeric(input_load$TopologySettings.LMTech)
	  # IntParam.BW_DM_C
	  # l_BW_DM_C <- 3
	  l_BW_DM_C <- input_load$IntParam.BW_DM_C
	  l_upop <- main_table$RB[stat_line] / l_BW_DM_C
	  #print("check1")
	  lm_temp <- calc_tco_simple_lm(input_load, l_upop, lm_tech)
	  #print("check_last")
	  main_table$Lm[stat_line] <- lm_temp
	  lm_total <- lm_total + lm_temp
	  #print(lm_temp)

	}
	else {
	  main_table$Lm[stat_line] <- 0
	}
  }

  #print('Check point 3')

  # !!! Add value for BN satellite connectivity !!!
  npv_cum_addon <- 0 # Calculate additional internet costs for FOCl and RTS


  npv_dtf_addon <- 0 # Calculate addititonal cost for the case when BaseNode is not yet connected to the Internet directly via fiber
  #if (use_metric == 0) {
  #if (BaseNode$DTF != 0) {
  #  if (!use_cown)
  #	npv_dtf_addon <- calc_bn_focl_npv_addon(input_load, BaseNode$DTF, main_BN$FRB)
  #  else
  #	npv_dtf_addon <- calc_bn_focl_cown_addon(input_load, BaseNode$DTF, main_BN$FRB)
  #}
  #}

  #if (use_metric == 1) {
  #if (BaseNode$DTF != 0) {
  #  if (!use_cown)
  #	npv_dtf_addon <- calc_bn_focl_simple_npv_addon(input_load, BaseNode$DTF, main_BN$FRB)
  #  else
  #	npv_dtf_addon <- calc_bn_focl_simple_cown_addon(input_load, BaseNode$DTF, main_BN$FRB)
  #}
  #}
  #
  #if (use_metric > 1) {  # case of simplified and over-simplified
  #if (BaseNode$DTF != 0) {
  #  if (!use_cown)
  #	npv_dtf_addon <- 0
  #  else
  #	npv_dtf_addon <- 0
  #}
  #}


  npv_cum_total <- npv_cum_total + npv_cum_addon + npv_dtf_addon # Show last addon

  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Caclulating  NPV/COWN addon for InternetCost - %f", npv_cum_addon)))
  }

  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, i18n$t(sprintf("Formula_4_5_6: Caclulating  NPV/COWN addon for connection BN to the Internet (DTF>0) - %f", npv_dtf_addon)))
  }

  total_links = mrs
  if (!use_cown)
	# add mpv calculation for lm_total and 1 for default number of clusters
	stats_v <- c(total_links, num_focl, num_rts, num_sat, num_cell, round(npv_total, 2), round(npv_cum_total, 2), round(npv_cum_addon, 2), round(npv_dtf_addon, 2), 0, 1, len_focl, frb_focl, metric_focl, len_rts, frb_rts, metric_rts, len_sat, frb_sat, metric_sat, len_cell, frb_cell, metric_cell)
  else
	stats_v <- c(total_links, num_focl, num_rts, num_sat, num_cell, -1 * round(npv_total, 2), -1 * round(npv_cum_total, 2), -1 * round(npv_cum_addon, 2), -1 * round(npv_dtf_addon, 2), round(lm_total, 2), 1, len_focl, frb_focl, -1 * metric_focl, len_rts, frb_rts, -1 * metric_rts, len_sat, frb_sat, -1 * metric_sat, len_cell, frb_cell, -1 * metric_cell)

  if (as.numeric(input_load$TopologySettings.UseMetric) == 2) {
	unit_price <- as.numeric(input_load$TopologySettings.CostlessUnitPrice)
	stats_v <- c(total_links, num_focl, num_rts, num_sat, num_cell, -1 * round(npv_total, 2), -1 * round(npv_cum_total, 2), -1 * round(npv_cum_total * unit_price, 2), -1 * round(npv_dtf_addon, 2), 0, 1, len_focl, frb_focl, -1 * metric_focl, len_rts, frb_rts, -1 * metric_rts, len_sat, frb_sat, -1 * metric_sat, len_cell, frb_cell, -1 * metric_cell)
  }
  #result <- list(v1,main_BN,main_table)
  temp_lables = c()
  #iconv(pull(interim_sorted_nodes[, 1]), "UTF-8", "UTF-8", sub = '')
  #temp_lables[1] <- sprintf("%s RB: %f FRB: %f M: %f CM: %f",iconv(main_BN$OBJECTNAME[1],"UTF-8", "UTF-8", sub = ''), main_BN$RB[1],main_BN$FRB[1],main_BN$NPV[1],main_BN$NPVC[1])
  temp_lables[i] <- v1$name[1]
  for (i in 2:nrow(v1)) {
	#temp_lables[i] <- sprintf("%s <br> RB: %f FRB: %f <br> M: %f CM: %f LM: %f", iconv(main_table$OBJECTNAME[i - 1], "UTF-8", "UTF-8", sub = ''), main_table$RB[i - 1], main_table$FRB[i - 1], main_table$NPV[i - 1], main_table$NPVC[i - 1], main_table$Lm[i-1])
	temp_lables[i] <- sprintf("%s <br> RB: %f FRB: %f <br> M: %f CM: %f", iconv(main_table$OBJECTNAME[i - 1], "UTF-8", "UTF-8", sub = ''), main_table$RB[i - 1], main_table$FRB[i - 1], main_table$NPV[i - 1], main_table$NPVC[i - 1])
  }
  v1$name <- temp_lables
  result <- list(v1, main_BN, main_table, stats_v, npv4matrix)
  #print('Before retur of SAT results ...')
  #print('v1 ----- ')
  #print(v1)
  #print('main BN ----')
  #print(main_BN)
  #print('Main Table -----')
  #print(main_table)
  #print('Stat Vector ----')
  #print(stats_v)
  #print('NPV4XM -----')
  #print(npv4matrix)
  return(result)
}
