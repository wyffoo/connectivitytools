library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Maximum distance and bandwidth based lattice clustering algorithm

library(igraph)
library(geosphere)

#global list of clusters
# clust_list <- list()

#Determination of the lattice borders coordinates
formula_4_4_1 <- function (input, output, objects, intermediate = NULL)
{
  req (input)
  req (objects)

  #global_log_enabled <- T # Disable for integration

  global_log_enabled <- F # Set in F to run in production
  if (exists("bWriteLog", input)) global_log_enabled <- T   # Enabling global logging in case of production

  req (input$TopologySettings.MaximumNodeNumber)
  req (input$TopologySettings.MaxRB)
  req (input$TopologySettings.MaxDist)

  max_nodes_number_limit <- input$TopologySettings.MaximumNodeNumber
  max_rb_limit <- input$TopologySettings.MaxRB
  max_dist <- input$TopologySettings.MaxDist

  numberofobj <- nrow(objects)
  Westernmost <- 180 #possible maximum for longtitude
  Easternmost <- -180 #possible maximum for longtitude
  Northernmost <- -90 #possible maximum for lattitude
  Southernmost <- 90 #possible maximum for lattitude

  TotalBandwidth <- 0

  #objects$x <- as.integer(0)
  #objects$y <- as.integer(0)

  if (numberofobj > 0)
  {
	for (i in 1: numberofobj)
	{
	  lon <- as.numeric(objects [[i,2]])
	  lat <- as.numeric(objects [[i,3]])

	  temp_coord <- mercator(c(lon,lat))

	  #objects$x[i] <- temp_coord[1]
	  #objects$y[i] <- temp_coord[2]

	  rb <- as.numeric(objects [[i,4]])

	  TotalBandwidth <- TotalBandwidth + rb

	  if (lon > Easternmost) {
		Easternmost <- lon
		#Easternmost_x <- temp_coord[1]
	  }

	  if (lon < Westernmost) {
		Westernmost <- lon
		#Westernmost_x <- temp_coord[1]
	  }

	  if (lat > Northernmost) {
		Northernmost <- lat
		#Northernmost_y <- temp_coord[2]
	  }

	  if (lat < Southernmost) {
		Southernmost <- lat
		#Southernmost_y <- temp_coord[2]
	  }

	}

  }


  border_data <- matrix (nrow = 5, ncol = 2)


  border_data [1,1] <- i18n$t("Westernmost longitude of lattice")
  border_data [1,2] <- as.numeric (Westernmost)

  border_data [2,1] <- i18n$t("Easternmost longitude of lattice")
  border_data [2,2] <- as.numeric (Easternmost)

  border_data [3,1] <- i18n$t("Northernmost latitude of lattice")
  border_data [3,2] <- as.numeric (Northernmost)

  border_data [4,1] <- i18n$t("Southernmost latitude of lattice")
  border_data [4,2] <- as.numeric (Southernmost)

  border_data [5,1] <- i18n$t("Total required bandwidth for all objects, Mbps")
  border_data [5,2] <- as.numeric (TotalBandwidth)

  temp_coord_1 <- mercator(c(as.numeric(Westernmost),as.numeric(Northernmost)))
  Westernmost_x <- temp_coord_1[1]
  Northernmost_y <- temp_coord_1[2]
  temp_coord_2 <- mercator(c(as.numeric(Easternmost),as.numeric(Southernmost)))
  Easternmost_x <- temp_coord_2[1]
  Southernmost_y <- temp_coord_2[2]

  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_1: Westernmost longitude of lattice "), paste(as.numeric(Westernmost), " ", Westernmost_x), sep = ": "))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_1: Easternmost longitude of lattice "), paste(as.numeric(Easternmost), " ", Easternmost_x), sep = ": "))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_1: Northernmost latitude of lattice "), paste(as.numeric(Northernmost), " ", Northernmost_y), sep = ": "))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_1: Southernmost latitude of lattice "), paste(as.numeric(Southernmost), " ", Southernmost_y), sep = ": "))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_1: Total required bandwidth for all objects, Mbps "), TotalBandwidth, sep = ": "))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_1: Required bandwidth limit per cluster, Mbps "), max_rb_limit, sep = ": "))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_1: Nodes number limit per cluster "), max_nodes_number_limit, sep = ": "))
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_1: Maximum distance limit, meters "), max_dist, sep = ": "))

  }

  longs <- pull(objects[,2])
  lats <- pull(objects[,3])
  loc_names = iconv(pull(objects[,1]), "UTF-8", "UTF-8",sub='')

  v1 <- data.frame(ids = 1:numberofobj, name = loc_names, x = as.numeric(longs), y = as.numeric(lats)) # visualization data

  cur_area <- list(c(as.numeric (Westernmost), as.numeric (Northernmost)),c(as.numeric (Easternmost),as.numeric (Southernmost)))
  #process_areas(cur_area)
  #area_stats <- get_area_stats(cur_area,objects)
  #clust_list <<- list()

  # Initialize log files
  #cat("Clustering process:",file="cluster-log.txt",sep="\n",append=FALSE)
  #cat("Clustering process:",file="cluster-list.txt",sep="\n",append=FALSE)

  clust_list <- process_areas(cur_area,max_nodes_number_limit,max_rb_limit,max_dist,objects)
  clust_number <- length(clust_list)

  #str(clust_list)

  if (global_log_enabled) {
	.GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste(i18n$t("Formula_4_5_1: Number of clusters "), clust_number, sep = ": "))
  }
  #clust_list <<- c(clust_list,cur_area)
  #result <- list(v1,border_data)
  result <- list(v1,cur_area,clust_list)
  return (result)
}

get_area_stats <- function (c_area,objects) {

  numberofobj <- nrow(objects)
  TotalNodes <- 0
  TotalRB <- 0
  AreaSize <- 0

  latmin <- 90
  latmax <- -90
  lonmax <- -180
  lonmin <- 180

  l_lons <- c()
  l_lats <- c()
  l_nodes_list <- c()

  nastyDebug <- F

  #todel
  #print(sprintf("number of objects: %d ",  numberofobj))
  #print(sprintf("testing access to objects i=2,3,4: %f %f %f ", objects [[1,2]], objects [[1,3]], objects [[1,4]]))

  if (numberofobj > 0)
  {
	for (i in 1: numberofobj)
	{
	  #lon <- pull(objects [i,2])
	  lon <- as.numeric(objects [[i,2]])
	  #lat <- pull(objects [i,3])
	  lat <- as.numeric(objects [[i,3]])

	  if (between(lon,c_area[[1]][1],c_area[[2]][1]) && between(lat,c_area[[2]][2],c_area[[1]][2])) {
		#if ((lon >= c_area[[1]][1]) && (lon <= c_area[[2]][1]) && (lat <= c_area[[1]][2]) && (lat >= c_area[[1]][2])) {
		rb <- as.numeric(objects [[i,4]])
		TotalRB <- TotalRB + rb
		TotalNodes <- TotalNodes + 1

		#todel
		#print(sprintf("lon = %f lat = %f inside area:  (%f %f), (%f %f) ", lon, lat, c_area[[1]][1], c_area[[2]][1], c_area[[2]][2],c_area[[1]][2]))

		l_nodes_list <- append(l_nodes_list, i)
		#l_lons <- c(l_lons,lon)
		#print(lon)
		#print(l_lons)
		l_lons <- append(l_lons,lon)
		#l_lats <- c(l_lats,lat)
		l_lats <- append(l_lats,lat)
		#print(lat)
		#print(l_lats)

		if (lon > lonmax) lonmax <- lon
		if (lon < lonmin) lonmin <- lon
		if (lat > latmax) latmax <- lat
		if (lat < latmin) latmin <- lat

		# Nasty debug
		#if (as.character(objects [[i,1]]) == '857a6353fffffff') {
		#if (as.character(objects [[i,1]]) == '857a6a2bfffffff') {
		#	nastyDebug <- T
		#}
	  }
	}
	#AreaSize <- max(abs(lonmax-lonmin),abs(latmax-latmin))
	if (length(l_lons) > 1) {
	  #print(cbind(l_lons,l_lats))
	  #print(distm(cbind(l_lons,l_lats)))
	  AreaSize <-  max( distm(cbind(l_lons,l_lats)))

	  #todel
	  #print(sprintf("Area size:  %f ", AreaSize))
	  #AreaSize <- max(abs(lonmax-lonmin),abs(latmax-latmin))
	}
	else
	  AreaSize <- 0
  }
  else
	return(list(0.0,0.0,0.0))

  if (TotalNodes < 1) AreaSize <- -1

  if (nastyDebug == T) {
	print(sprintf("process_area: p1 (%f %f) p2 (%f %f) objects: %d total_nodes: %d, total_rb: %f, area_size: %f ------------------- ", c_area[[1]][1], c_area[[1]][2], c_area[[2]][1], c_area[[2]][2], numberofobj, TotalNodes, TotalRB, AreaSize))
  }

  result <- list(TotalNodes,TotalRB,AreaSize,l_nodes_list)
  return(result)
}

get_area_split_v <- function (c_area) {
  #m1p <- 	geosphere::midPoint(c_a[[1]],c(c_area[[2]][1],c_area[[1]][2]),a = 6378137, f=1/298.257223563)
  #m2p <- 	geosphere::midPoint(c(c_area[[1]][1],c_area[[2]][2]),c_area[[2]],a = 6378137, f=1/298.257223563)
  r1 <- c_area[[1]]
  mid_x <- round((c_area[[1]][1]+c_area[[2]][1])/2,5)
  r2 <- c(mid_x,c_area[[2]][2])
  r3 <- c(mid_x,c_area[[1]][2])
  r4 <-	c_area[[2]]
  result <- list(r1,r2,r3,r4)

  return(result)
}

get_area_split_h <- function (c_area) {
  #m1p <- 	geosphere::midPoint(c_area[[1]],c(c_area[[2]][1],c_area[[1]][2]),a = 6378137, f=1/298.257223563)
  #m2p <- 	geosphere::midPoint(c(c_area[[1]][1],c_area[[2]][2]),c_area[[2]],a = 6378137, f=1/298.257223563)
  r1 <- c_area[[1]]
  mid_y <- round((c_area[[1]][2]+c_area[[2]][2])/2,5)
  r2 <- c(c_area[[2]][1], mid_y)
  r3 <- c(c_area[[1]][1], mid_y)
  r4 <-	c_area[[2]]
  result <- list(r1,r2,r3,r4)

  return(result)
}


process_areas <- function (c_area,max_nodes_number_limit,max_rb_limit,max_dist,objects) {

  local_log <- F

  temp <- get_area_stats(c_area,objects)
  total_nodes <- temp[[1]]
  total_rb <- temp[[2]]
  area_size <- temp[[3]]
  nodes_index_list <- temp[[4]]
  obj_num <- nrow(objects)

  min_area_size <- 0 # in meters
  min_node_number <- 1 # nodes

  # find dome object fot debugging purpose
  nastyDebug2 <- F
  #if (!identical(which(objects$OBJECTNAME == "857a6a2bfffffff"),integer(0))) {
  #	nastyDebug2 <- T
  #		print(sprintf("Upper case, process_area: p1 (%f %f) p2 (%f %f) objects: %d total_nodes: %d limit: %d, total_rb: %f limit: %f, area_size: %f limit %f  ", c_area[[1]][1], c_area[[1]][2], c_area[[2]][1], c_area[[2]][2], obj_num, total_nodes, max_nodes_number_limit, total_rb, max_rb_limit, area_size, max_dist))
  #}

  if (local_log) {
	cat(sprintf("process_area: p1 (%f %f) p2 (%f %f) objects: %d total_nodes: %d limit: %d, total_rb: %f limit: %f, area_size: %f limit %f ------------------- ", c_area[[1]][1], c_area[[1]][2], c_area[[2]][1], c_area[[2]][2], obj_num, total_nodes, max_nodes_number_limit, total_rb, max_rb_limit, area_size, max_dist), file = "cluster-log.txt", sep = "\n", append = TRUE)
	#print(sprintf("process_area: p1 (%f %f) p2 (%f %f) total_nodes: %d, total_rb: %f, area_size: %f ------------------- ",c_area[[1]][1],c_area[[1]][2],c_area[[2]][1],c_area[[2]][2],total_nodes,total_rb,area_size))
  }

  if (total_nodes <= min_node_number) {
	#clust_list <<- c(clust_list,c_area)
	if (nastyDebug2) print('A1')
	if (local_log) {
	  #print(clust_list)
	  #cat(print(clust_list),file="cluster-list.txt",sep="\n",append=TRUE)
	  cat("few nodes", file = "cluster-list.txt", sep = "\n", append = TRUE)
	  cat("few nodes ... stop", file = "cluster-log.txt", sep = "\n", append = TRUE)
	}
	return(c_area)
  }

  if (area_size <= min_area_size) {
	#clust_list <<- c(clust_list,c_area)
	if (nastyDebug2) print('A2')
	if (local_log) {
	  #print(clust_list)
	  #cat(print(clust_list),file="cluster-list.txt",sep="\n",append=TRUE)
	  cat("small area", file = "cluster-list.txt", sep = "\n", append = TRUE)
	  cat("small area ... stop", file = "cluster-log.txt", sep = "\n", append = TRUE)
	}
	return(c_area)
  }

  if (total_nodes <= max_nodes_number_limit && total_rb <= max_rb_limit && area_size <= max_dist) {
	#clust_list <<- c(clust_list,c_area)
	if (nastyDebug2) print('A3')
	if (local_log) {
	  #print(clust_list)
	  cat("cond ok", file = "cluster-list.txt", sep = "\n", append = TRUE)
	  cat("cond ok", file = "cluster-log.txt", sep = "\n", append = TRUE)
	}
	return(c_area)
  }
  else {

	objects <- objects[nodes_index_list,]

	cond_nodes <- total_nodes <= max_nodes_number_limit
	cond_rb <- total_rb <= max_rb_limit
	cond_area <- area_size <= max_dist

	if (local_log) {
	  cat(sprintf("conds - nodes: %i rb: %i area: %i", cond_nodes, cond_rb, cond_area), file = "cluster-list.txt", sep = "\n", append = TRUE)
	  cat(sprintf("conds - nodes: %i rb: %i area: %i", cond_nodes, cond_rb, cond_area), file = "cluster-log.txt", sep = "\n", append = TRUE)
	}

	if (nastyDebug2) print('A4')
	#if (abs(c_area[[2]][1]-c_area[[1]][1]) >= abs(c_area[[1]][2]-c_area[[2]][2]))
	#    temp <- get_area_split_v(c_area) # result <- list(r1,r2,r3,r4)
	#else
	#    temp <- get_area_split_h(c_area)

	v_split_ok <- F
	temp <- get_area_split_v(c_area)   # Check Possibility for Split Verticaly
	pt_stats_1v <- get_area_stats(temp[1:2],objects)
	pt_stats_2v <- get_area_stats(temp[3:4],objects)
	if ((pt_stats_1v[[1]] >= min_node_number) && (pt_stats_1v[[3]] >= min_area_size) && (pt_stats_2v[[1]] >= min_node_number) && (pt_stats_2v[[3]] >= min_area_size))
	  v_split_ok <- T

	if (nastyDebug2)
	  print(sprintf("Check_v, (nn, area): p1 (%f %f) p2 (%f %f) %s", pt_stats_1v[[1]], pt_stats_1v[[3]], pt_stats_2v[[1]], pt_stats_2v[[3]], v_split_ok))

	max_dist_v <- max(pt_stats_1v[[3]],pt_stats_2v[[3]])
	if (nastyDebug2) print(max_dist_v)

	h_split_ok <- F
	temp <- get_area_split_h(c_area)   # Check Possibility for Split Horizontaly
	pt_stats_1h <- get_area_stats(temp[1:2],objects)
	pt_stats_2h <- get_area_stats(temp[3:4],objects)
	if ((pt_stats_1h[[1]] >= min_node_number) && (pt_stats_1h[[3]] >= min_area_size) && (pt_stats_2h[[1]] >= min_node_number) && (pt_stats_2h[[3]] >= min_area_size))
	  h_split_ok <- T

	if (nastyDebug2)
	  print(sprintf("Check_h, (nn, area): p1 (%f %f) p2 (%f %f) %s", pt_stats_1h[[1]], pt_stats_1h[[3]], pt_stats_2h[[1]], pt_stats_2h[[3]], h_split_ok))

	max_dist_h <- max(pt_stats_1h[[3]],pt_stats_2h[[3]])
	if (nastyDebug2) print(max_dist_h)

	if (h_split_ok && (v_split_ok == F))
	  temp <- get_area_split_h(c_area)

	if ((h_split_ok == F) && v_split_ok)
	  temp <- get_area_split_v(c_area)

	if (h_split_ok && v_split_ok) {
	  #if (abs(c_area[[2]][1]-c_area[[1]][1]) >= abs(c_area[[1]][2]-c_area[[2]][2])) # ?
	  if (max_dist_h > max_dist_v) # Chose better splitting methods
		temp <- get_area_split_v(c_area) # result <- list(r1,r2,r3,r4)
	  else
		temp <- get_area_split_h(c_area)
	}

	if ((h_split_ok == F) && (v_split_ok == F)) {
	  if (nastyDebug2) print('A5 no splitting ...')
	  if (local_log) {
		cat("spliting is not possible, minimum limits are reached", file = "cluster-list.txt", sep = "\n", append = TRUE)
		cat("spliting is not possible, minimum limits are reached", file = "cluster-log.txt", sep = "\n", append = TRUE)
	  }
	  # This trick was added to avoid big asymmetric clusters and long links !
	  # If normal splitting is not allowed lets split it and continue processing of only one branch, rejecting empty cluster
	  if (nastyDebug2) print('A5 force splitting ...')
	  if (max_dist_h > max_dist_v) {
		temp <- get_area_split_v(c_area)
		if ((pt_stats_1v[[1]] >= min_node_number) && (pt_stats_1v[[3]] >= min_area_size))
		  return(process_areas(temp[1:2], max_nodes_number_limit, max_rb_limit, max_dist, objects))
		else
		  return(process_areas(temp[3:4], max_nodes_number_limit, max_rb_limit, max_dist, objects))
	  } # result <- list(r1,r2,r3,r4)
	  else {
		temp <- get_area_split_h(c_area)
		if ((pt_stats_1h[[1]] >= min_node_number) && (pt_stats_1h[[3]] >= min_area_size))
		  return(process_areas(temp[1:2], max_nodes_number_limit, max_rb_limit, max_dist, objects))
		else
		  return(process_areas(temp[3:4], max_nodes_number_limit, max_rb_limit, max_dist, objects))
	  }
	  if ((pt_stats_1v[[1]] >= min_node_number) &&
		(pt_stats_1v[[3]] >= min_area_size) &&
		(pt_stats_2[[1]] >= min_node_number) &&
		(pt_stats_2[[3]] >= min_area_size))
		return(c_area)
	}

	temp_list_1 <- process_areas(temp[1:2],max_nodes_number_limit,max_rb_limit,max_dist,objects)
	temp_list_2 <- process_areas(temp[3:4],max_nodes_number_limit,max_rb_limit,max_dist,objects)
	return(append(temp_list_1,temp_list_2))
  }
  print("Clustering: Something going wrong !")
  return(0)
}


#Determination of the lattice cells number
formula_4_4_2 <- function (input, output, intermediate = NULL)
{
  req (input)

  TotalBandwidth <- 0
  TotalBandwidth <- input$Intermediate.TotalBandwidth

  WestLon <- 0
  WestLon <- input$Intermediate.WestLon

  EastLon <- 0
  EastLon <- input$Intermediate.EastLon

  NorthLat <- 0
  NorthLat <- input$Intermediate.NorthLat

  SouthLat <- 0
  SouthLat <- input$Intermediate.SouthLat


  if (!is.null(intermediate))
  {
	WestLon <- as.numeric (intermediate$WestLon)
	EastLon <- as.numeric (intermediate$EastLon)
	NorthLat <- as.numeric (intermediate$NorthLat)
	SouthLat <- as.numeric (intermediate$SouthLat)
	TotalBandwidth <- as.numeric (intermediate$TotalBandwidth)
  }


  #Calculation distance between top-left and top-right corners of the lattice

  LonFirst <- WestLon
  LatFirst <- NorthLat
  LonSecond <- EastLon
  LatSecond <- NorthLat

  top_left_to_top_right <- distm (c(as.numeric (LonFirst), as.numeric (LatFirst)),c(as.numeric (LonSecond), as.numeric (LatSecond)),fun=distHaversine)
  top_left_to_top_right <- round (as.numeric (top_left_to_top_right), digits = 2)

  #Calculation distance between bottom-left and bottom-right corners of the lattice

  LonFirst <- WestLon
  LatFirst <- SouthLat
  LonSecond <- EastLon
  LatSecond <- SouthLat

  bottom_left_to_bottom_right <- distm (c(as.numeric (LonFirst), as.numeric (LatFirst)),c(as.numeric (LonSecond), as.numeric (LatSecond)),fun=distHaversine)
  bottom_left_to_bottom_right <- round (as.numeric (bottom_left_to_bottom_right), digits = 2)

  #Identification maximum distance between left and right borders
  max_between_left_and_right <- max (top_left_to_top_right, bottom_left_to_bottom_right)

  #Calculation distance between top-left and bottom-left corners of the lattice

  LonFirst <- WestLon
  LatFirst <- NorthLat
  LonSecond <- WestLon
  LatSecond <- SouthLat

  top_left_to_bottom_left <- distm (c(as.numeric (LonFirst), as.numeric (LatFirst)),c(as.numeric (LonSecond), as.numeric (LatSecond)),fun=distHaversine)
  top_left_to_bottom_left <- round (as.numeric (top_left_to_bottom_left), digits = 2)

  #Calculation distance between top-right and bottom-right corners of the lattice

  LonFirst <- EastLon
  LatFirst <- NorthLat
  LonSecond <- EastLon
  LatSecond <- SouthLat

  top_right_to_bottom_right <- distm (c(as.numeric (LonFirst), as.numeric (LatFirst)),c(as.numeric (LonSecond), as.numeric (LatSecond)),fun=distHaversine)
  top_right_to_bottom_right <- round (as.numeric (top_right_to_bottom_right), digits = 2)


  #Identification maximum distance between top and bottom borders
  max_between_top_and_bottom <- max (top_left_to_bottom_left, top_right_to_bottom_right)

  #Identification possible number of cells between left and right borders

  max_cells_from_west_to_east <- round (as.numeric (max_between_left_and_right/input$TopologySettings.MaximumDistanceBetweenObjects), digits = 0)

  #Identification possible number of cells between top and bottom borders
  max_cells_from_north_to_south <- round (as.numeric (max_between_top_and_bottom/input$TopologySettings.MaximumDistanceBetweenObjects), digits = 0)

  max_cells_by_rb <- round (as.numeric (TotalBandwidth/input$TopologySettings.MaximumCumulativeBandwidth), digits = 0)

  result <- matrix (nrow = 4, ncol = 2)


  max_cells_by_area <- max_cells_from_west_to_east*max_cells_from_north_to_south

  area_rb_density <- max_cells_by_area / max_cells_by_rb

  if (area_rb_density < 1)
  {
	max_cells_from_west_to_east <- round (as.numeric (max_cells_from_west_to_east*area_rb_density), digits = 0)
	max_cells_from_north_to_south <- round (as.numeric (max_cells_from_north_to_south*area_rb_density), digits = 0)
  }

  result [1,1] <- i18n$t("Possible number of cells between left and right borders of lattice")
  result [1,2] <- as.numeric (max_cells_from_west_to_east)

  result [2,1] <- i18n$t("Possible number of cells between top and bottom borders of lattice")
  result [2,2] <- as.numeric (max_cells_from_north_to_south)

  result [3,1] <- i18n$t("Maximum distance between left and right borders")
  result [3,2] <- as.numeric (max_between_left_and_right)

  result [4,1] <- i18n$t("Maximum distance between top and bottom borders")
  result [4,2] <- as.numeric (max_between_top_and_bottom)


  return (result)
}


#Determination of the lattice cells borders coordinates
formula_4_4_3 <- function (input, output, intermediate = NULL)
{
  req (input)

  WestLon <- 0
  WestLon <- input$Intermediate.WestLon

  EastLon <- 0
  EastLon <- input$Intermediate.EastLon

  NorthLat <- 0
  NorthLat <- input$Intermediate.NorthLat

  SouthLat <- 0
  SouthLat <- input$Intermediate.SouthLat

  MaxLeftToRightCells <- 0
  MaxLeftToRightCells <- input$Intermediate.MaxLeftToRightCells

  MaxTopToBottomCells <- 0
  MaxTopToBottomCells <- input$Intermediate.MaxTopToBottomCells

  max_between_left_and_right <- 0
  max_between_left_and_right <- input$Intermediate.max_between_left_and_right

  max_between_top_and_bottom <- 0
  max_between_top_and_bottom <- input$Intermediate.max_between_top_and_bottom


  if (!is.null(intermediate))
  {

	WestLon <- as.numeric (intermediate$WestLon)
	EastLon <- as.numeric (intermediate$EastLon)
	NorthLat <- as.numeric (intermediate$NorthLat)
	SouthLat <- as.numeric (intermediate$SouthLat)
	MaxLeftToRightCells <- as.numeric (intermediate$max_cells_from_west_to_east)
	MaxTopToBottomCells <- as.numeric (intermediate$max_cells_from_north_to_south)
	max_between_left_and_right <- as.numeric (intermediate$max_between_left_and_right)
	max_between_top_and_bottom <- as.numeric (intermediate$max_between_top_and_bottom)
  }

  size_of_one_cell_from_left_to_right <- max_between_left_and_right/MaxLeftToRightCells

  size_of_one_cell_from_top_to_bottom <- max_between_top_and_bottom/MaxTopToBottomCells


  result <- matrix (nrow = MaxTopToBottomCells, ncol = MaxLeftToRightCells)

  rEarth <- 6371.01 # Earth's average radius in km
  epsilon <- 0.000001 # threshold for floating-point equality

  WestLonCur <- WestLon

  NorthLatCur <- NorthLat


  size_of_one_cell_from_left_to_right = size_of_one_cell_from_left_to_right / rEarth # normalize linear distance to radian angle

  size_of_one_cell_from_top_to_bottom = size_of_one_cell_from_top_to_bottom / rEarth # normalize linear distance to radian angle



  for (i in 1:MaxTopToBottomCells)
  {


	rlat1 = NorthLatCur*pi/180
	rlon1 = WestLonCur*pi/180

	rbearing = pi/2

	NorthLatCell <- NorthLatCur

	cosrbear <- cos(rbearing)


	sintoptobot <- sin(size_of_one_cell_from_top_to_bottom)


	cosrlat <- cos(rlat1)


	costoptobot <- cos(size_of_one_cell_from_top_to_bottom)


	sinlat <- sin(rlat1)


	rlat = asin(sinlat  * costoptobot  + cosrlat  * sintoptobot  *  cosrbear)



	SouthLatCell <- rlat*180/pi
	WestLonCur <- WestLon

	for (j in 1:MaxLeftToRightCells)
	{

	  rlon1 = WestLonCur*pi/180

	  rbearing = pi

	  coslat <- cos(rlat)


	  abscoslat <- abs(coslat)


	  sinrbearing <- sin(rbearing)


	  sinlefttoright <- sin(size_of_one_cell_from_left_to_right)


	  cosrlat <- cos(rlat)


	  valint <- (sinrbearing* sinlefttoright) / cosrlat


	  if (valint > 1)
		valint <- 1

	  if (valint < -1)
		valint <- -1

	  asin2 <- asin (valint)



	  elem1 <- (rlon1 - asin2 + pi )


	  if ((coslat == 0) || (abscoslat < epsilon)) # Endpoint a pole
		rlon=rlon1
	  else
		rlon = (elem1 %% (2*pi) ) - pi



	  WestLonCell <- WestLonCur
	  EastLonCell <- rlon*180/pi

	  result [i,j] <- paste ("Westernmost =", WestLonCell, "\r\n",
							 "Easternmost =", EastLonCell, "\r\n",
							 "Northernmost =", NorthLatCell, "\r\n",
							 "Southernmost =", SouthLatCell, "\r\n",sep = "")

	  WestLonCur <- EastLonCell


	}

	NorthLatCur <- SouthLatCell

  }

  return (result)
}

#Selecting of all objects in the cell by borders coordinates
formula_4_4_4 <- function (input, output, intermediate = NULL)
{
  req (input)


  result <- 0

  return (result)
}


algorithm4_4_impl <- function(input, output, intermediate = NULL)
{

  #Determination of the lattice borders coordinates
  borders =  formula_4_4_1 (input, output, intermediate)

  Westernmost <- borders [1,2]
  Easternmost <- borders [2,2]
  Northernmost <- borders [3,2]
  Southernmost <- borders [4,2]
  TotalBandwidth <- borders [5,2]

  # Determination of the lattice cells number


  intermediate2 <- list (TotalBandwidth = 0.0, WestLon = 0.0, EastLon = 0.0, NorthLat = 0.0, SouthLat = 0.0, max_cells_from_west_to_east = 0, max_cells_from_north_to_south = 0, max_between_left_and_right = 0, max_between_top_and_bottom = 0)

  intermediate2$TotalBandwidth <- TotalBandwidth
  intermediate2$WestLon <- Westernmost
  intermediate2$EastLon <- Easternmost
  intermediate2$NorthLat <- Northernmost
  intermediate2$SouthLat <- Southernmost

  maxcells =  formula_4_4_2 (input, output, intermediate2)

  max_cells_from_west_to_east <- as.numeric (maxcells [1,2])

  max_cells_from_north_to_south <- as.numeric (maxcells [2,2])

  max_between_left_and_right <- as.numeric (maxcells [3,2])
  max_between_top_and_bottom <- as.numeric (maxcells [4,2])


  intermediate2$max_cells_from_west_to_east <- max_cells_from_west_to_east
  intermediate2$max_cells_from_north_to_south <- max_cells_from_north_to_south

  intermediate2$max_between_left_and_right <- max_between_left_and_right
  intermediate2$max_between_top_and_bottom <- max_between_top_and_bottom

  result =  formula_4_4_3 (input, output, intermediate2)

  return (result)

}

algorithm4_4 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)

  switch (input$formula,
		  ALL = {

			.GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

			req (input$Files.ListOfObjects)

			result <- algorithm4_4_impl (input, output)

			output$c_names <- NULL
			output$data <- renderTable(result, colnames=FALSE)

			output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

		  },
		  FORMULA_4_4_1 = {#Determination of the lattice borders coordinates

			req (input$Files.ListOfObjects)
			objects <- vroom::vroom(input$Files.ListOfObjects, altrep = FALSE)
			req(objects)

			.GlobalEnv$mylog <- matrix("Detailed Calculation Log:")

			result <- formula_4_4_1 (input, output, objects)

			v1 <- result[[1]]
			#print(v1)
			cur_area <- result[[2]]
			area_stats <- result[[3]]
			clust_list <-  result[[3]]
			#print(cur_area)
			#print(borders)
			#browser()
			#m1p <- 	geosphere::midPoint(c(as.numeric(borders[1,2]),as.numeric(borders[3,2])),c(as.numeric(borders[2,2]),as.numeric(borders[3,2])),a = 6378137, f=1/298.257223563)
			#m2p <- 	geosphere::midPoint(c(as.numeric(borders[2,2]),as.numeric(borders[3,2])),c(as.numeric(borders[2,2]),as.numeric(borders[4,2])),a = 6378137, f=1/298.257223563)

			#main_area <- list(c(as.numeric (Westernmost), as.numeric (Northernmost)),c(as.numeric (Easternmost),as.numeric (Southernmost)))

			#areas_t <- get_area_split_h(cur_area)
			#area1 <- areas_t[1:2]
			#area2 <- areas_t[3:4]
			#print(area1)
			#areas_t <- get_area_split_v(area1)
			#area3 <- areas_t[1:2]
			#area4 <- areas_t[3:4]
			#print(area3)

			#0011DD
			map_4_4_1 <- leaflet(data=v1[,]) %>% addTiles() %>%
			  addMarkers(lng=v1$x, lat=v1$y, popup=v1$name) %>%
			  addRectangles(
				lng1=cur_area[[1]][1], lat1=cur_area[[1]][2],
				lng2=cur_area[[2]][1], lat2=cur_area[[2]][2],
				weight = 1,
				color = "#8B00DD",
				opacity = "90",
				fillColor = "transparent"
			  )
			#seq(from=1, to=length(clust_list), by=2)
			for (i in seq(from=1, to=length(clust_list), by=2)) {
			  l_area <- clust_list[[i]]
			  r_area <- clust_list[[i+1]]
			  map_4_4_1 <- addRectangles(map_4_4_1,
										 lng1 = as.numeric(l_area[1]), lat1 = as.numeric(l_area[2]),
										 lng2 = as.numeric(r_area[1]), lat2 = as.numeric(r_area[2]),
										 weight = 1,
										 color = "#8B00DD",
										 opacity = "90",
										 fillColor = "transparent"
			  )
			}
			#addMarkers(lng=v1$x, lat=v1$y) %>%
			#addProviderTiles(providers$Stamen.TonerLite) %>%
			#addAwesomeMarkers(lng=as.numeric(BaseNode[,2]), lat=as.numeric(BaseNode[,3]),
			#icon = awesomeIcons(markerColor = "red"),
			#popup=paste(iconv(BaseNode[,1], "UTF-8", "UTF-8",sub=''),"- BaseNode of the cluster"))

			# for(i in 1:nrow(v1)){
			# map_4_5_6 <- addPolylines(map_4_5_6, lng = c(as.numeric(v1$x[i]),as.numeric(BaseNode[,2])),
			# lat = c(as.numeric(v1$y[i]),as.numeric(BaseNode[,3])),
			# color = "#02F",
			# weight = 2,
			# opacity = 0.9)
			# }


			output$mymap <- renderLeaflet(map_4_4_1)
			#output$data <- renderTable(borders, colnames=FALSE)
			#output$data2 <- renderTable(rbind(m1p,m2p), colnames=FALSE)
			output$data <- renderTable(cur_area, colnames=FALSE)
			#output$data2 <- renderTable(area_stats, colnames=FALSE)
			output$data3 <- renderTable(clust_list, colnames=FALSE)

			output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)
		  },
		  FORMULA_4_4_2 = {#Determination of the lattice cells number

			req (input$Intermediate.TotalBandwidth)
			req (input$Intermediate.WestLon)
			req (input$Intermediate.EastLon)
			req (input$Intermediate.NorthLat)
			req (input$Intermediate.SouthLat)
			req (input$TopologySettings.MaximumDistanceBetweenObjects)
			req (input$TopologySettings.MaximumCumulativeBandwidth)


			.GlobalEnv$mylog <- matrix("Detailed Calculation Log:")

			result <- formula_4_4_2 (input, output)

			output$data <- renderTable(result, colnames=FALSE)
		  },
		  FORMULA_4_4_3 = {#Determination of the lattice cells borders coordinates

			req (input$Intermediate.WestLon)
			req (input$Intermediate.EastLon)
			req (input$Intermediate.NorthLat)
			req (input$Intermediate.SouthLat)
			req (input$Intermediate.MaxLeftToRightCells)
			req (input$Intermediate.MaxTopToBottomCells)
			req (input$Intermediate.max_between_left_and_right)
			req (input$Intermediate.max_between_top_and_bottom)


			.GlobalEnv$mylog <- matrix("Detailed Calculation Log:")

			result <- formula_4_4_3 (input, output)

			output$data <- renderTable(result, colnames=FALSE)
		  },
		  FORMULA_4_4_4 = {#Selecting of all objects in the cell by borders coordinates

			#req (input$GeneralVariables.STCPlaceChangeCoeff)
			req (input$Files.ListOfObjects)
			req (input$Intermediate.WestLon)
			req (input$Intermediate.EastLon)
			req (input$Intermediate.NorthLat)
			req (input$Intermediate.SouthLat)


			.GlobalEnv$mylog <- matrix("Detailed Calculation Log:")

			result <- formula_4_4_4 (input, output)

			output$data <- renderTable(result, colnames=FALSE)
		  },

		  stop ("No!")

  )
}
