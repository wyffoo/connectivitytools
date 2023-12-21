#Execution: Rscript --default-packages="methods,dplyr,shiny,gmp,readr" --vanilla giga.R
suppressWarnings(suppressMessages(library("readr",quietly = T)))
suppressWarnings(suppressMessages(library("geosphere",quietly = T)))
suppressWarnings(suppressMessages(library("igraph",quietly = T)))
suppressWarnings(suppressMessages(library("jsonlite",quietly = T)))
suppressWarnings(suppressMessages(library("stringr",quietly = T)))
suppressWarnings(suppressMessages(library("dplyr",quietly = T)))
suppressWarnings(suppressMessages(library("leaflet",quietly = T)))
#suppressWarnings(suppressMessages(library("Rmpfr",quietly = T)))

args <- commandArgs(trailingOnly = TRUE)
object_id <- args[1]
input <- vroom::vroom(args[3], col_names = TRUE)
result <- c()
input$algorithm <- "ALL"

if (input$InitialData.EnableDetailedLog == '1') {
  input$bWriteLog <- T
}
input$TopologySettings.Algorithm <- '2'
if (args[2] == 'topology') {
  if (input$TopologySettings.Algorithm == '1') {
	input$Files.ListOfObjects <- args[4]
	source("method4.R")
	if (input$InitialData.NetInOutputOptimization == 'npv') {
	  input$InitialData.UseNPV <- 1
	} else {
	  input$InitialData.UseNPV <- 0
	}
	result$main <- method4_impl(input)
	result$status <- .GlobalEnv$top_calc_status
	result$summary <- .GlobalEnv$top_summary
	cat("--START:OBJECT_ID--\n")
	cat(0)
	cat("\n--END:OBJECT_ID--\n")
	cat("--START:STATUS--\n")
	cat(.GlobalEnv$top_calc_status)
	cat("\n--END:STATUS--\n")
	cat("--START:RESULTS--\n")
	cat(toJSON(result))
	cat("\n--END:RESULTS--\n")
	cat("--START:LOG--\n")
	# for (r in 1:nrow(.GlobalEnv$mylog))
	cat(toJSON(.GlobalEnv$mylog),"\n")
	cat("--END:LOG--\n")
	quit("no")
  } else {
	input$Files.ListOfObjects <- args[4]
	source("method4.R")
	if (input$InitialData.NetInOutputOptimization == 'npv') {
	  input$InitialData.UseNPV <- 1
	} else {
	  input$InitialData.UseNPV <- 0
	}
	tmp_result <- algorithm4_5_impl(input)
	#cat(toJSON(tmp_result[8]))
	#my_map_lines, my_markers, my_awe_markers
	result$summary <- tmp_result[5]
	result$map_lines <- tmp_result[7]
	result$map_markers <- tmp_result[8]
	result$map_awe_markers <- tmp_result[9]
	#cat(toJSON(result))
	#  result$status <- .GlobalEnv$top_calc_status
	#result$summary <- .GlobalEnv$top_summary
	cat("--START:OBJECT_ID--\n")
	cat(0)
	cat("\n--END:OBJECT_ID--\n")
	cat("--START:STATUS--\n")
	cat(.GlobalEnv$top_calc_status)
	cat("\n--END:STATUS--\n")
	cat("--START:RESULTS--\n")
	cat(toJSON(result))
	cat("\n--END:RESULTS--\n")
	cat("--START:LOG--\n")
	# for (r in 1:nrow(.GlobalEnv$mylog))
	cat(toJSON(.GlobalEnv$mylog),"\n")
	cat("--END:LOG--\n")
	quit("no")
  }
} else if (args[2] == 'dtf') {
  input$Files.ListOfObjects <- args[4]
  source("algorithm4_1.R")
  result$main <- formula_4_1_3(input)
  result$status <- .GlobalEnv$top_calc_status
  cat("--START:OBJECT_ID--\n")
  cat(0)
  cat("\n--END:OBJECT_ID--\n")
  cat("--START:STATUS--\n")
  cat(.GlobalEnv$top_calc_status)
  cat("\n--END:STATUS--\n")
  cat("--START:RESULTS--\n")
  cat(toJSON(result))
  cat("\n--END:RESULTS--\n")
  cat("--START:LOG--\n")
  # for (r in 1:nrow(.GlobalEnv$mylog))
  cat(toJSON(.GlobalEnv$mylog),"\n")
  cat("--END:LOG--\n")
  quit("no")
} else {
  template_name <- args[2]
}

#Calculate traffic if file is not null
if (args[4] != "null") {
	input$Files.Traffic <- args[4]
}

if (input$InitialData.CalculateElectricity == '1') {
  input$SchoolSpecificData.IsEnergyInSchool <- '0'
} else {
  input$SchoolSpecificData.IsEnergyInSchool <- '1'
}

if (input$CountriesDataType == 'city') {
  source("method1.R")
  result$main <- method1_impl(input)
} else if (template_name == "SimpleBBCalcOnly") {
  source("method2.R")
  required_bandwidth <- algorithm2_0_impl(input)
  result$required_bandwidth <- as.numeric(required_bandwidth[, -1])
  input$SchoolSpecificData.RequiredBandwidth <- result$required_bandwidth
  result$main <- method2_impl(input)
} else if (template_name == "ExtendedBBCalcOnly") {
  source("method2.R")
  required_bandwidth <- algorithm2_0_impl(input)
  result$required_bandwidth <- as.numeric(required_bandwidth[, -1])
  input$SchoolSpecificData.RequiredBandwidth <- result$required_bandwidth
  result$main <- method2_impl(input)
} else if (template_name == "SimpleBBCalcLAN") {
  source("method2.R")
  required_bandwidth <- algorithm2_0_impl(input)
  result$required_bandwidth <- as.numeric(required_bandwidth[, -1])
  input$SchoolSpecificData.RequiredBandwidth <- result$required_bandwidth
  result$main <- method2_impl(input)

  source("method3.R")
  result$costs <- method3_impl(input)

} else if (template_name == "ExtendedBBCalcLAN") {
  source("method2.R")
  required_bandwidth <- algorithm2_0_impl(input)
  result$required_bandwidth <- as.numeric(required_bandwidth[, -1])
  input$SchoolSpecificData.RequiredBandwidth <- result$required_bandwidth
  result$main <- method2_impl(input)

  source("method3.R")
  result$costs <- method3_impl(input)


} else if (template_name == "BBRBOnly") {
  source("method2.R")
  result$main <- method2_impl(input)


  #METHOD2 - сюда матрица трафик не нужна
  #METHOD4 - для топологии (если выбрано, но пока не доделано)
  #Для этого шаблона у нас будет результат только из метода METHOD2
  #Просто весь метод вызываем
  #Результат складываем так
  #Колонка 11  <- result (1)
  #Колонка 12  <- result (2)
  #Колонка 13  <- result (3)
  #Колонка 14 <- result (4)
  #Колонка 15  <- result (5)
  #Колонка 16   <- result (6)
  #source("method2.R")
  #result <- method2_impl(input)

} else if (template_name == "SimpleBBRBLAN" || template_name == "ExtendedBBRBLAN") {
  source("method2.R")
  result$main <- method2_impl(input)

  source("method3.R")
  result$costs <- method3_impl(input)

} else if (template_name == "SimpleLANOnly" || template_name == "ExtendedLANOnly") {
  source("method3.R")
  result$costs <- method3_impl(input)
}

#Removing names, keeping only array of values
result$main <- result$main[, -1]
result$costs <- result$costs[, -1]

#Process Electricity results
#1/2 CAPEX/OPEX Fiber optical cable line
#3/4 CAPEX/OPEX Microwave radio link
#5/6 CAPEX/OPEX Satellite communication link
#7/8 CAPEX/OPEX Mobile cellular network
#Save global log to result
result$status <- .GlobalEnv$top_calc_status
cat("--START:OBJECT_ID--\n")
cat(object_id)
cat("\n--END:OBJECT_ID--\n")
cat("--START:STATUS--\n")
cat(.GlobalEnv$top_calc_status)
cat("\n--END:STATUS--\n")
cat("--START:RESULTS--\n")
cat(toJSON(result, pretty = FALSE))
cat("\n--END:RESULTS--\n")
cat("--START:LOG--\n")
cat(.GlobalEnv$mylog, sep = "\n")
cat("--END:LOG--\n")
