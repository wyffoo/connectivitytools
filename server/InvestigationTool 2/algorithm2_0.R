library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")

i18n$set_translation_language("en")

#Algorithm for necessary network channel capacity calculation

erlang <- function (Y,C)
{
  req (Y)
  req (C)

  if (Y == 0)
  {
    return(0)
  }

  s <- 0

  for (i in 1:C)
  {
    s = (1.0 + s)*(i/Y)
  }

  return (1.0/(1.0 + s))
}

sessionscount <- function (Y,P)
{
  req (Y)
  req (P)

  l <- 0
  r <- round (Y, digits =0)

  fR <- erlang (Y,r)

  while (fR > P)
  {
    l = r
    r = r + 32
    fR = erlang (Y,r)
  }

  while ((r-l) > 1)
  {
    mid = round ((l+r)/2, digits = 0)

    fMid = erlang (Y, mid)

    if (fMid > P)
    {
      l = mid
    }
    else
    {
      r = mid
    }
  }

  return (r)
}


#Duration of the one session
formula_2_0_1 <- function (input, intermediate = NULL)
{
  req (input)


  result =  (8*input$InitialDataTraffic.InformationVolume)/input$InitialDataTraffic.BitrateOfService
  result <- round (as.numeric (result), digits = 4)

  return (result)
}

#Expected request intensity by users
formula_2_0_2 <- function (input, intermediate = NULL)
{
  req (input)


  result =  input$InitialDataTraffic.IntensivityPerUser*input$InitialDataTraffic.NumberOfSubscribers
  result <- round (as.numeric (result), digits = 4)

  return (result)
}

#External channel load by users
formula_2_0_3 <- function (input, intermediate = NULL)
{
  req (input)

  OveralIntensivity <- 0
  OveralIntensivity <- input$Intermediate.OveralIntensivity

  DurationOfOneSession <- 0
  DurationOfOneSession <- input$Intermediate.DurationOfOneSession


  if (!is.null(intermediate))
  {
    OveralIntensivity <- as.numeric (intermediate$OveralIntensivity)
    DurationOfOneSession <- as.numeric (intermediate$DurationOfOneSession)

  }

  result =  OveralIntensivity*(DurationOfOneSession/3600)
  result <- round (as.numeric (result), digits = 4)
  return (result)
}

#Number of parallel sessions
formula_2_0_4 <- function (input, intermediate = NULL)
{
  req (input)

  ExternalLoad <- 0
  ExternalLoad <- input$Intermediate.ExternalLoad


  if (!is.null(intermediate))
  {
    ExternalLoad <- as.numeric (intermediate$ExternalLoad)

  }

  result =  sessionscount (ExternalLoad, input$InitialDataTraffic.LevelOfQuality)
  result <- round (as.numeric (result), digits = 0)
  return (result)
}

#Overall network channel capacity
formula_2_0_5 <- function (input, intermediate = NULL)
{
  req (input)

  NumberOfSessions <- 0
  NumberOfSessions <- input$Intermediate.NumberOfSessions


  if (!is.null(intermediate))
  {
    NumberOfSessions <- as.numeric (intermediate$NumberOfSessions)

  }

  result =  NumberOfSessions*input$InitialDataTraffic.BitrateOfService
  result <- round (as.numeric (result), digits = 4)
  return (result)
}

#Necessary (sufficient) network channel capacity
formula_2_0_6 <- function (input, intermediate = NULL)
{
  req (input)

  OveralNetworkCapacity <- 0
  OveralNetworkCapacity <- input$Intermediate.OveralNetworkCapacity


  if (!is.null(intermediate))
  {
    OveralNetworkCapacity <- as.numeric (intermediate$OveralNetworkCapacity)

  }

  result =  OveralNetworkCapacity*1.2
  result <- round (as.numeric (result), digits = 4)
  return (result)
}


algorithm2_0_impl <- function(input, intermediate = NULL)
{


  result <- matrix (nrow = 1, ncol = 2)

  if (is.na(input$Files.Traffic))
  #if (!exists ("input$Files.Traffic"))
  {
    # Duration of the one session
    DurationOfOneSession =  formula_2_0_1 (input, intermediate)
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Duration of the one session, second"), DurationOfOneSession, sep = ": "))

    # Expected request intensity by users
    OveralIntensivity =  formula_2_0_2 (input, intermediate)
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Expected request intensity by users, requests/hour"), OveralIntensivity, sep = ": "))

    # External channel load by users
    intermediate2 <- list (DurationOfOneSession = 0.0, OveralIntensivity = 0.0)
    intermediate2$DurationOfOneSession <- DurationOfOneSession
    intermediate2$OveralIntensivity <- OveralIntensivity

    ExternalLoad =  formula_2_0_3 (input, intermediate2)
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("External channel load by users, Erlang"), ExternalLoad, sep = ": "))

    # Number of parallel sessions
    intermediate3 <- list (ExternalLoad = 0.0)
    intermediate3$ExternalLoad <- ExternalLoad

    NumberOfSessions =  formula_2_0_4 (input, intermediate3)
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Number of parallel sessions, sessions"), NumberOfSessions, sep = ": "))

    # Overall network channel capacity
    intermediate4 <- list (NumberOfSessions = 0.0)
    intermediate4$NumberOfSessions <- NumberOfSessions

    OveralNetworkCapacity =  formula_2_0_5 (input, intermediate4)
    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Overall network channel capacity, Mbit/s"), OveralNetworkCapacity, sep = ": "))

    # Necessary (sufficient) network channel capacity
    intermediate5 <- list (OveralNetworkCapacity = 0.0)
    intermediate5$OveralNetworkCapacity <- OveralNetworkCapacity


    result [1,1] = i18n$t("Necessary (sufficient) network channel capacity, Mbit/s")
    result [1,2] = formula_2_0_6 (input, intermediate5)

  }
  else
  {
    objects <- NULL

    objects <- vroom::vroom(input$Files.Traffic, altrep = FALSE)

    req (objects)

    numberofobj <- nrow(objects)

    rb <- 0
    total_numberofdevices <- 0
    total_bitrate <- 0
    total_intencity <- 0
    total_datavolume <- 0
    total_DurationOfOneSession <- 0
    total_NumberOfSessions <- 0

    for (i in 1: numberofobj)
    {

        numberofdevices <- as.numeric (objects [i,1])
        bitrate <- as.numeric (objects [i,2])

        intencity <- as.numeric (objects [i,3])
        datavolume <- as.numeric (objects [i,4])

        total_numberofdevices <- total_numberofdevices + numberofdevices
        total_bitrate <- total_bitrate + bitrate
        total_intencity <- total_intencity + intencity
        total_datavolume <- total_datavolume  + datavolume

        DurationOfOneSession <-  (8.38*datavolume)/bitrate

        total_DurationOfOneSession <- total_DurationOfOneSession + DurationOfOneSession

        OveralIntensivity <-  intencity*numberofdevices
        ExternalLoad <-  OveralIntensivity*(DurationOfOneSession/3600)
        NumberOfSessions <-  sessionscount (ExternalLoad, input$InitialDataTraffic.LevelOfQuality)

        total_NumberOfSessions <- total_NumberOfSessions + NumberOfSessions

        OveralNetworkCapacity <-  NumberOfSessions*bitrate

        if (exists ("bWriteLog", input))
          .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Group (User devices - Service) #"), i ,
                                                            i18n$t(": Number of devices:") ,
                                                            numberofdevices,
                                                            i18n$t(" ; Intensity per device, request/hour:"),
                                                            intencity,
                                                            i18n$t(" ; Bitrate per session, Mbps:"),
                                                            bitrate,
                                                            i18n$t(" ; Volume of data per one session, MB:"),
                                                            datavolume,
                                                            i18n$t(" ; Expected duration of one session, seconds:"),
                                                            DurationOfOneSession,
                                                            i18n$t(" ; Expected load by the group, Erl:"),
                                                            round (ExternalLoad, digits = 2),
                                                            i18n$t(" ; Expected number of simultaneous sessions, sessions:"),
                                                            NumberOfSessions,
                                                            i18n$t(" ; Required bandwidth for the Group, Mbps:"),
                                                            OveralNetworkCapacity,
                                                            sep = ""))

        rb <- rb + OveralNetworkCapacity
    }

   # rb <-  rb*1.2

    numberofrecords <- as.numeric(numberofobj)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("QoE degradation probability"), input$InitialDataTraffic.LevelOfQuality, sep = ": "))

    total_numberofdevices <- total_numberofdevices / numberofrecords
    total_numberofdevices <- round (total_numberofdevices, digits = 0)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Mean number of devices, devices"), total_numberofdevices, sep = ": "))


    total_bitrate <- total_bitrate / numberofrecords
    total_bitrate <- round (total_bitrate, digits = 4)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Mean bitrate per session, Mbit/s"), total_bitrate, sep = ": "))

    total_intencity <- total_intencity / numberofrecords
    total_intencity <- round (total_intencity, digits = 4)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Mean intensity per device, requests/hour"), total_intencity, sep = ": "))

    total_datavolume <- total_datavolume / numberofrecords
    total_datavolume <- round (total_datavolume, digits = 4)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Mean volume of data per one session, MB"), total_intencity, sep = ": "))

    total_DurationOfOneSession <- total_DurationOfOneSession / numberofrecords
    total_DurationOfOneSession <- round (total_DurationOfOneSession, digits = 4)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Mean duration of one session, second"), total_DurationOfOneSession, sep = ": "))

    total_NumberOfSessions <- total_NumberOfSessions / numberofrecords
    total_NumberOfSessions <- round (total_NumberOfSessions, digits = 0)

    if (exists ("bWriteLog", input))
      .GlobalEnv$mylog <- append(.GlobalEnv$mylog, paste (i18n$t("Mean number of simultaneous sessions, sessions"), total_NumberOfSessions, sep = ": "))

    result [1,1] = i18n$t("Necessary (sufficient) network channel capacity, Mbit/s")
    result [1,2] = rb
  }

  return (result)
}


algorithm2_0 <- function(input, output)
{
  req (input)
  req (input$formula)
  req (output)

  switch (input$formula,
          ALL = {

            req (input$InitialDataTraffic.InformationVolume)
            req (input$InitialDataTraffic.BitrateOfService)
            req (input$InitialDataTraffic.IntensivityPerUser)
            req (input$InitialDataTraffic.NumberOfSubscribers)
            req (input$InitialDataTraffic.LevelOfQuality)

            .GlobalEnv$mylog <- matrix(i18n$t("Detailed Calculation Log:"))

            result <- algorithm2_0_impl (input)

            output$c_names <- NULL
            output$data <- renderTable(result, colnames=FALSE)

            output$log <- renderTable(.GlobalEnv$mylog, colnames = FALSE)

          },
          FORMULA_2_0_1 = {#Duration of the one session

            req (input$InitialDataTraffic.InformationVolume)
            req (input$InitialDataTraffic.BitrateOfService)

            result <- formula_2_0_1 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_0_2 = {#Expected request intensity by users

            req (input$InitialDataTraffic.IntensivityPerUser)
            req (input$InitialDataTraffic.NumberOfSubscribers)

            result <- formula_2_0_2 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_0_3 = {#External channel load by users

            req (input$Intermediate.OveralIntensivity)
            req (input$Intermediate.DurationOfOneSession)

            result <- formula_2_0_3 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_0_4 = {#Number of parallel sessions

            req (input$Intermediate.ExternalLoad)
            req (input$InitialDataTraffic.LevelOfQuality)

            result <- formula_2_0_4 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_0_5 = {#Overall network channel capacity

            req (input$Intermediate.NumberOfSessions)
            req (input$InitialDataTraffic.BitrateOfService)

            result <- formula_2_0_5 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
          FORMULA_2_0_6 = {#Necessary (sufficient) network channel capacity

            req (input$Intermediate.OveralNetworkCapacity)

            result <- formula_2_0_6 (input)

            output$data <- renderTable(result, colnames=FALSE)
          },
         stop ("No!")

  )
}
