# Copyright (c) Microsoft Corporation.  All rights reserved.

# Third Party Programs. This software enables you to obtain software applications from other sources. 
# Those applications are offered and distributed by third parties under their own license terms.
# Microsoft is not developing, distributing or licensing those applications to you, but instead, 
# as a convenience, enables you to use this software to obtain those applications directly from 
# the application providers.
# By using the software, you acknowledge and agree that you are obtaining the applications directly
# from the third party providers and under separate license terms, and that it is your responsibility to locate, 
# understand and comply with those license terms.
# Microsoft grants you no license rights for third-party software or applications that is obtained using this software.

#
# WARNINGS:   
#
# CREATION DATE: 06/01/2017
#
# LAST UPDATE: 08/12/2017
#
# VERSION: 1.0.0
#
# R VERSION TESTED: 3.4.0, 3.3.4, MRO 3.2.2
# 
# AUTHOR: pbicvsupport@microsoft.com
#
# REFERENCES: https://robjhyndman.com/papers/ComplexSeasonality.pdf

Sys.setlocale("LC_ALL","English") # Internationalization 

############ User Parameters #########


whichInfo = "none"
if(exists("settings_info_params_whichInfo"))
  whichInfo = settings_info_params_whichInfo

##PBI_PARAM: Show cumulative value inside shown period (actual + predicted)?
#Type:logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
showInfoCumSum = FALSE

##PBI_PARAM: Show TBATS method selected
#Type:logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
showInfoMethodTBATS = FALSE

##PBI_PARAM: Show information criterion of the found model
#Type:logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
showInfoCriterion = FALSE

if(whichInfo == "AIC")
{
  showInfoCriterion = TRUE
}else{
  if(whichInfo == "cumulative")
    showInfoCumSum = TRUE
  else
    if(whichInfo == "method")
      showInfoMethodTBATS = TRUE}



##PBI_PARAM: Forecast length
#Type:integer, Default:500, Range:NA, PossibleValues:NA, Remarks: NULL means choose forecast length automatically
forecastLength=500
if(exists("settings_forecastPlot_params_forecastLength"))
{
  forecastLength = as.numeric(settings_forecastPlot_params_forecastLength)
  if(is.na(forecastLength))
    forecastLength = 10
  forecastLength = round(max(min(forecastLength,1e+6),1))
}

##PBI_PARAM: Seasonal factor #1
#Type:enum, Default:"none", Range:NA, PossibleValues:none,manual, hour, day,...,year 
targetSeason1 = "none"
if(exists("settings_forecastPlot_params_targetSeason1"))
{
  targetSeason1 = settings_forecastPlot_params_targetSeason1
}

##PBI_PARAM: Seasonal factor #2
#Type:enum, Default:"none", Range:NA, PossibleValues:none,manual, hour, day,...,year 
targetSeason2 = "none"
if(exists("settings_forecastPlot_params_targetSeason2"))
{
  targetSeason2 = settings_forecastPlot_params_targetSeason2
}

##PBI_PARAM: Number of data points in smaller season period
#Type:integer, Default:1, Range:NA, PossibleValues:NA, 
freq1=1
if(exists("settings_forecastPlot_params_freq1"))
{
  freq1 = as.numeric(settings_forecastPlot_params_freq1)
  if(is.na(freq1))
    freq1 = 1
  freq1 = round(max(min(freq1,1e+6),1))
}

##PBI_PARAM: Number of data points in larger season period
#Type:integer, Default:1, Range:NA, PossibleValues:NA, 
freq2=1
if(exists("settings_forecastPlot_params_freq2"))
{
  freq2 = as.numeric(settings_forecastPlot_params_freq2)
  if(is.na(freq2))
    freq2 = 1
  freq2 = round(max(min(freq2,1e+6),1))
}

##PBI_PARAM: Confidence level
#Type:number, Default:0.5, Range:[0,1], PossibleValues:NA, 
confInterval1 = 0.5
if (exists("settings_conf_params_confInterval1")) 
{ 
  confInterval1 = as.numeric(settings_conf_params_confInterval1)
  if(is.na(confInterval1))
    confInterval1 = 0.5
}

##PBI_PARAM: Confidence level
#Type:number, Default:0.995, Range:[0,1], PossibleValues:NA, 
confInterval2 = 0.995
if (exists("settings_conf_params_confInterval2")) 
{ 
  confInterval2 = as.numeric(settings_conf_params_confInterval2)
  if(is.na(confInterval2))
    confInterval1 = 0.995
}
#swap if required
if(confInterval1>confInterval2)
{
  temp = confInterval2
  confInterval2 = confInterval1
  confInterval1 = temp
}

lowerConfInterval = confInterval1
upperConfInterval = confInterval2


##PBI_PARAM: Show period
#Type:string, Default:"all", Range:NA, PossibleValues:('all','hour','mday','week','mon','year') 
showFromTo = "all" 
if(exists("settings_graph_params_showFromTo"))
  showFromTo = settings_graph_params_showFromTo

possibleFromTo = c('all','hour','mday','week','mon','year')
if(!(showFromTo %in% possibleFromTo))
  showFromTo = possibleFromTo[1]


##PBI_PARAM: Shift period (for example 24 to start week at monday)
#Type:number, Default:0, Range:NA, PossibleValues:NA 
refPointShift = 0
if(exists("settings_graph_params_refPointShift"))
  refPointShift = settings_graph_params_refPointShift

##PBI_PARAM: Show fitted 
#Type:bool, Default:"F", Range:NA, PossibleValues:NA
showInPlotFitted = FALSE
if(exists("settings_graph_params_showInPlotFitted"))
  showInPlotFitted = settings_graph_params_showInPlotFitted

##PBI_PARAM: Show fitted 
#Type:bool, Default:"F", Range:NA, PossibleValues:NA
valuesNonNegative = FALSE
if(exists("settings_additional_params_valuesNonNegative"))
  valuesNonNegative = settings_additional_params_valuesNonNegative

##PBI_PARAM: Use par. processing 
#Type:bool, Default:"F", Range:NA, PossibleValues:NA
useParProc = FALSE
if(exists("settings_additional_params_useParProc"))
  useParProc = settings_additional_params_useParProc

##PBI_PARAM: fast algo?
#Type:bool, Default:"F", Range:NA, PossibleValues:NA
algModeFast = TRUE
if(exists("settings_additional_params_algModeFast"))
  algModeFast = settings_additional_params_algModeFast

##PBI_PARAM: Enables user to force certain type of X-ticks formats
#Type:string, Default:"auto", Range:NA, PossibleValues:
userFormatX = "auto" # 
if(exists("settings_axes_params_userFormatX"))
  userFormatX = settings_axes_params_userFormatX

##PBI_PARAM: Y axis numbers format
#Type:bool, Default:"F", Range:NA, PossibleValues:NA
showScientificY = FALSE
if(exists("settings_axes_params_showScientificY"))
  showScientificY = settings_axes_params_showScientificY

##PBI_PARAM: Y axis label col
#Type:string, Default:"black", Range:NA, PossibleValues:NA
labelsTextCol = "black"
if(exists("settings_axes_params_labelsTextCol"))
  labelsTextCol = settings_axes_params_labelsTextCol

##PBI_PARAM: labels text size
#Type:number, Default:1.2, Range:[8,50]/12, PossibleValues:NA
sizeLabel = 12
if(exists("settings_axes_params_textSize"))
  sizeLabel = as.numeric(settings_axes_params_textSize)

##PBI_PARAM Color of time series line
#Type:string, Default:"orange", Range:NA, PossibleValues:"orange","blue","green","black"
pointsCol = "orange"
if(exists("settings_graph_params_dataCol"))
  pointsCol = settings_graph_params_dataCol

##PBI_PARAM Color of forecast line
#Type:string, Default:"red", Range:NA, PossibleValues:"red","blue","green","black"
forecastCol = "red"
if(exists("settings_graph_params_forecastCol"))
  forecastCol = settings_graph_params_forecastCol


fittedCol = "green"
if(exists("settings_graph_params_fittedCol"))
  fittedCol = settings_graph_params_fittedCol

#PBI_PARAM Transparency of scatterplot points
#Type:numeric, Default:0.4, Range:[0,1], PossibleValues:NA, Remarks: NA
transparency = 1
if(exists("settings_graph_params_percentile"))
  transparency = as.numeric(settings_graph_params_percentile)/100

#PBI_PARAM Size of points on the plot
#Type:numeric, Default: 1 , Range:[0.1,5], PossibleValues:NA, Remarks: NA
pointCex = 1
if(exists("settings_graph_params_weight"))
  pointCex = as.numeric(settings_graph_params_weight)/10

#PBI_PARAM Size of subtitle on the plot
#Type:numeric, Default: 0.75 , Range:[0.1,5], PossibleValues:NA, Remarks: NA
cexSub = 0.75
if(exists("settings_info_params_textSize"))
  cexSub = as.numeric(settings_info_params_textSize)/12


numDigitsInfo = 0
if(exists("settings_info_params_numDigitsInfo"))
  numDigitsInfo = as.numeric(settings_info_params_numDigitsInfo)

##PBI_PARAM Color of info text
#Type:string, Default:"red", Range:NA, PossibleValues:"red","blue","green","black"
infoTextCol = "gray50"
if(exists("settings_info_params_infoTextCol"))
  infoTextCol = settings_info_params_infoTextCol

##PBI_PARAM: export out data to HTML?
#Type:logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
keepOutData = FALSE
if(exists("settings_export_params_show"))
  keepOutData = settings_export_params_show 

##PBI_PARAM: method of export interface
#Type: string , Default:"copy",  Range:NA, PossibleValues:"copy", "download",  Remarks: NA
exportMethod = "copy"
if(exists("settings_export_params_method"))
  exportMethod = settings_export_params_method 

##PBI_PARAM: limit the out table exported
#Type: string , Default:1000,  Range:NA, PossibleValues:"1000", "10000", Inf,  Remarks: NA
limitExportSize = 10000
if(exists("settings_export_params_limitExportSize"))
  limitExportSize = as.numeric(settings_export_params_limitExportSize)


###############Internal parameters definitions#################

#PBI_PARAM Minimal number of points
#Type:integer, Default:15, Range:[5,50], PossibleValues:NA, Remarks: NA
minPoints = 15


#PBI_PARAM Shaded band for confidence interval
#Type:logical, Default:TRUE, Range:NA, PossibleValues:NA, Remarks: NA
fillConfidenceLevels=TRUE



#PBI_PARAM Size of warnings font
#Type:numeric , Default:cexSub*12, Range:NA, PossibleValues:[1,50], Remarks: NA
sizeWarn = cexSub * 12

#PBI_PARAM Size of ticks on axes 
#Type:numeric , Default:8, Range:NA, PossibleValues:[1,50], Remarks: NA
sizeTicks = 8

##PBI_PARAM: Should warnings text be displayed?
#Type:logical, Default:TRUE, Range:NA, PossibleValues:NA, Remarks: NA
showWarnings = TRUE

#PBI_PARAM opacity of conf interval color
transparencyConfInterval = 0.3 

###############Library Declarations###############
###############Internal functions definitions#################

source('./r_files/utils.r')
source('./r_files/flatten_HTML.r')

libraryRequireInstall("plotly")
libraryRequireInstall("caTools")

###############Upfront input correctness validations (where possible)#################

pbiWarning = NULL
useParallel = useParProc
showInfo = any(c(showInfoCumSum, showInfoCriterion, showInfoMethodTBATS))

if(!exists("Date") || !exists("Value"))
{
  dataset=data.frame()
  pbiWarning  = cutStr2Show("Both 'Date' and 'Value' fields are required.", strCex = 0.85)
  timeSeries = ts()
  showWarnings = TRUE
}else{
  dataset = cbind(Date,Value)
  dataset <- dataset[complete.cases(dataset),] #remove corrupted rows
  labTime = "Time"
  labValue = names(dataset)[ncol(dataset)]
  
  dataset[,2] = as.numeric(dataset[,2])
  N = nrow(dataset)
  
  if(N == 0 && exists("Date") && nrow(Date) > 0 &&  exists("Value")){
    pbiWarning1  = cutStr2Show("Wrong date type. Only 'Date', 'Time', 'Date/Time' are allowed without hierarchy", strCex = 0.85)
    pbiWarning = paste(pbiWarning1, pbiWarning, sep ="\n")
    timeSeries = ts()
    showWarnings = TRUE
  }else {
    dataset = dataset[order(dataset[,1]),]
    parsed_dates = strptime(dataset[,1], "%Y-%m-%dT%H:%M:%S", tz="UTC")
    labTime = names(Date)[1]
    
    if((any(is.na(parsed_dates))))
    {
      pbiWarning1  = cutStr2Show("Wrong or corrupted 'Date'.", strCex = 0.85)
      pbiWarning2  = cutStr2Show("Only 'Date', 'Time', 'Date/Time' types are allowed without hierarchy", strCex = 0.85)
      pbiWarning = paste(pbiWarning1, pbiWarning2, pbiWarning, sep ="\n")
      timeSeries=ts()
      showWarnings=TRUE
    }
    else
    {
      interval = difftime(parsed_dates[length(parsed_dates)],parsed_dates[1])/(length(parsed_dates)-1) # force equal spacing 
      myFreq1 = getFrequency1(parsed_dates = parsed_dates, values = dataset[,2],
                              tS = targetSeason1, f = freq1)
      myFreq2 = getFrequency1(parsed_dates = parsed_dates, values = dataset[,2],
                              tS = targetSeason2, f = freq2)
      timeSeries=ts(data = dataset[,2], start=1, frequency = round(myFreq1))
    }
  }
}



##############Main Visualization script###########

pbiInfo = NULL

gooodpd = goodPlotDimension(3.25, 2.5)

if(length(timeSeries) >= minPoints && gooodpd) {
  
  # compute part of dates to show
  actTimes = as.POSIXlt(seq(from=parsed_dates[1], to = parsed_dates[length(parsed_dates)], length.out = length(parsed_dates)))
  allTimes = as.POSIXlt(seq(from=parsed_dates[1], to = (parsed_dates[length(parsed_dates)]+interval*(forecastLength)), length.out = length(parsed_dates)+forecastLength))
  fFromTo = indexShowFromTo(showFromTo,actTimes, allTimes, refPointShift)
  myInclude = length(actTimes) - fFromTo[1] + 1
  myForecastLength = min(forecastLength,fFromTo[2] - length(actTimes)) 
  
  if(myForecastLength == 0)# need to forecast next day/week/etc
  {
    myForecastLength = myInclude
    fFromTo = c(length(actTimes)+1,length(actTimes)+ myForecastLength)
    myInclude = 0
    myForecastLength = min(forecastLength,myForecastLength) 
  }
  
  freqs = joinFreq(myFreq1, myFreq2)
  timeSeries = msts(dataset[,2], seasonal.periods=freqs, start= (-N+myInclude ) / max(freqs) )#
  
  if(algModeFast)# mode with preselected parameters
    fit <- tbats(timeSeries, use.box.cox = FALSE, use.trend = FALSE, use.damped.trend = FALSE,
                 use.arma.errors = FALSE, max.p= 2, max.q = 2,
                 max.P = 1, max.Q = 1, max.order= 3, max.d = 1, max.D = 0, use.parallel = useParallel )
  else
    fit <- tbats(timeSeries, use.parallel = useParallel) 
  
  #TODO: add user mode with all tbats params from GUI
  
  if(lowerConfInterval == 0)
    lowerConfInterval = NULL; 
  
  if (is.null(forecastLength))
    prediction = forecast(fit, level=c(lowerConfInterval, upperConfInterval))
  else
    prediction = forecast(fit, level=c(lowerConfInterval, upperConfInterval), h = myForecastLength)
  
  if(valuesNonNegative)
  {
    prediction$mean[prediction$mean < 0] = 0
    prediction$upper[prediction$upper < 0] = 0
    prediction$lower[prediction$lower < 0] = 0
    prediction$fitted[prediction$fitted < 0] = 0
  }
  
  inI = seq(from = fFromTo[1],length.out = myInclude)
  
  #calculate cumulative forecast
  tempVal = sum(dataset[inI, 2])
  if(is.na(tempVal))
    tempVal = 0
  myCumSum = sum(prediction$mean) + tempVal
  
  
  if(showInfo && showInfoMethodTBATS)
    pbiInfo=paste(pbiInfo,"", prediction$method, "", sep="")
  
  if(showInfoCumSum)
    pbiInfo=paste(pbiInfo, "Cumulative forecast: ", format(myCumSum, digits=4, nsmall = numDigitsInfo, scientific = F,  big.mark       = ","),"", sep="")
  
  if(showInfoCriterion)
    pbiInfo=paste(pbiInfo, "AIC: ", format(fit$AIC, digits=4, nsmall = numDigitsInfo, scientific = F,  big.mark   = ","), "", sep="")
  
  pbiInfo = cutStr2Show(pbiInfo,strCex = cexSub,isH = TRUE, maxChar = 5, partAvailable = 0.9)
  
  #axes labels
  labTimeShort = cutStr2Show(labTime, strCex = sizeLabel / 6, isH = TRUE, partAvailable = 0.8)
  labValueShort = cutStr2Show(labValue, strCex = sizeLabel / 6, isH = FALSE, partAvailable = 0.75)
  
  NpF = myInclude + myForecastLength
  
  par(mar = c(5+showInfo,6 + (1 - showScientificY) , 1, 2))
  
  #format  x_with_f
  numTicks = FindTicksNum1(NpF, max(freqs)) # find based on plot size
  numTicks = min(numTicks, NpF)
  
  fromDate = allTimes[fFromTo[1]]
  toDate = allTimes[fFromTo[2]]
  
  x_with_f_exist = as.POSIXlt(seq(from=fromDate, to = toDate, by = interval))
  iii = unique(round(seq(from = 1, to = length(x_with_f_exist), length.out = numTicks)))
  x_with_f = x_with_f_exist[iii]
  
  if(userFormatX == "auto")
    userFormatX = NULL;
  
  x_with_forcast_formatted = flexFormat(dates = x_with_f, orig_dates = parsed_dates, freq = max(freqs), myformat = userFormatX)
  
  #format  
  f_full = as.POSIXlt(seq(from=tail(parsed_dates,1), to = (tail(parsed_dates,1)+interval*(forecastLength)), length.out = forecastLength+1))
  
  #historical data
  x1 = seq(0,length.out = length(prediction$x[inI]))
  y1 = as.numeric(prediction$x[inI])
  
  #forecast
  x2 = seq(length(prediction$x[inI]),length.out = length(prediction$mean))
  y2 = as.numeric(prediction$mean)
  
  #fitted data 
  y3 = as.numeric(prediction$fitted[inI])
  
  # ggplot construction  
  p1a <- ggplot(data = NULL,aes(x = x1, y = y1) )
  
  if(sum(!is.na(y1)) > 1)
    p1a<-p1a+geom_line(col = alpha(pointsCol, transparency), lwd = pointCex)
  else
    p1a<-p1a+geom_point(col = alpha(pointsCol, transparency), size = pointCex)
  
  if(showInPlotFitted)
  {
    if(sum(!is.na(y3))>1)
      p1a <- p1a + geom_line(inherit.aes = FALSE ,data = NULL, mapping = aes(x = x1, y = y3), col=alpha(fittedCol,transparency), lty = 2,  lwd = pointCex * 0.75)
    else
      p1a <- p1a + geom_point(inherit.aes = FALSE ,data = NULL, mapping = aes(x = x1, y = y3), col=alpha(fittedCol,transparency), size = pointCex)
  }
  
  if(length(y2)>1)
    p1a <- p1a + geom_line(inherit.aes = FALSE ,data = NULL, mapping = aes(x = x2, y = y2), col=alpha(forecastCol,transparency), lwd = pointCex)
  else
    p1a <- p1a + geom_point(inherit.aes = FALSE ,data = NULL, mapping = aes(x = x2, y = y2), col=alpha(forecastCol,transparency), size = pointCex)
  
  #conf intervals
  if(!is.null(lowerConfInterval))
  {
    lower2 = lower1 = as.numeric(prediction$lower[,1])
    upper2 = upper1 = as.numeric(prediction$upper[,1])
    id = x2
    cf_full = as.character(f_full)
    p1a <- p1a + geom_ribbon( inherit.aes = FALSE , mapping = aes(x = id, ymin = lower1 , ymax = upper1), fill = "blue4", alpha = 0.25)
  }
  
  if(upperConfInterval>0.01)
  {
    if(!is.null(lowerConfInterval))
    {  
      lower2 = as.numeric(prediction$lower[,2])
      upper2 = as.numeric(prediction$upper[,2])
    }
    else
    {  
      lower1 = lower2 = as.numeric(prediction$lower[,1])
      upper1 = upper2 = as.numeric(prediction$upper[,1])
    } 
    
    id = x2
    names(lower2) = names(upper2) = names(lower1) = names(upper1)=  names(f_full) = id 
    cf_full = as.character(f_full)
    p1a <- p1a + geom_ribbon( inherit.aes = FALSE , mapping = aes(x = id, ymin = lower2, ymax = upper2), fill = "gray50", alpha = 0.25)
  }
  
  #design 
  p1a <- p1a + labs (title = pbiInfo, caption = NULL) + theme_bw() 
  p1a <- p1a + xlab(labTimeShort) + ylab(labValueShort) 
  p1a <- p1a + scale_x_continuous(breaks = seq(0,length(prediction$x[inI]) + length(prediction$mean)-1, length.out = numTicks), labels = x_with_forcast_formatted) 
  p1a <- p1a +  theme(axis.text.x  = element_text(angle = getAngleXlabels1(x_with_forcast_formatted), 
                                                  hjust=1, size = sizeTicks, colour = "gray60"),
                      axis.text.y  = element_text(vjust = 0.5, size = sizeTicks, colour = "gray60"),
                      plot.title  = element_text(hjust = 0.5, size = sizeWarn, colour = infoTextCol), 
                      axis.title=element_text(size =  sizeLabel, colour = labelsTextCol),
                      axis.text=element_text(size =  sizeTicks,  colour = labelsTextCol),
                      panel.border = element_blank())
  
} else{ #empty plot
  
  #empty plot
  showWarnings = TRUE
  if(gooodpd)
    pbiWarning1  = cutStr2Show("Not enough data points", strCex = sizeWarn / 6, partAvailable = 0.85)
  else
  {
    pbiWarning1 = "Visual is "
    pbiWarning1 = cutStr2Show(pbiWarning1, strCex = sizeWarn / 6, partAvailable = 0.9)
    pbiWarning2 = "too small "
    pbiWarning2 = cutStr2Show(pbiWarning2, strCex = sizeWarn / 6, partAvailable = 0.9)
    pbiWarning1 <- paste(pbiWarning1, "<br>", pbiWarning2, sep="")
    sizeWarn = 7 #smaller
  }
  pbiWarning<-paste(pbiWarning, pbiWarning1 , sep="<br>")
  
}
#add warning as subtitle (or upper title since plotly has bug)
if(showWarnings && !is.null(pbiWarning))
{
  p1a = ggplot() + labs (title = pbiWarning, caption = NULL) + theme_bw() +
    theme(plot.title  = element_text(hjust = 0.5, size = sizeWarn), 
          axis.title = element_text(size =  sizeLabel),
          axis.text = element_text(size =  sizeTicks),
          panel.border = element_blank())
}
ggp <- plotly_build(p1a)

if(!(showWarnings && !is.null(pbiWarning)))
{#design plotly 
  
  if(myInclude > 0)
  {
    indHistData = 1
    indFitted = 0 
    indForcData = 2
    indLow = 3
    indHigh = 4
    
    if(showInPlotFitted)
    {
      indFitted = 2
      indForcData = 3
      indLow = 4
      indHigh = 5
    }
  }
  else
  {
    indHistData = 0
    indFitted = 0 
    indForcData = 2
    indLow = 3
    indHigh = 4
    
    if(showInPlotFitted)
    {
      indForcData = 1
      indLow = 2
      indHigh = 3
    }
    
  }
  
  if(indHistData)
    ggp$x$data[[indHistData]]$text = paste("Historical data:<br>", labTime, ": ", allTimes[fFromTo[1]:N], "<br>", labValue, ": ", round(y1,2) , sep ="" ) 
  
  if(indForcData)
    ggp$x$data[[indForcData]]$text = paste("Forecast data:<br>",labTime, ": ", allTimes[(N+1):fFromTo[2]], "<br>", labValue, ": ", round(y2,2) , sep ="" ) 
  
  
  if(indFitted)
    ggp$x$data[[indFitted]]$text = paste("Fitted data:<br>",labTime, ": ", allTimes[fFromTo[1]:N], "<br>", labValue, ": ", round(y3,2) , sep ="" ) 
  
  
  if(length(ggp$x$data)>=indLow && indLow)
  {
    iii =  as.character(ggp$x$data[[indLow]]$x)
    ddd = as.character(allTimes[(N + 1) : fFromTo[2]])
    names(ddd) = as.character(x2)
    ddd1 = ddd[iii]
    ggp$x$data[[indLow]]$text = paste("Conf. interval1:<br>", labTime, ": ", ddd1, "<br> lower: ", lower1[iii],"<br> upper: ", upper1[iii], sep ="" ) 
  }
  
  if(length(ggp$x$data)>=indHigh)
  {
    iii =  as.character(ggp$x$data[[indHigh]]$x)
    ddd = as.character(allTimes[(N+1):fFromTo[2]])
    names(ddd) = as.character(x2)
    ddd1 = ddd[iii]
    ggp$x$data[[indHigh]]$text = paste("Conf. interval2:<br>", labTime, ": ", ddd1, "<br> lower: ", lower2[iii],"<br> upper: ", upper2[iii], sep ="" ) 
  }
  
  ggp$x$layout$margin$l = ggp$x$layout$margin$l+10
  
  if(ggp$x$layout$xaxis$tickangle < -40)
    ggp$x$layout$margin$b = ggp$x$layout$margin$b+40
  
}

############# Create and save widget ###############

p <- ggp

disabledButtonsList <- list('toImage', 'sendDataToCloud', 'zoom2d', 'pan', 'pan2d', 'select2d', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian')
p$x$config$modeBarButtonsToRemove = disabledButtonsList

p <- config(p, staticPlot = FALSE, editable = FALSE, sendData = FALSE, showLink = FALSE,
            displaylogo = FALSE,  collaborate = FALSE, cloud=FALSE, sizingPolicy = htmlwidgets::sizingPolicy(
              browser.padding = 0
            ))

internalSaveWidget(p, 'out.html')

if(keepOutData)
{
  padNA1 = rep(NA,length(x1))
  padNA2 = rep(NA,length(x2))
  if(!exists("lower1"))
    lower1 = lower2 = upper1 = upper2 = padNA2;
  
  
  lower1 = c(padNA1,lower1)
  lower2 = c(padNA1,lower2)
  upper1 = c(padNA1,upper1)
  upper2 = c(padNA1,upper2)
  
  exportDF = data.frame(Date = as.character(x_with_f_exist),Value = c(y1,y2),Fitted = c(y3,padNA2),
                        lower1 = lower1,
                        lower2 = lower2,
                        upper1 = upper1,
                        upper2 = upper2)
  
  colnames(exportDF)[c(1,2)] = c(labTime,labValue)
  
  KeepOutDataInHTML(df = exportDF, htmlFile = 'out.html', exportMethod = exportMethod, limitExportSize = limitExportSize)
}
