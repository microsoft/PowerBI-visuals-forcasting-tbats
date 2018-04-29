#utils.r 
#file with small util methods 

libraryRequireInstall = function(packageName, ...)
{
  if(!require(packageName, character.only = TRUE)) 
    warning(paste("*** The package: '", packageName, "' was not installed ***", sep=""))
}

libraryRequireInstall("graphics")
libraryRequireInstall("scales")
libraryRequireInstall("forecast")
libraryRequireInstall("zoo")
libraryRequireInstall("ggplot2")
libraryRequireInstall("lubridate")


# Postprocess text string (usually to show as labels)
# if very very long abbreviate
# if looooooong convert to lo...
# if shorter than maxChar remove 
cutStr2Show = function(strText, strCex = 0.8, abbrTo = 100, isH = TRUE, maxChar = 3, partAvailable = 1)
{
  # partAvailable, wich portion of window is available, in [0,1]
  if(is.null(strText))
    return (NULL)
  
  SCL = 0.075 * strCex / 0.8
  pardin = par()$din
  gStand = partAvailable * (isH * pardin[1] + (1 - isH) * pardin[2]) / SCL
  
  # if very very long abbreviate
  if(nchar(strText) > abbrTo && nchar(strText) > 1)
    strText = abbreviate(strText, abbrTo)
  
  # if looooooong convert to lo...
  if(nchar(strText) > round(gStand) && nchar(strText) > 1)
    strText = paste(substring(strText, 1, floor(gStand)), "...", sep = "")
  
  # if shorter than maxChar remove 
  if(gStand <= maxChar)
    strText = NULL
  
  return(strText) 
}


# verify if "perSeason" is good for "frequency" parameter
freqSeason = function(seasons, perSeason)
{
  if((seasons > 5 && perSeason > 3) || (seasons > 2 && perSeason > 7))
    return (perSeason)
  
  return(1)
}

# find frequency using the dates, targetS is a "recommended" seasonality 
findFreq = function(dates, targetS = "Automatic")
{
  freq = 1
  N = length(dates)
  nnn = c("Minute", "Hour", "Day", "Week", "Month", "Quater", "Year")
  seasons = rep(NaN, 7)
  names(seasons) = nnn
  perSeason = seasons
  
  seasons["Day"] = round(as.numeric(difftime(dates[length(dates)], dates[1]), units = "days"))
  seasons["Hour"] = round(as.numeric(difftime(dates[length(dates)], dates[1]), units = "hours"))
  seasons["Minute"]=round(as.numeric(difftime(dates[length(dates)], dates[1]), units = "mins"))
  seasons["Week"]=round(as.numeric(difftime(dates[length(dates)], dates[1]), units = "weeks"))
  seasons["Month"] = seasons["Day"] / 30
  seasons["Year"] = seasons["Day"] / 365.25
  seasons["Quater"] = seasons["Year"] * 4
  
  perSeason = N / seasons
  
  if(targetS != "Automatic") # target 
    freq = perSeason[targetS]
  
  if(freq < 2 || round(freq) > 24) # if TRUE, target season factor is not good 
    freq = 1
  
  for( s in rev(nnn)) # check year --> Quater --> etc
    if(freq == 1 || round(freq) > 24)
      freq = freqSeason(seasons[s],perSeason[s])
  
  
  if(round(freq) > 24) # limit of exp smoothing R implementation
    freq = 1
  
  return(freq)
}

# Find number of ticks on X axis 
FindTicksNum1 = function(n, f, flag_ggplot = TRUE)
{
  factorGG = (if(flag_ggplot) 0.525 else 1)
  
  tn = 10* factorGG # default minimum
  mtn = 20 * factorGG # default max
  
  D = 2 # tick/inch
  numCircles = n / f
  xSize = par()$din[1]
  tn = min(max(round(xSize * D * factorGG), tn), mtn)
  return(tn) 
}



#format labels on X-axis automatically 
flexFormat = function(dates, orig_dates, freq = 1, myformat = NULL)
{
  
  days=(as.numeric(difftime(dates[length(dates)], dates[1]), units = "days"))
  months = days / 30
  years = days / 365.25
  
  
  constHour = length(unique(orig_dates$hour)) == 1
  constMin = length(unique(orig_dates$min)) == 1
  constSec = length(unique(orig_dates$sec)) == 1
  constMon = length(unique(orig_dates$mon)) == 1
  
  timeChange = any(!constHour,!constMin,!constSec)
  
  if(is.null(myformat))
  {
    if(years > 10){
      if(constMon)
      {
        myformat = "%Y" #many years => only year :2001
      }else{
        myformat = "%m/%y" #many years + months :12/01
      }
    }else{
      if(years > 1 && N < 50){
        myformat = "%b %d, %Y" #several years, few samples:Jan 01, 2010
      }else{
        if(years > 1){
          myformat = "%m/%d/%y" #several years, many samples: 01/20/10
        }else{
          if(years <= 1 && !timeChange)
            myformat = "%b %d" #1 year,no time: Jan 01
        }  
      }
    }
  }
  if(is.null(myformat) && timeChange)
    if(years > 1){
      myformat = "%m/%d/%y %H:%M" # 01/20/10 12:00
    }else{
      if(days > 1){
        myformat = "%b %d, %H:%M" # Jan 01 12:00
      }else{
        if(days <= 1){
          myformat = "%H:%M" # Jan 01 12:00
        }  
      }
    }
  if(!is.null(myformat)){
    if(myformat == "%Y,Q%q")
      dates = as.yearqtr(dates)
    dates1 = format(dates,  myformat)
  }else{
    dates1 = as.character(1:length(dates)) # just id 
  }
  return(dates1)
}

joinFreq = function (f1 ,f2 = NULL)
{
  if(is.null(f1) || is.na(f1) || f1 < 1)
    f1 = NULL
  if(is.null(f2) || is.na(f2) || f2 < 1)
    f2 = NULL
  
  f = sort(unique(c(f1,f2)))
  if(is.null(f) || is.na(f) || f < 1)
    f = 1
  
  return(f)
  
}


indexShowFromTo = function(showFromTo,datesActual, datesAll, refPointShift = 0)
{
  
  secShift = -refPointShift*60*60
  # datesActual$hour = datesActual$hour +  refPointShift
  # datesAll$hour = datesAll$hour + refPointShift
  
  datesActual = datesActual +  secShift
  datesAll = datesAll + secShift
  
  datesActual = as.POSIXlt(datesActual)
  datesAll = as.POSIXlt(datesAll)
  
  Lall = length(datesAll)
  Lactual = length(datesActual)
  
  if(showFromTo == "all")
  {
    frto = c(1,Lall)
    return(frto)
  }
  
  if(showFromTo == "hour")
  {
    v = datesAll$hour
    target = v[Lactual]
  }
  
  if(showFromTo == "mday")
  {
    v = datesAll$mday
    target = v[Lactual]
  }
  if(showFromTo == "mon")
  {
    v = datesAll$mon
    target = v[Lactual]
  }
  if(showFromTo == "year")
  {
    v = datesAll$year
    target = v[Lactual]
  }
  
  if(showFromTo == "week")
  {
    
    v = lubridate::week(datesAll)
    target = v[Lactual]
  }
  
  
  fr = Lactual
  # go backward 
  for (i in rev(1:Lactual))
  {
    if(v[i] == target)
      fr = i
    else
      break;
  }
  
  to = Lactual
  # go forward 
  for (i in (Lactual:Lall))
  {
    if(v[i] == target)
      to = i
    else
      break;
  }
  frto = c(fr,to)
  
  return(frto)
  
}

# return FALSE if canvas is too small
goodPlotDimension = function(minWidthInch = 3,minHeightInch = 2.2)
{
  re = (par()$din[1] > minWidthInch) & (par()$din[2] > minHeightInch)
  return(re)
}


getAngleXlabels1 = function(mylabels)
{
  NL = length(mylabels)
  NC = nchar(mylabels[1]) * 1.1
  
  lenPerTick = par()$din[1] / (NL * NC)
  
  #lot of space -> 0 
  if(lenPerTick > 0.15)
    return(0)
  
  # no space --> -90
  if(lenPerTick < 0.050)
    return(90)
  
  # few space --> - 45
  return(45)
  
}

# verify if "perSeason" is good for "frequency" parameter
freqSeason2 = function(seasons, perSeason)
{
  if((seasons >= 3 && perSeason >= 3) || (seasons >= 2 && perSeason >= 5))
    return (perSeason)
  return(1)
}


# find frequency using the dates, targetS is a "recommended" seasonality 
findFreqFromDates1 = function(dates, targetS = "auto")
{
  freq = 1
  N = length(dates)
  nnn = c("hour", "day", "week", "month", "quarter", "year")
  seasons = rep(NaN, 6)
  names(seasons) = nnn
  perSeason = seasons
  
  seasons["day"] = round(as.numeric(difftime(dates[length(dates)],dates[1]), units = "days"))
  seasons["hour"] = round(as.numeric(difftime(dates[length(dates)],dates[1]), units = "hours"))
  seasons["week"] = round(as.numeric(difftime(dates[length(dates)],dates[1]), units = "weeks"))
  seasons["month"]  =  seasons["day"] / 30
  seasons["year"]  =  seasons["day"] / 365.25
  seasons["quarter"] = seasons["year"] * 4
  
  perSeason = N / seasons
  
  if(targetS!="auto") # target 
    freq = perSeason[targetS]
  else
    freq = 1
  
  if(freq < 2) # if TRUE, target season factor is not good 
    freq = 1
  
  for( s in rev(nnn)) # check year --> quarter --> etc
    if(freq == 1 )
      freq = freqSeason2(seasons[s],perSeason[s])
  
  return(round(freq))
}

#get valid frequency parameter, based on input from user 
getFrequency1 = function(parsed_dates, values, tS, f)
{
  myFreq = f
  
  if(!(tS %in% c("none","manual"))) #detect from date
  {  
    myFreq = findFreqFromDates1(parsed_dates, targetS = tS)
  }else{
    if(tS == "none")
    { myFreq = 1}
    else
    {# NOT YET IMPLEMENTED
      # if(tS == "autodetect from value")
      #   myFreq = freqFromValue1(values)
    }
  }
  numPeriods = floor(length(values) / myFreq)
  if(numPeriods < 2)
    myFreq = findFreqFromDates1(parsed_dates, targetS = "auto")
  return(myFreq)
}


