############### Flattening HTML functions ###############
libraryRequireInstall = function(packageName, ...)
{
  if(!require(packageName, character.only = TRUE)) 
    warning(paste("*** The package: '", packageName, "' was not installed ***", sep=""))
}

libraryRequireInstall("XML")
libraryRequireInstall("htmlwidgets")
libraryRequireInstall("caTools")

internalSaveWidget <- function(widget, fname)
{
  tempFname = paste(fname, ".tmp", sep="")
  htmlwidgets::saveWidget(widget, file = tempFname, selfcontained = FALSE)
  
  wordA = '"padding":40'
  wordB = '"padding":0'
  ReplaceStrInFile(tempFname,tempFname, wordA , wordB)
 
  FlattenHTML(tempFname, fname)
}

FlattenHTML <- function(fnameIn, fnameOut)
{
  # Read and parse HTML file
  # Embed all js and css files into one unified file
  
  if(!file.exists(fnameIn))
    return(FALSE)
  
  dir = dirname(fnameIn)
  html = htmlTreeParse(fnameIn, useInternal = TRUE)
  top = xmlRoot(html)
  
  # extract all <script> tags with src value
  srcNode=getNodeSet(top, '//script[@src]')
  for (node in srcNode)
  {
    b = xmlAttrs(node)
    fname = file.path(dir, b['src'])
    alternateSrc = FindSrcReplacement(fname)
    if (!is.null(alternateSrc))
    {
      s = alternateSrc
      names(s) = 'src'
      newNode = xmlNode("script",attrs = s)
      replaceNodes(node, newNode)
    }else{
      str=ReadFileForEmbedding(fname);
      if (!is.null(str))
      {      
        newNode = xmlNode("script", str, attrs = c(type = "text/javascript"))
        replaceNodes(node, newNode)
      }
    }
  }
  
  # extract all <link> tags with src value
  linkNode=getNodeSet(top, '//link[@href]')
  for (node in linkNode)
  {
    b = xmlAttrs(node)
    fname = file.path(dir, b['href'])
    str = ReadFileForEmbedding(fname, FALSE);
    if (!is.null(str))
    {
      newNode = xmlNode("style", str)
      replaceNodes(node, newNode)
    }
  }
  
  saveXML(html, file = fnameOut)
  return(TRUE)
}

ReadFileForEmbedding <- function(fname, addCdata = TRUE)
{
  data = ReadFullFile(fname)
  if (is.null(data))
    return(NULL)

  str = paste(data, collapse ='\n')
  if (addCdata) {
    str = paste(cbind('// <![CDATA[', str,'// ]]>'), collapse ='\n')
  }
  return(str)
}

ReadFullFile <- function(fname)
{
  if(!file.exists(fname))
    return(NULL)
  
  con = file(fname, open = "r")
  data = readLines(con)
  close(con)
  return(data)
}

ConvertDF64encoding = function (df, withoutEncoding = FALSE)
{
  header_row <- paste(names(df), collapse=", ")
  tab <- apply(df, 1, function(x)paste(x, collapse=", "))
  
  if(withoutEncoding){
    text <- paste(c(header_row, tab), collapse="\n")
    x <- text
  }
  else
  {
    text <- paste(c(header_row, tab), collapse="\n")
    x <- caTools::base64encode(text)
  }
  return(x)
}

FindSrcReplacement <- function(str)
{
  # finds reference to 'plotly' js and replaces with a version from CDN
  # This allows the HTML to be smaller, since this script is not fully embedded in it
  str <- iconv(str, to="UTF-8")
  pattern = "plotlyjs-(\\w.+)/plotly-latest.min.js"
  match1=regexpr(pattern, str)
  attr(match1, 'useBytes') <- FALSE
  strMatch=regmatches(str, match1, invert = FALSE)
  if (length(strMatch) == 0) return(NULL)
  
  pattern2 = "-(\\d.+)/"
  match2 = regexpr(pattern2, strMatch[1])
  attr(match2, 'useBytes') <- FALSE
  strmatch = regmatches(strMatch[1], match2)
  if (length(strmatch) == 0) return(NULL)
  
  # CDN url is https://cdn.plot.ly/plotly-<Version>.js
  # This matches the specific version used in the plotly package used.
  verstr = substr(strmatch, 2, nchar(strmatch)-1)
  str = paste('https://cdn.plot.ly/plotly-', verstr,'.min.js', sep='')
  return(str)
}

ReplaceStrInFile <- function(filenameA,filenameB,wordA,wordB)
{
  tx  <- readLines(filenameA)
  tx2  <- gsub(pattern = wordA, replace = wordB, x = tx)
  writeLines(tx2, con=filenameB)

}

#ReadFullFileReplaceString
ReadFullFileReplaceString <- function(fnameIn, fnameOut, sourceString,targetString)
{
  if(!file.exists(fnameIn))
    return(NULL)
  
  tx  <- readLines(fnameIn)
  tx2  <- gsub(pattern = sourceString, replace = targetString, x = tx)
  writeLines(tx2, con = fnameOut)
}

KeepOutDataInHTML = function(df, htmlFile = 'out.html', exportMethod = "copy", limitExportSize = 1000)
{
  if(nrow(df)>limitExportSize)
    df = df[1:limitExportSize,]
  
  outDataString64 = ConvertDF64encoding(df)
  
  linkElem = '\n<a href=""  download="data.csv"  style="position: absolute; top:0px; left: 0px; z-index: 20000;" id = "mydataURL">export</a>\n'
  updateLinkElem = paste('<script>\n link_element = document.getElementById("mydataURL");link_element.href = outDataString64href;', '\n</script> ', sep =' ')
  var64 = paste('<script> outDataString64 ="', outDataString64, '"; </script>', sep ="")
  var64href = paste('<script> outDataString64href ="data:;base64,', outDataString64, '"; </script>', sep ="")
  
  buttonElem = '<button style="position: absolute; top:0px; left: 0px; z-index: 20000;"  onclick="myFunctionCopy(1)">copy to clipboard</button>'
  funcScript = '<script> 
  function myFunctionCopy(is64) 
  {
  const el = document.createElement("textarea");
  if(is64)
  {
  el.value = atob(outDataString64);
  }
  else
  {
  el.value = outDataStringPlane;
  }
  document.body.appendChild(el);
  el.select();
  document.execCommand("copy");
  document.body.removeChild(el);};	
  </script>'
  
  if(exportMethod == "copy")
    endOfBody = paste(var64,funcScript, buttonElem,'\n</body>',sep ="")
  else#"download"
    endOfBody = paste(linkElem,var64, var64href,updateLinkElem,'\n</body>',sep ="")
  
  ReadFullFileReplaceString('out.html', 'out.html', '</body>', endOfBody)
  
}


#################################################

