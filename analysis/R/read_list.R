# general function

require(XLConnect)

function_read_list <- function(file) {    
  
  wb = loadWorkbook(file) 
  data = readWorksheet(wb, sheet = getSheets(wb))
  data = data[sapply(data, function(x) dim(x)[1]) > 0] # Erasing empty data.frames/sheets
  
  return(data)
  
}

