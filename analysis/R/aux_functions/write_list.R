# general function

require(XLConnect)

function_write_list <-function(my_list, wb_name = 'var1.xlsx') { 
  wb <- loadWorkbook(wb_name, create = TRUE)
  createSheet(wb, names(my_list))
  writeWorksheet(wb, my_list, names(my_list),header=TRUE)
  saveWorkbook(wb)
}