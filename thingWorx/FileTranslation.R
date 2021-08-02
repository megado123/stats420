library(readr)
##tibble
mbew_selected_Matnr <- read_csv("C:/Users/a05v6zz/Desktop/mbew-selected-Matnr.csv")
mbew_selected_Matnr = as.data.frame(mbew_selected_Matnr)

result = mbew_selected_Matnr

typeof(mbew_selected_Matnr[, 1])
names(mbew_selected_Matnr[1])

silly_fun = function(arg1){
  for (i in 3:ncol(arg1)){
    if(typeof(arg1[ , i]) == "double" ){
      result[ , i] = arg1[ , i]/150
    }
    else{
      result[, i] = arg1[ , i]
    }
  }
  return(result)
}

result= silly_fun(mbew_selected_Matnr)

write.csv(result, "C:/Users/a05v6zz/Desktop/mbew-selected-Matnr2.csv")

View(z)

View(mbew_selected_Matnr)

(typeof(mbew_selected_Matnr[ , 6]))
       