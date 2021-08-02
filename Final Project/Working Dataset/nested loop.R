

new_data = data.frame(
  y   = fb$`Lifetime Post Consumers`,
  x1  = fb$`Page total likes`,
  x2  = 1 * as.numeric(fb$Type == "Link"),
  x3  = 1 * as.numeric(fb$Type == "Photo"),
  x4  = 1 * as.numeric(fb$Type == "Status"),
  x5  = 1 * as.numeric(fb$Category == "1"),
  x6  = 1 * as.numeric(fb$Category == "2"),
  x7  = fb$`Post Month`,
  x8 = fb$`Post Weekday`,
  x9 = fb$`Post Hour`,
  x10 = fb$Paid,
  x11 = fb$`Lifetime Post Total Reach`,
  x12 = fb$`Lifetime Post Total Impressions`,
  x13 = fb$`Lifetime Engaged Users`,
  x14 = fb$`Lifetime Post Consumptions`,
  x15 = fb$`Lifetime Post Impressions by people who have liked your Page`,
  x16 = fb$`Lifetime Post reach by people who like your Page`,
  x17 = fb$`Lifetime People who have liked your Page and engaged with your post`,
  x18 = fb$comment,
  x19 = fb$like,
  x20 = fb$share
)


cleaned_data = new_data[complete.cases(new_data), ]

variables = colnames(new_data)
predictor_variables = variables[which(variables != "y")]

getAdditiveModels = function(num_predictor_variables, variables, data_set)
{
  p_val           = rep(0, num_predictor_variables ^ 2)
  bp_p_value      = rep(0, num_predictor_variables ^ 2)
  shapiro_p_value = rep(0, num_predictor_variables ^ 2)
  strmodel        = rep(0, num_predictor_variables ^ 2)
  rss             = rep(0, num_predictor_variables ^ 2)
  beta_parameter  = rep(0, num_predictor_variables ^ 2)
  index = 1;
  
  for (j in 1:num_predictor_variables)
  {
    smallestrss = 0
    smallestrssIndex = 0 
    for (i in 1:length(variables))
    {

      if(j == 1) #at the beginning
      {
        startpointer          = index
        s1                    = "log(y)"
        s2                    = variables[i]
        strmodel[index]       = paste(s1, " ~ " , s2)
        beta_parameter[index] = 1
      }
      else
      {
        s2                    = variables[i]
        strmodel[index]       = paste(previousBestModel , " + " , s2)
        beta_parameter[index] = j
      }
      print(strmodel[index])
      model                = lm(strmodel[index], data = data_set)
      rss[index]           = sum(resid(model)^2) 
      
      
      if (smallestrssIndex == 0 || rss[index] < smallestrss){
        
        smallestrssIndex = index
        smallestrss = rss[index]
        print(paste(smallestrssIndex, "!!!"))
      }
      index = index + 1
    }
   #for a given number of predictor variables create temp vector and find the min rss

    previousBestModel = strmodel[smallestrssIndex]
  }
  data.frame(strmodel, rss)
}


results = getAdditiveModels(20, predictor_variables, cleaned_data)

head(results, 20)
