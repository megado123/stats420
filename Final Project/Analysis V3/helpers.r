#--------------------------------------------------
#
#--------------------------------------------------
helpers.anova_selection <- function(models, alpha) {
    # This takes a list of models in hierarchical order, compares the nth and (nth + 1) and returns the first
    # nth model where the (nth + 1) model fails the ANOVA F-test 
    # 
    # Args:
    #   models: A list of models in hierachical order
    #   alpha: The alpha for the ANOVA F-test.
    #
    # Returns:
    #   The nth model where the (nth + 1) model fails the ANOVA F-test, or the last model in the list.
    #
    
    model_count <- length(models)
    
    for (i in 1:model_count) {
        
        # If we made it to the last model, then we are done
        if (i == model_count) {
            return (models[[model_count]])
        }
        
        
        small_model <- models[[i]]
        big_model <- models[[i + 1]]
        
        p_val <- anova(small_model, big_model)[2, "Pr(>F)"]

        if (p_val > alpha) {
            return (small_model)   
        }
    }
}


#--------------------------------------------------
#
#--------------------------------------------------
# This is a little helper function to create the formula from the model as a string
helpers.make_formula_string <- function(model) {
    # This is a helper to create the formula string from the model
    #
    # Args:
    #   model: The model object created from lm() function
    #
    # Returns:
    #   The model formula as a string, e.g., " log(y) ~ xi + I(x1^2)"
    #
    formula_string_parts <- deparse(formula(model))
    return (paste(formula_string_parts, sep = "", collapse = ""))
}

#--------------------------------------------------
#
#--------------------------------------------------
helpers.analyze_model <- function(model) {
    # This performs some basic analysis on the model.
    #
    # Args:
    #   model: The model object created from lm()
    #   train_response: The response verctor from the training dataset
    #   test_response: The response vector from the testing dataset
    #   test_data: The test dataset
    #   rmse_response_tranform: A function used to transform the fitted data for the RMSE calculation
    #
    #   Returns:
    #       A list with values "rmse", "test_rmse", "adj_r_squared", "bp_p_value", "shapiro_p_value"
    #
    retVal <- list(
      loocv_rmse = NA,
        #rmse = NA,
        #test_rmse = NA,
        adj_r_squared = NA,
        bp_p_value = NA,
        shapiro_p_value = NA
    )
    
    #calc_loocv_rmse
      retVal$loocv_rmse = sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
    
    
    # Calculate RMSE
    #retVal$rmse <- sqrt(mean((train_response - rmse_response_tranform(fitted(model)))^2))
    
    # Calculate test RMSE
    #retVal$test_rmse <- sqrt(mean((test_response - predict(model, newdata = test_data))^2))
    
    # Adjusted R^2
    retVal$adj_r_squared <- summary(model)$adj.r.squared
    
    # BP Test
    retVal$bp_p_value <- bptest(model)$p.value
    
    # SW Test
    retVal$shapiro_p_value <- shapiro.test(resid(model))$p.value
    
    return (retVal)
}



#--------------------------------------------------
#
#--------------------------------------------------
helpers.model_builder <- function(predictor_name, response_name, data) {
    # This will build log and polynomial models from the predictor and response.
    #
    # Args:
    #   predictor_name: The name of the predictor variable
    #   response_name: The name of the respone variable
    #   data: The data to fit the model withh.
    #
    # Returns:
    #   A list of object created with the lm() function. The models will be named:
    #   "second_order", "third_order", "log_1", and "log_2"

    
    # This is the object we will return.
    retVal <- list(
        second_order = NA,
        third_order = NA,
        log_1 = NA,
        log_2 = NA
    )
    
    # A helper to make a string like "log(var_name)"
    log_string <- function(var_name) {
        paste("log(", var_name, ")", sep = "")
    }
    
    # A helper to make a string like "I(var_name^power)"
    power_string <- function(var_name, power) {
        paste("I(", var_name, "^", power, ")", sep = "")
    }
    
    # A helper to make a an additive formula string like "response ~ var1 + var2 + var3"
    make_additive_formula <- function(response_string, ...) {
        rhs <- paste(list(...), collapse = " + ")
        return (as.formula((paste(response_string, "~", rhs))))
    }
    
    # Make the second order model "response + predictor + I(predictor^2)"
    second_order_formula <- make_additive_formula(
        log_string(response_name),
        predictor_name,
        power_string(predictor_name, 2))
    
    retVal$second_order = lm(second_order_formula, data = data)
    
    # Make the second order model "response + predictor + I(predictor^2) + I(predictor^3)"
    third_order_formula <- make_additive_formula(
        log_string(response_name),
        predictor_name,
        power_string(predictor_name, 2),
        power_string(predictor_name, 3))
    
    retVal$third_order = lm(third_order_formula, data = data)
    
    # Make the first log model "response + predictor + log(predictor)"
    log_1_formula <- make_additive_formula(
        log_string(response_name),
        predictor_name,
        log_string(predictor_name))
    
    retVal$log_1 = lm(log_1_formula, data = data)
    
    
    
    # Make the first log model "response + log(predictor)"
    log_2_formula <- make_additive_formula(
        log_string(response_name),
        log_string(predictor_name))
    
    retVal$log_2 = lm(log_2_formula, data = data)
    

    return (retVal)
}



#--------------------------------------------------
#
#--------------------------------------------------
helpers.step_model_selection <- function(initial_model, scope, penalty, isBackward) {
    # This is just a wrapper on the step() function with. Instead of
    # returning the selected model, this will return a list of all the models that were tried, and,
    # the list will be in hierachical order.
    #
    # Args:
    #   initial_model: This is an object created with the lm() function. The search will start here.
    #   scope: This is passed to the step() function's "scope" parameter when doing forward search. This should be a model formula
    #   penality: The penality to use for selection. "2" is AIC, "log(n)" is BIC, and 0 means look at RSS.
    #   isBackward: set to TRUE to perform backward search
    #
    # Returns:
    #   A list (hierachical ordered) of models found during the forward selection process.
    #
    
    return_models <- list()
    model_index <- 1

    keep_function <- function(current_model, aic) {
        return_models[[model_index]] <<- current_model
        model_index <<- model_index + 1
    }
    
    if (isBackward == TRUE) {
        step(initial_model, direction = "backward", keep = keep_function, k = penalty, trace = 0)    
    } else {
        step(initial_model, scope = scope, direction = "forward", keep = keep_function, k = penalty, trace = 0)
    }
    
    if (isBackward == TRUE) {
        return (rev(return_models))    
    } else {
        return (return_models)
    }
    
}



#--------------------------------------------------
#
#--------------------------------------------------
helpers.r_j_squared_and_vif <- function(predictors, data) {
    # Get the value for R^2_j and variance inflation factor (VIF) for each predictor.
    #
    # Args:
    #   predictors: An array of names for the predictors in an additive MLR model
    #   data: The data to build the models with
    #
    # Retuns:
    #   An a data.frame of R^2_j values
    
    
    predictor_count <- length(predictors)

    # Init the return value
    retVal <- data.frame( 
        predictor = rep(0, predictor_count),
        rj_squared = rep(0, predictor_count),
        vif = rep(0, predictor_count)
    )

    for (i in 1:predictor_count) {

        current_response <- predictors[i]
        retVal$predictor[i] <- current_response

        if (is.factor(data[[current_response]])) {
            # The predictor is categorical so  no need to make model
            retVal$rj_squared[i] <- NA
            retVal$vif[i] <- NA
        } else {
            # Get a list of predictors that does not contain the predictor
            # that we are using as a response.
            current_predictors <- predictors[predictors != current_response]

            # Create the string like "x1 + x2 + x3..."
            predictor_formula <- paste(current_predictors, collapse = "+")

            # Complete the formula like "y ~ x1 + x2 + x3..."
            current_formula <- paste(current_response, "~", predictor_formula)

            # Build the model and get the r2 value
            model <- lm(current_formula, data = data)
            retVal$rj_squared[i] <- summary(model)$r.squared
            retVal$vif[i] <- 1/(1 - retVal$rj_squared[i])
        }
    }

    return (retVal)
}