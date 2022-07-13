######################################
# Toolbox with basic auxiliary functions used throughout the package including:
# - checking functions
# - matrix and data frame functions
# - decay functions
# - residual sum of squares functions for fitting decay
# - interpolation functions
######################################

# Check if activity patterns are well defined and return the time series vector
define_activity = function(input_pattern) {
    if (length(input_pattern) == 1) {
        activity_pattern = activity_patterns[which(activity_patterns$id ==
                                                       input_pattern), "value"]
    } else {
        assertVector(input_pattern)
        assertNumeric(input_pattern, lower = 0, upper = 1, any.missing = FALSE)
        activity_pattern = input_pattern
    }

    return(activity_pattern)
}



##### Matrix and data frame functions #####
rep.row = function(x,n){
    return(matrix(rep(x,each=n),nrow=n))
}

rep.col<-function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

split_tibble = function(tibble, column) {
    tibble %>% split(., .[,column])
}

# Converts a data frame to a list of its rows with attribute names
# corresponding to the first column
get_list_from_table = function(df) {
    df.list = setNames(split(df[,-1], seq(nrow(df)), drop = TRUE), df[,1])
    return(df.list)
}

##### Decay functions #####
# Average exponential decay
avg_expDecay = function(initialEffects, halflife, duration) {
    integral = initialEffects * (halflife / log(2)) *
        (1 - exp(-log(2) * duration / halflife))
    averageEffect = integral / duration
    return(averageEffect)
}

# Functions for calculating effect decays
Weibull_function = function(time, p) {
    return(exp( -(time/p$L)^p$k * log(2)))
}

step_function = function(time, p){
    output = ifelse(time >= p$L, 0, 1)
    return(output)
}

linear_function = function(time, p){
    return(1 - time/p$L)
}

exponential_function = function(time, p){
    return(exp(-time/p$L * log(2)))
}

hill_function = function(time, p){
    return(1 / (1 + (time/p$L)^p$k))
}

smoothcompact_function = function(time, p){
    output = (1 - (time>=p$L)) * exp((1 - (time>=p$L)) *
                                         (p$k - p$k/(1 - (time/p$L)^2)))
    return(output)
}

parabolic_growth_function = function(time, p){
    return(p$a*time + p$b*time^2)
}

logit = function(x){
    return(log(x/(1-x)))
}

ilogit = function(x){
    return(exp(x)/(1+exp(x)))
}

# Generic exponential decay function definition:
# par[1] = initialEffect, par[2] = halflife
exponential_decay = function(par, p, num_p){
    time = (p - 0.5)*P_DURATION/num_p
    effect = par[1] * exponential_function(time, list(L = par[2]))
    return(effect)
}

# Generic Weibull decay function definition:
# par[1] = initialEffect, par[2] = halflife,
# par[3] = exponent
weibull_decay = function(par, p, num_p){
    time = (p-0.5)*P_DURATION/num_p
    effect = par[1] * Weibull_function(time=time, list(L=par[2], k=par[3]))
    return(effect)
}
# Generic Hill decay function definition:
# par[1] = initialEffect, par[2] = halflife,
# par[3] = exponent
hill_decay <- function(par, p, num_p){
    time = (p-0.5)*P_DURATION/num_p
    effect = par[1] * hill_function(time=time, list(L=par[2], k=par[3]))
    return(effect)
}

# Generic step decay function definition:
# par[1] = initialEffect, par[2] = time to full decay
step_decay = function(par, p, num_p){
    time = (p-0.5)*P_DURATION/num_p
    effect = par[1] * step_function(time=time, list(L=par[2]))
    return(effect)
}

# Generic linear decay function definition:
# par[1] = initialEffect, par[2] = time to full decay,
linear_decay = function(par, p, num_p){
    time = (p-0.5)*P_DURATION/num_p
    effect = par[1] * linear_function(time=time, list(L=par[2]))
    return(effect)
}

# Generic smooth-compact decay function definition:
# par[1] = initialEffect, par[2] = time to full decay,
# par[3] = exponent
smoothcompact_decay = function(par, p, num_p){
    time = (p-0.5)*P_DURATION/num_p
    effect = par[1] * smoothcompact_function(time=time, list(L=par[2], k=par[3]))
    return(effect)
}

# Definitions of the residual sum of squares functions to be optimized
RSS_exponential_decay = function(data, par){
    with(data, sum((exponential_decay(par, points, nrow(data)) - values)^2))
}

RSS_weibull_decay = function(data, par) {
    with(data, sum((weibull_decay(par, points, nrow(data)) - values)^2))
}

RSS_hill_decay = function(data, par) {
    with(data, sum((hill_decay(par, points, nrow(data)) - values)^2))
}

RSS_step_decay = function(data, par) {
    with(data, sum((step_decay(par, points, nrow(data)) - values)^2))
}

RSS_linear_decay = function(data, par) {
    with(data, sum((linear_decay(par, points, nrow(data)) - values)^2))
}

RSS_smoothcompact_decay = function(data, par) {
    with(data, sum((smoothcompact_decay(par, points, nrow(data)) - values)^2))
}


# Generic function for calculating decay over a set of interpolation points
# given a specified decay function (exponential, Weibull, ...)
# Suggestion: approxfun
interp_decay = function(decay_f, decay_p, num_points) {
    time = (1:num_points-0.5)*decay_p$duration/num_points
    interp_effect = do.call(decay_f, list(time = time, p = decay_p))
    return(decay_p$initialEffect * interp_effect)
}

# Spline interpolation
spline_interpolation = function(yvar, semester, nips, duration) {
    required_data = data.frame(semester)
    required_data$net_age = (semester - 0.5)*0.5
    required_data$yvar = yvar
    net_age = seq(1:nips)*duration/nips
    prediction_points = data.frame(net_age)
    lm2 = lm(yvar ~ bs(net_age), data = required_data)
    prediction = predict(lm2, data.frame(prediction_points))
    return(prediction)
}

# Fit 5 types of decay curves to intervention effects interpolation points
# and estimate their parameters as well the best fit to build a GVI
get_best_decay_fit = function(values, duration, param_name, plot_flag) {

    # Build the dataframe with the intervention effects at interpolation points
    tab=NULL
    num_p = length(values)
    tab$points = c(1:num_p)
    # Convert the points to fit in the overall duration of the intervention
    tab$time = (tab$points-0.5)*duration/num_p
    tab$values = values
    tab = as.data.frame(tab)

    # Build the dataframe with the raw values at interpolation points
    data_tab = cbind.data.frame("raw data", 1:nrow(tab), tab$values)
    colnames(data_tab) = c("decay_function", "time", "value")

    # Initialize variables
    P_DURATION <<- duration
    values_tab = NULL
    best_fit = NULL
    best_fit$decay = ""
    best_fit$RSS = 100000
    best_fit$params = NULL

    exponential_fit = weibull_fit = hill_fit =
        linear_fit = smoothcompact_fit = NULL

    nls_control = list(maxiter = 100)

    # Fit the different decay types and find the best fit:
    tryCatch(
        expr = {
            nls_control = list(maxiter = 100)
            exponential_fit = nlsLM(values ~ a*exp(-time/L*log(2)),
                                    data = tab, start = list(a = tab$values[1],
                                                             L = duration),
                                    control = nls_control)
            exponential_fit$name = "exponential"
            print("Exponential decay fitted successfully.")
        },
        error = function(e){
            message("An exponential decay could not be fitted.")
        }
    )

    tryCatch(
        expr = {
            weibull_fit = nlsLM(values ~ a*exp(-(time/L)^k * log(2)),
                                data = tab, start = list(a = tab$values[1],
                                                         L = duration, k = 1),
                                control = nls_control)
            weibull_fit$name = "Weibull"
            print("Weibull decay fitted successfully.")
        },
        error = function(e){
            message("A Weibull decay could not be fitted.")
        }
    )

    tryCatch(
        expr = {
            hill_fit = nlsLM(values ~ a/(1+(time/L)^k),
                             data = tab, start = list(a = tab$values[1],
                                                      L = duration, k = 1),
                             control = nls_control)
            hill_fit$name = "Hill"
            print("Hill decay fitted successfully.")
        },
        error = function(e){
            message("A Hill decay could not be fitted.")
        }
    )

    tryCatch(
        expr = {
            linear_fit = nlsLM(values ~ a*(1-time/L), data = tab,
                               start = list(a = tab$values[1], L = duration),
                               control = nls_control)
            linear_fit$name = "linear"
            print("Linear decay fitted successfully.")
        },
        error = function(e){
            message("A linear decay could not be fitted.")
        }
    )

    tryCatch(
        expr = {
            smoothcompact_fit = nlsLM(values ~ a*(c(1)-(time>=L))*
                                          exp((c(1)-(time>=L))*k*
                                                  (c(1)-1/(1-(time/L)^2))),
                                      data = tab,
                                      start = list(a = tab$values[1],
                                                   k = 1, L = duration),
                                      control = nls_control)
            smoothcompact_fit$name = "smooth-compact"
            print("Smooth-compact decay fitted successfully.")
        },
        error = function(e){
            message("A smooth-compact decay could not be fitted.")
        }
    )


    # Concatenate all the successfully found fits so far
    fit_list = list(exponential_fit = exponential_fit,
                    weibull_fit = weibull_fit,
                    hill_fit = hill_fit, linear_f = linear_fit,
                    smoothcompact_fit = smoothcompact_fit)

    # Find the best fit so far and obtain all the fitted curves
    for (fit_obj in fit_list) {
        if (!is.null(fit_obj)) {
            # extract the residual sum of squares
            fit_obj$RSS = fit_obj$m$deviance()
            if (fit_obj$RSS < best_fit$RSS) {
                best_fit$RSS = fit_obj$RSS
                best_fit$decay = fit_obj$name
                best_fit$params = fit_obj$m$getPars()
            }
            # get the fitted values
            pred = fit_obj$m$predict()
            # construct the data table with all the fits resuts (for plotting)
            values_decay = cbind.data.frame(fit_obj$name, 1:nrow(tab),
                                            pred, tab$values, fit_obj$RSS)
            colnames(values_decay) = c("decay_function", "time", "value",
                                       "initial_value", "RSS")
            values_tab = rbind.data.frame(values_tab, values_decay)
        }
    }

    # For the step decay:
    L_step = which.max(cumsum(tab$values)^2/c(1:nrow(tab)))
    pred = do.call("step_decay", list(par = c(mean(tab$values[1:L_step]),
                                              L_step*P_DURATION/num_p),
                                      p = 1:nrow(tab), nrow(tab)))

    RSS = sum((pred - tab$values)^2)
    # Check if best than previous fits
    if (RSS < best_fit$RSS) {
        best_fit$RSS = RSS
        best_fit$decay = "step"
        best_fit$params = c(init_effect = tab$values[1], L = L_step)
    }
    # Construct the data table with all the fits resuts (for plotting)
    values_decay = cbind.data.frame("step", 1:nrow(tab), pred,
                                    tab$values, RSS)
    colnames(values_decay) = c("decay_function", "time", "value",
                               "initial_value", "RSS")
    values_tab = rbind.data.frame(values_tab, values_decay)

    # For the constant decay:
    pred = rep(mean(tab$values), nrow(tab))
    RSS = sum((pred - tab$values)^2)
    # Check if best than previous fits
    if (RSS <= best_fit$RSS) {
        best_fit$RSS = RSS
        best_fit$decay = "constant"
        # In OpenMalaria we need to specify L even for the constant decay
        best_fit$params = c(init_effect = tab$values[1], L = 1)
    }
    # Construct the data table with all the fits resuts (for plotting)
    values_decay = cbind.data.frame("constant", 1:nrow(tab), pred,
                                    tab$values, RSS)
    colnames(values_decay) = c("decay_function", "time", "value",
                               "initial_value", "RSS")
    values_tab = rbind.data.frame(values_tab, values_decay)

    if (plot_flag) {
        # Plot the fitted decays
        p = plot_fits(data_tab, values_tab,
                      paste("\nFitting ", param_name,
                            "\nBest fit:", best_fit$decay))
        plot(p)

        # Print the best fit
        print("Best decay fit:")
        print(best_fit)
    }

    return(best_fit)
}

# Prepare snippet for GVI intervention in the base xml file
prepare_GVI_snippet = function(species, best_fit, param_name, id) {
    func = tolower(paste(best_fit$decay))

    # If there is no specified decay, return null
    if(is.null(best_fit)) {
        GVI_result = list(GVI_xml_snippet = NULL,
                          GVI_attributes = NULL)

        return(GVI_result)
    }

    init_effect = best_fit$params[1]
    L = best_fit$params[2]
    if(func %in% c("weibull", "hill", "smoothcompact")) {
        k = paste(' k="',  best_fit$params[3], '"', sep="")
        decay_attrs = list(L = as.numeric(L), `function` = func,
                           k = as.numeric(best_fit$params[3]))
    } else {
        k = ''
        decay_attrs = list(L = as.numeric(L), `function` = func)
    }
    init_effect_d = as.numeric(init_effect * (param_name == "deterrency"))
    init_effect_pre = as.numeric(init_effect * (param_name == "preprandial"))
    init_effect_post = as.numeric(init_effect * (param_name == "postprandial"))

    # Construct the snippet

    nonHumanHosts_snippet = newXMLNode("nonHumanHosts",
                                       attrs = list(name = "unprotectedAnimals"))
    invisible(newXMLNode("mosqRelativeEntoAvailability",
                         parent = nonHumanHosts_snippet,
                         attrs = list(value = 1)))

    GVI_snippet = newXMLNode("GVI", attrs = list(name = paste0("GVI_", id),
                                                id = paste0("GVI_", id, "_1")))
    invisible(newXMLNode("anophelesParams",
                         parent = GVI_snippet,
                         attrs = list(mosquito = species, propActive=1)))
    invisible(newXMLNode("decay", parent = GVI_snippet, attrs = decay_attrs))
    invisible(newXMLNode("deterrency", parent = GVI_snippet,
                         attrs = list(value = init_effect_d)))
    invisible(newXMLNode("preprandialKillingEffect", parent = GVI_snippet,
                         attrs = list(value = init_effect_pre)))
    invisible(newXMLNode("postprandialKillingEffect", parent = GVI_snippet,
                         attrs = list(value = init_effect_post)))

    GVI_attributes = list(decay_function = func,
                          mosquito_species = species,
                          init_effect_d = init_effect_d,
                          init_effect_pre = init_effect_pre,
                          init_effect_post = init_effect_post,
                          decay_L = L)
    if(func %in% c("weibull", "hill", "smoothcompact")) {
        GVI_attributes$k = best_fit$params[3]
    }

    # Create the result list containing the GVI snippet and its attributes
    GVI_result = list(GVI_xml_snippet = GVI_snippet,
                      GVI_attributes = GVI_attributes)

    return(GVI_result)
}

