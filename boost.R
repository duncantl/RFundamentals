boost =
function(formula, data, numIterations = 50) 
{
    N = nrow(data)    
    weights = rep(1/N, N)
    fits = vector("list", numIterations)
    for(i in seq_len(numIterations)) {
         fits[[i]] = fit = stats::lm(formula, data, weights = weights)
         weights = updateWeights(fit, weights)
    }
    fits
}

updateWeights =
function(fit, w)
{
     w + rexp(length(w), 1)
}




boost2 =
    # Change the formula's environment ! Bold move.
function(formula, data, numIterations = 50) 
{
    environment(formula) = sys.frame(sys.nframe())
    N = nrow(data)    
    weights = rep(1/N, N)
    fits = vector("list", numIterations)

    for(i in seq_len(numIterations)) {
         fits[[i]] = fit = stats::lm(formula, data, weights = weights)
         weights = updateWeights(fit, weights)
    }
    fits
}


#data[["weights"]] = weights


boost1 =
    # Add weights as a column.
function(formula, data, numIterations = 50) 
{
    environment(formula) = sys.frame(sys.nframe())
    N = nrow(data)    
    weights = rep(1/N, N)
    fits = vector("list", numIterations)

    wvarName = ".weights"
    data[[ wvarName ]]  = weights

    env = sys.frame(sys.nframe())
    expr = substitute(stats::lm(formula, data, weights = wvarName), list(wvarName = as.name(wvarName)))
    
    for(i in seq_len(numIterations)) {
         # stats::lm(formula, data, weights = wvarName) #!!!! No, need to insert the symbol, not the name
         fits[[i]] = fit = eval( expr,  env )
         data[[ wvarName ]] = updateWeights(fit, weights)
    }
    fits
}


boost3 =
    # Create a new environment containing the weights and have the original
    # environment of the formula be its parent so .
function(formula, data, numIterations = 50) 
{
    e = new.env(parent = environment(formula))
    environment(formula) = e
    N = nrow(data)    
    e$weights = rep(1/N, N)
    fits = vector("list", numIterations)

    for(i in seq_len(numIterations)) {
         fits[[i]] = fit = stats::lm(formula, data, weights = weights)
         e$weights = updateWeights(fit, e$weights)
    }
    fits
}

