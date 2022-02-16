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
    w
}
