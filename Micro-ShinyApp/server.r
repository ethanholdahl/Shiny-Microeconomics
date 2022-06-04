# Define server logic
function(input, output, session) {
  
  ######################## FUNCTIONS ########################
  ########### WILL BE WRAPPED IN A PACKAGE LATER ############
  
  getMarginal = function(UfunStr){
    Ufun = parse(text = UfunStr)
    MUx=D(Ufun, 'x')
    MUy=D(Ufun, 'y')
    return(list(Ufun, MUx, MUy))
  }
  
  getVars = function(Ufun, x, y, Px, Py) {
    MUx=D(Ufun, 'x')
    MUy=D(Ufun, 'y')
    MUx = eval(MUx)
    MUy = eval(MUy)
    MRSxy = MUx/MUy
    Cost = x*Px + y*Py
    U = eval(Ufun)
    slopeBL = Px/Py
    return(tibble(MUx, MUy, MRSxy, U, Cost, slopeBL, x, y))
  }
  
  solveBundle = function(x, Ufun, Px, Py, I){
    vars = getVars(Ufun, x[1], x[2], Px, Py)
    result = crossprod(c(vars$MRSxy, vars$Cost) - c(Px/Py, I))
    return(result)
  }
  
  exactBundle = function(Ufun, Px, Py, I, precision = .0001){
    result = optim(c(1,1), solveBundle, Ufun = Ufun, Px = Px, Py = Py, I = I)$par
    result1 = optim(result, solveBundle, Ufun = Ufun, Px = Px, Py = Py, I = I)$par
    diff = abs(result-result1)
    count = 0
    while ((diff>precision)[1]||(diff>precision)[2]){
      count = count + 1
      result = result1
      result1 = optim(result, solveBundle, Ufun = Ufun, Px = Px, Py = Py, I = I)$par
      if (count > 2 & (result1[1] < 0 | result1[2] < 0)){
        #to catch when the solution doesn't converge due to runaway outside of the positive quadrent
        result = result1
      }
      diff = abs(result-result1)
    }
    return(result1)
  }
  
  optimalBundle = function(Ufun, Px, Py, I) {
    MUx=D(Ufun, 'x')
    MUy=D(Ufun, 'y')
    if (class(MUx) == "numeric" & class(MUy) == "numeric") {
      #linear utility function. Likely want corner Solution
      if ((MUx / Px) > (MUy / Py)) {
        #spend all $ on good x
        bundle = c(I / Px, 0)
      } else {
        if ((MUx / Px) < (MUy / Py)) {
          #spend all $ on good y
          bundle = c(0, I / Py)
        } else {
          #equality. Any allocation works. Default to spend all $ such that x=y
          bundle = c(I / (Px + Py), I / (Px + Py))
        }
      }
    } else {
      #not linear, use exactBundle()
      bundle = exactBundle(Ufun, Px, Py, I)
      #check for negatives
      if (bundle[1] < 0){
        #spend all $ on good y
        bundle = c(0, I / Py)
      } else {
        if(bundle[2] < 0){
          #spend all $ on good x
          bundle = c(I / Px, 0)
        }
      }
    }
    return(bundle)
  }
  
  
  solveIntermediate = function(x, Ufun, Px, Py, U){
    vars = getVars(Ufun, x[1], x[2], Px, Py)
    result = crossprod(c(vars$MRSxy, vars$U) - c(Px/Py, U))
    return(result)
  }
  
  exactIntermediate = function(Ufun, Px, Py, U, precision = .0001){
    result = optim(c(1,1), solveIntermediate, Ufun = Ufun, Px = Px, Py = Py, U = U)$par
    result1 = optim(result, solveIntermediate, Ufun = Ufun, Px = Px, Py = Py, U = U)$par
    diff = abs(result-result1)
    count = 0
    while ((diff>precision)[1]||(diff>precision)[2]){
      count = count + 1
      result = result1
      result1 = optim(result, solveIntermediate, Ufun = Ufun, Px = Px, Py = Py, U = U)$par
      if (count > 2 & (result1[1] < 0 | result1[2] < 0)){
        #to catch when the solution doesn't converge due to runaway outside of the positive quadrent
        result = result1
      }
      diff = abs(result-result1)
    }
    return(result1)
  }
  
  optimalIntermediate = function(Ufun, Px, Py, U, xmax, ymax) {
    MUx=D(Ufun, 'x')
    MUy=D(Ufun, 'y')
    if (class(MUx) == "numeric" & class(MUy) == "numeric") {
      #linear utility function. Likely want corner Solution
      if ((MUx / Px) > (MUy / Py)) {
        #spend all $ on good x
        #make U the same as before
        bundle = c(U / MUx, 0)
      } else {
        if ((MUx / Px) < (MUy / Py)) {
          #spend all $ on good y
          #make U the same as before
          bundle = c(0, U / MUy)
        } else {
          #equality. Any allocation works. Default to spend all $ such that x=y
          bundle = c(U / (MUx + MUy), U / (MUx + MUy))
        }
      }
    } else {
      #not linear, use exactIntermediate
      bundle = exactIntermediate(Ufun, Px, Py, U)
      #check for negatives
      if (bundle[1] < 0){
        #spend all $ on good y
        #make U the same as before
        bundle = c(0, getYValues_U(Ufun, U, 0, ymax))
      } else {
        if(bundle[2] < 0){
          #spend all $ on good x
          #make U the same as before
          bundle = c(getXValues_U(Ufun, U, 0, xmax), 0)
        }
      }
    }
    return(bundle)
  }
  
  ###################### PLOT FUNCTIONS #####################
  
  ######### MAKE INDIFFERENCE CURVE ##########
  
  getUValue_U = function(Ufun, x, y) {
    U <- eval(Ufun) 
    return(U)
  }
  
  solveXValue_U = function(Ufun, U, y, x) crossprod(getUValue_U(Ufun, x, y) - U)
  
  getXValues_U = function(Ufun, U, y, xmax){
    x = optimize(solveXValue_U, c(0:xmax*5), Ufun = Ufun, U = U, y = y)$minimum
    return(x)
  }
  
  solveYValue_U = function(Ufun, U, x, y) crossprod(getUValue_U(Ufun, x, y) - U)
  
  getYValues_U = function(Ufun, U, x, ymax){
    y = optimize(solveYValue_U, c(0:ymax*5), Ufun = Ufun, U = U, x = x)$minimum
    return(y)
  }
  
  makeIndifferenceCurve = function(Ufun, U, xmax, ymax, precision = .01, color = "red"){
    x = seq(0, xmax, precision)
    y = sapply(x, getYValues_U, Ufun = Ufun, U = U, ymax = ymax)
    indifferenceCurve=tibble(x = x, y = y, U = as.factor(U))
    if (class(color) == "numeric"){
      color = as.factor(color)
      indifferenceCurveGeom = geom_path(data = indifferenceCurve, aes(x = x, y = y, color = color))
    } else {
      indifferenceCurveGeom = geom_path(data = indifferenceCurve, aes(x = x, y = y), color = color)
    }
    return(indifferenceCurveGeom)
  }
  
  makeAllIndiffernceCurves = function(Ufun, Ulist, xmax, ymax, precision = .01){
    x = seq(0, xmax, precision)
    for (i in 1:length(Ulist)) {
      y = sapply(x, getYValues_U, Ufun = Ufun, U = Ulist[i], ymax = ymax)
      if (i == 1) {
        indifferenceCurves = tibble(x = x, y = y, U = Ulist[i])
      } else {
        new_u = tibble(x = x, y = y, U = Ulist[i])
        indifferenceCurves = indifferenceCurves %>%
          bind_rows(new_u, id = NULL)
      }
    }
    indifferenceCurves = indifferenceCurves %>%
      arrange(as.numeric(U)) %>%
      mutate(U = as.factor(U))
    indifferenceCurvesGeom = geom_path(data = indifferenceCurves, aes(x = x, y = y, color = U))
  }
  
  
  ############# MRS INDIFFERENCE CURVE ############
  
  makeMRSCurve = function(Ufun, U, xmax, ymax, precision = .01){
    x = seq(0, xmax, precision)
    y = sapply(x, getYValues_U, Ufun = Ufun, U = U, ymax = ymax)
    MUx=D(Ufun, 'x')
    MUy=D(Ufun, 'y')
    indifferenceCurve = tibble(x = x, y = y, U = U, MRSxy = eval(MUx)/eval(MUy))
    MRSCurveGeom = geom_path(data = indifferenceCurve, aes(x = x, y = y, color = MRSxy))
    MRSPointsGeom = geom_point(data = indifferenceCurve, aes(x = x, y = y, color = MRSxy))
    return(list(MRSCurveGeom, MRSPointsGeom))
  }
  
  
  ############# MAKE BUDGET LINE #############
  
  makeBudgetLine = function(Px, Py, I, color = "blue", linetype = "solid"){
    x = seq(0, I/Px, length.out = 100)
    y = seq(I/Py, 0, length.out = 100)
    budgetLine = tibble(x,y)
    budgetLineGeom = geom_path(data = budgetLine, aes(x=x, y=y), color = color, linetype = linetype)  
    return(budgetLineGeom)
  }
  
  makeBudgetLines = function(Pxlist, Pylist, Ilist, color = TRUE, linetype = "solid"){
    #first make all lists the same length (should all be length 1 or n)
    nLines = max(length(Pxlist), length(Pylist), length(Ilist))
    if(length(Pxlist) != nLines){
      Pxlist = rep(Pxlist, nLines)
    }
    if(length(Pylist) != nLines){
      Pylist = rep(Pylist, nLines)
    }
    if(length(Ilist) != nLines){
      Ilist = rep(Ilist, nLines)
    }
    x = as.vector(sapply(Ilist/Pxlist, seq, from = 0, length.out = 100))
    y = as.vector(sapply(Ilist/Pylist, seq, to = 0, length.out = 100))
    Budget_Lines = x
    for(i in 0:(nLines-1)){
      Budget_Lines[(i*100+1):(i*100+100)] = rep(paste0(" I = ", Ilist[(i+1)], ", Px = ", Pxlist[(i+1)], ", Py = ", Pylist[(i+1)]), 100)
    }
    budgetLineData = tibble(x, y, Budget_Lines)
    if(color == TRUE){
      budgetLinesGeom = geom_line(data = budgetLineData, aes(x = x, y = y, color = Budget_Lines), linetype = linetype)
    } else {
      budgetLinesGeom = geom_line(data = budgetLineData, aes(x = x, y = y, group = Budget_Lines), color = color, linetype = linetype)
    }
    return(budgetLinesGeom)
  }
  
  
  ########## MAKE INCOME EXPANSION ###########
  
  getMRS = function(Ufun, x, y){
    MUx=D(Ufun, 'x')
    MUy=D(Ufun, 'y')
    MRSxy = eval(MUx)/eval(MUy)
    return(MRSxy)
  }
  
  solveYValue_IE <- function(Ufun, x, y, Px, Py) crossprod(getMRS(Ufun, x, y) - (Px/Py))
  
  getYValues_IE = function(Ufun, x, ymax, Px, Py){
    y = optimize(solveYValue_IE, c(0:ymax*5), Ufun = Ufun, x = x, Px = Px, Py = Py)$minimum
    return(y)
  }
  
  makeIncomeExpansion = function(Ufun, Px, Py, xmax, ymax, precision = .01, color = "darkgreen") {
    MUx = D(Ufun, 'x')
    MUy = D(Ufun, 'y')
    if (class(MUx) == "numeric" & class(MUy) == "numeric") {
      #linear utility function. Likely want corner Solution
      if ((MUx / Px) > (MUy / Py)) {
        #spend all $ on good x
        x = c(0, xmax*1.2)
        y = c(0, 0)
      } else {
        if ((MUx / Px) < (MUy / Py)) {
          #spend all $ on good y
          x = c(0, 0)
          y = c(0, ymax*1.2)
        } else {
          #equality. Any allocation works. Default to spend all $ such that x=y
          x = c(0, max(xmax*1.2, ymax*1.2))
          y = c(0, max(xmax*1.2, ymax*1.2))
        }
      }
    } else {
      x = seq(precision, xmax*1.2, precision)
      y = sapply(x, getYValues_IE, Ufun = Ufun, ymax = ymax, Px = Px, Py = Py)
    }
    incomeExpansion = tibble(x = x, y = y)
    incomeExpansionGeom = geom_path(data = incomeExpansion, aes(x = x, y = y), color = color)
    return(incomeExpansionGeom)
  }
  
  makeIncomeExpansionPlotly = function(Ufun, Px, Py, I, xmax, ymax){
    bundles = sapply(I, optimalBundle, Ufun = Ufun, Px = Px, Py = Py)
    x = bundles[1,]
    y = bundles[2,]
    U = c()
    for (i in 1:length(I)){
      U = c(U,round(getUValue_U(Ufun, x[i], y[i]),4))
    }
    plot = ggplot()+
      makeBudgetLines(Px, Py, I, color = "blue") +
      makeAllIndiffernceCurves(Ufun, U, xmax, ymax) +
      scale_color_viridis_d("U(x,y)", begin = .25, end = .85, option="plasma")+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)+
      makeIncomeExpansion(Ufun, Px, Py, xmax, ymax)+
      coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
    
    plot = ggplotly(plot)
    return(plot)
  }
  
  new_scale <- function(new_aes) {
    structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
  }
  
  makeIncomeExpansionPlot = function(Ufun, Px, Py, I, xmax, ymax){
    bundles = sapply(I, optimalBundle, Ufun = Ufun, Px = Px, Py = Py)
    x = bundles[1,]
    y = bundles[2,]
    U = c()
    for (i in 1:length(I)){
      U = c(U,round(getUValue_U(Ufun, x[i], y[i]),4))
    }
    plot = ggplot()+
      makeBudgetLines(Px, Py, I) +
      scale_color_viridis_d("Budget Lines", option = "mako", begin = .3, end = .7) +
      new_scale("color")+
      makeAllIndiffernceCurves(Ufun, U, xmax, ymax) +
      scale_color_viridis_d("U(x,y)", begin = .25, end = .85, option="plasma") +
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)+
      makeIncomeExpansion(Ufun, Px, Py, xmax, ymax)+
      coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
    
    return(plot)
  }
  
  ######### CONSTRAINED OPTIMIZATION #########
  
  makeOptimalBundle_Point = function(Ufun, Px, Py, I, color = "black", size = 3){
    bundle = optimalBundle(Ufun, Px, Py, I)
    x = bundle[1]
    y = bundle[2]
    optimalBundle_PointGeom = annotate("point", x = x, y = y, color = color, size = size)
    return(optimalBundle_PointGeom)
  }
  
  makeOptimalBundle_Indifference = function(Ufun, Px, Py, I, xmax, ymax, precision = .01, color = "red"){
    bundle = optimalBundle(Ufun, Px, Py, I)
    results = round(getVars(Ufun, bundle[1], bundle[2], Px, Py),2)
    U = results$U
    x = seq(0, xmax, precision)
    y = sapply(x, getYValues_U, Ufun = Ufun, U = U, ymax = ymax)
    indifferenceCurve=tibble(x = x, y = y, U = as.factor(U))
    if (class(color) == "numeric"){
      color = as.factor(color)
      indifferenceCurveGeom = geom_path(data = indifferenceCurve, aes(x = x, y = y, color = U))
    } else {
      indifferenceCurveGeom = geom_path(data = indifferenceCurve, aes(x = x, y = y), color = color)
    }
    return(indifferenceCurveGeom)
  }
  
  ##### SUBSTITUTION/INCOME/TOTAL EFFECT #####
  
  makeEffectsPlot = function(Ufun, Px, Py, I, xmax, ymax){
    bundle_1 = optimalBundle(Ufun, Px[1], Py[1], I[1])
    results_1 = round(getVars(Ufun, bundle_1[1], bundle_1[2], Px[1], Py[1]),2)
    
    bundle_2 = optimalIntermediate(Ufun, Px[2], Py[2], results_1$U, xmax, ymax)
    results_2 = round(getVars(Ufun, bundle_2[1], bundle_2[2], Px[2], Py[2]),2)
    
    bundle_3 = optimalBundle(Ufun, Px[2], Py[2], I[2])
    results_3 = round(getVars(Ufun, bundle_3[1], bundle_3[2], Px[2], Py[2]),2)
    
    effectsPlot = ggplot() + 
      makeOptimalBundle_Indifference(Ufun, Px[1], Py[1], I[1], xmax, ymax, color = 1) +
      makeOptimalBundle_Indifference(Ufun, Px[2], Py[2], I[2], xmax, ymax, color = 1) +
      scale_color_viridis_d(begin = .5, end = .8, option="plasma") +
      labs(color = "U(x,y)") +
      makeBudgetLine(Px[1], Py[1], I[1]) + 
      makeBudgetLine(Px[2], Py[2], I[2], color = "lightblue") + 
      makeBudgetLine(Px[2], Py[2], results_2$Cost, color = "lightblue", linetype = "longdash") + 
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      makeIncomeExpansion(Ufun, Px[1], Py[1], xmax, ymax) +
      makeIncomeExpansion(Ufun, Px[2], Py[2], xmax, ymax, color = "green") +
      annotate("point", x = results_1$x, y = results_1$y, size = 3) +
      annotate("point", x = results_2$x, y = results_2$y, size = 3) +
      annotate("point", x = results_3$x, y = results_3$y, size = 3) +
      coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
    effectsPlot = ggplotly(effectsPlot)
    
    substitutionEffect = c(results_2$x-results_1$x, results_2$y-results_1$y)
    incomeEffect = c(results_3$x-results_2$x, results_3$y-results_2$y)
    totalEffect = substitutionEffect+incomeEffect
    return(list(effectsPlot, substitutionEffect, incomeEffect, totalEffect))
  }
  
  ############### ENGEL CURVES ###############
  
  makeEngelData = function(Ufun, Px, Py, xmax, ymax, precision = .01) {
    MUx = D(Ufun, 'x')
    MUy = D(Ufun, 'y')
    if (class(MUx) == "numeric" & class(MUy) == "numeric") {
      #linear utility function. Likely want corner Solution
      if ((MUx / Px) > (MUy / Py)) {
        #spend all $ on good x
        x = c(0, xmax*1.2)
        y = c(0, 0)
      } else {
        if ((MUx / Px) < (MUy / Py)) {
          #spend all $ on good y
          x = c(0, 0)
          y = c(0, ymax*1.2)
        } else {
          #equality. Any allocation works. Default to spend all $ such that x=y
          x = c(0, max(xmax*1.2, ymax*1.2))
          y = c(0, max(xmax*1.2, ymax*1.2))
        }
      }
    } else {
      x = seq(precision, xmax*1.2, precision)
      y = sapply(x, getYValues_IE, Ufun = Ufun, ymax = ymax, Px = Px, Py = Py)
    }
    engelCurvesData = tibble(x = x, y = y, I = x*Px + y*Py) %>%
      arrange(I) %>%
      round(3)
    return(engelCurvesData)
  }
  
  makeEngelCurveX = function(Ufun, Px, Py, xmax, ymax, precision = .01){
    engelData = makeEngelData(Ufun, Px, Py, xmax, ymax)
    engelCurveX = ggplot() +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_path(data = engelData, aes(x = x, y = I), color = "darkgreen") +
      coord_cartesian(xlim = c(0,xmax), ylim = c(0,max(engelData$I)))
    return(engelCurveX)
  }
  
  makeEngelCurveY = function(Ufun, Px, Py, xmax, ymax, precision = .01){
    engelData = makeEngelData(Ufun, Px, Py, xmax, ymax)
    engelCurveY = ggplot() +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_path(data = engelData, aes(x = y, y = I), color = "darkgreen") +
      coord_cartesian(xlim = c(0,ymax), ylim = c(0,max(engelData$I)))
    return(engelCurveY)
  }
  
  ########## DERIVED DEMAND CURVE ############
  
  makeDemandDataX = function(Ufun, Py, I, addedPx, Pxmin, Pxmax, precision = .5){
    Px = c(seq(Pxmin, Pxmax, precision), addedPx)
    bundles = sapply(Px, optimalBundle, Ufun = Ufun, Py = Py, I = I)
    x = round(bundles[1,], 2)
    addedX = x[(length(x)-length(addedPx)+1):length(x)]
    bundleList = bundles[, (length(x)-length(addedPx)+1):length(x)]
    demandX = tibble(Px, x) %>%
      arrange(Px)
    demandAddedX = tibble(Px = addedPx, x = addedX)
    return(list(demandX, demandAddedX, bundleList))
  }
  
  makeDemandDataY = function(Ufun, Px, I, addedPy, Pymin, Pymax, precision = .5){
    Py = c(seq(Pymin, Pymax, precision), addedPy)
    bundles = sapply(Py, optimalBundle, Ufun = Ufun, Px = Px, I = I)
    y = round(bundles[2,], 2)
    addedY = y[(length(y)-length(addedPy)+1):length(y)]
    bundleList = bundles[, (length(y)-length(addedPy)+1):length(y)]
    demandY = tibble(Py, y) %>%
      arrange(Py)
    demandAddedY = tibble(Py = addedPy, y = addedY)
    return(list(demandY, demandAddedY, bundleList))
  }
  
  makeDerivedDemandPlotX = function(Ufun, Py, I, addedPx, Pxmin, Pxmax, xmax, ymax, precision = .5){
    demandDataX = makeDemandDataX(Ufun, Py, I, addedPx, Pxmin, Pxmax, precision)
    
    demandX = demandDataX[[1]]
    demandAddedX = demandDataX[[2]]
    bundleList = demandDataX[[3]]
    
    Ulist = c()
    for (i in 1:length(addedPx)){
      x = bundleList[1, i]
      y = bundleList[2, i]
      results = round(getVars(Ufun, bundleList[1, i], bundleList[2, i], addedPx[i], Py), 2)
      Ulist = c(Ulist, results$U)
    }
    
    indiffCurvesPlot = ggplot() + 
      makeAllIndiffernceCurves(Ufun, Ulist, xmax, ymax)+
      scale_color_viridis_d(begin = .1, end = .8, option="plasma") +
      labs(color = "U(x,y)") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
    
    for (i in 1:length(addedPx)){
      indiffCurvesPlot = indiffCurvesPlot +
        makeBudgetLine(addedPx[i], Py, I) +
        makeOptimalBundle_Point(Ufun, addedPx[i], Py, I)
    }
    
    indiffCurvesPlot = ggplotly(indiffCurvesPlot) %>%
      layout(yaxis = list(title = "y"), xaxis = list(title = "x"))
    
    demandPlot = ggplot()+
      geom_path(data = demandX, aes(x = x, y = Px), color = "red") +
      geom_point(data = demandAddedX, aes(x = x, y = Px), size = 3) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0, xmax), ylim = c(0, Pxmax)) 
    
    demandPlot = ggplotly(demandPlot) %>%
      layout(yaxis = list(title = "Px"), xaxis = list(title = "x"))
    
    derivedDemandPlot = subplot(indiffCurvesPlot, demandPlot, nrows = 2, shareX = TRUE, titleY = TRUE, titleX = TRUE) %>%
      layout(title = list(text = "Derived Demand Curve for x"))
    
    return(derivedDemandPlot)
  }
  
  makeDerivedDemandPlotY = function(Ufun, Px, I, addedPy, Pymin, Pymax, xmax, ymax, precision = .5){
    demandDataY = makeDemandDataY(Ufun, Px, I, addedPy, Pymin, Pymax, precision)
    
    demandY = demandDataY[[1]]
    demandAddedY = demandDataY[[2]]
    bundleList = demandDataY[[3]]
    
    Ulist = c()
    for (i in 1:length(addedPy)){
      x = bundleList[1, i]
      y = bundleList[2, i]
      results = round(getVars(Ufun, bundleList[1, i], bundleList[2, i], Px, addedPy[i]), 2)
      Ulist = c(Ulist, results$U)
    }
    
    indiffCurvesPlot = ggplot() + 
      makeAllIndiffernceCurves(Ufun, Ulist, xmax, ymax)+
      scale_color_viridis_d(begin = .1, end = .8, option="plasma") +
      labs(color = "U(x,y)") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
    
    for (i in 1:length(addedPy)){
      indiffCurvesPlot = indiffCurvesPlot +
        makeBudgetLine(Px, addedPy[i], I) +
        makeOptimalBundle_Point(Ufun, Px, addedPy[i], I)
    }
    
    indiffCurvesPlot = indiffCurvesPlot +
      coord_flip()
    
    indiffCurvesPlot = ggplotly(indiffCurvesPlot) %>%
      layout(yaxis = list(title = "x"), xaxis = list(title = "y"))
    
    demandPlot = ggplot()+
      geom_path(data = demandY, aes(x = y, y = Py), color = "red") +
      geom_point(data = demandAddedY, aes(x = y, y = Py), size = 3) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0, xmax), ylim = c(0, Pxmax)) 
    
    demandPlot = ggplotly(demandPlot) %>%
      layout(yaxis = list(title = "Py"), xaxis = list(title = "y"))
    
    derivedDemandPlot = subplot(indiffCurvesPlot, demandPlot, nrows = 2, shareX = TRUE, titleY = TRUE, titleX = TRUE) %>%
      layout(title = list(text = "Derived Demand Curve for y"))
    
    return(derivedDemandPlot)
  }
  
  ##### FEASIBLE VS INFEASIBLE BUNDLES #######
  
  makeRandomBundles = function(xmax, ymax, N){
    x = runif(N, max = xmax)
    y = runif(N, max = ymax)
    return(list(x, y))
  }
  
  makeRandomFeasibilityGraphs = function(Px, Py, I, xmax, ymax, x, y){
    bundles = tibble(x, y, Cost = x*Px + y*Py, Feasibility = x)
    N = dim(bundles)[1]
    for (i in 1:N){
      if(I>=bundles$Cost[i]){
        bundles$Feasibility[i] = "Feasible"
      } else {
        bundles$Feasibility[i] = "Infeasible"
      }
    }
    bundles = bundles %>%
      arrange(Cost)
    
    bundlesPlot = ggplot()+
      geom_point(data = bundles, aes(x = x, y = y, color = Cost), size = 3) +
      scale_color_viridis_c(guide = "none", option = "inferno", begin = .3, end = .9) +
      makeBudgetLine(Px, Py, I) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
    bundlesPlot = bundlesPlot %>%
      ggplotly()
    
    feasibilityPlot = ggplot()+
      geom_point(data = bundles, aes(x = x, y = y, alpha = Feasibility, color = Cost), size = 3) +
      scale_color_viridis_c(guide = "none", option = "inferno", begin = .3, end = .9) +
      scale_alpha_discrete(range = c(1,.3*min(N, 2000)/N)) +
      labs(alpha = c("Bundles")) +
      makeBudgetLine(Px, Py, I) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
    feasibilityPlot = feasibilityPlot %>%
      ggplotly() %>%
      layout(showlegend = TRUE)
    return(list(bundlesPlot, feasibilityPlot))
  }
  
  makeFeasibilityGraphs = function(Px, Py, I, on = 2, over = 2, under = 2, rand = 6){
    xon = runif(on, max = I/Px)
    yon = (I-Px*xon)/Py
    xrand = runif(rand, max = I/Px*1.2)
    yrand = runif(rand, max = I/Py*1.2)
    xover = runif(over, min = 0, max = I/Px*1.2)
    yover=c()
    for(i in 1:over){
      x = xover[i]
      yover = c(yover, runif(1, min = max((I-x*Px)/Py, 0), max = I/Py*1.2))
    } 
    xunder = runif(under, max = I/Px)
    yunder = c()
    for(i in 1:under){
      x = xunder[i]
      yunder = c(yunder, runif(1, min = 0, max = (I-x*Px)/Py))
    } 
    x = c(xrand, xon, xover, xunder)
    y = c(yrand, yon, yover, yunder)
    
    bundles = tibble(x, y, Cost = x*Px + y*Py, Feasibility = x)
    for (i in 1:length(x)){
      if(I>=bundles$Cost[i]){
        bundles$Feasibility[i] = "Feasible"
      } else {
        bundles$Feasibility[i] = "Infeasible"
      }
    }
    bundles = bundles %>%
      arrange(Cost)
    
    bundlesPlot = ggplot()+
      geom_point(data = bundles, aes(x = x, y = y, color = Cost), size = 3) +
      scale_color_viridis_c(guide = "none", option = "inferno", begin = .3, end = .9) +
      makeBudgetLine(Px, Py, I) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0,I/Px*1.2), ylim = c(0,I/Py*1.2))
    bundlesPlot = bundlesPlot %>%
      ggplotly()
    
    feasibilityPlot = ggplot()+
      geom_point(data = bundles, aes(x = x, y = y, alpha = Feasibility, color = Cost), size = 3) +
      scale_color_viridis_c(guide = "none", option = "inferno", begin = .3, end = .9) +
      scale_alpha_discrete(range = c(1,.2)) +
      labs(alpha = c("Bundles")) +
      makeBudgetLine(Px, Py, I) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0,I/Px*1.2), ylim = c(0,I/Py*1.2))
    feasibilityPlot = feasibilityPlot %>%
      ggplotly() %>%
      layout(showlegend = TRUE)
    return(list(bundlesPlot, feasibilityPlot))
  }
  
  ############### ELASTICITIES ###############
  
  
  elasticityPx = function(Ufun, Px, Py, I, precision = .00001){
    Pxlist = seq(Px-precision, Px+precision, precision)
    bundles = round(sapply(Pxlist, optimalBundle, Ufun = Ufun, Py = Py, I = I),7)
    pDX = (bundles[1,1]-bundles[1,3])/(bundles[1,2])
    pDP = 2*precision/Px
    pDY = (bundles[2,3]-bundles[2,1])/(bundles[2,2])
    ePriceDemandX = round(pDX/pDP,2)
    eCrossDemandY = round(pDY/pDP,2)
    return(list(ePriceDemandX, eCrossDemandY))
  }
  
  elasticityPy = function(Ufun, Px, Py, I, precision = .00001){
    Pylist = seq(Py-precision, Py+precision, precision)
    bundles = round(sapply(Pylist, optimalBundle, Ufun = Ufun, Px = Px, I = I),7)
    pDY = (bundles[2,1]-bundles[2,3])/(bundles[2,2])
    pDP = 2*precision/Py
    pDX = (bundles[1,3]-bundles[1,1])/(bundles[1,2])
    ePriceDemandY = round(pDY/pDP,2)
    eCrossDemandX = round(pDX/pDP,2)
    return(list(eCrossDemandX, ePriceDemandY))
  }
  
  elasticityI = function(Ufun, Px, Py, I, precision = .00001){
    Ilist = seq(I-precision, I+precision, precision)
    bundles = round(sapply(Ilist, optimalBundle, Ufun = Ufun, Px = Px, Py = Py),7)
    pDY = (bundles[2,3]-bundles[2,1])/(bundles[2,2])
    pDI = 2*precision/I
    pDX = (bundles[1,3]-bundles[1,1])/(bundles[1,2])
    eIncomeDemandY = round(pDY/pDI,2)
    eIncomeDemandX = round(pDX/pDI,2)
    return(list(eIncomeDemandX, eIncomeDemandY))
  }
  
  getElasticities = function(Ufun, Px, Py, I, precision = .00001){
    ePx = elasticityPx(Ufun, Px, Py, I, precision)
    ePy = elasticityPy(Ufun, Px, Py, I, precision)
    eI = elasticityI(Ufun, Px, Py, I, precision)
    elasticities = tibble(type = c("Price Elasticity of Demand", "Cross Price Elasticity", "Income Elasticity of Demand"),
                          x = c(ePx[[1]], ePy[[1]], eI[[1]]),
                          y = c(ePy[[2]], ePx[[2]], eI[[2]])
    )
    return(elasticities)
  }
  
  ######### PIECEWISE MARKET DEMAND ##########
  
  getQValue_Choke = function(Dfun, p) {
    Q <- eval(Dfun) 
    return(Q)
  }
  
  makePiecewise = function(demandList, NList){
    num = length(demandList)
    chokeList = list()
    chokeVector = c()
    demandNList = list()
    demandNListExpanded = list()
    for (i in 1:num){
      demandExpr = Ryacas::yac_expr(demandList[[i]])
      p = 0
      chokeList[[i]] = yac_str(paste0("OldSolve(",demandList[[i]],"==0, p)")) %>% y_rmvars() %>% yac_expr() %>% eval()
      chokeVector = c(chokeVector, chokeList[[i]])
      demandNList[[i]] = Ryacas::yac_str(paste0(NList[[i]], "*(", demandList[[i]], ")"))
      demandNListExpanded[[i]] = Ryacas::yac('Expand(%)')
    }
    chokes = sort(unique(c(0,chokeVector)))
    demandMarket = list()
    demandMarketSimp = list()
    demandMarketExpanded = list()
    for (i in 1:(length(chokes)-1)){
      #get index of demand functions not choked out
      demandIndicies = (1:length(chokeVector))[chokeVector>chokes[i]]
      for (j in 1:length(demandIndicies)){
        if (j > 1){
          demandMarket[[i]] = Ryacas::yac_str(paste(demandMarket[[i]], demandNList[[demandIndicies[j]]], sep = " + "))
        } else {
          demandMarket[[i]] = Ryacas::yac_str(demandNList[[demandIndicies[j]]])
        }
      }
      demandMarketSimp[[i]] = demandMarket[[i]] %>%
        Ryacas::yac_symbol() %>%
        Ryacas::simplify() %>%
        Ryacas::yac_str()
      demandMarketExpanded[[i]] = Ryacas::yac('Expand(%)')
    }
    return(list(demandNList, demandNListExpanded, chokeList, chokes, demandMarket, demandMarketSimp, demandMarketExpanded))
  }
  
  makePiecewisePlot = function(demandNList, chokeList, chokes, demandMarket, supply = 0){
    for (i in 1:length(demandNList)){
      demadExpr = Ryacas::yac_expr(demandNList[[i]])
      p = seq(0,chokeList[[i]], .01)
      Q = sapply(p, getQValue_Choke, Dfun = demadExpr)
      if(i == 1){
        individualDemandData = tibble("Demand_Function" = as.factor(i), p = p, Q = Q) 
      } else {
        individualDemandData = individualDemandData %>%
          bind_rows(tibble("Demand_Function" = as.factor(i), p = p, Q = Q)) 
      }
    }
    
    for (i in 1:length(demandMarket)){
      demadExpr = Ryacas::yac_expr(demandMarket[[i]])
      p = seq(chokes[i],chokes[i+1], .01)
      Q = sapply(p, getQValue_Choke, Dfun = demadExpr)
      if(i == 1){
        piecewiseDemandData = tibble("Demand_Function" = "Market", p = p, Q = Q) 
      } else {
        piecewiseDemandData = piecewiseDemandData %>%
          bind_rows(tibble("Demand_Function" = "Market", p = p, Q = Q)) 
      }
    }
    supplyExpr = Ryacas::yac_expr(supply)
    p = seq(0, chokes[length(chokes)], .01)
    Q = sapply(p, getQValue_Choke, Dfun = supplyExpr)
    supplyData = tibble("Supply_Function" = "Market", p = p, Q = Q)
    
    plist = list()
    for(i in 1:length(demandMarket)){
      plist[[i]] = Ryacas::yac_str(paste0("Solve(", demandMarket[[i]], " == ", supply, ", p)")) %>%
        Ryacas::y_rmvars() %>% 
        Ryacas::yac_expr() %>% 
        eval() %>%
        round(2)
      if(plist[[i]] < chokes[[i+1]] & plist[[i]] >= chokes[[i]]){
        correctPiece = i
      }
    }
    p = plist[[correctPiece]]
    Q = round(eval(Ryacas::yac_expr(supply)),2)
    equilibriumData = tibble(p = p, Q = Q, Equilibrium = "Market Equilibrium")
    Q = seq(0, max(piecewiseDemandData$Q)*1.2, 1)
    equilibriumPriceLine = tibble(p = p, Q = Q, Equilibrium = "Price Level")
    Q = c()
    Qindividual = c()
    for (i in 1:length(chokeList)){
      Qindividual = c(Qindividual, round(eval(Ryacas::yac_expr(demandNList[[i]])),2))
      if(chokeList[[i]] > p){
        Q = c(Q,Qindividual[i])
      }
    }
    equilibriumIndividual = tibble(p = p, Q = Q, Equilibrium = "Group Equilibrium")
    
    demandPlot = ggplot()+
      geom_line(data = individualDemandData, aes(x = Q, y = p, color = Demand_Function)) +
      scale_color_viridis_d(option = "viridis", begin = 0, end = .9, name = "Demand Function") +
      geom_line(data = piecewiseDemandData, aes(x = Q, y = p, group = Demand_Function), color = "darkgreen") + 
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0,max(piecewiseDemandData$Q)), ylim = c(0,chokes[length(chokes)]))
    demandPlot = ggplotly(demandPlot)
    
    marketPlot = ggplot()+
      geom_line(data = individualDemandData, aes(x = Q, y = p, color = Demand_Function)) +
      scale_color_viridis_d(option = "viridis", begin = 0, end = .9, name = "Demand Function") +
      geom_line(data = piecewiseDemandData, aes(x = Q, y = p, group = Demand_Function), color = "darkgreen") +
      geom_line(data = supplyData, aes(x = Q, y = p, group = Supply_Function), color = "red") +
      geom_point(data = equilibriumData, aes(x = Q, y = p, group = Equilibrium), size = 3) +
      geom_line(data = equilibriumPriceLine, aes(x = Q, y = p, group = Equilibrium), linetype = "dashed") +
      geom_point(data = equilibriumIndividual, aes(x = Q, y = p, group = Equilibrium), size = 2) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0,max(piecewiseDemandData$Q)), ylim = c(0,chokes[length(chokes)]))
    marketPlot = ggplotly(marketPlot)
    return(list(demandPlot, marketPlot, plist, correctPiece, equilibriumData$Q, Qindividual))
  }
  
  ####################### PRODUCTION ########################
  
  ############### ISOQUANTS ##################
  
  getQValue_Q = function(prodFun, K, L) {
    Q <- eval(prodFun) 
    return(Q)
  }
  
  getKValues_Q = function(prodFun, L, Q){
    prodFunK = Ryacas::yac_str(paste0(prodFun," Where L==",L))
    K = NA
    K = try(Ryacas::yac_str(paste0("OldSolve(",prodFunK,"==",Q,", K)")) %>% y_rmvars() %>% yac_expr() %>% eval(), silent = TRUE)
    if (inherits(K, "try-error")){
      K = NA
    }
    return(K)
  }
  
  makeIsoquantCurve = function(prodfun, Q, LMax, smooth = 100, color = "red"){
    prodFun = Ryacas::yac_expr(prodfun)
    L = seq(0, LMax, length.out = smooth)
    L = round(L, 2)
    K = sapply(L, getKValues_Q, prodFun = prodFun, Q = Q)
    K = round(K, 2)
    isoquantCurve=tibble(L = L, K = K, Isoquant_Line = paste0("Q= ", Q))
    if (class(color) == "numeric"){
      color = as.factor(color)
      isoquantCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, color = Isoquant_Line))
    } else {
      isoquantCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, group = Isoquant_Line), color = color)
    }
    return(isoquantCurveGeom)
  }
  
  makeAllIsoquantCurves = function(prodfun, QList, LMax, color = TRUE, smooth = 100){
    QList = sort(QList)
    prodFun = Ryacas::yac_expr(prodfun)
    L = seq(0, LMax, length.out = smooth)
    L = round(L, 2)
    for (i in 1:length(QList)) {
      K = sapply(L, getKValues_Q, prodFun = prodFun, Q = QList[i])
      K = round(K, 2)
      if (i == 1) {
        isoquantCurves = tibble(L = L, K = K, Isoquant_Line = paste0("Q= ", QList[i]))
      } else {
        new_q = tibble(L = L, K = K, Isoquant_Line = paste0("Q= ", QList[i]))
        isoquantCurves = isoquantCurves %>%
          bind_rows(new_q, id = NULL)
      }
    }
    isoquantCurves$Isoquant_Line = factor(isoquantCurves$Isoquant_Line, levels = unique(isoquantCurves$Isoquant_Line))
    if(color == TRUE){
      isoquantCurvesGeom = geom_path(data = isoquantCurves, aes(x = L, y = K, color = Isoquant_Line))
    } else {
      isoquantCurvesGeom = geom_path(data = isoquantCurves, aes(x = L, y = K, group = Isoquant_Line), color = color)
    }
    return(isoquantCurvesGeom)
  }
  
  ################ MRTS ######################
  
  makeMRTSCurve = function(prodfun, Q, LMax, smooth = 100){
    prodFun = Ryacas::yac_expr(prodfun)
    L = seq(0, LMax, length.out = smooth)
    L = round(L, 2)
    K = sapply(L, getKValues_Q, prodFun = prodFun, Q = Q)
    K = round(K, 2)
    MPL=Ryacas::yac_expr(Ryacas::yac_str(paste0("D(L) ",prodfun)))
    MPK=Ryacas::yac_expr(Ryacas::yac_str(paste0("D(K) ",prodfun)))
    isoquantCurve = tibble(L = L, K = K, Q = Q, MRTS_LK = round(eval(MPL)/eval(MPK), 2))
    MRTSCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, color = MRTS_LK))
    MRTSPointsGeom = geom_point(data = isoquantCurve, aes(x = L, y = K, color = MRTS_LK))
    return(list(MRTSCurveGeom, MRTSPointsGeom))
  }
  
  makeMRTSCurveGraph = function(prodfun, Q, LMax, smooth = 1000){
    MRTSCurve = makeMRTSCurve(prodfun, Q, LMax, smooth = smooth)
    plotly = ggplotly(ggplot() +
                        makeIsoquantCurve(prodfun, Q, LMax, color = "black") +
                        MRTSCurve[[2]] +
                        MRTSCurve[[1]] +
                        scale_color_viridis_c(option = "viridis", limits = c(0, 10), direction = -1) +
                        geom_hline(yintercept = 0) +
                        geom_vline(xintercept = 0) +
                        coord_cartesian(xlim = c(0, LMax), ylim = c(0, LMax))
    )
    return(plotly)
  }
  
  ############### ISOCOSTS ###################
  
  makeIsocostLine = function(r, w, C, color = "blue", linetype = "solid"){
    L = seq(0, C/w, length.out = 100)
    L = round(L, 2)
    K = seq(C/r, 0, length.out = 100)
    K = round(K, 2)
    Isocost_Line = rep(paste0(" C = ", C, ", w = ", w, ", r = ", r))
    isocostLine = tibble(L = L, K = K, Isocost_Line = Isocost_Line)
    isocostLineGeom = geom_path(data = isocostLine, aes(x = L, y = K, group = Isocost_Line), color = color, linetype = linetype)  
    return(isocostLineGeom)
  }
  
  makeAllIsocostLines = function(rList, wList, CList, color = TRUE, linetype = "solid"){
    #first make all lists the same length (should all be length 1 or n)
    nLines = max(length(wList), length(rList), length(CList))
    if(length(wList) != nLines){
      wList = rep(wList, nLines)
    }
    if(length(rList) != nLines){
      rList = rep(rList, nLines)
    }
    if(length(CList) != nLines){
      CList = rep(CList, nLines)
    }
    L = as.vector(sapply(CList/wList, seq, from = 0, length.out = 100))
    L = round(L, 2)
    K = as.vector(sapply(CList/rList, seq, to = 0, length.out = 100))
    K = round(K, 2)
    Isocost_Line = L
    for(i in 0:(nLines-1)){
      Isocost_Line[(i*100+1):(i*100+100)] = rep(paste0(" C = ", CList[(i+1)], ", w = ", wList[(i+1)], ", r = ", rList[(i+1)]), 100)
    }
    isocostLineData = tibble(L = L, K = K, Isocost_Line = Isocost_Line)
    isocostLineData$Isocost_Line = factor(isocostLineData$Isocost_Line, levels = unique(isocostLineData$Isocost_Line))
    if(color == TRUE){
      isocostLinesGeom = geom_line(data = isocostLineData, aes(x = L, y = K, color = Isocost_Line), linetype = linetype)
    } else {
      isocostLinesGeom = geom_line(data = isocostLineData, aes(x = L, y = K, group = Isocost_Line), color = color, linetype = linetype)
    }
    return(isocostLinesGeom)
  }
  
  ########### COST MINIMIZATION ##############
  
  calculateProductionLR = function(prodfun, w, r, steps = FALSE) {
    K = caracas::symbol('K')
    L = caracas::symbol('L')
    Q = caracas::symbol('Q')
    prodFun = caracas::as_sym(prodfun)
    MPL = caracas::der(prodFun, L)
    MPK = caracas::der(prodFun, K)
    #Test if K or L do not have constant returns to scale 
    KisVar1 = caracas::tex(caracas::der(MPL/MPK, K)) != 0
    LisVar1 = caracas::tex(caracas::der(MPL/MPK, L)) != 0
    prodFunLR = list()
    Kexpansion = list()
    Lexpansion = list()
    j = 0
    perfectsSubs = FALSE
    perfectsSubsInterior = FALSE
    if(!LisVar1 & !KisVar1){
      #Perfect substitutes
      perfectsSubs = TRUE
      if(caracas::as_expr(MPL)/w > caracas::as_expr(MPK)/r){
        #L more economical at all input levels
        Kexpansion[[1]] = caracas::as_sym(0)
        prodFunLR[[1]] = caracas::subs(prodFun, K, 0)
        Lexpansion[[1]] = caracas::solve_sys(prodFunLR[[1]], Q, L)[[1]]$L
      } else if(caracas::as_expr(MPL)/w < caracas::as_expr(MPK)/r){
        #K more economical at all input levels
        Lexpansion[[1]] = caracas::as_sym(0)
        prodFunLR[[1]] = caracas::subs(prodFun, L, 0)
        Kexpansion[[1]] = caracas::solve_sys(prodFunLR[[1]], Q, K)[[1]]$K
      } else {
        # L and K equally economical at all input levels
        # Set K = L (arbitrarily)
        perfectsSubsInterior = TRUE
        prodFunLR[[1]] = caracas::subs(prodFun, L, K)
        Kexpansion[[1]] = caracas::solve_sys(prodFunLR[[1]], Q, K)[[1]]$K
        Lexpansion[[1]] = Kexpansion[[1]]
      }
    }
    Lcritical = c()
    Kcritical = c()
    if(LisVar1) Lcritical = try(caracas::solve_sys(MPL/MPK, w/r, L))
    if(KisVar1) Kcritical = try(caracas::solve_sys(MPL/MPK, w/r, K))
    if(LisVar1 | KisVar1){
      #Not perfect substitutes. Possible interior solution
      #search for critical values to test for corner solutions
      #Lcrit first
      #add 0 to critical value
      Lcrit = list(caracas::as_sym(0))
      if(length(Lcritical>0)){
        for(i in 1:length(Lcritical)){
          Lcrit[[i+1]] = (Lcritical[[i]]$L)
        }
      }
      lenLcrit = length(Lcrit)
      for(i in 1:lenLcrit){
        Lexpansion_try = Lcrit[[i]]
        prodFunLR_try = caracas::subs(prodFun, L, Lcrit[[i]])
        results = caracas::solve_sys(prodFunLR_try, Q, K)
        if(length(results)==0) next
        for(k in 1:length(results)){
          j = j + 1
          Lexpansion[[j]] = Lexpansion_try
          prodFunLR[[j]] = prodFunLR_try
          Kexpansion[[j]]= results[[k]]$K
        }
      }
      #Kcrit second
      #add 0 to critical value
      Kcrit = list(caracas::as_sym(0))
      if(length(Kcritical>0)){
        for(i in 1:length(Kcritical)){
          Kcrit[[i+1]] = (Kcritical[[i]]$K)
        }
      }
      lenKcrit = length(Kcrit)
      for(i in 1:lenKcrit){
        Kexpansion_try = Kcrit[[i]]
        prodFunLR_try = caracas::subs(prodFun, K, Kcrit[[i]])
        results = caracas::solve_sys(prodFunLR_try, Q, L)
        if(length(results)==0) next
        for(k in 1:length(results)){
          j = j + 1
          Kexpansion[[j]] = Kexpansion_try
          prodFunLR[[j]] = prodFunLR_try
          Lexpansion[[j]]= results[[k]]$L
        }
      }
    }
    
    #LR expansion Path
    LRcost = list()
    Qcrit = c()
    
    #Calculate LR cost curve under possible solutions
    for(i in 1:length(prodFunLR)){
      KinL = caracas::tex(caracas::der(Lexpansion[[i]],K))!=0
      LinK = caracas::tex(caracas::der(Kexpansion[[i]],L))!=0
      if(LinK) Kexpansion[[i]] = caracas::subs(Kexpansion[[i]], L, Lexpansion[[i]])
      if(KinL) Lexpansion[[i]] = caracas::subs(Lexpansion[[i]], K, Kexpansion[[i]])
      LRcost[[i]] = Lexpansion[[i]]*w + Kexpansion[[i]]*r
    }
    
    #Identify all critical points for LR cost curve
    if(length(prodFunLR) > 1){
      for(i in 1:(length(prodFunLR)-1)){
        for(j in (i+1):length(prodFunLR)){
          Qcritical = caracas::solve_sys(LRcost[[i]], LRcost[[j]], Q)
          if (length(Qcritical) == 0) next
          for(k in length(Qcritical)){
            Qcrit = c(Qcrit, caracas::as_expr(Qcritical[[k]]$Q))
          }
        }
        #add Qcrit for levels of Q at which levels of L or K is 0
        LisVar = caracas::tex(caracas::der(Lexpansion[[i]],Q))!=0
        KisVar = caracas::tex(caracas::der(Kexpansion[[i]],Q))!=0
        if(LisVar){
          result = caracas::solve_sys(Lexpansion[[i]], Q)
          if(length(result)>0){
            for(j in length(result)){
              Qcrit = c(Qcrit, caracas::as_expr(result[[j]]$Q))
            }
          }
        }
        if(KisVar){
          result = caracas::solve_sys(Kexpansion[[i]], Q)
          if(length(result)>0){
            for(j in length(result)){
              Qcrit = c(Qcrit, caracas::as_expr(result[[j]]$Q))
            }
          }
        }
      }
    }
    
    #Test which LR cost curve is positive and least expensive for all levels of output
    if(!is.null(Qcrit)){
      Qcrit = sort(unique(Qcrit[Qcrit>0]))
      Qbins = c(0, Qcrit, max(0, Qcrit)+2)
    } else {
      Qbins = c(0, 2)
    }
    Qtests = c()
    for(i in 1:(length(Qbins)-1)){
      Qtests = c(Qtests, (Qbins[i] + Qbins[i+1])/2)
    }
    
    minLRcostIndex = list()
    for(i in 1:length(Qtests)){
      costTest = c()
      Q = Qtests[i]
      for(j in 1:length(LRcost)){
        Klevel = eval(caracas::as_expr(Kexpansion[[j]]))
        Llevel = eval(caracas::as_expr(Lexpansion[[j]]))
        if(Klevel >= 0 && Llevel >=0){
          costTest = c(costTest, eval(caracas::as_expr(LRcost[[j]])))
        } else {
          costTest = c(costTest, Inf)
        }
      }
      minC = min(costTest[costTest>0])
      minLRcostIndex[[i]] = match(minC, costTest)
    }
    
    #Test if cost function is piecewise
    if (length(minLRcostIndex) > 1){
      #Piecewise. consolidate pieces
      remove = c()
      for(i in 1:(length(minLRcostIndex)-1)){
        if(minLRcostIndex[[i]] == minLRcostIndex[[i+1]]){
          remove = c(remove, i)
        }
      }
      Qcrit = Qcrit[-remove]
      Qbins = Qbins[-(remove+1)]
      minLRcostIndex = minLRcostIndex[-remove]
    }
    Qbins[length(Qbins)] = Inf
    
    #return LR cost function, LR expansion paths, LR prodFun, Piecewise Bounds
    LRExpansion = list()
    for(i in 1:length(minLRcostIndex)){
      LRExpansion[[i]] = list(cost = LRcost[[minLRcostIndex[[i]]]], prodFun = prodFunLR[[minLRcostIndex[[i]]]], Lexpansion = Lexpansion[[minLRcostIndex[[i]]]], Kexpansion = Kexpansion[[minLRcostIndex[[i]]]], Qmin = Qbins[i], Qmax = Qbins[i+1])
    }
    if (steps){
      return(list(LRExpansion = LRExpansion, MPL = MPL, MPK = MPK, perfectsSubs = perfectsSubs, perfectsSubsInterior = perfectsSubsInterior, LisVar1 = LisVar1, KisVar1 = KisVar1, Lcritical = Lcritical, Kcritical = Kcritical, Lexpansion = Lexpansion, Kexpansion = Kexpansion, prodFunLR = prodFunLR, minLRcostIndex = minLRcostIndex, Qbins = Qbins, LRcost = LRcost))
    } else {
      return(LRExpansion)
    }
  }
  
  makeCostMinPoints = function(ProductionLR, QList, color = "black", size = 3){
    #ProductionLR may be piecewise. Need to find correct piece.
    point = "Cost Minimization Solution"
    L = c()
    K = c()
    for (Q in QList){
      ProductionLR_Q = findVarsQ(ProductionLR, Q)[[1]]
      L = c(L, round(eval(caracas::as_expr(ProductionLR_Q$Lexpansion)),2))
      K = c(K, round(eval(caracas::as_expr(ProductionLR_Q$Kexpansion)),2))
    }
    data = tibble(L = L, K = K, point = point)
    Point_Geom = geom_point(data = data, aes(x = L, y = K, group = point), color = color, size = size)
    return(Point_Geom)
  }
  
  findVarsQ = function(ProductionLR, Q){
    for(i in 1:length(ProductionLR)){
      if(ProductionLR[[i]]$Qmin <= Q && ProductionLR[[i]]$Qmax >= Q) break
    }
    ProductionLR_Q = ProductionLR[[i]]
    C = round(eval(caracas::as_expr(ProductionLR_Q$cost)),2)
    L = round(eval(caracas::as_expr(ProductionLR_Q$Lexpansion)),2)
    K = round(eval(caracas::as_expr(ProductionLR_Q$Kexpansion)),2)
    return(list(ProductionLR_Q = ProductionLR_Q, C = C, L = L, K = K))
  }
  
  makeCostMinGraph = function(prodfun, w, r, Q, smooth = 100){
    ProductionLR = calculateProductionLR(prodfun, w, r)
    ProductionLR_Q = findVarsQ(ProductionLR, Q)[[1]]
    C = findVarsQ(ProductionLR, Q)[[2]]
    L = findVarsQ(ProductionLR, Q)[[3]]
    K = findVarsQ(ProductionLR, Q)[[4]]
    LMax = 1.5*C/w
    KMax = 1.5*C/r
    
    plotly = ggplotly(ggplot() + 
                        makeIsocostLine(r, w, C) +
                        makeIsoquantCurve(prodfun, Q, LMax, smooth = smooth) +
                        makeCostMinPoints(ProductionLR, Q) +
                        geom_hline(yintercept = 0) +
                        geom_vline(xintercept = 0) +
                        coord_cartesian(xlim = c(0, LMax), ylim = c(0, KMax))
    )
    return(plotly)
  }
  
  ######## FIRM'S SR EXPANSION PATH ##########
  
  calculateSRExpansion = function(prodfun, K, w, r){
    Kval = K
    K = caracas::symbol('K')
    L = caracas::symbol('L')
    Q = caracas::symbol('Q')
    prodFun = caracas::as_sym(prodfun)
    prodFunSR = caracas::subs(prodFun, K, Kval)
    results = caracas::solve_sys(prodFunSR, Q, L)
    j = 0
    LexpansionSR = list()
    costSR = list()
    if (length(results)>0){
      for(i in 1:length(results)){
        if(caracas::subs(results[[i]]$L, Q, 10)[[1]] > 0){
          j = j+1
          LexpansionSR[[j]] = results[[i]]$L
          costSR[[j]] = LexpansionSR[[j]]*w + Kval*r
        }
      }
    }
    return(list(prodFunSR = prodFunSR, LexpansionSR = LexpansionSR, costSR = costSR, K = Kval))
  }
  
  makeSRExpansion = function(K, LMax, color = "orange"){
    L = round(seq(0, LMax*1.2, length.out = 100),2)
    Line = "Short Run Expansion Path"
    data = tibble(L = L, K = K, Line = Line)
    geom = geom_line(data = data, aes(x = L, y = K, group = Line), color = color)
    return(geom)
  }
  
  makeSRCostMinPoints = function(ProductionSR, QList, color = "black", size = 3){
    #ProductionLR may be piecewise. Need to find correct piece.
    point = "SR Cost Minimization Solution"
    K = ProductionSR$K
    L = c()
    for (Q in QList){
      L = c(L, round(eval(caracas::as_expr(ProductionSR$LexpansionSR[[1]])),2))
    }
    data = tibble(L = L, K = K, point = point)
    Point_Geom = geom_point(data = data, aes(x = L, y = K, group = point), color = color, size = size)
    return(Point_Geom)
  }
  
  makeSRProductionGraph = function(prodfun, K, w, r, LMax, color = "orange", smooth = 100){
    prodFunSR = calculateSRExpansion(prodfun, K, w, r)$prodFunSR
    L = seq(0, LMax, length.out = smooth) %>%
      round(2)
    Q = eval(caracas::as_expr(prodFunSR)) %>%
      round(2)
    Line = "Short Run Production"
    data = tibble(L = L, Q = Q, Line = Line)
    plotly = ggplotly(ggplot() +
                        geom_line(data = data, aes(x = L, y = Q, group = Line), color = color) +
                        geom_hline(yintercept = 0) +
                        geom_vline(xintercept = 0) +
                        coord_cartesian(xlim = c(0, NA), ylim = c(0, NA))
    )
    return(plotly)
  }
  
  ############# FIRM LR VS SR  ###############
  
  makeLRExpansion = function(ProductionLR, QMax, color = "darkgreen"){
    Q = round(seq(0, QMax, length.out = 100),2)
    rProductionLR = try(calculateProductionLR(prodfun, w, r), silent = TRUE)
    if (inherits(ProductionLR, "try-error")){
      #USE seq(), min(), and max() to graph lines
    }
    result = sapply(Q, findVarsQ, ProductionLR = ProductionLR)
    L = round(unlist(result[3,]),2)
    K = round(unlist(result[4,]),2)
    Line = "Long Run Expansion Path"
    data = tibble(L = L, K = K, Line = Line)
    geom = geom_line(data = data, aes(x = L, y = K, group = Line), color = color)
    return(geom)
  }
  
  makeLRvSRExpansionGraph = function(prodfun, w, r, Q, smooth = 100){
    ProductionLR = calculateProductionLR(prodfun, w, r)
    result = findVarsQ(ProductionLR, Q)
    ProductionLR_Q = result[[1]]
    C = result[[2]]
    L = result[[3]]
    K = result[[4]]
    LMax = 1.5*C/w
    KMax = 1.5*C/r
    QMax = Q*4
    
    plotly = ggplotly(ggplot() + 
                        makeIsocostLine(r, w, C) +
                        makeIsoquantCurve(prodfun, Q, LMax, smooth = smooth) +
                        geom_hline(yintercept = 0) +
                        geom_vline(xintercept = 0) +
                        makeSRExpansion(K, LMax) +
                        makeLRExpansion(ProductionLR, QMax) +
                        makeCostMinPoints(ProductionLR, Q) +
                        coord_cartesian(xlim = c(0, LMax), ylim = c(0, KMax))
    )
    return(plotly)
  }
  
  makeLRvSRCostExpansionGraph = function(prodfun, w, r, Q, QList, smooth = 100){
    ProductionLR = calculateProductionLR(prodfun, w, r)
    result = findVarsQ(ProductionLR, Q)
    ProductionLR_Q = result[[1]]
    C = result[[2]]
    L = result[[3]]
    K = result[[4]]
    QList = c(QList, Q)
    QMax = max(QList)
    results = sapply(QList, findVarsQ, ProductionLR = ProductionLR)
    CList = unlist(results[2,])
    ProductionSR = calculateSRExpansion(prodfun, K, w, r)
    costSR = ProductionSR$costSR[[1]]
    for(q in QList){
      Q = q
      CList = c(CList, round(eval(caracas::as_expr(costSR)),2))
    }
    CList = unique(CList)
    CMax = max(CList)
    LMax = 1.2*CMax/w
    KMax = 1.2*CMax/r
    plotly = ggplotly(ggplot() + 
                        makeAllIsocostLines(r, w, CList, color = "blue") +
                        makeAllIsoquantCurves(prodfun, QList, LMax, color = "red", smooth = smooth) +
                        geom_hline(yintercept = 0) +
                        geom_vline(xintercept = 0) +
                        makeSRExpansion(K, LMax) +
                        makeLRExpansion(ProductionLR, QMax*4) +
                        makeSRCostMinPoints(ProductionSR, QList) +
                        makeCostMinPoints(ProductionLR, QList) + 
                        coord_cartesian(xlim = c(0, LMax), ylim = c(0, KMax))
    )
    return(plotly)
  }
  
  makeFirmExpansionGraph = function(prodfun, QMax, QNum, w, r, smooth = 100){
    QList = seq(QMax/QNum, QMax, length.out = QNum)
    ProductionLR = calculateProductionLR(prodfun, w, r)
    results = sapply(QList, findVarsQ, ProductionLR = ProductionLR)
    CList = unlist(results[2,])
    LMax = max(CList)/w
    KMax = max(CList)/r
    plotly = ggplotly(ggplot() +
                        makeAllIsocostLines(r, w, CList, color = "blue") +
                        makeAllIsoquantCurves(prodfun, QList, LMax*1.5, smooth = smooth) +
                        scale_color_viridis_d("Q = f(K,L)", begin = .25, end = .85, option="plasma") +
                        geom_hline(yintercept = 0) +
                        geom_vline(xintercept = 0) +
                        makeLRExpansion(ProductionLR, QMax*2) +
                        coord_cartesian(xlim = c(0, LMax), ylim = c(0, KMax))
    )
    plot = ggplot() +
      makeAllIsocostLines(r, w, CList) +
      scale_color_viridis_d("Isocost Lines", option = "mako", begin = .3, end = .7, direction = 1) +
      new_scale("color") +
      makeAllIsoquantCurves(prodfun, QList, LMax*1.5, smooth = smooth) +
      scale_color_viridis_d("Q = f(K,L)", begin = .25, end = .85, option="plasma") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      makeLRExpansion(ProductionLR, QMax*2) +
      coord_cartesian(xlim = c(0, LMax), ylim = c(0, KMax))
    return(list(plotly, plot))
  }
  
  
  ########## SHINY SERVER CODE ##########
  
  values <- reactiveValues(myurl = c(), parent_tab = "",
                           getMarginalOutput = NULL,
                           IndifferencePlot = NULL,
                           MRSPlot = NULL,
                           BudgetNewBundles = NULL,
                           BudgetPlot = NULL,
                           ConstrainedUtilityPlot = NULL,
                           IncomeExpansionPlot = NULL,
                           EngelPlots = NULL,
                           DerivedDemandPlot = NULL,
                           IncSubEffectsPlot = NULL,
                           MarketDemandPlot = NULL,
                           MarketDemandPiecewiseData = NULL,
                           IsoquantsPlot = NULL,
                           IsocostsPlot = NULL,
                           MRTSPlot = NULL,
                           SRProductionPlot = NULL,
                           CostMinPlot = NULL,
                           LRExpansionPlot = NULL,
                           SRLRExpansionPlot = NULL,
                           SRLRExpansionCPlot = NULL,
                           CostMinStepsPlot = NULL,
                           CostMinStepsAnswers = NULL,
                           CostMinStepsSolutions = NULL
                           )
  
  # url navigation code from Dean Attali
  observe({
    input$bannerTabs
    query <- parseQueryString(session$clientData$url_search)
    # output$a1 = renderPrint({query})
    url <- query$url
    if (is.null(url)) {
      url <- ""
    }

    # "depth" is how many levels the url in the query string is
    depth <- function(x)
      length(unlist(strsplit(x, "/")))

    # if we reached the end, done!
    if (length(values$myurl) == depth(url)) {
      return()
    }
    # base case - need to tell it what the first main nav name is
    else if (length(values$myurl) == 0) {
      values$parent_tab <- "bannerTabs"
    }
    # if we're waiting for a tab switch but the UI hasn't updated yet
    else if (is.null(input[[values$parent_tab]])) {
      return()
    }
    # same - waiting for a tab switch
    else if (tail(values$myurl, 1) != input[[values$parent_tab]]) {
      return()
    }
    # the UI is on the tab that we last switched to, and there are more
    # tabs to switch inside the current tab
    # make sure the tabs follow the naming scheme
    else {
      values$parent_tab <- paste0(tail(values$myurl, 1), "_tabs")
    }
    # figure out the id/value of the next tab
    new_tab <- unlist(strsplit(url, "/"))[length(values$myurl) + 1]
    # easy peasy.
    updateTabsetPanel(session, values$parent_tab, new_tab)
    values$myurl <- c(values$myurl, new_tab)
  })
  
  ########## Consumer Behavior - Study ##########
  
  ###### Utility ######
  
  observeEvent(input$RunUtilityFunction, {
    values$getMarginalOutput = getMarginal(input$UtilityFunction)
  })
  
  output$UtilityFunction = renderText({
    if(is.null(values$getMarginalOutput[[1]])){
      "Press Calculate"
    } else {
      as.character(values$getMarginalOutput[[1]])
    }
   
  })
  output$UtilityMUx = renderPrint({
    if(is.null(values$getMarginalOutput[[2]])){
      "Press Calculate"
    } else {
      values$getMarginalOutput[[2]]
    }
  })
  output$UtilityMUy = renderPrint({
    if(is.null(values$getMarginalOutput[[3]])){
      "Press Calculate"
    } else {
      values$getMarginalOutput[[3]]
    }
  })
  
  ###### Indifference Curves ######
  
  observeEvent(input$RunIndifferencePlot, {
    Ufun = parse(text = input$IndifferenceFunction)
    Ulist = seq(from = input$IndifferenceMaxU/input$IndifferenceNumCurves, to = input$IndifferenceMaxU,length.out = input$IndifferenceNumCurves)
    plot = ggplot() +
      makeAllIndiffernceCurves(Ufun, Ulist, input$IndifferenceXMax, input$IndifferenceYMax) +
      scale_color_viridis_d(begin = .25, end = .85, option="plasma") +
      labs(color = "U(x,y)") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0, input$IndifferenceXMax), ylim = c(0, input$IndifferenceYMax))
    
    values$IndifferencePlot =  ggplotly(plot)
  })
  
  output$IndifferencePlot = renderPlotly({
    values$IndifferencePlot
  })
  
  ###### MRS Curve ######
  
  observeEvent(input$RunMRSPlot, {
    Ufun = parse(text = input$MRSFunction)
    U = input$MRSU
    xmax = input$MRSXMax
    ymax = input$MRSYMax
    plot = ggplot()+
      makeIndifferenceCurve(Ufun, U, xmax, ymax, color = "black") +
      makeMRSCurve(Ufun, U, xmax, ymax)[2] +
      makeMRSCurve(Ufun, U, xmax, ymax)[1] +
      scale_color_viridis_c(option = "viridis", limits = c(0, 10), direction = -1) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax))
    values$MRSPlot = ggplotly(plot)
  })
  
  output$MRSPlot = renderPlotly({
    values$MRSPlot
  })
  
  ###### Budget Constraints ######
  
  observeEvent(input$RunBudgetNewBundles, {
    values$BudgetNewBundles = makeRandomBundles(input$BudgetXMax, input$BudgetYMax, input$BudgetN)
    values$BudgetPlot =  makeRandomFeasibilityGraphs(input$BudgetPx, input$BudgetPy, input$BudgetI, input$BudgetXMax, input$BudgetYMax, values$BudgetNewBundles[[1]], values$BudgetNewBundles[[2]])
  })
  
  
  observeEvent(input$RunBudgetPlot, {
    if(is.null(values$BudgetNewBundles)){
      values$BudgetNewBundles = makeRandomBundles(input$BudgetPx, input$BudgetPy, input$BudgetI, input$BudgetXMax, input$BudgetYMax, input$BudgetN)
    }
    values$BudgetPlot =  makeRandomFeasibilityGraphs(input$BudgetPx, input$BudgetPy, input$BudgetI, input$BudgetXMax, input$BudgetYMax, values$BudgetNewBundles[[1]], values$BudgetNewBundles[[2]])
  })
  
  output$BudgetPlot = renderPlotly({
    if(input$BudgetFeasibility) {
      return(values$BudgetPlot[[2]])
    } else {
      return(values$BudgetPlot[[1]])
    }
    })
  
  ###### Constrained Optimization ######
  
  observeEvent(input$RunConstrainedUtilityPlot, {
    Ufun = parse(text = input$ConstrainedUtilityFunction)
    xmax = input$ConstrainedUtilityXMax
    ymax = input$ConstrainedUtilityYMax
    Px = input$ConstrainedUtilityPx
    Py = input$ConstrainedUtilityPy
    I = input$ConstrainedUtilityI
    plot = ggplot() + 
      makeOptimalBundle_Indifference(Ufun, Px, Py, I, xmax, ymax, color = 1)+
      scale_color_viridis_d(begin = .6, end = .8, option="plasma") +
      labs(color = "U(x,y)") +
      makeBudgetLine(Px, Py, I) + 
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      makeOptimalBundle_Point(Ufun, Px, Py, I) +
      coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
    values$ConstrainedUtilityPlot = ggplotly(plot)
  })
  
  output$ConstrainedUtilityPlot = renderPlotly({
    values$ConstrainedUtilityPlot
  })
  
  ########## Individual and Market Demand - Study ##########
  
  ###### Income Expansion Path ######
  
  observeEvent(input$RunIncomeExpansionPlot, {
    Ufun = parse(text = input$IncomeExpansionFunction)
    Px = input$IncomeExpansionPx
    Py = input$IncomeExpansionPy
    I = seq(input$IncomeExpansionIMax/input$IncomeExpansionINum, input$IncomeExpansionIMax, length.out = input$IncomeExpansionINum)
    xmax = input$IncomeExpansionXMax
    ymax = input$IncomeExpansionYMax
    values$IncomeExpansionPlot = makeIncomeExpansionPlotly(Ufun, Px, Py, I, xmax, ymax)
  })
  
  output$IncomeExpansionPlot = renderPlotly({
    values$IncomeExpansionPlot
  })
  
  ###### Engel Curves ######
  
  observeEvent(input$RunEngelPlots, {
    Ufun = parse(text = input$IncomeExpansionFunction)
    Px = input$IncomeExpansionPx
    Py = input$IncomeExpansionPy
    xmax = input$IncomeExpansionXMax
    ymax = input$IncomeExpansionYMax
    engelData = makeEngelData(Ufun, Px, Py, xmax, ymax)
    
    engelCurveX = ggplot() +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_path(data = engelData, aes(x = x, y = I), color = "darkgreen") +
      coord_cartesian(xlim = c(0,input$IncomeExpansionXMax), ylim = c(0,max(engelData$I)))
    engelCurveX = ggplotly(engelCurveX) %>%
      layout(yaxis = list(title = "I"), xaxis = list(title = "x"))
    
    engelCurveY = ggplot() +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_path(data = engelData, aes(x = y, y = I), color = "darkgreen") +
      coord_cartesian(xlim = c(0,input$IncomeExpansionYMax), ylim = c(0,max(engelData$I)))
    engelCurveY = ggplotly(engelCurveY) %>%
      layout(yaxis = list(title = "I"), xaxis = list(title = "y"))
    
    engelPlots = subplot(engelCurveY, engelCurveX, nrows = 1, shareY = TRUE, titleY = TRUE, titleX = TRUE) %>%
      layout(title = list(text = "Engel Curves"), margin = c(0,0,.1,0))
    
    values$EngelPlots = engelPlots
  })
  
  output$EngelPlots = renderPlotly({
    values$EngelPlots
  })

  ###### Derived Demand Curve ######
  
  observeEvent(input$RunDerivedDemandPlot, {
    Ufun = parse(text = input$DerivedDemandFunction)
    Pxmin = input$DerivedDemandPxmin
    Pxmax = input$DerivedDemandPxmax
    Py = input$DerivedDemandPy
    I = input$DerivedDemandI
    xmax = input$DerivedDemandXMax
    ymax = input$DerivedDemandYMax
    addedPx = seq(input$DerivedDemandPointMax/input$DerivedDemandPointN, input$DerivedDemandPointMax, length.out = input$DerivedDemandPointN)
    values$DerivedDemandPlot = makeDerivedDemandPlotX(Ufun, Py, I, addedPx, Pxmin, Pxmax, xmax, ymax)
  })
  output$DerivedDemandPlot = renderPlotly({
    values$DerivedDemandPlot
  })
  
  ###### Income and Substitution Effects ######
  
  observeEvent(input$RunIncSubEffectsPlot, {
    Ufun = parse(text = input$IncSubEffectsFunction)
    Px = c(input$IncSubEffectsPx, input$IncSubEffectsPx1)
    Py = c(input$IncSubEffectsPy, input$IncSubEffectsPy1)
    I = c(input$IncSubEffectsI, input$IncSubEffectsI1)
    xmax = input$IncSubEffectsXMax
    ymax = input$IncSubEffectsYMax
    values$IncSubEffectsPlot = makeEffectsPlot(Ufun, Px, Py, I, xmax, ymax)
  })
  
  output$IncSubEffectsPlot = renderPlotly({
    values$IncSubEffectsPlot[[1]]
  })
  output$IncSubEffectsSub = renderText({
    if(is.null(values$IncSubEffectsPlot[[2]])){
      "Press Draw Graph"
    } else {
      values$IncSubEffectsPlot[[2]]
    }
  })
  output$IncSubEffectsInc = renderText({
    if(is.null(values$IncSubEffectsPlot[[3]])){
      "Press Draw Graph"
    } else {
      values$IncSubEffectsPlot[[3]]
    }
  })
  output$IncSubEffectsTotal = renderText({
    if(is.null(values$IncSubEffectsPlot[[4]])){
      "Press Draw Graph"
    } else {
      values$IncSubEffectsPlot[[4]]
    }
  })
  
  ###### Market Demand Curve ######
  
  output$MarketDemandFunsList <- renderUI({
    MarketDemandFuns <- lapply(1:5, function(i){
      textInput(inputId = paste0("MarketDemandFun", i), label = paste0("Demand Function", i, ": Q", i, "(p)="), value = paste0(sample(10:100,1), "-",sample(2:10,1),"*p"), width = '200px')
    })
    shinyjs::hidden(MarketDemandFuns)
  })
  
  output$MarketDemandFunsNList <- renderUI({
    MarketDemandFunsN <- lapply(1:5, function(i){
      numericInput(inputId = paste0("MarketDemandFunN", i), label = paste0("How many people have demand function Q", i, "?"), value = 1, min = 1, width = '160px')
    })
    shinyjs::hidden(MarketDemandFunsN)
  })
  
  output$MarketDemandSupply <- renderUI({
    MarketDemandSupply <- lapply(1, function(i){
      textInput(inputId = paste0("MarketDemandSupply", i), label = paste0("Supply Function: Q(p)="), value = paste0(sample(2:20,1),"*p"), width = '200px')
    })
    shinyjs::hidden(MarketDemandSupply)
  })
  
  observeEvent(input$RunMarketDemandSupply, {
    if(input$RunMarketDemandSupply){
      shinyjs::show(id = paste0("MarketDemandSupply1"))
    } else {
      shinyjs::hide(id = paste0("MarketDemandSupply1"))
    }
  })
  
  observeEvent(input$RunMarketDemandNFuns, {
    n <- seq(length.out = as.numeric(input$MarketDemandNFuns))
    lapply(seq(5), function(i) {
      if(i %in% n) {
        shinyjs::show(id = paste0("MarketDemandFun", i))
        shinyjs::show(id = paste0("MarketDemandFunN", i))
      } else {
        shinyjs::hide(id = paste0("MarketDemandFun", i))
        shinyjs::hide(id = paste0("MarketDemandFunN", i))
      }
    })
    num = input$MarketDemandNFuns
    demandList1 = list(input$MarketDemandFun1, input$MarketDemandFun2, input$MarketDemandFun3, input$MarketDemandFun4, input$MarketDemandFun5)
    NList1 = list (input$MarketDemandFunN1, input$MarketDemandFunN2, input$MarketDemandFunN3, input$MarketDemandFunN4, input$MarketDemandFunN5)
    demandList = list()
    NList = list()
    for (i in 1:num){
      demandList[[i]] =  demandList1[[i]]
      NList[[i]] = NList1[[i]]
    }
    values$MarketDemandPiecewiseData = makePiecewise(demandList, NList)
  })
  
  observeEvent(input$RunMarketDemandPlot, {
    n <- seq(length.out = as.numeric(input$MarketDemandNFuns))
    lapply(seq(5), function(i) {
      if(i %in% n) {
        shinyjs::show(id = paste0("MarketDemandFunN", i))
        shinyjs::show(id = paste0("MarketDemandFun", i))
      } else {
        shinyjs::hide(id = paste0("MarketDemandFunN", i))
        shinyjs::hide(id = paste0("MarketDemandFun", i))
      }
    })
    num = input$MarketDemandNFuns
    demandList1 = list(input$MarketDemandFun1, input$MarketDemandFun2, input$MarketDemandFun3, input$MarketDemandFun4, input$MarketDemandFun5)
    NList1 = list (input$MarketDemandFunN1, input$MarketDemandFunN2, input$MarketDemandFunN3, input$MarketDemandFunN4, input$MarketDemandFunN5)
    demandList = list()
    NList = list()
    for (i in 1:num){
      demandList[[i]] =  demandList1[[i]]
      NList[[i]] = NList1[[i]]
    }
    values$MarketDemandPiecewiseData = makePiecewise(demandList, NList)
    values$MarketDemandPlot = makePiecewisePlot(values$MarketDemandPiecewiseData[[1]], values$MarketDemandPiecewiseData[[3]], values$MarketDemandPiecewiseData[[4]], values$MarketDemandPiecewiseData[[5]], input$MarketDemandSupply1)
  })
  
  observeEvent(input$RunMarketDemandPiecewise, {
    n <- seq(length.out = as.numeric(input$MarketDemandNFuns))
    lapply(seq(5), function(i) {
      if(i %in% n) {
        shinyjs::show(id = paste0("MarketDemandFunN", i))
        shinyjs::show(id = paste0("MarketDemandFun", i))
      } else {
        shinyjs::hide(id = paste0("MarketDemandFunN", i))
        shinyjs::hide(id = paste0("MarketDemandFun", i))
      }
    })
    num = input$MarketDemandNFuns
    demandList1 = list(input$MarketDemandFun1, input$MarketDemandFun2, input$MarketDemandFun3, input$MarketDemandFun4, input$MarketDemandFun5)
    NList1 = list (input$MarketDemandFunN1, input$MarketDemandFunN2, input$MarketDemandFunN3, input$MarketDemandFunN4, input$MarketDemandFunN5)
    demandList = list()
    NList = list()
    for (i in 1:num){
      demandList[[i]] =  demandList1[[i]]
      NList[[i]] = NList1[[i]]
    }
    values$MarketDemandPiecewiseData = makePiecewise(demandList, NList)
    num = length(values$MarketDemandPiecewiseData[[7]])
    str = c("$$
      Q_{Market}(p) = 
      \\begin{cases}")
    for (i in 1:num){
      str = paste0(str, values$MarketDemandPiecewiseData[[7]][[i]], " & \\text{if $p \\in{[", values$MarketDemandPiecewiseData[[4]][i], ",", values$MarketDemandPiecewiseData[[4]][i+1], ")}$} \\\\ 
                   ")
    }
    str = paste0(str, "0 & \\text{otherwise} 
                 \\end{cases} 
                 $$")
    
    output$MarketDemandPiecewiseFun = renderUI({withMathJax(HTML(
      str))
    })
  })
  
  
  observeEvent(input$MarketDemandEquilibrium, {
    shinyjs::show(id = paste0("MarketDemandSupply1"))
  })
  
  output$MarketDemandPlot = renderPlotly({
    if(input$MarketDemandEquilibrium) {
      return(values$MarketDemandPlot[[2]])
    } else {
      return(values$MarketDemandPlot[[1]])
    }
  })
  
  ########## Production - Study ##########
  
  ###### Isoquant Curves ######
  
  observeEvent(input$RunIsoquantsPlot, {
    prodfun = input$IsoquantsProdfun
    QMax = input$IsoquantsQMax
    QNum = input$IsoquantsQNum
    LMax = input$IsoquantsLMax
    smooth = input$IsoquantsSmooth
    QList = seq(from = QMax/QNum, to = QMax, length.out = QNum)
    
    plot = ggplot() +
      makeAllIsoquantCurves(prodfun, QList, LMax, smooth = smooth) +
      scale_color_viridis_d("Q = f(K,L)", begin = .25, end = .85, option="plasma") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0, LMax), ylim = c(0, LMax))
    
    values$IsoquantsPlot =  ggplotly(plot)
  })
  
  output$IsoquantsPlot = renderPlotly({
    values$IsoquantsPlot
  })
  
  ###### Isocost Curves ######
  
  observeEvent(input$RunIsocostsPlot, {
    r = input$IsocostsR
    w = input$IsocostsW
    CMax = input$IsocostsCMax
    CNum = input$IsocostsCNum
    CList = seq(from = CMax/CNum, to = CMax, length.out = CNum)
    
    plot = ggplot() +
      makeAllIsocostLines(r, w, CList) +
      scale_color_viridis_d("Isocost Lines", option = "mako", begin = .3, end = .7, direction = 1) +
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)
    
    values$IsocostsPlot =  ggplotly(plot)
  })
  
  output$IsocostsPlot = renderPlotly({
    values$IsocostsPlot
  })
  
  ###### MRTS Curve ######
  
  observeEvent(input$RunMRTSPlot, {
    prodfun = input$MRTSProdfun
    Q = input$MRTSQ
    LMax = input$MRTSLMax
    smooth = input$MRTSSmooth
    
    values$MRTSPlot = makeMRTSCurveGraph(prodfun, Q, LMax, smooth = smooth)
  })
  
  output$MRTSPlot = renderPlotly({
    values$MRTSPlot
  })
  
  ###### SR Production Curve ######
  
  observeEvent(input$RunSRProductionPlot, {
    prodfun = input$SRProductionProdfun
    K = input$SRProductionK
    w = input$SRProductionW
    r = input$SRProductionR
    LMax = input$SRProductionLMax
    smooth = input$SRProductionSmooth
    
    values$SRProductionPlot =  makeSRProductionGraph(prodfun, K, w, r, LMax, smooth = smooth)
  })
  
  output$SRProductionPlot = renderPlotly({
    values$SRProductionPlot
  })
  
  ###### Cost Min Plot ######
  
  observeEvent(input$RunCostMinPlot, {
    prodfun = input$CostMinProdfun
    Q = input$CostMinQ
    w = input$CostMinW
    r = input$CostMinR
    smooth = input$CostMinSmooth
    
    values$CostMinPlot =  makeCostMinGraph(prodfun, w, r, Q, smooth = smooth)
  })
  
  output$CostMinPlot = renderPlotly({
    values$CostMinPlot
  })
  
  
  ###### LR Expansion ######
  
  observeEvent(input$RunLRExpansionPlot, {
    prodfun = input$LRExpansionProdfun
    QMax = input$LRExpansionQMax
    QNum = input$LRExpansionQNum
    w = input$LRExpansionW
    r = input$LRExpansionR
    smooth = input$LRExpansionSmooth
    
    values$LRExpansionPlot =  makeFirmExpansionGraph(prodfun, QMax, QNum, w, r, smooth = smooth)[[1]]
  })
  
  output$LRExpansionPlot = renderPlotly({
    values$LRExpansionPlot
  })
  
  ###### SR vs LR Expansion ######
  
  observeEvent(input$RunSRLRExpansionPlot, {
    prodfun = input$SRLRExpansionProdfun
    w = input$SRLRExpansionW
    r = input$SRLRExpansionR
    Q = input$SRLRExpansionQ
    smooth = input$SRLRExpansionSmooth
    
    values$SRLRExpansionPlot =  makeLRvSRExpansionGraph(prodfun, w, r, Q, smooth = smooth)
  })
  
  output$SRLRExpansionPlot = renderPlotly({
    values$SRLRExpansionPlot
  })
  
  ###### SR vs LR Expansion - Costs ######
  
  observeEvent(input$RunSRLRExpansionCPlot, {
    prodfun = input$SRLRExpansionCProdfun
    w = input$SRLRExpansionCW
    r = input$SRLRExpansionCR
    Q = input$SRLRExpansionCQ
    QList = as.numeric(unlist(strsplit(input$SRLRExpansionCQList, ",")))
    smooth = input$SRLRExpansionCSmooth
    
    values$SRLRExpansionCPlot =  makeLRvSRCostExpansionGraph(prodfun, w, r, Q, QList, smooth = smooth)
  })
  
  output$SRLRExpansionCPlot = renderPlotly({
    values$SRLRExpansionCPlot
  })
  
  ###### Production - Practice ######
  
  
  ###### TEST FUN #######
  
  stepsCostMin = function(prodfun, w, r, Q){
    K = caracas::symbol('K')
    L = caracas::symbol('L')
    prodFun = caracas::as_sym(prodfun)
    MPL = caracas::der(prodFun, L)
    MPK = caracas::der(prodFun, K)
    MRTS = MPL/MPK
    #Test if K or L do not have constant returns to scale 
    KisVar1 = caracas::tex(caracas::der(MRTS, K)) != 0
    LisVar1 = caracas::tex(caracas::der(MRTS, L)) != 0
    KprodFunSolutions = list()
    LprodFunSolutions = list()
    Ksolutions_K1 = list()
    Ksolutions_L = list()
    Ksolutions_K2 = list()
    Lsolutions_L1 = list()
    Lsolutions_K = list()
    Lsolutions_L2 = list()
    Ksolutions_C = list()
    Lsolutions_C = list()
    Kpossible = list()
    Lpossible = list()
    Kpositive = list()
    Lpositive = list()
    Kpositive_0 = list()
    Lpositive_0 = list()
    prodFunSolution = list()
    j = 0
    perfectSubs = FALSE
    perfectSubsInterior = FALSE
    perfectSubsCornerL = FALSE
    if(!LisVar1 && !KisVar1){
      #Perfect substitutes
      perfectSubs = TRUE
      if(caracas::as_expr(MPL)/w > caracas::as_expr(MPK)/r){
        perfectSubsCornerL = TRUE
        #L more economical at all input levels
      } else if(caracas::as_expr(MPL)/w < caracas::as_expr(MPK)/r){
        perfectSubsCornerL = FALSE
        #K more economical at all input levels
      } else {
        # L and K equally economical at all input levels
        # Set K = L (arbitrarily)
        perfectSubsInterior = TRUE
      }
      return(list(MPL = MPL, MPK = MPK, MRTS = MRTS, perfectSubs = perfectSubs, perfectSubsInterior = perfectSubsInterior, perfectSubsCornerL = perfectSubsCornerL))
    }
    Lcritical = c()
    Kcritical = c()
    if(LisVar1) Lcritical = try(caracas::solve_sys(MPL/MPK, w/r, L))
    if(KisVar1) Kcritical = try(caracas::solve_sys(MPL/MPK, w/r, K))
    #Not perfect substitutes. Possible interior solution
    #search for critical values to test for corner solutions
    #Lcrit first
    #add 0 to critical value
    Lcrit = list(caracas::as_sym(0))
    if(length(Lcritical>0)){
      for(i in 1:length(Lcritical)){
        Lcrit[[i+1]] = (Lcritical[[i]]$L)
      }
    }
    lenLcrit = length(Lcrit)
    for(i in 1:lenLcrit){
      Lsolutions_try = Lcrit[[i]]
      prodFunSolutions_try = caracas::subs(prodFun, L, Lsolutions_try) %>% N(4)
      K = caracas::symbol('K', positive = TRUE)
      results = caracas::solve_sys(prodFunSolutions_try, Q, K)
      K = caracas::symbol('K')
      if(length(results)==0){
        #not possible
        j = j + 1
        Lpossible[[j]] = FALSE
        Lpositive[[j]] = FALSE
        Lsolutions_L1[[j]] = Lsolutions_try
        Lsolutions_K[[j]] = NULL
        Lsolutions_L2[[j]] = NULL
        LprodFunSolutions[[j]] = prodFunSolutions_try
        Lsolutions_C[[j]] = NULL
        next
      }
      startj = j + 1
      for(k in 1:length(results)){
        j = j + 1
        Lpossible[[j]] = TRUE
        Lpositive[[j]] = TRUE
        Lsolutions_L1[[j]] = Lsolutions_try %>% N(4)
        Lsolutions_K[[j]] = results[[k]]$K %>% N(4)
        KinLsol = caracas::tex(caracas::der(Lsolutions_try, K)) != 0
        if(KinLsol){
          Lsolutions_L2[[j]] = caracas::subs(Lsolutions_L1[[j]], K, Lsolutions_K[[j]]) %>% N(4)
        } else {
          Lsolutions_L2[[j]] = Lsolutions_L1[[j]]
        }
        LprodFunSolutions[[j]] = prodFunSolutions_try
        Lsolutions_C[[j]] = (Lsolutions_L2[[j]] * w + Lsolutions_K[[j]] * r) %>% N(4)
        if(!is.complex(caracas::as_expr(Lsolutions_K[[j]])) || !is.complex(caracas::as_expr(Lsolutions_L2[[j]]))){
          if(caracas::as_expr(Lsolutions_K[[j]]) < 0 || caracas::as_expr(Lsolutions_L2[[j]]) < 0){
            Lpositive[[j]] = FALSE
            Lpositive_0[[j]] = TRUE
            Lpossible[[j]] = FALSE
          }
        } else {
          Lpossible[[j]] = FALSE
          Lpositive[[j]] = FALSE
        }
      }
      if(FALSE %in% unlist(Lpositive)[startj:j] && TRUE %in% unlist(Lpositive)[startj:j]){
        for(k in j:startj){
          if(!Lpositive[[k]]){
            #delete these!
            Lpossible = Lpossible[-k]
            Lpositive_0 = Lpositive_0[-k]
            Lpositive = Lpositive[-k]
            Lsolutions_L1 = Lsolutions_L1[-k]
            Lsolutions_K = Lsolutions_K[-k]
            Lsolutions_L2 = Lsolutions_L2[-k]
            LprodFunSolutions = LprodFunSolutions[-k]
            Lsolutions_C = Lsolutions_C[-k]
            j = j - 1
          }
        }
      }
    }
    j = 0
    #Kcrit second
    #add 0 to critical value
    Kcrit = list(caracas::as_sym(0))
    if(length(Kcritical>0)){
      for(i in 1:length(Kcritical)){
        Kcrit[[i+1]] = (Kcritical[[i]]$K)
      }
    }
    lenKcrit = length(Kcrit)
    for(i in 1:lenKcrit){
      Ksolutions_try = Kcrit[[i]] %>% N(4)
      prodFunSolutions_try = caracas::subs(prodFun, K, Ksolutions_try) %>% N(4)
      L = caracas::symbol('L', positive = TRUE)
      results = caracas::solve_sys(prodFunSolutions_try, Q, L)
      L = caracas::symbol('L')
      if(length(results)==0){
        #not possible
        j = j + 1
        Kpossible[[j]] = FALSE
        Kpositive[[j]] = FALSE
        Ksolutions_K1[[j]] = Ksolutions_try
        Ksolutions_L[[j]] = NULL
        Ksolutions_K2[[j]] = NULL
        KprodFunSolutions[[j]] = prodFunSolutions_try
        Ksolutions_C[[j]] = NULL
        next
      }
      startj = j + 1
      for(k in 1:length(results)){
        j = j + 1
        Kpossible[[j]] = TRUE
        Kpositive[[j]] = TRUE
        Ksolutions_K1[[j]] = Ksolutions_try
        Ksolutions_L[[j]] = results[[k]]$L %>% N(4)
        LinKsol = caracas::tex(caracas::der(Ksolutions_try, L)) != 0
        if(LinKsol){
          Ksolutions_K2[[j]] = caracas::subs(Ksolutions_K1[[j]], L, Ksolutions_L[[j]]) %>% N(4)
        } else {
          Ksolutions_K2[[j]] = Ksolutions_K1[[j]]
        }
        KprodFunSolutions[[j]] = prodFunSolutions_try
        Ksolutions_C[[j]] = (Ksolutions_L[[j]] * w + Ksolutions_K2[[j]] * r) %>% N(4)
        if(!is.complex(caracas::as_expr(Ksolutions_L[[j]])) || !is.complex(caracas::as_expr(Ksolutions_K2[[j]]))){
          if(caracas::as_expr(Ksolutions_L[[j]]) < 0 || caracas::as_expr(Ksolutions_K2[[j]]) < 0){
            Kpositive[[j]] = FALSE
            Kpositive_0[[j]] = TRUE
            Kpossible[[j]] = FALSE
          }
        } else {
          Kpossible[[j]] = FALSE
          Kpositive[[j]] = FALSE
        }
      }
      if(FALSE %in% unlist(Kpositive)[startj:j] && TRUE %in% unlist(Kpositive)[startj:j]){
        for(k in j:startj){
          if(!Kpositive[[k]]){
            #delete these!
            Kpossible = Kpossible[-k]
            Kpositive_0 = Kpositive_0[-k]
            Kpositive = Kpositive[-k]
            Ksolutions_K1 = Ksolutions_K1[-k]
            Ksolutions_L = Ksolutions_L[-k]
            Ksolutions_K2 = Ksolutions_K2[-k]
            KprodFunSolutions = KprodFunSolutions[-k]
            Ksolutions_C = Ksolutions_C[-k]
            j = j - 1
          }
        }
      }
    }
    Lcost = c()
    for(i in 1:length(Lpossible)){
      if(Lpossible[[i]]){
        Lcost = c(Lcost, caracas::as_expr(Lsolutions_C[[i]]))
      } else {
        Lcost = c(Lcost, Inf)
      }
    }
    LcostMin= min(Lcost)
    LSol = which(Lcost %in% LcostMin)
    
    Kcost = c()
    for(i in 1:length(Kpossible)){
      if(Kpossible[[i]]){
        Kcost = c(Kcost, caracas::as_expr(Ksolutions_C[[i]]))
      } else {
        Kcost = c(Kcost, Inf)
      }
    }
    KcostMin= min(Kcost)
    KSol = which(Kcost %in% KcostMin)
    
    KcostMin = round(KcostMin, 2)
    LcostMin = round(LcostMin, 2)
    
    Equal = KcostMin == LcostMin
    Kbetter = KcostMin < LcostMin
    
    costMin = min(KcostMin, LcostMin)
    
    return(list(MPL = MPL, MPK = MPK, MRTS = MRTS, perfectSubs = perfectSubs, perfectSubsInterior = perfectSubsInterior, perfectSubsCornerL = perfectSubsCornerL, 
                Lcritical = Lcritical, Lsolutions_L1 = Lsolutions_L1, LprodFunSolutions = LprodFunSolutions, Lsolutions_K = Lsolutions_K, Lsolutions_L2 = Lsolutions_L2, Lsolutions_C = Lsolutions_C, Lpossible = Lpossible, Lpositive = Lpositive,
                Kcritical = Kcritical, Ksolutions_K1 = Ksolutions_K1, KprodFunSolutions = KprodFunSolutions, Ksolutions_L = Ksolutions_L, Ksolutions_K2 = Ksolutions_K2, Ksolutions_C = Ksolutions_C, Kpossible = Kpossible, Kpositive = Kpositive,
                Lcost = Lcost, LcostMin = LcostMin, LSol = LSol, Kcost = Kcost, KcostMin = KcostMin, KSol = KSol, Equal = Equal, Kbetter = Kbetter, costMin = costMin))
  }
  
  
  ###### END TEST FUN ######
  
  
  observeEvent(input$CostMinStepsQ, {
    updateNumericInput(session, inputId = "CostMinStepsAnswerC",
                       label = paste0("How much does the production of ", input$CostMinStepsQ ," units cost?"))
  })
  
  observeEvent(input$RunCostMinStepsPlot, {
    prodfun = input$CostMinStepsProdfun
    Q = input$CostMinStepsQ
    w = input$CostMinStepsW
    r = input$CostMinStepsR
    smooth = input$CostMinStepsSmooth
    
    values$CostMinStepsPlot =  makeCostMinGraph(prodfun, w, r, Q, smooth = smooth)
  })
  
  output$CostMinStepsPlot = renderPlotly({
    values$CostMinStepsPlot
  })
  
  observeEvent(input$RunCostMinStepsAnswers, {
    prodfun = input$CostMinStepsProdfun
    Q = input$CostMinStepsQ
    w = input$CostMinStepsW
    r = input$CostMinStepsR
    CAns = input$CostMinStepsAnswerC
    LAns = input$CostMinStepsAnswerL
    KAns = input$CostMinStepsAnswerK
    
    ProductionLR = calculateProductionLR(prodfun, w, r)
    ProductionLR_Q = findVarsQ(ProductionLR, Q)[[1]]
    C = findVarsQ(ProductionLR, Q)[[2]]
    L = findVarsQ(ProductionLR, Q)[[3]]
    K = findVarsQ(ProductionLR, Q)[[4]]
    
    if(CAns == C && LAns == L && KAns == K){
      values$CostMinStepsAnswers = h3("Well Done! All your answers are correct!")
    } else {
      values$CostMinStepsAnswers = h4("There is atleast one answer that is not correct.")
    }
  })
  
  output$CostMinStepsAnswers = renderUI({
    values$CostMinStepsAnswers
  })
  
  observeEvent(input$RunCostMinStepsSolutions, {
    prodfun = input$CostMinStepsProdfun
    Q = input$CostMinStepsQ
    w = input$CostMinStepsW
    r = input$CostMinStepsR
    ProductionLR = calculateProductionLR(prodfun, w, r)
    ProductionLR_Q = findVarsQ(ProductionLR, Q)[[1]]
    C = findVarsQ(ProductionLR, Q)[[2]]
    L = findVarsQ(ProductionLR, Q)[[3]]
    K = findVarsQ(ProductionLR, Q)[[4]]
    #correct solutions
    #adjust width for solutions
    results = stepsCostMin(prodfun, w, r, Q)
    MPL = caracas::tex(results$MPL)
    MPK = caracas::tex(results$MPK)
    MRTS = caracas::tex(results$MRTS)
    perfectSubs = results$perfectSubs
    perfectSubsInterior = results$perfectSubsInterior
    perfectSubsCornerL = results$perfectSubsCornerL
    if(perfectSubs){
      if(perfectSubsInterior){
        values$CostMinStepsSolutions = withMathJax(HTML(paste0("
                                                               <h3>Step 1: Solve for \\(MP_L\\) and \\(MP_K\\)</h3>
                                                               <p>$$MP_L =", MPL,", \\qquad  MP_K =", MPK,"$$</p>
                                                               <p>$$MRTS_{LK} = \\frac{MP_L}{MP_K} = \\frac{", MPL,"}{", MPK,"} = ", MRTS,"$$</p>
                                                               <p> Note: K and L are perfect substitutes. This will likely result in a corner solutions </p>
                                                               <h3>Step 2: Find ratio of prices</h3>
                                                               <p>$$\\frac{w}{r} = \\frac{", w,"}{", r,"}$$</p>
                                                               <p>$$\\text{slope Isocost line} = - \\frac{", w,"}{", r,"}$$</p>
                                                               <h3>Step 3: Set \\(MRTS_{LK} = \\text{-slope Isocost line}\\)</h3>
                                                               <p>$$MRTS_{LK} = \\text{-slope Isocost line}$$</p>
                                                               <p>$$", MRTS," = \\frac{", w,"}{", r,"}$$</p>
                                                               <p> We just found that \\(MRTS_{LK} = \\)-slope Isocost line, that \\(\\frac{MP_L}{w} = \\frac{MP_K}{r}\\) for all values of L and K.</p>
                                                               <p> This means that <i>any combination</i> of inputs that produces ", Q," units of quantity is equally cost efficient, even interior solutions!</p>
                                                               <p> So, technically there are infinite solutions here. The following set contains them all.</p>
                                                               <p>$$ K = \\alpha * \\frac{", Q, "}{", MPK,"} \\qquad L = (1-\\alpha) * \\frac{", Q, "}{", MPL,"} \\qquad \\text{where} \\quad \\alpha \\in [0,1] $$</p>
                                                               <h3>Step 4: Solve system of equations with target quantity</h3>
                                                               <p>$$", Q," = ", prodfun,"$$</p>
                                                               <p>$$ C = K * r + L * w $$</p>
                                                               <p>$$ C = \\alpha * \\frac{", Q, "}{", MPK,"} * ", r," + (1-\\alpha) * \\frac{", Q, "}{", MPL,"} *", w, "$$</p>
                                                               <p>$$ C = \\alpha *", C, " + (1-\\alpha) *", C," $$</p>
                                                               <p>$$ C = ", C, "$$</p>
                                                               ")))
      } else {
        if(perfectSubsCornerL){
          values$CostMinStepsSolutions = withMathJax(HTML(paste0("
                                                                 <h3>Step 1: Solve for \\(MP_L\\) and \\(MP_K\\)</h3>
                                                                 <p>$$MP_L =", MPL,", \\qquad  MP_K =", MPK,"$$</p>
                                                                 <p>$$MRTS_{LK} = \\frac{MP_L}{MP_K} = \\frac{", MPL,"}{", MPK,"} = ", MRTS,"$$</p>
                                                                 <p> Note: K and L are perfect substitutes. This will likely result in a corner solutions </p>
                                                                 <h3>Step 2: Find ratio of prices</h3>
                                                                 <p>$$\\frac{w}{r} = \\frac{", w,"}{", r,"}$$</p>
                                                                 <p>$$\\text{slope Isocost line} = - \\frac{", w,"}{", r,"}$$</p>
                                                                 <h3>Step 3: Set \\(MRTS_{LK} = \\text{-slope Isocost line}\\)</h3>
                                                                 <p>$$MRTS_{LK} = \\text{-slope Isocost line}$$</p>
                                                                 <p>$$", MRTS," \\neq \\frac{", w,"}{", r,"}$$</p>
                                                                 <p> We just found that \\(MRTS_{LK} \\neq \\)-slope Isocost line, that \\(\\frac{MP_L}{w} \\neq \\frac{MP_K}{r}\\) for all values of L and K.</p>
                                                                 <p> This means we must have a corner solution! Now we must figue out if we want to only employ K or L.</p>
                                                                 <p> $$ \\frac{MP_L}{w} = \\frac{", MPL,"}{", w,"} \\qquad \\frac{MP_K}{r} = \\frac{", MPK,"}{", r,"} $$
                                                                 <p> Since \\(\\frac{MP_L}{w} > \\frac{MP_K}{r}\\), we should only use L in production. </p>
                                                                 <p>$$ K = 0 \\qquad L = \\frac{", Q, "}{", MPL,"} $$</p>
                                                                 <h3>Step 4: Solve system of equations with target quantity</h3>
                                                                 <p>$$", Q," = ", prodfun,"$$</p>
                                                                 <p>$$ C = K * r + L * w $$</p>
                                                                 <p>$$ C = 0 * ", r," + \\frac{", Q, "}{", MPL,"} *", w, "$$</p>
                                                                 <p>$$ C = ", C, "$$</p>
                                                                 ")))
        } else {
          values$CostMinStepsSolutions = withMathJax(HTML(paste0("
                                                                 <h3>Step 1: Solve for \\(MP_L\\) and \\(MP_K\\)</h3>
                                                                 <p>$$MP_L =", MPL,", \\qquad  MP_K =", MPK,"$$</p>
                                                                 <p>$$MRTS_{LK} = \\frac{MP_L}{MP_K} = \\frac{", MPL,"}{", MPK,"} = ", MRTS,"$$</p>
                                                                 <p> Note: K and L are perfect substitutes. This will likely result in a corner solutions </p>
                                                                 <h3>Step 2: Find ratio of prices</h3>
                                                                 <p>$$\\frac{w}{r} = \\frac{", w,"}{", r,"}$$</p>
                                                                 <p>$$\\text{slope Isocost line} = - \\frac{", w,"}{", r,"}$$</p>
                                                                 <h3>Step 3: Set \\(MRTS_{LK} = \\text{-slope Isocost line}\\)</h3>
                                                                 <p>$$MRTS_{LK} = \\text{-slope Isocost line}$$</p>
                                                                 <p>$$", MRTS," \\neq \\frac{", w,"}{", r,"}$$</p>
                                                                 <p> We just found that \\(MRTS_{LK} \\neq \\)-slope Isocost line, that \\(\\frac{MP_L}{w} \\neq \\frac{MP_K}{r}\\) for all values of L and K.</p>
                                                                 <p> This means we must have a corner solution! Now we must figue out if we want to only employ K or L.</p>
                                                                 <p> $$ \\frac{MP_L}{w} = \\frac{", MPL,"}{", w,"} \\qquad \\frac{MP_K}{r} = \\frac{", MPK,"}{", r,"} $$
                                                                 <p> Since \\(\\frac{MP_L}{w} < \\frac{MP_K}{r}\\), we should only use K in production. </p>
                                                                 <p>$$ K = \\frac{", Q, "}{", MPK,"} \\qquad L = 0 $$</p>
                                                                 <h3>Step 4: Solve system of equations with target quantity</h3>
                                                                 <p>$$", Q," = ", prodfun,"$$</p>
                                                                 <p>$$ C = K * r + L * w $$</p>
                                                                 <p>$$ C = \\frac{", Q, "}{", MPK,"} * ", r," + 0 *", w, "$$</p>
                                                                 <p>$$ C = ", C, "$$</p>
                                                                ")))
        }
      }
    } else {
      solutionMessage = paste0("
                               <h3>Step 1: Solve for \\(MP_L\\) and \\(MP_K\\)</h3>
                               <p>$$MP_L =", MPL,", \\qquad  MP_K =", MPK,"$$</p>
                               <p>$$MRTS_{LK} = \\frac{MP_L}{MP_K} = \\frac{", MPL,"}{", MPK,"} = ", MRTS,"$$</p>
                               <h3>Step 2: Find ratio of prices</h3>
                               <p>$$\\frac{w}{r} = \\frac{", w,"}{", r,"}$$</p>
                               <p>$$\\text{slope Isocost line} = - \\frac{", w,"}{", r,"}$$</p>
                               <h3>Step 3: Set \\(MRTS_{LK} = \\text{-slope Isocost line}\\)</h3>
                               <p>$$MRTS_{LK} = \\text{-slope Isocost line}$$</p>
                               <p>$$", MRTS," = \\frac{", w,"}{", r,"}$$</p>
                               <h3>Step 4: Solve system of equations with target quantity</h3>
                               <p>$$", Q," = ", prodfun,"$$</p>
                               ")

      if(length(results$Lcritical) > 0){
        addLMessage = list()
        for(i in 2:(1+length(results$Lcritical))){
          addLMessage[[i]] = ""
          Lcritical = caracas::tex(results$Lsolutions_L1[[i]])
          LprodFunSolutions = caracas::tex(results$LprodFunSolutions[[i]])
          LSolsStr = paste0("<p>$$ L = ", Lcritical, "$$</p>")
          addLMessage[[i]] = paste0(addLMessage[[i]], LSolsStr, "
                            <p> Substitute the solution for L into the production function</p>
                            <p>$$", Q, " = ", LprodFunSolutions,"$$</p>
                            ")
          if(!results$Lpossible[[i]]){
            addLMessage[[i]] = paste0(addLMessage[[i]], "<p> No non-negative values of both K and L can produce", Q, "units of output when L = ", Lcritical, "</p>
                                      ")
          } else {
            Lsolutions_K = caracas::tex(results$Lsolutions_K[[i]])
            Lsolutions_L = caracas::tex(results$Lsolutions_L2[[i]])
            Lsolutions_C = caracas::tex(results$Lsolutions_C[[i]])
            addLMessage[[i]] = paste0(addLMessage[[i]], "<p> Now solving for K we get </p>
                                      <p>$$ K = ", Lsolutions_K, "$$</p>
                                      ")
            if(Lsolutions_L != Lcritical){
              addLMessage[[i]] = paste0(addLMessage[[i]], "<p> Substituting \\( K = ", Lsolutions_K, "\\) into \\( L = ", Lcritical, "\\) we get </p>
                                      <p>$$ L = ", Lsolutions_L, "$$</p>
                                      ")
            }
            addLMessage[[i]] = paste0(addLMessage[[i]], "<p> Now we can calculate the total cost of production with these values of L and K </p>
                                      <p>$$ C = L*w + K*r $$</p>
                                      <p>$$ C = ", Lsolutions_L,"*", w,"+", Lsolutions_K,"*", r, "$$</p>
                                      <p>$$ C = ", Lsolutions_C, "$$</p>
                                      ")
          }
        }
      }

      if(length(results$Kcritical) > 0){
        addKMessage = list()
        for(i in 2:(1+length(results$Kcritical))){
          addKMessage[[i]] = ""
          Kcritical = caracas::tex(results$Ksolutions_K1[[i]])
          KprodFunSolutions = caracas::tex(results$KprodFunSolutions[[i]])
          KSolsStr = paste0("<p>$$ K = ", Kcritical, "$$</p>")
          addKMessage[[i]] = paste0(addKMessage[[i]], KSolsStr, "
                            <p> Substitute the solution for K into the production function</p>
                            <p>$$", Q, " = ", KprodFunSolutions,"$$</p>
                            ")
          if(!results$Kpossible[[i]]){
            addKMessage[[i]] = paste0(addKMessage[[i]], "<p> No non-negative values of both K and L can produce", Q, "units of output when K = ", Kcritical, "</p>
                                      ")
          } else {
            Ksolutions_L = caracas::tex(results$Ksolutions_L[[i]])
            Ksolutions_K = caracas::tex(results$Ksolutions_K2[[i]])
            Ksolutions_C = caracas::tex(results$Ksolutions_C[[i]])
            addKMessage[[i]] = paste0(addKMessage[[i]], "<p> Now solving for L we get </p>
                                      <p>$$ L = ", Ksolutions_L, "$$</p>
                                      ")
            if(Ksolutions_K != Kcritical){
              addKMessage[[i]] = paste0(addKMessage[[i]], "<p> Substituting \\( L = ", Ksolutions_L, "\\) into \\( K = ", Kcritical, "\\) we get </p>
                                      <p>$$ K = ", Ksolutions_K, "$$</p>
                                      ")
            }
            addKMessage[[i]] = paste0(addKMessage[[i]], "<p> Now we can calculate the total cost of production with these values of L and K </p>
                                      <p>$$ C = L*w + K*r $$</p>
                                      <p>$$ C = ", Ksolutions_K,"*", w,"+", Ksolutions_L,"*", r, "$$</p>
                                      <p>$$ C = ", Ksolutions_C, "$$</p>
                                      ")
          }
        }
      }

      # check corners
      for(i in 1){
        addLMessage[[i]] = ""
        Lcritical = caracas::tex(results$Lsolutions_L1[[i]])
        LprodFunSolutions = caracas::tex(results$LprodFunSolutions[[i]])
        LSolsStr = paste0("<p>$$ L = ", Lcritical, "$$</p>")
        addLMessage[[i]] = paste0(addLMessage[[i]], LSolsStr,"
                            <p> Substitute the solution for L into the production function</p>
                            <p>$$", Q, " = ", LprodFunSolutions,"$$</p>
                            ")
        if(!results$Lpossible[[i]]){
          addLMessage[[i]] = paste0(addLMessage[[i]], "<p> It is not possible to produce ", Q, " units of output when L = 0</p>
                                      ")
        } else {
          Lsolutions_K = caracas::tex(results$Lsolutions_K[[i]])
          Lsolutions_L = caracas::tex(results$Lsolutions_L2[[i]])
          Lsolutions_C = caracas::tex(results$Lsolutions_C[[i]])
          addLMessage[[i]] = paste0(addLMessage[[i]], "<p> Now solving for K we get </p>
                                      <p>$$ K = ", Lsolutions_K, "$$</p>
                                      ")
          addLMessage[[i]] = paste0(addLMessage[[i]], "<p> Now we can calculate the total cost of production when L = 0 and K =", Lsolutions_K,"</p>
                                      <p>$$ C = L*w + K*r $$</p>
                                      <p>$$ C = ", Lsolutions_L,"*", w,"+", Lsolutions_K,"*", r, "$$</p>
                                      <p>$$ C = ", Lsolutions_C, "$$</p>
                                      ")
        }
      }

      for(i in 1){
        addKMessage[[i]] = ""
        Kcritical = caracas::tex(results$Ksolutions_K1[[i]])
        KprodFunSolutions = caracas::tex(results$KprodFunSolutions[[i]])
        KSolsStr = paste0("<p>$$ K = ", Kcritical, "$$</p>")
        addKMessage[[i]] = paste0(addKMessage[[i]], KSolsStr,"
                            <p> Substitute the solution for K into the production function</p>
                            <p>$$", Q, " = ", KprodFunSolutions,"$$</p>
                            ")
        if(!results$Kpossible[[i]]){
          addKMessage[[i]] = paste0(addKMessage[[i]], "<p> It is not possible to produce ", Q, " units of output when K = 0</p>
                                      ")
        } else {
          Ksolutions_L = caracas::tex(results$Ksolutions_L[[i]])
          Ksolutions_K = caracas::tex(results$Ksolutions_K2[[i]])
          Ksolutions_C = caracas::tex(results$Ksolutions_C[[i]])
          addKMessage[[i]] = paste0(addKMessage[[i]], "<p> Now solving for L we get </p>
                                      <p>$$ L = ", Ksolutions_L, "$$</p>
                                      ")
          addKMessage[[i]] = paste0(addKMessage[[i]], "<p> Now we can calculate the total cost of production with these values of L and K </p>
                                      <p>$$ C = L*w + K*r $$</p>
                                      <p>$$ C = ", Ksolutions_K,"*", w,"+", Ksolutions_L,"*", r, "$$</p>
                                      <p>$$ C = ", Ksolutions_C, "$$</p>
                                      ")
        }
      }

      # Separate into rows if Lcritical and Kcritical are both not NULL
      if(length(results$Kcritical)>0){
        if(length(results$Lcritical)>0){
          solutionMessage = paste0(solutionMessage, "<div class='row'>
                                   <div class='column'>
                                   <h3>If we attack this problem by solving for L:</h3>
                                   <div class='row'>
                                   ")
          colwidth = 100/length(results$Lcritical)
          for(i in 1:length(results$Lcritical)){
            solutionMessage = paste0(solutionMessage, "
                                     <div class='column' style='width: ", colwidth,"%;'>",
                                     addLMessage[[i+1]],
                                     "</div>
                                     ")
          }
          solutionMessage = paste0(solutionMessage, "</div>
                                   </div>
                                   <div class='column'>
                                   <h3>If we attack this problem by solving for K:</h3>
                                   <div class='row'>
                                   ")
          colwidth = 100/length(results$Kcritical)
          for(i in 1:length(results$Kcritical)){
            solutionMessage = paste0(solutionMessage, "
                                     <div class='column' style='width: ", colwidth,"%;'>",
                                     addKMessage[[i+1]],
                                     "</div>
                                     ")
          }
          solutionMessage = paste0(solutionMessage, "</div>
                                   </div>
                                   </div>
                                   ")
        } else {
          #K only
          solutionMessage = paste0(solutionMessage, "<div class='row'>
                                   ")
          colwidth = 100/length(results$Kcritical)
          for(i in 1:length(results$Kcritical)){
            solutionMessage = paste0(solutionMessage, "
                                     <div class='column' style='width: ", colwidth,"%;'>",
                                     addKMessage[[i+1]],
                                     "</div>
                                     ")
          }
          solutionMessage = paste0(solutionMessage, "</div>
                                   ")
        }
      } else {
        #L only
        solutionMessage = paste0(solutionMessage, "<div class='row'>
                                   ")
        colwidth = 100/length(results$Lcritical)
        for(i in 1:length(results$Lcritical)){
          solutionMessage = paste0(solutionMessage, "
                                     <div class='column' style='width: ", colwidth,"%;'>",
                                   addLMessage[[i+1]],
                                   "</div>
                                     ")
        }
        solutionMessage = paste0(solutionMessage, "</div>
                                   ")
      }

      #add check for corner solutions
      solutionMessage = paste0(solutionMessage, "<div class='row'>
                                   <h3>Step 5: Check for corner solutions</h3>
                                   <p>$$", Q," = ", prodfun,"$$</p>
                                   <div class='column'>",
                               addLMessage[[1]], "
                                   </div>
                                   <div class='column'>",
                               addKMessage[[1]], "
                                   </div>
                                   </div>
                                   ")
      #Solution
      solutionMessage = paste0(solutionMessage, "<h3>Step 6: Select the lowest cost solution from the set of solutions above</h3>
                               <h3>The solution is: \\(C = ", C, " \\qquad L = ", L, " \\qquad K = ", K, "\\)</h3>")

      values$CostMinStepsSolutions = withMathJax(HTML(solutionMessage))
    }
  })
  
  output$CostMinStepsSolutions = renderUI({
    values$CostMinStepsSolutions
  })
  
  
  

  
}
