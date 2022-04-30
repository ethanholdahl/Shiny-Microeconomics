
## Load and install the packages
#library("tidyverse", "shiny", "stringr", "plotly", "shinyjs")
#theme_set(theme_minimal())


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
    indifferenceCurve=tibble(x = x, y = y, U = U, MRSxy = eval(MUx)/eval(MUy))
    MRSCurveGeom = geom_path(data = indifferenceCurve, aes(x = x, y = y, color = MRSxy))
  }
  
  ############# MAKE BUDGET LINE #############
  
  makeBudgetLine = function(Px, Py, I, color = "blue", linetype = "solid"){
    x = seq(0, I/Px, length.out = 100)
    y = seq(I/Py, 0, length.out = 100)
    budgetLine = tibble(x,y)
    budgetLineGeom = geom_path(data = budgetLine, aes(x=x, y=y), color = color, linetype = linetype)  
    return(budgetLineGeom)
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
  
  makeDerivedDemandPlotX = function(Ufun, Py, I, addedPx, Pxmin, Pxmax, precision = .5, xmax, ymax){
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
      geom_path(data = demandX, aes(x = x, y = Px), color = "orange") +
      geom_point(data = demandAddedX, aes(x = x, y = Px), size = 3) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0, xmax), ylim = c(0, Pxmax)) 
    
    demandPlot = ggplotly(demandPlot) %>%
      layout(yaxis = list(title = "Px"), xaxis = list(title = "x"))
    
    derivedDemandPlot = subplot(indiffCurvesPlot, demandPlot, nrows = 2, shareX = TRUE, titleY = TRUE, titleX = TRUE) %>%
      layout(title = list(text = "Derived Demand Curve"))
    
    return(derivedDemandPlot)
  }
  
  makeDerivedDemandPlotY = function(Ufun, Px, I, addedPy, Pymin, Pymax, precision = .5, xmax, ymax){
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
      geom_path(data = demandY, aes(x = y, y = Py), color = "orange") +
      geom_point(data = demandAddedY, aes(x = y, y = Py), size = 3) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0, xmax), ylim = c(0, Pxmax)) 
    
    demandPlot = ggplotly(demandPlot) %>%
      layout(yaxis = list(title = "Py"), xaxis = list(title = "y"))
    
    derivedDemandPlot = subplot(indiffCurvesPlot, demandPlot, nrows = 2, shareX = TRUE, titleY = TRUE, titleX = TRUE) %>%
      layout(title = list(text = "Derived Demand Curve"))
    
    return(derivedDemandPlot)
  }
  
  ##### FEASIBLE VS INFEASIBLE BUNDLES #######
  
  makeFeasibilityGraphs = function(Px, Py, I, on = 2, over = 2, under = 2, rand = 6){
    xon = runif(on, max = I/Px)
    yon = (I-Px*xon)/Py
    xrand = runif(rand, max = I/Px*1.2)
    yrand = runif(rand, max = I/Py*1.2)
    xover = runif(over, min = 0, max = 3*xmax/4)
    yover=c()
    for(i in 1:over){
      x = xover[i]
      yover = c(yover, runif(1, min = max((I-x*Px)/Py,0), max = 3*ymax/4))
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
  
  
  ########## SHINY SERVER CODE ##########
  
  values <- reactiveValues(myurl = c(), parent_tab = "")
  
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
  output$UtilityFunction = renderText({
    as.character(getMarginal(input$UtilityFunction)[[1]])
  })
  output$UtilityMUx = renderPrint({
    getMarginal(input$UtilityFunction)[[2]]
  })
  output$UtilityMUy = renderPrint({
    getMarginal(input$UtilityFunction)[[3]]
  })
  
  ###### Indifference Curves ######
  output$IndifferencePlot = renderPlotly({
    Ufun = parse(text = input$IndifferenceFunction)
    Ulist = seq(from = input$IndifferenceMaxU/input$IndifferenceNumCurves, to = input$IndifferenceMaxU,length.out = input$IndifferenceNumCurves)
    plot = ggplot() +
      makeAllIndiffernceCurves(Ufun, Ulist, input$IndifferenceXMax, input$IndifferenceYMax) +
      scale_color_viridis_d(begin = .25, end = .85, option="plasma") +
      labs(color = "U(x,y)") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0, input$IndifferenceXMax), ylim = c(0, input$IndifferenceYMax))
    
    plot = ggplotly(plot)
    return(plot)
  })
  ###### MRS Curves ######
  output$MRSPlot = renderPlotly({
    Ufun = parse(text = input$MRSFunction)
    U = input$MRSU
    xmax = input$MRSXMax
    ymax = input$MRSYMax
    plot = ggplot()+
      makeIndifferenceCurve(Ufun, U, xmax, ymax, color = "black") +
      makeMRSCurve(Ufun, U, xmax, ymax) +
      scale_color_viridis_c(option = "viridis", limits = c(0, 10), direction = -1) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax))
    ggplotly(plot)
    return(plot)
  })
}
