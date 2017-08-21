##FRBS

setDefaultParametersIfMissing <- function(control, defaults) {
  for(i in names(defaults)) {
    if(is.null(control[[i]])) control[[i]] <- defaults[[i]]
  }
  control
}

calc.implFunc <- function(antecedent, consequent, type.implication.func = "ZADEH"){
  if (type.implication.func == "DIENES_RESHER"){
    if (consequent > (1 - antecedent))
      temp.rule.degree <- consequent
    else
      temp.rule.degree <- (1 - antecedent)
  }
  else if (type.implication.func == "LUKASIEWICZ"){
    temp.rule.degree <- min(1 - antecedent + consequent, 1)
  }
  else if (type.implication.func == "ZADEH"){
    if (antecedent < 0.5 || (1 - antecedent) > consequent)
      temp.rule.degree <- 1 - consequent
    else {
      if (antecedent < consequent)
        temp.rule.degree <- antecedent
      else
        temp.rule.degree <- consequent
    }
  }
  else if (type.implication.func == "GOGUEN"){
    if (antecedent < consequent)
      temp.rule.degree <- 1
    else
      temp.rule.degree <- consequent / antecedent
  }
  else if (type.implication.func == "GODEL"){
    if (antecedent <= consequent)
      temp.rule.degree <- 1
    else
      temp.rule.degree <- consequent
  }
  else if (type.implication.func == "SHARP"){
    if (antecedent <= consequent)
      temp.rule.degree <- 1
    else
      temp.rule.degree <- 0
  }
  else if (type.implication.func == "MIZUMOTO"){
    temp.rule.degree <- 1 - antecedent + antecedent * consequent
  }
  else if (type.implication.func == "DUBOIS_PRADE"){
    if (consequent == 0)
      temp.rule.degree <- 1 - antecedent
    else { 
      if (antecedent == 1)
        temp.rule.degree <- consequent
      else
        temp.rule.degree <- 1
    }  
  }
  else {
    temp.rule.degree <- min(antecedent, consequent)
  }
  return (temp.rule.degree)
}

frbs.learn <- function (data.train, range.data = NULL, method.type = c("ANFIS"), control = list()) {
  options(verbose = FALSE)
  method.type <- toupper(method.type)
  colnames.var <- colnames(data.train)
  mod <- NULL
  if (class(data.train) != "matrix") {
    data.train <- as.matrix(data.train)
  }
  if (is.null(range.data)) {
    dt.min <- matrix(do.call(pmin, lapply(1:nrow(data.train), function(i) data.train[i, ])), nrow = 1)
    dt.max <- matrix(do.call(pmax, lapply(1:nrow(data.train), function(i) data.train[i, ])), nrow = 1)
    range.data <- rbind(dt.min, dt.max)
  }
  range.data.ori <- range.data
  data.train.ori <- data.train
  num.labels <- control$num.labels
  name <- control$name
  type.tnorm <- control$type.tnorm
  type.snorm <- control$type.snorm
  type.defuz <- control$type.defuz
  type.implication.func <- control$type.implication.func
  num.labels <- matrix(rep(num.labels, ncol(range.data)), nrow = 1)
  data.tra.norm <- norm.data(data.train.ori, range.data.ori, min.scale = 0, max.scale = 1)
  max.iter <- control$max.iter
  step.size <- control$step.size
  if (method.type == "HYFIS"){
    control <- setDefaultParametersIfMissing(control, list(num.labels = 7, max.iter = 10,
                                                           step.size = 0.01, type.mf = "GAUSSIAN", type.defuz = "WAM", type.tnorm = "MIN",
                                                           type.snorm = "MAX", type.implication.func = "ZADEH", name="sim-0"))
    modelSpecific <- HyFIS(data.tra.norm, num.labels, max.iter, step.size, type.tnorm, type.snorm, type.defuz, type.implication.func)
    mod <- modelSpecific
  }
  else if (method.type == "ANFIS"){
    control <- setDefaultParametersIfMissing(control, list(num.labels = 7, max.iter = 10, 
                                                           step.size = 0.01, type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH", alpha.heuristic = 1, name="sim-0"))
    modelSpecific <- ANFIS(data.tra.norm, num.labels, max.iter, step.size, type.tnorm, type.snorm, type.implication.func)
    mod <- modelSpecific
  }
  mod$range.data.ori <- range.data.ori
  mod$method.type <- method.type
  mod$name <- name
  mod <- convert.params(mod)
  mod$colnames.var <- colnames.var
  mod <- frbsObjectFactory(mod)
  varout.mf <- mod$varout.mf.mamd
  rule.mamd <- mod$rule.mamd
  mod$rule.mamdani <- rep.rule.mamd(mod)
  if (!is.null(mod$rule) && !is.null(mod$num.labels)) 
    mod$rule <- rep.rule(mod)
  return(mod)
}

ANFIS <- function (data.train, num.labels, max.iter = 10, step.size = 0.01, type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH")
{
  progressbar <- txtProgressBar(min = 0, max = max.iter, style = 3)
  type.mf <- "GAUSSIAN"
  range.data <- matrix(nrow = 2, ncol = ncol(data.train))
  range.data[1, ] <- 0
  range.data[2, ] <- 1
  mod <- WM(data.train, num.labels, type.mf, type.tnorm, type.implication.func)
  rule.mamd <- mod$rule
  varinp.mf.mamd <- mod$varinp.mf
  varout.mf.mamd <- mod$varout.mf
  data.test <- as.matrix(data.train[, 1:(ncol(data.train) - 1)])
  range.input <- range.data[, -ncol(range.data), drop = FALSE]
  range.output <- range.data[, ncol(range.data), drop = FALSE]
  num.varinput <- ncol(data.train) - 1
  num.labels.input <- num.labels[, -ncol(num.labels), drop = FALSE]
  names.fvalinput <- colnames(mod$varinp.mf)
  rule <- mod$rule
  varinp.mf <- mod$varinp.mf
  rule.data.num <- mod$rule.data.num
  ind.nonDuplicate <- which(duplicated(rule[, -ncol(rule)]) == FALSE, arr.ind = TRUE)
  rule <- rule[ind.nonDuplicate, , drop = FALSE]
  rule.data.num <- rule.data.num[ind.nonDuplicate, , drop = FALSE]
  n.rowrule <- nrow(rule)
  num.ele <- n.rowrule * (num.varinput + 1)
  rand <- runif(num.ele, min = 0, max = 1)
  func.tsk <- matrix(rand, nrow = n.rowrule, ncol = num.varinput + 
                       1, byrow = T)
  type.model <- "TSK"
  mod$rule <- rule
  mod$rule.data.num <- rule.data.num
  mod$method.type <- "ANFIS"
  mod$type.tnorm <- type.tnorm
  mod$type.snorm <- type.snorm
  mod$type.model <- "TSK"
  mod$func.tsk <- func.tsk
  mod$num.labels <- mod$num.labels[, -length(mod$num.labels), 
                                   drop = FALSE]
  mod <- frbsObjectFactory(mod)
  withProgress(message = 'Learning...', detail = "0 %", value = 0, {
    for (iter in 1:max.iter) {
      for (i in 1:nrow(data.test)) {
        dt.i <- data.test[i, , drop = FALSE]
        dt.train.i <- data.train[i, , drop = FALSE]
        res <- frbs.eng(mod, dt.i)
        def <- res$predicted.val
        miu.rule <- res$miu.rule
        param.new <- ANFIS.update(dt.train.i, def, rule.data.num, 
                                  miu.rule, mod$func.tsk, mod$varinp.mf, step.size)
        mod$func.tsk <- param.new$func.tsk.new
        mod$varinp.mf <- param.new$varinp.mf
      }
      setTxtProgressBar(progressbar, iter)
      incProgress(1/max.iter, detail = paste0(round(100*iter/max.iter, digits = 2), " %"))
    }
    #setProgress(1)
  })
  close(progressbar)
  rule <- matrix(c(unlist(rule)), nrow = length(rule), byrow = TRUE)
  num.labels <- num.labels[, -ncol(num.labels), drop = FALSE]
  mod <- list(num.labels = num.labels, rule = rule, rule.data.num = rule.data.num, 
              varinp.mf = mod$varinp.mf, func.tsk = mod$func.tsk, 
              type.tnorm = type.tnorm, type.snorm = type.snorm, type.defuz = NULL, 
              type.model = "TSK", type.mf = "GAUSSIAN", type.implication.func = type.implication.func, 
              varout.mf.mamd = varout.mf.mamd, rule.mamd = rule.mamd)
  return(mod)
}

WM <- function (data.train, num.labels, type.mf = "GAUSSIAN", type.tnorm = "PRODUCT", type.implication.func = "ZADEH", classification = FALSE, range.data = NULL) 
{
  if (!is.null(range.data)) {
    range.data = range.data
  }
  else if (is.null(range.data)) {
    range.data <- matrix(nrow = 2, ncol = ncol(data.train))
    range.data[1, ] <- 0
    range.data[2, ] <- 1
  }
  fuzzyTerm <- create.fuzzyTerm(classification, num.labels)
  names.fvalinput <- fuzzyTerm$names.fvalinput
  names.fvaloutput <- fuzzyTerm$names.fvaloutput
  names.fvalues <- c(names.fvalinput, names.fvaloutput)
  nrow.data.train <- nrow(data.train)
  num.varinput <- ncol(data.train)
  #inisialisasi
  seg <- matrix(ncol = 1)
  center.value <- matrix(ncol = 1)
  var.mf <- matrix(0, nrow = 5, ncol = sum(num.labels))
  #gunakan data.train sebagai data
  data <- data.train
  ####### Wang and Mendel's Steps begin ####################
  ###Step 1: Divide the Input and Output Spaces Into Fuzzy Regions
  jj <- 0
  #loop untuk seluruh variabel
  for (i in 1:num.varinput){
    seg <- num.labels[1, i]
    kk <- 1
    delta.point <- (range.data[2, i] - range.data[1, i])/(seg + 1)
    for (j in 1:num.labels[1, i]) {
      jj <- jj + 1
      delta.gau <- (range.data[2, i] - range.data[1, i])/(seg - 1)
      #GAUSSIAN
      #Row 2 = mean, Row 3 = standar deviasi
      if (kk%%num.labels[1, i] == 1) {
        var.mf[1, jj] <- 5
        var.mf[2, jj] <- range.data[1, i]
        var.mf[3, jj] <- 0.35 * delta.gau
        var.mf[4, jj] <- NA
        var.mf[5, jj] <- NA
      }
      else if (kk%%num.labels[1, i] == 0) {
        var.mf[1, jj] <- 5
        var.mf[2, jj] <- range.data[2, i]
        var.mf[3, jj] <- 0.35 * delta.gau
        var.mf[4, jj] <- NA
        var.mf[5, jj] <- NA
      }
      else {
        var.mf[1, jj] <- 5
        var.mf[2, jj] <- range.data[1, i] + (j - 1) * 
          delta.gau
        var.mf[3, jj] <- 0.35 * delta.gau
        var.mf[4, jj] <- NA
        var.mf[5, jj] <- NA
      }
      kk <- kk + 1
    }
  }
  ## Step 2: Generate Fuzzy Rules from Given Data Pairs.
  ## Step 2a: Determine the degree of data pairs.
  ## MF is matrix membership function. The dimension of MF is n x m, where n is number of data and m is num.labels * input variable (==ncol(var.mf))
  
  MF <- fuzzifier(data, num.varinput, num.labels, var.mf)
  colnames(MF) <- c(names.fvalues)
  MF.max <- matrix(0, nrow = nrow(MF), ncol = ncol(MF))
  k <- 1
  for (i in 1:length(num.labels)) {
    start <- k
    end <- start + num.labels[1, i] - 1
    MF.temp <- MF[, start:end]
    for (m in 1:nrow(MF)) {
      max.MFTemp <- max(MF.temp[m, ], na.rm = TRUE)
      max.loc <- which.max(MF.temp[m, ])
      MF.max[m, k + max.loc - 1] <- max.MFTemp
    }
    k <- k + num.labels[1, i]
  }
  colnames(MF.max) <- c(names.fvalues)
  rule.matrix <- MF.max
  # Step III
  ## determine the degree of the rule
  degree.rule <- matrix(nrow = nrow(rule.matrix), ncol = 1)
  degree.ante <- matrix(nrow = nrow(rule.matrix), ncol = 1)
  ## calculate t-norm in antecedent part and implication function with consequent part
  for (i in 1:nrow(rule.matrix)) {
    temp.ant.rule.degree <- c(1)
    max.ant.indx <- c(ncol(rule.matrix) - num.labels[1, ncol(num.labels)])
    for (j in 1:max.ant.indx) {
      if (rule.matrix[i, j] != 0) {
        if (type.tnorm == "PRODUCT") {
          temp.ant.rule.degree <- temp.ant.rule.degree * rule.matrix[i, j]
        }
        else if (type.tnorm == "MIN") {
          temp.ant.rule.degree <- min(temp.ant.rule.degree, rule.matrix[i, j])
        }
        else if (type.tnorm == "HAMACHER") {
          temp.ant.rule.degree <- (temp.ant.rule.degree * rule.matrix[i, j])/(temp.ant.rule.degree + rule.matrix[i, j] - temp.ant.rule.degree * rule.matrix[i, j])
        }
        else if (type.tnorm == "YAGER") {
          temp.ant.rule.degree <- 1 - min(1, ((1 - temp.ant.rule.degree) + (1 - rule.matrix[i, j])))
        }
        else if (type.tnorm == "BOUNDED") {
          temp.ant.rule.degree <- max(0, (temp.ant.rule.degree + rule.matrix[i, j] - 1))
        }
      }
    }
    degree.ante[i] <- temp.ant.rule.degree
    for (k in (max.ant.indx + 1):ncol(rule.matrix)) {
      if (rule.matrix[i, k] != 0) {
        temp.rule.degree <- calc.implFunc(degree.ante[i], rule.matrix[i, k], type.implication.func)
      }
    }
    degree.rule[i] <- temp.rule.degree
  }
  rule.matrix[rule.matrix > 0] <- 1
  rule.matrix.bool <- rule.matrix
  ## find the same elements on matrix rule considering degree of rules
  rule.red.mat <- rule.matrix
  temp <- cbind(degree.rule, degree.ante, rule.matrix.bool)
  temp <- temp[order(temp[, c(1)], decreasing = TRUE), ]
  indx.nondup <- which(duplicated(temp[, -c(1, 2)]) == FALSE, arr.ind = TRUE)
  rule.complete <- temp[indx.nondup, , drop = FALSE]
  degree.ante <- rule.complete[, 2, drop = FALSE]
  degree.rule <- rule.complete[, 1, drop = FALSE]
  rule.red.mat <- rule.complete[, 3:ncol(rule.complete)]
  
  ## delete incomplete rule
  ind.incomplete <- which(rowSums(rule.red.mat) != num.varinput)
  rule.red.mat[ind.incomplete, ] <- NA
  degree.rule[ind.incomplete] <- NA
  degree.ante[ind.incomplete] <- NA
  
  ## create rule in numeric
  seqq <- seq(1:ncol(rule.red.mat))
  rule.data.num <- t(apply(rule.red.mat, 1, function(x) x * seqq))
  
  ## rule.data.num is the numbers representing the sequence of string of variable names   
  rule.data.num <- na.omit(rule.data.num)
  degree.ante <- na.omit(degree.ante)
  degree.rule <- na.omit(degree.rule)
  rule.data.num[which(rule.data.num == 0)] <- NA
  rule.data.num <- t(apply(rule.data.num, 1, na.omit))
  
  ## build the rule into list of string
  res <- generate.rule(rule.data.num, num.labels, names.fvalinput, names.fvaloutput)
  rule <- res$rule
  
  #############################################################
  ### Collect the Input data for "frbs for testing"
  #############################################################
  
  ## indx is index of output variable on num.labels
  indx <- length(num.labels)
  length.namesvar <- length(names.fvalues)
  
  ## cut membership function of output variable on var.mf
  ll <- (ncol(var.mf) - num.labels[1, indx])
  varinp.mf <- var.mf[, 1:ll, drop = FALSE]
  colnames(varinp.mf) <- names.fvalinput
  ## get varout.mf
  varout.mf <- var.mf[, (ll + 1):ncol(var.mf), drop = FALSE]
  colnames(varout.mf) <- names.fvaloutput
  
  ## clean rule
  rule <- na.omit(rule)
  
  ## range of original data
  range.data.ori <- range.data
  mod <- list(num.labels = num.labels, varout.mf = varout.mf, 
              rule = rule, varinp.mf = varinp.mf, degree.ante = degree.ante, 
              rule.data.num = rule.data.num, degree.rule = degree.rule, 
              range.data.ori = range.data.ori, type.mf = type.mf, 
              type.tnorm = type.tnorm, type.implication.func = type.implication.func)
  return(mod)
}

create.fuzzyTerm <- function (classification = FALSE, num.labels) {
  num.inpvar <- (ncol(num.labels) - 1)
  #fuzzy term variabel input
  if (num.labels[1, 1] == 3) {
    fuzzy.labels.inpvar <- rep(c("small", "medium", "large"), num.inpvar)
  }
  else if (num.labels[1, 1] == 5) {
    fuzzy.labels.inpvar <- rep(c("very.small", "small", "medium", "large", "very.large"), num.inpvar)
  }
  else if (num.labels[1, 1] == 7) {
    fuzzy.labels.inpvar <- rep(c("vv.small", "v.small", "small", "medium", "large", "v.large", "vv.large"), num.inpvar)
  }
  else {
    num.inp.var <- sum(num.labels[1, 1:(ncol(num.labels) - 1)])
    seq.inp.num <- seq(from = 1, to = num.inp.var, by = 1)
    temp <- list()
    k <- 0
    for (i in 1:num.inp.var) {
      k <- k + 1
      j <- ((i - 1)%/%num.labels[1, 1]) + 1
      var <- paste("v", j, sep = ".")
      fuz <- paste("a", k, sep = ".")
      new <- paste(var, fuz, sep = "_")
      temp <- append(temp, new)
      if (i%%num.labels[1, 1] == 0) {
        k <- 0
      }
    }
    fuzzy.labels.inpvar <- as.character(temp)
  }
  
  #fuzzy term variabel output
  if (num.labels[1, ncol(num.labels)] == 3) {
    fuzzy.labels.outvar <- c("small", "medium", "large")
  }
  else if (num.labels[1, ncol(num.labels)] == 5) {
    fuzzy.labels.outvar <- c("very.small", "small", "medium", "large", "very.large")
  }
  else if (num.labels[1, ncol(num.labels)] == 7) {
    fuzzy.labels.outvar <- c("vv.small", "v.small", "small", "medium", "large", "v.large", "vv.large")
  }
  else {
    num.out.var <- num.labels[1, ncol(num.labels)]
    seq.out.num <- seq(from = 1, to = num.out.var, by = 1)
    fuzzy.labels.outvar <- paste("c", seq.out.num, sep = ".")
  }
  return(list(names.fvalinput = fuzzy.labels.inpvar, names.fvaloutput = fuzzy.labels.outvar))
}

#Mengembalikan matrix of the degree of each linguistic terms based on the shape of the membership functions 
fuzzifier <- function (data, num.varinput, num.labels.input, varinp.mf) {
  ##count number of column of data
  ncol.data <- ncol(data)
  ##count number of column of varinp.mf (matrix used to build membership function) 
  ncol.var <- ncol(varinp.mf)
  ##Inisialitation matrix of Membership function
  MF <- matrix(nrow = nrow(data), ncol = ncol.var)
  #check
  if (ncol.data != num.varinput) 
    stop("Data is not the same as the number of input variables")
  if (ncol.var != sum(num.labels.input)) 
    stop("the parameter of membership function is not the same with variable")
  
  ##h is index equal to number of data
  ##i is index for numbering variable
  ##j is index equal to number of varinp.mf column
  ##ii is used for counting how many iteration has been done in the following loop
  ##jj is used for keeping the iteration continueing to next index in varinp.mf
  
  for (h in 1:nrow(data)) {
    jj <- 1
    for (i in 1:ncol(data)) {
      ii <- 1
      for (j in jj:ncol(varinp.mf)) {
        
        #Gaussian
        if (varinp.mf[1, j] == 5) {
          temp <- exp(-0.5 * (data[h, i] - varinp.mf[2, j])^2/varinp.mf[3, j]^2)
        }
        ##save membership function on MF for each data  
        MF[h, j] <- temp
        ii <- ii + 1
        jj <- jj + 1
        
        ##this checking is used for control the number of linguistic value for each variable
        if (ii > num.labels.input[1, i]) 
          break
      }
    }
  }
  return(MF)
}

#generate rule into a string form
generate.rule <- function (rule.data.num, num.labels, names.fTerms.inpvar = NULL, names.fTerms.outvar = NULL) 
{
  names.inp.var <- names.fTerms.inpvar
  names.out.var <- names.fTerms.outvar
  names.variable <- c(names.inp.var, names.out.var)
  ## build the rule into list of string
  rule <- matrix(nrow = nrow(rule.data.num), ncol = 2 * ncol(rule.data.num) - 1)
  for (i in 1:nrow(rule.data.num)) {
    k <- 0
    for (j in 1:ncol(rule.data.num)) {
      k <- k + 1
      if (j == ncol(rule.data.num) - 1) {
        if (rule.data.num[i, j] == 0) {
          rule[i, k] <- c("dont_care")
        }
        else {
          rule[i, k] <- c(names.variable[rule.data.num[i, j]])
        }
        rule[i, k + 1] <- c("->")
        k <- k + 1
      }
      else if (j == ncol(rule.data.num)) {
        if (rule.data.num[i, j] == 0) {
          rule[i, k] <- c("dont_care")
        }
        else {
          rule[i, k] <- c(names.variable[rule.data.num[i, j]])
        }
      }
      else {
        if (rule.data.num[i, j] == 0) {
          rule[i, k] <- c("dont_care")
        }
        else {
          rule[i, k] <- c(names.variable[rule.data.num[i, j]])
        }
        rule[i, k + 1] <- c("and")
        k <- k + 1
      }
    }
  }
  res <- list(rule = rule, names.varinput = names.inp.var, names.varoutput = names.out.var)
  return(res)
}

frbsObjectFactory <- function (mod) 
{
  class(mod) <- "frbs"
  return(mod)
}

#' This function involves four different processing steps on fuzzy rule-based systems. 
#' Firstly, the rulebase (see \code{\link{rulebase}}) validates 
#' the consistency of the fuzzy IF-THEN rules form. Then, the fuzzification 
#' (see \code{\link{fuzzifier}}) transforms crisp values 
#' into linguistic terms. Next, the inference calculates the degree of rule strengths using 
#' the t-norm and the s-norm. 
#' Finally, the defuzzification process calculates the results of the model using the Mamdani 
#' or the Takagi Sugeno Kang model.  
frbs.eng <- function (object, newdata) 
{
  if (object$type.model == "TSK") {
    object$num.labels = cbind(object$num.labels, object$num.labels[1, 1])
  }
  #ambil parameter
  range.output <- object$range.data.ori[, ncol(object$range.data.ori), 
                                        drop = FALSE]
  num.varinput <- ncol(newdata)
  
  ## change linguistic terms/labels to be unique
  temp <- ch.unique.fuzz(object$type.model, object$rule, object$varinp.mf, object$varout.mf, num.varinput, object$num.labels)
  rule <- temp$rule
  varinp.mf <- temp$varinp.mf
  varout.mf <- temp$varout.mf
  names.fvalinput <- temp$names.fvalinput
  names.fvaloutput <- temp$names.fvaloutput
  names.fvalues <- temp$names.fvalues
  num.labels.input <- object$num.labels[, -ncol(object$num.labels), drop = FALSE]
  type.defuz <- object$type.defuz
  type.tnorm <- object$type.tnorm
  type.snorm <- object$type.snorm
  type.model <- object$type.model
  func.tsk <- object$func.tsk
  range.output[1, ] <- 0
  range.output[2, ] <- 1
  
  ##################
  ### I. Rule Based Module
  ### In this function, Checking of the rule given by user will be done.
  ### There are two kinds of model used which are Mamdani and TSK rule model.
  
  rule <- rulebase(type.model, rule, func.tsk)
  
  ###################
  ### II. Fuzzification Module
  ### In this function, we convert crisp value into linguistic value based on the data and parameter of membership function.
  ### There are several membership function can be used such as triangular, trapezoid, gaussian and logistic/sigmoid.
  ###################
  MF <- fuzzifier(newdata, num.varinput, num.labels.input, varinp.mf)
  
  ###################
  ### III. Inference Module
  ### In this function, we will calculate the confidence factor on antecedent for each rule. We use AND, OR, NOT operator. 
  ###################
  ncol.MF <- ncol(MF)
  names.var <- names.fvalues[1:ncol.MF]
  colnames(MF) <- c(names.var)
  miu.rule <- inference(MF, rule, names.fvalinput, type.tnorm, type.snorm)
  if (is.null(object$method.type) == FALSE && object$method.type == "HYFIS") {
    if (!is.null(object$degree.rule)) 
      degree.rule <- object$degree.rule
    else degree.rule <- 1
    for (i in 1:nrow(miu.rule)) {
      miu.rule[i, ] <- miu.rule[i, ] * t(degree.rule)
    }
  }
  ###################
  ### IV. Defuzzification Module
  ### In this function, we calculate and convert linguistic value back into crisp value. 
  ###################
  def <- defuzzifier(newdata, rule, range.output, names.fvaloutput, 
                     varout.mf, miu.rule, type.defuz, type.model, func.tsk)
  res <- list(rule = rule, varinp.mf = varinp.mf, varout.mf = varout.mf, 
              MF = MF, miu.rule = miu.rule, func.tsk = func.tsk, predicted.val = def)
  return(res)
}

ch.unique.fuzz <- function (type.model, rule, varinp.mf, varout.mf = NULL, num.varinput, num.labels) 
{
  if (type.model == "MAMDANI") {
    num.labels.input <- num.labels[, -ncol(num.labels), drop = FALSE]
    if (!is.null(rule) && rule[1, 1] == "IF") {
      k <- 1
      new.rule <- matrix(NA, nrow = nrow(rule), ncol = (2 * 
                                                          num.varinput + 1))
      for (i in 1:num.varinput) {
        new.rule[, k] <- paste(rule[, (4 * i), drop = FALSE], i, sep = ".")
        new.rule[, k + 1] <- rule[, ((4 * i) + 1)]
        k <- k + 2
      }
      new.rule[, (ncol(new.rule) - 1)] <- "->"
      new.rule[, ncol(new.rule)] <- paste(rule[, ncol(rule), drop = FALSE], num.varinput + 1, sep = ".")
      rule <- new.rule
    }
    else if (!is.null(rule)) {
      i <- 1
      k <- 1
      while (i <= ncol(rule)) {
        rule[, i] <- paste(rule[, i, drop = FALSE], k, sep = ".")
        i <- i + 2
        k <- k + 1
      }
    }
  }
  else if (type.model == "TSK") {
    num.labels.input <- num.labels[, -ncol(num.labels), drop = FALSE]
    if (!is.null(rule) && rule[1, 1] == "IF") {
      k <- 1
      new.rule <- matrix(NA, nrow = nrow(rule), ncol = (2 * num.varinput))
      for (i in 1:num.varinput) {
        new.rule[, k] <- paste(rule[, (4 * i), drop = FALSE], i, sep = ".")
        new.rule[, k + 1] <- rule[, ((4 * i) + 1)]
        k <- k + 2
      }
      new.rule[, ncol(new.rule)] <- "->"
      rule <- new.rule
    }
    else if (!is.null(rule)) {
      i <- 1
      k <- 1
      while (i < ncol(rule)) {
        rule[, i] <- paste(rule[, i, drop = FALSE], k, sep = ".")
        i <- i + 2
        k <- k + 1
      }
    }
  }
  ## Change the linguistic values on input data
  seq.names <- rep(1:num.varinput, num.labels.input)
  names.fvalinput <- paste(colnames(varinp.mf), seq.names, 
                           sep = ".")
  colnames(varinp.mf) <- names.fvalinput
  ## Change the linguistic values on output data
  if (type.model == "MAMDANI") {
    seq.names <- rep(num.varinput + 1, num.labels[, ncol(num.labels)])
    names.fvaloutput <- paste(colnames(varout.mf), seq.names, 
                              sep = ".")
    colnames(varout.mf) <- names.fvaloutput
    names.fvalues <- c(names.fvalinput, names.fvaloutput)
  }
  else if (type.model == "TSK") {
    names.fvaloutput <- colnames(varout.mf)
    names.fvalues <- c(names.fvalinput, names.fvaloutput)
  }
  return(list(rule = rule, varinp.mf = varinp.mf, varout.mf = varout.mf, 
              names.fvalinput = names.fvalinput, names.fvaloutput = names.fvaloutput, 
              names.fvalues = names.fvalues))
}

defuzzifier <- function (data, rule = NULL, range.output = NULL, names.varoutput = NULL, varout.mf = NULL, miu.rule, type.defuz = NULL, type.model = "TSK", func.tsk = NULL) 
{
  ## Inisialitation
  def <- matrix(0, nrow = nrow(data), ncol = 1)
  def.temp <- matrix(0, nrow = nrow(data), ncol = 1)
  
  ## Mamdani type
  if (type.model == 1 || type.model == "MAMDANI") {
    #copy rule
    rule.temp <- matrix(unlist(rule), nrow = length(rule), byrow = TRUE)
    if (is.null(names.varoutput)) {
      stop("Please define the names of the output variable.")
    }
    ## check parameters of membership functions on output variables
    if (is.null(varout.mf)) {
      stop("Please define the parameters of membership functions on the output variable.")
    }
    ## Inisialitation
    cum <- matrix(0, nrow = nrow(data), ncol = ncol(varout.mf))
    div <- matrix(0, nrow = nrow(data), ncol = ncol(varout.mf))
    
    ##Check not zero on miu.rule
    for (k in 1:nrow(data)) {
      chck <- which(miu.rule[k, ] != 0)
      l.chck <- length(chck)
      cum <- matrix(0, nrow = nrow(data), ncol = l.chck)
      div <- matrix(0, nrow = nrow(data), ncol = l.chck)
      temp <- 0
      temp.d <- 0
      temp1 <- 0
      temp2 <- 0
      if (l.chck != 0) {
        indx <- matrix(nrow = l.chck, ncol = 2)
        temp.indx <- matrix(nrow = 1, ncol = 2)
        for (ii in 1:l.chck) {
          strg <- c(rule.temp[chck[ii], ncol(rule.temp)])
          aaa <- which(strg == names.varoutput)
          if (length(aaa) != 0) {
            indx[ii, 1] <- aaa
            indx[ii, 2] <- miu.rule[k, chck[ii]]
          }
        }
        if (nrow(indx) > 1) {
          indx <- indx[order(indx[, c(2)], decreasing = TRUE), 
                       ]
          indx.nondup <- which(duplicated(indx[, 1]) == 
                                 FALSE, arr.ind = TRUE)
          indx <- indx[indx.nondup, , drop = FALSE]
        }
        if (type.defuz == 1 || type.defuz == "WAM") {
          for (i in 1:nrow(indx)) {
            if (any(varout.mf[1, indx[i, 1]] == c(5, 6, 7))) {
              av.point <- varout.mf[2, indx[i, 1]]
            }
            else {
              av.point <- varout.mf[3, indx[i, 1]]
            }
            temp1 <- indx[i, 2] * av.point
            temp2 <- indx[i, 2]
            temp <- temp + temp1
            temp.d <- temp.d + temp2
            cum[k, i] <- temp
            div[k, i] <- temp.d
          }
          if (sum(div[k, ]) == 0) {
            def[k, 1] <- (min(range.output, na.rm = TRUE) + max(range.output, na.rm = TRUE))/2
          }
          else {
            def.temp[k, 1] <- sum(cum[k, ])/sum(div[k, ])
            if (def.temp[k, 1] <= min(range.output, 
                                      na.rm = TRUE)) {
              def[k, 1] <- min(range.output, na.rm = TRUE)
            }
            else if (def.temp[k, 1] >= max(range.output, 
                                           na.rm = TRUE)) {
              def[k, 1] <- max(range.output, na.rm = TRUE)
            }
            else {
              def[k, 1] <- def.temp[k, 1]
            }
          }
        }
        else if (any(type.defuz == c(2, 3)) || any(type.defuz == c("FIRST.MAX", "LAST.MAX"))) {
          max.temp <- max(indx[, 2], na.rm = TRUE)
          max.indx <- which(indx[, 2] == max.temp)
          aa <- varout.mf[2, indx[max.indx[1], 1]]
          bb <- varout.mf[3, indx[max.indx[1], 1]]
          cc <- varout.mf[4, indx[max.indx[1], 1]]
          dd <- varout.mf[5, indx[max.indx[1], 1]]
          if (varout.mf[1, indx[max.indx[1], 1]] == 
                1) {
            seqq <- seq(from = varout.mf[2, indx[max.indx[1], 1]], 
                        to = varout.mf[4, indx[max.indx[1], 1]], by = (varout.mf[4, indx[max.indx[1], 
                                                                                         1]] - varout.mf[2, indx[max.indx[1], 1]])/10)
            for (j in 1:length(seqq)) {
              if (seqq[j] < aa) {
                temp.miu <- 1
              }
              else if (seqq[j] >= aa & seqq[j] < bb) {
                temp.miu <- (seqq[j] - aa)/(bb - aa)
              }
              else if (seqq[j] >= bb & seqq[j] < cc) {
                temp.miu <- (seqq[j] - cc)/(bb - cc)
              }
              else temp.miu <- 1
              if (temp.miu >= indx[max.indx[1], 2]) {
                def[k, 1] <- seqq[j]
                if (type.defuz == 2 || type.defuz == 
                      "FIRST.MAX") {
                  break
                }
              }
            }
          }
          else if (varout.mf[1, indx[max.indx[1], 1]] == 
                     2) {
            seqq <- seq(from = varout.mf[2, indx[max.indx[1], 
                                                 1]], to = varout.mf[4, indx[max.indx[1], 
                                                                             1]], by = (varout.mf[4, indx[max.indx[1], 
                                                                                                          1]] - varout.mf[2, indx[max.indx[1], 1]])/10)
            for (j in 1:length(seqq)) {
              if (seqq[j] < bb) {
                temp.miu <- 1
              }
              else if (seqq[j] >= bb & seqq[j] < cc) {
                temp.miu <- (seqq[j] - cc)/(bb - cc)
              }
              else if (seqq[j] > cc) {
                temp.miu <- 1
              }
              if (temp.miu >= indx[max.indx[1], 2]) {
                def[k, 1] <- seqq[j]
                if (type.defuz == 2 || type.defuz == 
                      "FIRST.MAX") {
                  break
                }
              }
            }
          }
          else if (varout.mf[1, indx[max.indx[1], 1]] == 
                     3) {
            seqq <- seq(from = varout.mf[2, indx[max.indx[1], 
                                                 1]], to = varout.mf[4, indx[max.indx[1], 
                                                                             1]], by = (varout.mf[4, indx[max.indx[1], 
                                                                                                          1]] - varout.mf[2, indx[max.indx[1], 1]])/10)
            for (j in 1:length(seqq)) {
              if (seqq[j] < aa) {
                temp.miu <- 0
              }
              else if (seqq[j] >= aa & seqq[j] < bb) {
                temp.miu <- (seqq[j] - aa)/(bb - aa)
              }
              else if (seqq[j] > cc) {
                temp.miu <- 1
              }
              if (temp.miu >= indx[max.indx[1], 2]) {
                def[k, 1] <- seqq[j]
                if (type.defuz == 2 || type.defuz == 
                      "FIRST.MAX") {
                  break
                }
              }
            }
          }
          else if (varout.mf[1, indx[max.indx[1], 1]] == 
                     4) {
            seqq <- seq(from = varout.mf[2, indx[max.indx[1], 
                                                 1]], to = varout.mf[4, indx[max.indx[1], 
                                                                             1]], by = (varout.mf[4, indx[max.indx[1], 
                                                                                                          1]] - varout.mf[2, indx[max.indx[1], 1]])/10)
            for (j in 1:length(seqq)) {
              if (seqq[j] < aa) {
                temp.miu <- 0
              }
              else if (seqq[j] >= aa & seqq[j] < bb) {
                temp.miu <- (seqq[j] - aa)/(bb - aa)
              }
              else if (seqq[j] >= bb & seqq[j] < cc) {
                temp.miu <- 1
              }
              else if (seqq[j] >= cc & seqq[j] < dd) {
                temp.miu <- (seqq[j] - dd)/(cc - dd)
              }
              else {
                temp.miu <- 0
              }
              if (temp.miu >= indx[max.indx[1], 2]) {
                def[k, 1] <- seqq[j]
                if (type.defuz == 2 || type.defuz == 
                      "FIRST.MAX") {
                  break
                }
              }
            }
          }
          else if (varout.mf[1, indx[max.indx[1], 1]] == 
                     5) {
            seqq <- seq(from = min(range.output), to = max(range.output), 
                        by = (max(range.output) - min(range.output))/100)
            for (j in 1:length(seqq)) {
              temp.miu <- exp(-0.5 * (seqq[j] - aa)^2/bb^2)
              if (temp.miu >= indx[max.indx[1], 2]) {
                def[k, 1] <- seqq[j]
                if (type.defuz == 2 || type.defuz == 
                      "FIRST.MAX") {
                  break
                }
              }
              else {
                def[k, 1] <- (min(range.output, na.rm = TRUE) + 
                                max(range.output, na.rm = TRUE))/2
              }
            }
          }
          else if (varout.mf[1, indx[max.indx[1], 1]] == 
                     6) {
            seqq <- seq(from = min(range.output), to = max(range.output), 
                        by = (max(range.output) - min(range.output))/10)
            for (j in 1:length(seqq)) {
              temp.miu <- 1/(1 + exp(-aa * (seqq[j] - 
                                              bb)))
              if (temp.miu >= indx[max.indx[1], 2]) {
                def[k, 1] <- seqq[j]
                if (type.defuz == 2 || type.defuz == 
                      "FIRST.MAX") {
                  break
                }
              }
              else {
                def[k, 1] <- (min(range.output, na.rm = TRUE) + 
                                max(range.output, na.rm = TRUE))/2
              }
            }
          }
          else if (varout.mf[1, indx[max.indx[1], 1]] == 
                     7) {
            seqq <- seq(from = min(range.output), to = max(range.output), 
                        by = (max(range.output) - min(range.output))/10)
            for (j in 1:length(seqq)) {
              temp.miu <- 1/(1 + abs((seqq[j] - cc)/aa)^(2 * 
                                                           bb))
              if (temp.miu >= indx[max.indx[1], 2]) {
                def[k, 1] <- seqq[j]
                if (type.defuz == 2 || type.defuz == 
                      "FIRST.MAX") {
                  break
                }
              }
              else {
                def[k, 1] <- (min(range.output, na.rm = TRUE) + 
                                max(range.output, na.rm = TRUE))/2
              }
            }
          }
        }
        else if (type.defuz == 4 || type.defuz == "MEAN.MAX") {
          max.temp <- max(indx[, 2], na.rm = TRUE)
          max.indx <- which(indx[, 2] == max.temp)
          def[k, 1] <- 0.5 * (max(varout.mf[2:5, indx[max.indx[1], 
                                                      1]], na.rm = TRUE) + (varout.mf[2, indx[max.indx[1], 
                                                                                              1]]))
        }
        else if (type.defuz == 5 || type.defuz == "COG") {
          for (i in 1:nrow(indx)) {
            if (any(varout.mf[1, indx[i, 1]] == c(5, 
                                                  6, 7))) {
              av.point <- varout.mf[2, indx[i, 1]]
              temp1 <- indx[i, 2] * av.point * varout.mf[3, 
                                                         indx[i, 1]]
              temp2 <- indx[i, 2] * varout.mf[3, indx[i, 
                                                      1]]
              temp <- temp + temp1
              temp.d <- temp.d + temp2
            }
            else {
              av.point <- varout.mf[3, indx[i, 1]]
              temp1 <- indx[i, 2] * av.point
              temp2 <- indx[i, 2]
              temp <- temp + temp1
              temp.d <- temp.d + temp2
            }
            cum[k, i] <- temp
            div[k, i] <- temp.d
          }
          if (sum(div[k, ]) == 0) {
            def[k, 1] <- (min(range.output, na.rm = TRUE) + 
                            max(range.output, na.rm = TRUE))/2
          }
          else {
            def.temp[k, 1] <- sum(cum[k, ])/sum(div[k, 
                                                    ])
            if (def.temp[k, 1] <= min(range.output, 
                                      na.rm = TRUE)) {
              def[k, 1] <- min(range.output, na.rm = TRUE)
            }
            else if (def.temp[k, 1] >= max(range.output, 
                                           na.rm = TRUE)) {
              def[k, 1] <- max(range.output, na.rm = TRUE)
            }
            else {
              def[k, 1] <- def.temp[k, 1]
            }
          }
        }
      }
      else {
        def[k, 1] <- (min(range.output, na.rm = TRUE) + 
                        max(range.output, na.rm = TRUE))/2
      }
    }
  }
  else if (type.model == 2 || type.model == "TSK") {
    if (is.null(func.tsk)) {
      stop("Please define the linear equations on the consequent parts of the fuzzy IF-THEN rules.")
    }
    for (k in 1:nrow(data)) {
      data.m <- data[k, , drop = FALSE]
      if (ncol(func.tsk) > 1) {
        func.tsk.var <- func.tsk[, -ncol(func.tsk), 
                                 drop = FALSE]
        func.tsk.cont <- func.tsk[, ncol(func.tsk), 
                                  drop = FALSE]
        ff <- func.tsk.var %*% t(data.m) + func.tsk.cont
      }
      else if (ncol(func.tsk) == 1) {
        ff <- func.tsk
      }
      miu.rule.t <- miu.rule[k, , drop = FALSE]
      cum <- miu.rule.t %*% ff
      div <- sum(miu.rule.t)
      def[k, 1] <- cum/div
      if (div == 0) 
        def[k, 1] <- 0
      else {
        def[k, 1] <- cum/div
        if (def[k, 1] > max(range.output)) 
          def[k, 1] <- max(range.output)
        else if (def[k, 1] < min(range.output)) 
          def[k, 1] <- min(range.output)
      }
    }
  }
  return(def)
}

rulebase <- function (type.model, rule, func.tsk = NULL) 
{
  if (class(rule) == "matrix") {
    rule.list <- list()
    for (i in 1:nrow(rule)) {
      rule.list[i] <- list(rule[i, ])
    }
    rule <- rule.list
  }
  if (type.model == 1 || type.model == 2 || type.model ==  "MAMDANI" || type.model == "TSK") {
    for (i in 1:length(rule)) {
      equal <- unlist(rule[i])
      equal <- as.matrix(equal)
      check <- any(equal == "->")
      if (check == FALSE) 
        stop("rule must contain \"->\" as separating between antecendent and consequence")
    }
  }
  if (type.model == 2 || type.model == "TSK") {
    if (is.null(func.tsk)) 
      stop("You are using Takagi Sugeno Kang model, so the consequent part must be linear equations, please insert their values")
  }
  return(rule)
}

inference <- function (MF, rule, names.varinput, type.tnorm, type.snorm) 
{
  nMF <- nrow(MF)
  nrule <- length(rule)
  miu.rule <- matrix(nrow = nMF, ncol = nrule)
  colnames(MF) <- c(names.varinput)
  for (k in 1:nMF) {
    i <- 1
    for (i in 1:nrule) {
      temp <- unlist(rule[i])
      loc <- which(temp == "->")
      temp.fvalue <- strsplit(temp[1], split = " ")
      Mtemp.fvalue <- unlist(temp.fvalue)
      length.MtempValue <- length(Mtemp.fvalue)
      if (grepl("dont_care", Mtemp.fvalue[1])) {
        val.antecedent <- 1
      }
      else {
        if (length.MtempValue > 1) {
          if (Mtemp.fvalue[1] == "not") {
            if (Mtemp.fvalue[2] == "extremely") {
              val.antecedent.temp <- (MF[k, Mtemp.fvalue[3]])^3
              val.antecedent <- 1 - val.antecedent.temp
            }
            else if (Mtemp.fvalue[2] == "very") {
              val.antecedent.temp <- (MF[k, Mtemp.fvalue[3]])^2
              val.antecedent <- 1 - val.antecedent.temp
            }
            else if (Mtemp.fvalue[2] == "somewhat") {
              val.antecedent.temp <- (MF[k, Mtemp.fvalue[3]])^0.5
              val.antecedent <- 1 - val.antecedent.temp
            }
            else if (Mtemp.fvalue[2] == "slightly") {
              val.antecedent.temp <- (MF[k, Mtemp.fvalue[3]])^0.333
              val.antecedent <- 1 - val.antecedent.temp
            }
            else {
              val.antecedent <- 1 - MF[k, Mtemp.fvalue[2]]
            }
          }
          else {
            if (Mtemp.fvalue[1] == "extremely") 
              val.antecedent <- (MF[k, Mtemp.fvalue[2]])^3
            else if (Mtemp.fvalue[1] == "very") 
              val.antecedent <- (MF[k, Mtemp.fvalue[2]])^2
            else if (Mtemp.fvalue[1] == "somewhat") 
              val.antecedent <- (MF[k, Mtemp.fvalue[2]])^0.5
            else if (Mtemp.fvalue[1] == "slightly") 
              val.antecedent <- (MF[k, Mtemp.fvalue[2]])^0.333
            else if (Mtemp.fvalue[1] == "not") 
              val.antecedent <- 1 - (MF[k, Mtemp.fvalue[2]])
          }
        }
        else {
          val.antecedent <- MF[k, temp[1]]
        }
      }
      seqq <- seq(from = 1, to = loc, by = 2)
      for (j in seqq) {
        temp.split <- strsplit(temp[j + 2], split = " ")
        Mtemp.fvalue <- unlist(temp.split)
        length.MtempValue <- length(Mtemp.fvalue)
        if (grepl("dont_care", Mtemp.fvalue[1])) {
          val.antecedent.b <- 1
        }
        else {
          if (length.MtempValue > 1) {
            if (Mtemp.fvalue[1] == "not") {
              if (Mtemp.fvalue[2] == "extremely") {
                val.antecedent.temp <- (MF[k, Mtemp.fvalue[3]])^3
                val.antecedent.b <- 1 - val.antecedent.temp
              }
              else if (Mtemp.fvalue[2] == "very") {
                val.antecedent.temp <- (MF[k, Mtemp.fvalue[3]])^2
                val.antecedent.b <- 1 - val.antecedent.temp
              }
              else if (Mtemp.fvalue[2] == "somewhat") {
                val.antecedent.temp <- (MF[k, Mtemp.fvalue[3]])^0.5
                val.antecedent.b <- 1 - val.antecedent.temp
              }
              else if (Mtemp.fvalue[2] == "slightly") {
                val.antecedent.temp <- (MF[k, Mtemp.fvalue[3]])^0.333
                val.antecedent.b <- 1 - val.antecedent.temp
              }
              else val.antecedent.b <- 1 - MF[k, Mtemp.fvalue[2]]
            }
            else {
              if (Mtemp.fvalue[1] == "extremely") 
                val.antecedent.b <- (MF[k, Mtemp.fvalue[2]])^3
              else if (Mtemp.fvalue[1] == "very") 
                val.antecedent.b <- (MF[k, Mtemp.fvalue[2]])^2
              else if (Mtemp.fvalue[1] == "somewhat") 
                val.antecedent.b <- (MF[k, Mtemp.fvalue[2]])^0.5
              else if (Mtemp.fvalue[1] == "slightly") 
                val.antecedent.b <- (MF[k, Mtemp.fvalue[2]])^0.333
              else if (Mtemp.fvalue[1] == "not") 
                val.antecedent.b <- 1 - (MF[k, Mtemp.fvalue[2]])
            }
          }
          else {
            val.antecedent.b <- MF[k, temp[j + 2]]
          }
        }
        if ((temp[j + 1] == "1") || (temp[j + 1] == 
                                       "and")) {
          if (type.tnorm == 1 || type.tnorm == "MIN") {
            if (val.antecedent.b < val.antecedent) 
              val.antecedent <- val.antecedent.b
          }
          else if (type.tnorm == 2 || type.tnorm == 
                     "HAMACHER") {
            val.antecedent <- (val.antecedent * val.antecedent.b)/(val.antecedent + 
                                                                     val.antecedent.b - val.antecedent * val.antecedent.b)
          }
          else if (type.tnorm == 3 || type.tnorm == 
                     "YAGER") {
            temp.val.ante <- (1 - val.antecedent) + 
              (1 - val.antecedent.b)
            if (temp.val.ante <= 1) {
              val.antecedent <- temp.val.ante
            }
            else {
              val.antecedent <- 1
            }
          }
          else if (type.tnorm == 4 || type.tnorm == 
                     "PRODUCT") {
            val.antecedent <- val.antecedent * val.antecedent.b
          }
          else if (type.tnorm == 5 || type.tnorm == 
                     "BOUNDED") {
            temp.val.ante <- (val.antecedent * val.antecedent.b - 
                                1)
            if (temp.val.ante > 0) {
              val.antecedent <- temp.val.ante
            }
            else {
              val.antecedent <- 0
            }
          }
        }
        else if ((temp[j + 1] == "2") || (temp[j + 1] == 
                                            "or")) {
          if (type.snorm == 1 || type.snorm == "MAX") {
            if (val.antecedent.b > val.antecedent) 
              val.antecedent <- val.antecedent.b
          }
          else if (type.snorm == 2 || type.snorm == 
                     "HAMACHER") {
            val.antecedent <- (val.antecedent + val.antecedent.b - 
                                 2 * val.antecedent * val.antecedent.b)/(1 - 
                                                                           val.antecedent * val.antecedent.b)
          }
          else if (type.snorm == 3 || type.snorm == 
                     "YAGER") {
            temp.val.ante <- (val.antecedent + val.antecedent.b)
            if (temp.val.ante <= 1) {
              val.antecedent <- temp.val.ante
            }
            else {
              val.antecedent <- 1
            }
          }
          else if (type.snorm == 4 || type.snorm == 
                     "SUM") {
            val.antecedent <- (val.antecedent + val.antecedent.b - 
                                 val.antecedent * val.antecedent.b)
          }
          else if (type.snorm == 5 || type.snorm == 
                     "BOUNDED") {
            temp.val.ante <- (val.antecedent + val.antecedent.b)
            if (temp.val.ante <= 1) {
              val.antecedent <- temp.val.ante
            }
            else {
              val.antecedent <- 1
            }
          }
        }
        j <- j + 2
        if (j == loc - 1) 
          break
      }
      miu.rule[k, i] <- c(val.antecedent)
    }
  }
  return(miu.rule)
}

ANFIS.update <- function (data.train, def, rule.data.num, miu.rule, func.tsk, 
                          varinp.mf, step.size = 0.01) 
{
  num.varoutput <- 1
  data <- data.train
  alpha <- step.size
  data.m <- matrix(data[1, 1:(ncol(data.train) - num.varoutput)], 
                   ncol = 1)
  miu.rule.t <- as.matrix(miu.rule)
  func.tsk.var <- func.tsk[, 1:ncol(func.tsk) - 1]
  func.tsk.cont <- func.tsk[, ncol(func.tsk), drop = FALSE]
  indxx <- which.max(miu.rule.t)
  gal <- (def - data.train[1, ncol(data.train)])
  delta.out <- 0
  sum.miu <- sum(miu.rule.t, na.rm = TRUE)
  function.tsk <- function(i, j, func.tsk.var) {
    if (sum.miu != 0) {
      func.tsk.var[i, j] <<- func.tsk.var[i, j] - alpha * 
        gal * (miu.rule[i]/sum.miu) * data.train[1, 
                                                 j]
    }
    else {
      func.tsk.var[i, j] <<- func.tsk.var[i, j] - alpha * 
        gal * (miu.rule[i]) * data.train[1, j]
    }
  }
  vec.func.tsk <- Vectorize(function.tsk, vectorize.args = list("i", 
                                                                "j"))
  outer(1:nrow(func.tsk.var), 1:ncol(func.tsk.var), vec.func.tsk, 
        func.tsk.var)
  for (mm in 1:nrow(func.tsk.cont)) {
    if (sum.miu != 0) {
      func.tsk.cont[mm, 1] <- func.tsk.cont[mm, 1] - alpha * 
        gal * (miu.rule[mm]/sum.miu)
    }
    else {
      func.tsk.cont[mm, 1] <- func.tsk.cont[mm, 1] - alpha * 
        gal * miu.rule[mm]
    }
  }
  func.tsk.new <- cbind(func.tsk.var, func.tsk.cont)
  num.varinp <- ncol(data.train) - num.varoutput
  for (ii in 1:ncol(miu.rule.t)) {
    func.tsk.var.indxx <- func.tsk.new[ii, 1:ncol(func.tsk.new) - 
                                         1]
    func.tsk.cont.indxx <- func.tsk.new[ii, ncol(func.tsk.new)]
    ff.indxx <- func.tsk.var.indxx %*% data.m + func.tsk.cont.indxx
    tau.miu <- ff.indxx * miu.rule.t[1, ii]
    div <- sum(miu.rule.t)
    if (div == 0) 
      div <- 1e-04
    func.val <- tau.miu/div
    num.varinput <- ncol(data.train) - num.varoutput
    for (j in 1:num.varinput) {
      varinp.mf[2, rule.data.num[ii, j]] <- varinp.mf[2, 
                                                      rule.data.num[ii, j]] - step.size * gal * (func.val - 
                                                                                                   data.train[1, ncol(data.train)]) * miu.rule.t[1, 
                                                                                                                                                 ii]/div * (data.train[1, j] - varinp.mf[2, rule.data.num[ii, 
                                                                                                                                                                                                          j]])/(varinp.mf[3, rule.data.num[ii, j]])^2
      varinp.mf[3, rule.data.num[ii, j]] <- varinp.mf[3, 
                                                      rule.data.num[ii, j]] - step.size * gal * (func.val - 
                                                                                                   data.train[1, ncol(data.train)]) * miu.rule.t[1, 
                                                                                                                                                 ii]/div * (data.train[1, j] - varinp.mf[2, rule.data.num[ii, 
                                                                                                                                                                                                          j]])^2/(varinp.mf[3, rule.data.num[ii, j]])^3
      if (varinp.mf[2, rule.data.num[ii, j]] < 0) 
        varinp.mf[2, rule.data.num[ii, j]] <- 0
      if (varinp.mf[3, rule.data.num[ii, j]] < 0) 
        varinp.mf[3, rule.data.num[ii, j]] <- 0.001
      if (varinp.mf[2, rule.data.num[ii, j]] > 1) 
        varinp.mf[2, rule.data.num[ii, j]] <- 1
      if (varinp.mf[3, rule.data.num[ii, j]] > 1) 
        varinp.mf[3, rule.data.num[ii, j]] <- 1
    }
  }
  param.new <- list(func.tsk.new = func.tsk.new, varinp.mf = varinp.mf)
  return(param.new)
}

convert.params <- function (mod) 
{
  type.tnorm.str <- c("MIN", "HAMACHER", "YAGER", "PRODUCT", 
                      "BOUNDED")
  type.snorm.str <- c("MAX", "HAMACHER", "YAGER", "SUM", "BOUNDED")
  type.defuz.str <- c("WAM", "FIRST.MAX", "LAST.MAX", "MEAN.MAX", 
                      "COG")
  type.mf.str <- c("TRIANGLE", "TRAPEZOID", "GAUSSIAN", "SIGMOID", 
                   "BELL")
  type.model.str <- c("MAMDANI", "TSK", "FRBCS", "CLUSTERING", 
                      "APPROXIMATE")
  if (any(mod$type.model == c("MAMDANI", "TSK", "FRBCS", "APPROXIMATE"))) {
    if (is.null(mod$type.tnorm)) {
      warning("type.tnorm is not defined, it will be assigned to 'MIN' ")
      mod$type.tnorm <- "MIN"
    }
    else if (class(mod$type.tnorm) == "numeric") 
      mod$type.tnorm <- type.tnorm.str[mod$type.tnorm]
    else if (class(mod$type.tnorm) == "character") 
      mod$type.tnorm <- toupper(mod$type.tnorm)
    if (is.null(mod$type.snorm)) {
      warning("type.snorm is not defined, it will be assigned to 'MAX' ")
      mod$type.snorm <- "MAX"
    }
    else if (class(mod$type.snorm) == "numeric") 
      mod$type.snorm <- type.snorm.str[mod$type.snorm]
    else if (class(mod$type.snorm) == "character") 
      mod$type.snorm <- toupper(mod$type.snorm)
    mod$type.implication.func <- toupper(mod$type.implication.func)
  }
  if (!is.null(mod$type.defuz) && class(mod$type.defuz) == 
        "numeric") 
    mod$type.defuz <- type.defuz.str[mod$type.defuz]
  else if (!is.null(mod$type.defuz) && class(mod$type.defuz) == 
             "character") 
    mod$type.defuz <- toupper(mod$type.defuz)
  if (!is.null(mod$type.mf) && class(mod$type.mf) == "numeric") 
    mod$type.mf <- type.mf.str[mod$type.mf]
  else if (!is.null(mod$type.mf) && class(mod$type.mf) == 
             "character") 
    mod$type.mf <- toupper(mod$type.mf)
  if (is.null(mod$type.model)) 
    stop("please define type of model")
  else if (class(mod$type.model) == "numeric") 
    mod$type.model <- type.model.str[mod$type.model]
  else if (class(mod$type.model) == "character") 
    mod$type.model <- toupper(mod$type.model)
  if (mod$type.model == "MAMDANI" && is.null(mod$type.defuz)) {
    warning("type.defuz is not defined, it will be assigned to 'WAM' ")
    mod$type.defuz <- "WAM"
  }
  return(mod)
}

rep.rule <- function (object) 
{
  if (!inherits(object, "frbs")) 
    stop("not a legitimate frbs model")
  colnames.var <- object$colnames.var
  if (object$type.model == "TSK") 
    object$num.labels = cbind(object$num.labels, object$num.labels[1, 
                                                                   1])
  if (any(object$method.type == c("WM", "HYFIS", "GFS.THRIFT", 
                                  "ANFIS", "FIR.DM", "FS.HGD", "FRBCS.W", "FRBCS.CHI", 
                                  "GFS.GCCL", "FH.GBML", "SLAVE", "GFS.LT.RS", "MANUAL"))) {
    num.varinput <- length(colnames.var) - 1
    num.labels <- object$num.labels
    if (!is.null(object$varinp.mf)) {
      names.fTerms.inpvar <- colnames(object$varinp.mf)
    }
    else {
      names.fTerms.inpvar = NULL
    }
    if (!is.null(object$varout.mf)) {
      names.fTerms.outvar <- colnames(object$varout.mf)
    }
    else {
      names.fTerms.outvar = NULL
    }
    if (!is.null(object$rule.data.num)) {
      res <- generate.rule(object$rule.data.num, num.labels, 
                           names.fTerms.inpvar = names.fTerms.inpvar, names.fTerms.outvar = names.fTerms.outvar)
      rule <- res$rule
    }
    else {
      rule <- object$rule
    }
    new.rule <- matrix(nrow = nrow(rule), ncol = (ncol(rule) + 
                                                    2 * (num.varinput + 1)))
    k <- 1
    for (j in 1:num.varinput) {
      new.rule[, k] <- colnames.var[j]
      new.rule[, k + 1] <- "is"
      new.rule[, k + 2] <- rule[, 2 * j - 1]
      if (j < num.varinput) {
        new.rule[, k + 3] <- rule[, 2 * j]
      }
      else {
        new.rule[, k + 3] <- "THEN"
      }
      k <- k + 4
    }
    new.rule[, (ncol(new.rule) - 2)] <- colnames.var[num.varinput + 
                                                       1]
    new.rule[, (ncol(new.rule) - 1)] <- "is"
    new.rule[, ncol(new.rule)] <- rule[, ncol(rule)]
    if (object$type.model == "TSK") {
      rule <- new.rule[, 1:(ncol(new.rule) - 3), drop = FALSE]
    }
    else {
      rule <- new.rule
    }
    rule <- cbind("IF", rule)
  }
  else stop("It is not supported to create rule representation")
  return(rule)
}

plotMF <- function(object) {
  
  ## define whether object as a list or frbs object
  if (inherits(object, "frbs")) { 
    method.type <- object$method.type
  }
  else if (class(object) == "list") {
    method.type <- c("MANUAL")
  } else {
    stop("please input a frbs object or a list of parameters")
  }
  
  ## check whether sort of method can plot or not and get some parameters
  if (any(method.type == c("WM", "HYFIS", "ANFIS", "FS.HGD", "GFS.THRIFT", "FIR.DM", "FRBCS.W", 
                           "FRBCS.CHI", "GFS.GCCL", "FH.GBML", "SLAVE", "GFS.LT.RS", "MANUAL"))){
    if (any(method.type == c("WM", "HYFIS", "GFS.THRIFT"))){
      range.data <- matrix(nrow = 2, ncol = ncol(object$num.labels))
      range.data[1, ] <- 0
      range.data[2, ] <- 1
      num.varinput <- ncol(object$num.labels)
      num.fvalinput <- object$num.labels 
      ## get parameters of MF for all variables
      var.mf <- cbind(object$varinp.mf, object$varout.mf) 
      
    }
    else if (method.type == "GFS.LT.RS"){
      range.data <- matrix(nrow = 2, ncol = ncol(object$num.labels))
      range.data[1, ] <- 0
      range.data[2, ] <- object$num.labels[1,1] - 1  
      num.varinput <- ncol(object$num.labels)
      num.fvalinput <- object$num.labels 
      ## get parameters of MF for all variables
      var.mf <- cbind(object$varinp.mf, object$varout.mf) 
      if (object$mode.tuning == "LOCAL")
        warning("The plot shows a original shape of membership functions only")
    }
    else if (any(method.type == c("ANFIS", "FS.HGD", "FIR.DM", "FRBCS.W", "FRBCS.CHI", "GFS.GCCL", "FH.GBML", "SLAVE"))){
      var.mf <- object$varinp.mf
      ## for TSK and FRBCS model, plotting can be done only for input variables  	  
      if (any(method.type == c("ANFIS", "FIR.DM", "FS.HGD"))){
        range.data <- matrix(nrow = 2, ncol = (ncol(object$range.data.ori) - 1))
        range.data[1, ] <- 0
        range.data[2, ] <- 1
        num.fvalinput <- object$num.labels
        num.varinput <- ncol(object$range.data.ori) - 1
      } else { 
        range.data <- matrix(nrow = 2, ncol = ncol(object$range.data.ori))
        range.data[1, ] <- 0
        range.data[2, ] <- 1
        num.fvalinput <- object$num.labels[, -ncol(object$num.labels), drop = FALSE]  
        num.varinput <- ncol(object$range.data.ori)
      }		  
    } 
    ## get parameters when plotting from manual condition
    else {
      if (is.null(object$num.labels) || is.null(object$range.data.ori)) {
        stop("please input the matrix of num.labels and range.data")
      } else {
        if (!is.null(object$type.model) && object$type.model == "TSK") num.varinput <- ncol(object$range.data.ori) - 1
        else  num.varinput <- ncol(object$range.data.ori)
        if (!is.null(object$varout.mf)) var.mf <- cbind(object$varinp.mf, object$varout.mf)
        else if (!is.null(object$var.mf)) var.mf <- object$var.mf
        else var.mf <- object$varinp.mf
        range.data <- object$range.data.ori
        num.fvalinput <- object$num.labels
        if (!is.null(object$names.variables)){ 
          names.variables <- object$names.variables
        } else {
          names.variables <- paste("var", seq(1, ncol(object$range.data.ori)), sep = ".")
        }
      }
    }
    
    ##get number of column of var.mf
    col.var.mf <- ncol(var.mf)
    
    ## counter is used to make continue index j
    counter <- 1
    
    ## k is used to index number linguistic value on each variable on varinput.
    k <- 1
    
    ## make row plot 
    op <- par(mfrow = c(ceiling(num.varinput/2), 2))
    
    ## loop as many as number of input variable
    for (i in 1 : num.varinput){
      j <- counter
      
      ## Initialize plot
      MF.degree <- function(x){
        y <- x - x
        return (y)
      }
      if (!is.null(object$colnames.var)) { 
        names <- object$colnames.var[i]
      } else {
        names <- names.variables[i]
      }
      
      curve(MF.degree, range.data[1, i], range.data[2, i], ylim = c(0, 1), col = "white")
      title(main = names)
      
      ## loop as many as number of column of var.mf
      for (j in counter : col.var.mf){
        
        ## make boundary point
        oo <- range.data[1, i]
        aa <- var.mf[2, j]
        bb <- var.mf[3, j]
        cc <- var.mf[4, j]
        dd <- var.mf[5, j]
        mm <- range.data[2, i]
        
        ##condition for triangular type
        if (var.mf[1, j] == 1){				
          
          ## make a function for plotting, args (x) is sequence data
          f.y1 <- function(x){
            
            ## range of input data
            p.0 <- x[x >= oo & x <= aa]
            p.1 <- x[x > aa & x <= bb]
            p.2 <- x[x > bb & x <= cc]
            p.3 <- x[x > cc & x <= mm]
            
            ## build functions 
            y0 <- (p.0 - p.0)
            y1 <- (p.1 - aa) / (bb - aa)			
            y2 <- (p.2 - cc) / (bb - cc)			
            y4 <- (p.3 - p.3)
            y <- c(y0, y1, y2, y4)
            
            return (y)
          }
          
          curve(f.y1, oo, mm, add = TRUE, col = "violet")
        }
        ## condition for trapezoid in left side
        else if (var.mf[1, j] == 2){
          
          f.y2 <- function(x){
            
            p.1 <- x[x <= bb]
            p.2 <- x[x > bb & x <= cc]
            p.3 <- x[x > cc & x <= mm]
            
            y1 <- (p.1 - p.1 + 1)			
            y2 <- (p.2 - cc) / (bb - cc)			
            y3 <- (p.3 - p.3)
            
            y <- c(y1, y2, y3)
            
            return (y)
          }
          
          curve(f.y2, oo, mm, add=TRUE, col = "blue")
          
        }
        ## condition for trapezoid in right side
        else if (var.mf[1, j] == 3){
          f.y3 <- function(x){
            
            p.0 <- x[x >= oo & x <= aa]
            p.1 <- x[x > aa & x <= bb]
            p.2 <- x[x > bb & x <= mm]
            
            y0 <- (p.0 - p.0)
            y1 <- (p.1 - aa) / (bb - aa)
            y2 <- (p.2 - p.2 + 1)			
            y <- c(y0, y1, y2)
            
            return (y)
          }
          
          curve(f.y3, oo, mm, add = TRUE, col = "green")
          
        }
        ## condition for trapezoid in the middle
        else if (var.mf[1, j] == 4){
          
          f.y4 <- function(x){
            
            p.0 <- x[x >= oo & x <= aa]
            p.1 <- x[x > aa & x <= bb]
            p.2 <- x[x > bb & x <= cc]
            p.3 <- x[x > cc & x <= dd]
            p.4 <- x[x > dd & x <= mm]
            
            y0 <- (p.0 - p.0)
            y1 <- (p.1 - aa) / (bb - aa)
            y2 <- (p.2 - p.2 + 1)			
            y3 <- (p.3 - dd) / (cc - dd)
            y4 <- (p.4 - p.4)
            
            y <- c(y0, y1, y2, y3, y4)
            
            return (y)
          }
          
          curve(f.y4, oo, mm, add = TRUE, col = "red")
          
        }
        ## condition for gaussian shape
        else if (var.mf[1, j] == 5){
          
          f.y5 <- function(x){
            y <- exp(- 0.5 * (x - aa) ^ 2 / bb ^ 2)
            return (y)
          }
          ## plot the functions
          curve(f.y5, oo, mm, add = TRUE, col = "gray")
        }
        ## condition for sigmoid/logistic
        else if (var.mf[1, j] == 6){
          
          f.y6 <- function(x){
            y <- 1 / (1 + exp(- aa * (x - bb)))
            return (y)
          }
          #plot the functions
          curve(f.y6, oo, mm, add = TRUE, col = "black")
        }
        ## condition for generalized bell
        else if (var.mf[1, j] == 7){
          
          f.y7 <- function(x){
            y <- 1 / (1 + abs((x - cc)/aa) ^ (2 * bb))
            return (y)
          }
          
          ## plot the functions
          curve(f.y7, oo, mm, add = TRUE, col = "black")
        }
        
        counter <- j + 1 
        k <- k + 1
        if (k > num.fvalinput[1, i]){
          k <- 1
          break
        }
      }
    }
    
    par(op)
  } else {
    print("The plot is not supported by the used method, please have a look the documentation")
  }
}

summary.frbs <- function(object, ...){
  
  if(!inherits(object, "frbs")) stop("not a legitimate frbs model")
  cat("The name of model: ", object$name, "\n")
  cat("Model was trained using: ", object$method.type, "\n") 
  cat("The names of attributes: ", object$colnames.var, "\n")
  
  ## display for range.data
  if (any(object$method.type == c("FRBCS.W", "FRBCS.CHI", "GFS.GCCL", "FH.GBML", "SLAVE"))){
    colnames(object$range.data.ori) <- object$colnames.var[-length(object$colnames.var)]
    rownames(object$range.data.ori) <- c("min", "max")
    cat("The interval of input data: ", "\n")
    print(object$range.data.ori)
  }
  else if (object$method.type == "MANUAL") {
    if (object$type.model == "FRBCS"){
      colnames(object$range.data.ori) <- object$colnames.var[-length(object$colnames.var)]
      rownames(object$range.data.ori) <- c("min", "max")
      cat("The interval of input data: ", "\n")
      print(object$range.data.ori)
    }
    else if (any(object$type.model == c("MAMDANI", "TSK"))){
      range.data.ori <- object$range.data.ori
      colnames(range.data.ori) <- object$colnames.var
      rownames(range.data.ori) <- c("min", "max")
      cat("The interval of training data: ", "\n")
      print(range.data.ori)
    } else {
      stop("The model is not supported")
    }
  } else {
    range.data.ori <- object$range.data.ori
    colnames(range.data.ori) <- object$colnames.var
    rownames(range.data.ori) <- c("min", "max")
    cat("The interval of training data: ", "\n")
    print(range.data.ori)
  }
  
  ## display for schema of Inference	
  if (any(object$method.type == c("WM", "HYFIS", "ANFIS", "FIR.DM", "FS.HGD", "GFS.THRIFT", "FRBCS.W", "FRBCS.CHI", 
                                  "SLAVE", "GFS.GCCL", "FH.GBML", "GFS.FR.MOGUL", "GFS.LT.RS", "MANUAL"))){
    cat("Type of FRBS model:", "\n")
    print(object$type.model)
    cat("Type of membership functions:", "\n")
    print(object$type.mf)
    cat("Type of t-norm method:", "\n")
    if (object$type.tnorm == 1 || object$type.tnorm == "MIN") print("Standard t-norm (min)")
    else if (object$type.tnorm == 2 || object$type.tnorm == "HAMACHER") print("Hamacher product")
    else if (object$type.tnorm == 3 || object$type.tnorm == "YAGER") print("Yager class (with tao = 1)")
    else if (object$type.tnorm == 4 || object$type.tnorm == "PRODUCT") print("Product")
    else print("Bounded product")
    cat("Type of s-norm method:", "\n")
    if (object$type.snorm == 1 || object$type.snorm == "MAX") print("Standard s-norm")
    else if (object$type.snorm == 2 || object$type.snorm == "HAMACHER") print("Hamacher sum")
    else if (object$type.snorm == 3 || object$type.snorm == "YAGER") print("Yager class (with tao = 1)")
    else if (object$type.snorm == 4 || object$type.snorm == "SUM") print("Sum")
    else print("Bounded sum")
    
    if (any(object$method.type == c("WM", "HYFIS", "GFS.THRIFT", "GFS.LT.RS"))){
      cat("Type of defuzzification technique:", "\n")
      if (object$type.defuz == 1 || object$type.defuz == "WAM") print("Weighted average method")
      else if (object$type.defuz == 2 || object$type.defuz == "FIRST.MAX") print("first of maxima")
      else if (object$type.defuz == 3 || object$type.defuz == "LAST.MAX") print("last of maxima")
      else if (object$type.defuz == 4 || object$type.defuz == "MEAN.MAX") print("mean of maxima")
      else print("modified COG")
    }
    cat("Type of implication function:", "\n")
    print(object$type.implication.func)
  }
  
  ## display for parameters of membership function and rule
  if (any(object$method.type == c("WM", "HYFIS", "GFS.THRIFT", "ANFIS", "FIR.DM", "FS.HGD", "FRBCS.W", 
                                  "FRBCS.CHI", "SLAVE", "GFS.GCCL", "FH.GBML", "GFS.LT.RS", "MANUAL"))){
    num.labels <- object$num.labels
    rule <- as.data.frame(object$rule)
    cat("The names of linguistic terms on the input variables: ", "\n")
    print(colnames(object$varinp.mf))	
    cat("The parameter values of membership function on the input variable (normalized): ", "\n")
    print(object$varinp.mf)
    
    if (any(object$method.type == c("ANFIS", "FIR.DM", "FS.HGD"))){
      colnames(num.labels) <- object$colnames.var[-length(object$colnames.var)]
      cat("The number of linguistic terms on input variables", "\n")
      print(num.labels)
      cat("The fuzzy IF-THEN rules: ", "\n")
      print(rule)
      if (ncol(object$func.tsk) > 1){	
        seq.deg <- seq(from = 1, to = (ncol(object$func.tsk) - 1), by = 1)
        coef.var <- paste("var", seq.deg, sep = ".")
        names.func <- c(coef.var, "const")	
      } else {
        names.func <- c("const")	
      }
      colnames(object$func.tsk) <- names.func
      cat("The linear equations on consequent parts of fuzzy IF-THEN rules: ", "\n")
      print(object$func.tsk)		
    }
    
    else if (any(object$method.type == c("WM", "HYFIS", "GFS.THRIFT", "GFS.LT.RS"))){
      cat("The names of linguistic terms on the output variable: ", "\n")
      print(colnames(object$varout.mf))
      cat("The parameter values of membership function on the output variable (normalized): ", "\n")
      print(object$varout.mf)		
      colnames(num.labels) <- object$colnames.var
      cat("The number of linguistic terms on each variables", "\n")
      print(num.labels)
      cat("The fuzzy IF-THEN rules: ", "\n")
      print(rule)
    }
    
  }
  
  invisible(object)	
}

predict.frbs <- function(object, newdata, ...) {
  options(verbose=FALSE)
  
  ## Initial checking
  init.check <- validate.params(object, newdata)
  object <- init.check$object
  newdata <- init.check$newdata
  
  ## get the type of method
  m.type <- object$method.type
  
  ## 1. WM, ANFIS, HYFIS, FIR.DM, FS.HGD approach
  if (any(m.type == c("WM", "ANFIS", "HYFIS", "FIR.DM", "FS.HGD")) 
      || any(object$type.model == c("MAMDANI", "TSK"))) {
    range.data.ori <- object$range.data.ori
    range.input.ori <- range.data.ori[, 1:(ncol(range.data.ori) - 1)]
    range.output.ori <- range.data.ori[, ncol(range.data.ori), drop = FALSE]
    data.tst.norm <- norm.data(newdata, range.input.ori, min.scale = 0, max.scale = 1)
    res.comp <- frbs.eng(object, data.tst.norm)
    res.denorm <- denorm.data(res.comp$predicted.val, range.output.ori, min.scale = 0, max.scale = 1)
    res <- res.denorm
  }
  
  return(res)
}

validate.params <- function(object, newdata){
  if(!inherits(object, "frbs")) stop("not a legitimate frbs model")
  
  ## in case, new data are not a matrix  
  if (class(newdata) != "matrix"){
    if (class(newdata) == "numeric")
      newdata <- matrix(newdata, nrow = 1)
    else
      newdata <- as.matrix(newdata)
  }  
  
  ## Check range of new data
  for (i in 1 : ncol(newdata)){
    if (min(newdata[, i]) < object$range.data.ori[1, i])
      warning("There are your newdata which are out of the specified range")
    if (max(newdata[, i]) > object$range.data.ori[2, i])
      warning("There are your newdata which are out of the specified range")
  }
  
  ## Check values of inference parameters
  object <- convert.params(object)
  
  ## Check num.labels
  if (object$type.model == c("MAMDANI")){
    if (ncol(object$num.labels) != ncol(object$range.data.ori)) {
      stop("Please check your num.labels parameters")						
    }
  }
  else if (object$type.model == c("TSK")) {
    if (ncol(object$num.labels) != (ncol(object$range.data.ori) - 1)) {
      stop("Please check your num.labels parameters")						
    }
  }
  
  return(list(object = object, newdata = newdata))
}

norm.data <- function(dt.ori, range.data, min.scale = 0, max.scale = 1){
  row.data <- nrow(dt.ori)
  col.data <- ncol(dt.ori)
  data.norm <- matrix(nrow = row.data, ncol=col.data)
  
  # normalize all data on each column 
  for (j in 1:col.data){
    min.data <- range.data[1, j]
    max.data <- range.data[2, j]
    
    for (i in 1:row.data){
      data.norm[i, j] <- min.scale + (dt.ori[i, j] - min.data) * (max.scale - min.scale) / (max.data - min.data)
    }
  }
  
  return(data.norm)
}

denorm.data <- function(dt.norm, range.data, min.scale = 0, max.scale = 1){
  row.data <- nrow(dt.norm)
  col.data <- ncol(dt.norm)
  data.denorm <- matrix(nrow = row.data, ncol=col.data)
  
  # denormalize all data on each column 
  for (j in 1:col.data){
    min.data <- range.data[1, j]
    max.data <- range.data[2, j]
    
    for (i in 1:row.data){
      data.denorm[i, j] <- min.data + ((dt.norm[i, j] - min.scale)*(max.data - min.data))/ (max.scale - min.scale)
    }
  }
  
  return(data.denorm)
}


########edit
rep.rule.mamd <- function (object) {
  ## get names of variables
  colnames.var <- object$colnames.var
  
  ## make description on rule
    ## get number of input variables and number of linguistic terms	
    num.varinput <- length(colnames.var) - 1
    num.labels <- object$num.labels
    
    ## get names of linguistic terms
    if (!is.null(object$varinp.mf)){
      names.fTerms.inpvar <- colnames(object$varinp.mf)
    } else {names.fTerms.inpvar = NULL	}
    if (!is.null(object$varout.mf)){
      names.fTerms.outvar <- colnames(object$varout.mf)
    } else {names.fTerms.outvar = NULL }
    
    ##	construct rule from rule.data.num (or rule in matrix format).
    if (!is.null(object$rule.data.num)){
      res <- generate.rule(object$rule.data.num, num.labels, 
                           names.fTerms.inpvar = names.fTerms.inpvar, names.fTerms.outvar = names.fTerms.outvar)
      rule <- res$rule
    } else {
      rule <- object$rule
    }
    
    ## construct rule in IF ... THEN ... format
    new.rule <- matrix(nrow = nrow(rule), ncol = (ncol(rule) + 2 * (num.varinput + 1)))
    k <- 1
    
    for (j in 1 : num.varinput){				
      new.rule[, k] <- colnames.var[j] 
      new.rule[, k + 1] <- "is"
      new.rule[, k + 2] <- rule[, 2 * j - 1]
      
      if (j < num.varinput){
        ## new.rule[, k + 3] <- "and"
        ## A bug: when the boolean operator "or" (solved)
        new.rule[, k + 3] <- rule[, 2 * j]
      } else {
        new.rule[, k + 3] <- "THEN"
      }
      k <- k + 4
    }
    new.rule[, (ncol(new.rule) - 2)] <- colnames.var[num.varinput + 1] 
    new.rule[, (ncol(new.rule) - 1)] <- "is"
    new.rule[, ncol(new.rule)] <- rule[, ncol(rule)]
      rule <- new.rule
    rule <- cbind("IF", rule)
  return (rule)
}