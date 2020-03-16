# stats:::qr.lm()
qr.lm = function(x, ...) {
  if (is.null(r <- x$qr)) 
    stop("lm object does not have a proper 'qr' component.\n Contact app owner.")
  r
}

summary.glm.project = function (object, depvar, expvar, inter = NULL, dispersion = NULL, correlation = FALSE, symbolic.cor = FALSE, ...) 
{
  digits = max(3L, getOption("digits") - 3L)
  m = object
  fm = object$finalModel
  est.disp <- FALSE
  df.r <- fm$df.residual
  
  if (is.null(dispersion)) 
    dispersion <- if (fm$family$family %in% c("poisson", 
                                                  "binomial")) 
      1
  else if (df.r > 0) {
    est.disp <- TRUE
    if (any(fm$weights == 0)) 
      warning("observations with zero weight not used for calculating dispersion")
    sum((fm$weights * fm$residuals^2)[fm$weights > 
                                                0])/df.r
  }
  else {
    est.disp <- TRUE
    NaN
  }
  
  aliased <- is.na(coef(fm))
  p <- fm$rank
  if (p > 0) {
    p1 <- 1L:p
    Qr <- qr.lm(fm)
    coef.p <- fm$coefficients[Qr$pivot[p1]]
    covmat.unscaled <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    dimnames(covmat.unscaled) <- list(names(coef.p), names(coef.p))
    covmat <- dispersion * covmat.unscaled
    var.cf <- diag(covmat)
    s.err <- sqrt(var.cf)
    tvalue <- coef.p/s.err
    dn <- c("Estimate", "Std. Error")
    if (!est.disp) {
      pvalue <- 2 * pnorm(-abs(tvalue))
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "z value", "Pr(>|z|)"))
    }
    else if (df.r > 0) {
      pvalue <- 2 * pt(-abs(tvalue), df.r)
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "t value", "Pr(>|t|)"))
    }
    else {
      coef.table <- cbind(coef.p, NaN, NaN, NaN)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "t value", "Pr(>|t|)"))
    }
    df.f <- NCOL(Qr$qr)
  }
  else {
    coef.table <- matrix(, 0L, 4L)
    dimnames(coef.table) <- list(NULL, c("Estimate", 
                                         "Std. Error", "t value", "Pr(>|t|)"))
    covmat.unscaled <- covmat <- matrix(, 0L, 0L)
    df.f <- length(aliased)
  }
  keep <- match(c("call", "terms", "family", 
                  "deviance", "aic", "contrasts", "df.residual", 
                  "null.deviance", "df.null", "iter", 
                  "na.action"), names(fm), 0L)
  ans <- c(fm[keep], list(deviance.resid = residuals(fm, type = "deviance"), coefficients = coef.table, 
                          aliased = aliased, dispersion = dispersion, df = c(fm$rank, df.r, df.f), cov.unscaled = covmat.unscaled, cov.scaled = covmat))
  if (correlation && p > 0) {
    dd <- sqrt(diag(covmat.unscaled))
    ans$correlation <- covmat.unscaled/outer(dd, dd)
    ans$symbolic.cor <- symbolic.cor
  }

  class(ans) = "project"
  
  ### PRINTOUT ###
  
  # Header
  cat("Response variable    :", depvar, "\n")
  cat("Explanatory variables:", paste0(expvar, collapse = ", "), "\n")
  if(!is.null(inter)) {
    cat("Interactions         :", paste0(inter, collapse = ", "), "\n")
  }
  expl_var <- if (length(expvar) == 1) expvar else "x"
  cat(paste0("Null hyp.: the effect of ", expl_var, " on ", depvar, " is zero\n"))
  cat(paste0("Alt. hyp.: the effect of ", expl_var, " on ", depvar, " is not zero\n"))
  
  # Deviance
  cat("\nDeviance Residuals: \n")
  if (fm$df.residual > 5) {
    ans$deviance.resid <- setNames(quantile(ans$deviance.resid, na.rm = TRUE), c("Min", "1Q", "Median", "3Q", "Max"))
  }
  xx <- zapsmall(ans$deviance.resid, digits + 1L)
  print.default(xx, digits = 3, na.print = "", print.gap = 2L)
  
  # Coefficients
  if (length(ans$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    cat("\nCoefficients:\n")
    coefs <- ans$coefficients
    if (!is.null(aliased <- ans$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4L, dimnames = list(cn, 
                                                               colnames(coefs)))
      coefs[!aliased, ] <- fm$coefficients
    }
    printCoefmat(coefs, digits = 3L, signif.stars = TRUE, signif.legend = TRUE, na.print = "NA", ...)
  }
  
  r2 = (fm$null.deviance - fm$deviance) / fm$null.deviance
  loglik = fm$rank - fm$aic/2
  BIC = -2 * loglik + log(nobs(fm)) * (fm$df.null + 1 - fm$df.residual)
  
  # Metrics
  cat("\nPseudo R-squared: ", format(round(r2, 3)))
  cat("\nLog-likelihood: ", format(round(loglik, 3)))
  cat("\nBIC: ", format(round(BIC, 3)))
  cat("\nAIC: ", format(fm$aic, digits = max(4L, digits + 1L)), "\n\n", "Number of Fisher Scoring iterations: ", ans$iter, "\n", sep = "")
  
  class(ans) = "project"
  invisible(ans)
}



###################################################################################################################


### Modified summary.lm ###
summary.lm.project = function (object, depvar, expvar, inter = NULL, correlation = FALSE, symbolic.cor = FALSE, 
                               ...) 
{
  digits = max(3L, getOption("digits") - 3L)
  m = object
  fm <- object$finalModel
  p <- fm$rank
  df <- fm$df.residual
  rdf <- df[2L]
  
  if (p == 0) {
    r = fm$residuals
    n = length(r)
    w = fm$weights
    if (is.null(w)) {
      rss <- sum(r^2)
    }
    else {
      rss <- sum(w * r^2)
      r <- sqrt(w) * r
    }
    resvar <- rss/df
    
    ans = vector(mode = "list")
    class(ans) = "project"
    ans$aliased <- is.na(coef(fm))
    ans$residuals <- r
    ans$df <- c(0L, n, length(ans$aliased))
    ans$coefficients <- matrix(NA_real_, 0L, 4L, dimnames = list(NULL, 
                                                                 c("Estimate", "Std. Error", "t value", 
                                                                   "Pr(>|t|)")))
    ans$sigma <- sqrt(resvar)
    ans$r.squared <- ans$adj.r.squared <- 0
    ans$cov.unscaled <- matrix(NA_real_, 0L, 0L)
    if (correlation) 
      ans$correlation <- ans$cov.unscaled
    return(ans)
  }
  if (is.null(fm$terms)) 
    stop("invalid 'lm' object:  no 'terms' component")
  if (!inherits(fm, "lm")) 
    warning("calling summary.lm(<fake-lm-object>) ...")
  Qr <- qr.lm(fm)
  n <- NROW(Qr$qr)
  if (is.na(fm$df.residual) || n - p != fm$df.residual) 
    warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
  r <- fm$residuals
  f <- fm$fitted.values
  w <- fm$weights
  if (is.null(w)) {
    mss <- if (attr(fm$terms, "intercept")) 
      sum((f - mean(f))^2)
    else sum(f^2)
    rss <- sum(r^2)
  }
  else {
    mss <- if (attr(fm$terms, "intercept")) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    }
    else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }
  resvar <- rss/df
  if (is.finite(resvar) && resvar < (mean(f)^2 + var(c(f))) * 1e-30) 
    warning("essentially perfect fit: summary may be unreliable")
  p1 <- 1L:p
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- fm$coefficients[Qr$pivot[p1]]
  tval <- est/se
  ans = vector(mode = "list")
  ans$residuals <- r
  ans$coefficients <- cbind(Estimate = est, 'Std. Error' = se, 
                            't value' = tval, 'Pr(>|t|)' = 2 * pt(abs(tval), 
                                                                  df, lower.tail = FALSE))
  ans$aliased <- is.na(fm$coefficients)
  ans$sigma <- sqrt(resvar)
  ans$df <- c(p, df, NCOL(Qr$qr))
  if (p != attr(fm$terms, "intercept")) {
    df.int <- if (attr(fm$terms, "intercept")) 
      1L
    else 0L
    ans$r.squared <- mss/(mss + rss)
    ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - 
                                                       df.int)/df)
    ans$fstatistic <- c(value = (mss/(p - df.int))/resvar, 
                        numdf = p - df.int, dendf = df)
  }
  else ans$r.squared <- ans$adj.r.squared <- 0
  ans$cov.unscaled <- R
  dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1, 1)]
  if (correlation) {
    ans$correlation <- (R * resvar)/outer(se, se)
    dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
    ans$symbolic.cor <- symbolic.cor
  }
  if (!is.null(fm$na.action)) 
    ans$na.action <- fm$na.action
  
  class(ans) = "project"
  cat("Response variable    :", depvar, "\n")
  cat("Explanatory variables:", paste0(expvar, collapse = ", "), "\n")
  if(!is.null(inter)) {
    cat("Interactions         :", paste0(inter, collapse = ", "), "\n")
  }
  expl_var <- if (length(expvar) == 1) expvar else "x"
  cat(paste0("\nNull hyp.: the effect of ", expl_var, " on ", depvar, " is zero\n"))
  cat(paste0("Alt. hyp.: the effect of ", expl_var, " on ", depvar, " is not zero\n"))
  
  
  
  # print coefficients (from print.summary.lm)    
  if (length(ans$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    cat("\nCoefficients:\n")
    coefs <- ans$coefficients
    if (any(aliased <- ans$aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, colnames(coefs)))
      coefs[!aliased, ] <- fm$coefficients
    }
    printCoefmat(coefs, digits = 3L, signif.stars = TRUE, signif.legend = TRUE, na.print = "NA", ...)
  }
  
  # print residuals (from print.summary.lm) 
  cat(if (!is.null(ans$weights) && diff(range(ans$weights))) 
    "Weighted ", "\nResiduals:\n", sep = "")
  if (df > 5) {
    nam <- c("Min", "1Q", "Median", "3Q", 
             "Max")
    rq <- if (length(dim(r)) == 2) 
      structure(apply(t(r), 1, quantile), dimnames = list(nam, 
                                                          dimnames(r)[[2L]]))
    else {
      zz <- zapsmall(quantile(r), digits + 1L)
      structure(zz, names = nam)
    }
    print(rq, digits = digits, ...)
  }
  else if (df > 0) {
    print(r, digits = digits, ...)
  }
  else {
    cat("ALL", df, "residuals are 0: no residual degrees of freedom!")
    cat("\n")
  }
  
  # print metrics (from print.summary.lm)
  cat("\nResidual standard error:", format(signif(ans$sigma, digits)), "on", df, "degrees of freedom")
  cat("\n")
  if (nzchar(mess <- naprint(ans$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  if (!is.null(ans$fstatistic)) {
    cat("Multiple R-squared: ", formatC(ans$r.squared, digits = digits))
    cat("\nAdjusted R-squared: ", formatC(ans$adj.r.squared, digits = digits), "\nF-statistic:", 
                                  formatC(ans$fstatistic[1L], digits = digits), "on", ans$fstatistic[2L], "and", 
                                  ans$fstatistic[3L], "DF,  p-value:", format.pval(pf(ans$fstatistic[1L], 
                                  ans$fstatistic[2L], ans$fstatistic[3L], lower.tail = FALSE), digits = digits))
    cat("\n")
  }
  
  # RMSE
  mean(fm$residuals ^ 2, na.rm = TRUE) %>%
    sqrt(.) %>%
    round(3) %>%
    cat("Prediction error (RMSE): ", ., "\n")
  
  # print correlation (from print.summary.lm)
  correl <- ans$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1L) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(symnum(correl, abbr.colnames = NULL))
      }
      else {
        correl <- format(round(correl, 2), nsmall = 2, 
                         digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  
  print(anova.project(fm, depvar))
  
  invisible(ans)
}


###################################################################################################################

### Modified anova.lm ###
anova.project = function (object, depvar, ...) 
{
    if (length(list(object, ...)) > 1L) 
        return(anova.lmlist(object, ...))
    if (!inherits(object, "lm")) 
        warning("calling anova.lm(<fake-lm-object>) ...")
    w <- object$weights
    ssr <- sum(if (is.null(w)) object$residuals^2 else w * object$residuals^2)
    mss <- sum(if (is.null(w)) object$fitted.values^2 else w * 
        object$fitted.values^2)
    if (ssr < 1e-10 * mss) 
        warning("ANOVA F-tests on an essentially perfect fit are unreliable")
    dfr <- df.residual(object)
    p <- object$rank
    if (p > 0L) {
        p1 <- 1L:p
        comp <- object$effects[p1]
        asgn <- object$assign[stats:::qr.lm(object)$pivot][p1]
        nmeffects <- c("(Intercept)", attr(object$terms, 
            "term.labels"))
        tlabels <- nmeffects[1 + unique(asgn)]
        ss <- c(vapply(split(comp^2, asgn), sum, 1), ssr)
        df <- c(lengths(split(asgn, asgn)), dfr)
    }
    else {
        ss <- ssr
        df <- dfr
        tlabels <- character()
    }
    ms <- ss/df
    f <- ms/(ssr/dfr)
    P <- pf(f, df, dfr, lower.tail = FALSE)
    table <- data.frame(df, ss, ms, f, P)
    table[length(P), 4:5] <- NA
    dimnames(table) <- list(c(tlabels, "Residuals"), c("Df", 
        "Sum Sq", "Mean Sq", "F value", "Pr(>F)"))
    if (attr(object$terms, "intercept")) 
        table <- table[-1, ]
    structure(table, heading = c("Analysis of Variance Table\n"), 
        class = c("anova", "data.frame"))
}



###################################################################################################################




# get first-level interaction terms (from radiant.data package)
# https://github.com/radiant-rstats/radiant.data/blob/6f92860ab56dbcfa4deaf557fa0c76aa962fe541/R/radiant.R

iterms = function(vars, nway = 2, sep = ":") {
  sapply(2:min(as.integer(nway), length(vars)), function(x) apply(combn(vars, x), 2, paste, collapse = sep)) %>%
    unlist() %>%
    as.vector()
}

# get n-th terms (from radiant.data package)
qterms = function(vars, nway = 2) {
  sapply(2:as.integer(nway), function(x) glue("I({vars}^{x})")) %>%
    as.vector()
  
}



###################################################################################################################


# check if package is installed. If not, then install it (-> avoid duplicate installation of packages)
pkg = function(x) 
{
  if ( !require(x, character.only = TRUE) ) 
  {
    if ( !(x %in% installed.packages()) ) 
    {
      print( paste0("[+] ", x, " package not installed. Installing...") )
      install.packages(x) 
      library(x, character.only = TRUE)
    }
    else
    {
      if( !library(x) )
        print( paste0("[+] ", x, " package installed but could not be loaded") )
    }
  }
  else
    print( paste0("[+] ", x, " package loaded") )
}


###################################################################################################################


# print.train modified to print LDA model
print.train.project = function (x, printCall = FALSE, details = TRUE, selectCol = FALSE, showSD = TRUE, depvar = NULL, expvar = NULL, inter = NULL, ...) 
{
    if (!is.null(x$modelInfo$label)) {
        cat("Response variable    :", depvar, "\n")
        cat("Explanatory variables:", paste0(expvar, collapse = ", "), "\n")
        if(!is.null(inter))
          cat("Interactions         :", paste0(inter, collapse = ", "), "\n")
        cat("\n")
        
    }
    if (printCall) 
        caret:::printCall(x$call)
    if (!is.null(x$trainingData)) {
        chDim <- dim(x$trainingData)
        chDim[2] <- chDim[2] - 1
        if (x$modelType == "Classification") {
            lev <- levels(x)
            if (is.character(lev)) 
                chDim <- c(chDim, length(lev))
        }
        else lev <- NULL
        chDim <- format(chDim)
        cat(chDim[1], " samples", sep = "")
        if (!is.null(x$control$indexFinal)) 
            cat(",", length(x$control$indexFinal), "used for final model\n")
        else cat("\n")
        cat(chDim[2], " predictor", ifelse(chDim[2] > 1, 
            "s\n", "\n"), sep = "")
        if (is.character(lev)) {
            cat(chDim[3], "classes:", paste("'", 
                lev, "'", sep = "", collapse = ", "), 
                "\n")
        }
        cat("\n")
    }
    if (!is.null(x$preProc)) {
        pp_list(x$preProc$method)
    }
    else {
        if (inherits(x, "train.recipe")) {
            step_names <- function(x) gsub("^step_", "", 
                class(x)[1])
            steps_used <- unlist(lapply(x$recipe$steps, step_names))
            ppText <- paste("Recipe steps:", paste(steps_used, 
                collapse = ", "))
            cat(caret:::truncateText(ppText), "\n")
        }
        else cat("No pre-processing\n")
    }
    if (!is.null(x$control$index)) {
        resampleN <- unlist(lapply(x$control$index, length))
        numResamp <- length(resampleN)
        resampText <- caret:::resampName(x)
        cat("Resampling:", resampText, "\n")
        if (x$control$method != "none") {
            outLabel <- x$metric
            resampleN <- as.character(resampleN)
            if (numResamp > 5) 
                resampleN <- c(resampleN[1:6], "...")
            cat("Summary of sample sizes:", paste(resampleN, 
                collapse = ", "), "\n")
        }
    }
    if (!is.null(x$control$sampling)) {
        cat("Addtional sampling using ")
        cat(switch(x$control$sampling$name, down = "down-sampling", 
            up = "up-sampling", smote = "SMOTE", 
            rose = "ROSE", custom = "a custom function"))
        if (!is.null(x$preProc)) {
            if (x$control$sampling$first) 
                cat(" prior to pre-processing")
            else cat(" after to pre-processing")
        }
        cat("\n\n")
    }
    if (x$control$method != "none") {
        tuneAcc <- x$results
        tuneAcc <- tuneAcc[, names(tuneAcc) != "parameter"]
        cat("Resampling results")
        if (dim(tuneAcc)[1] > 1) 
            cat(" across tuning parameters")
        if (showSD) 
            cat(" (values below are 'mean (sd)')")
        cat(":\n\n")
        if (dim(tuneAcc)[1] > 1) {
            numParam <- length(x$bestTune)
            finalTune <- x$bestTune
            optValues <- paste(names(finalTune), "=", format(finalTune, 
                ...))
            optString <- paste0("The final ", ifelse(numParam > 
                1, "values", "value"), " used for the model ", 
                ifelse(numParam > 1, "were ", "was "), 
                stringFunc(optValues), ".")
            finalTune$Selected <- "*"
            if (any(names(tuneAcc) %in% "method")) 
                names(tuneAcc)[names(tuneAcc) %in% "method"] <- ".method"
            if (any(names(finalTune) %in% "method")) 
                names(finalTune)[names(finalTune) %in% "method"] <- ".method"
            tuneAcc <- merge(tuneAcc, finalTune, all.x = TRUE)
            if (any(names(tuneAcc) %in% ".method")) 
                names(tuneAcc)[names(tuneAcc) %in% ".method"] <- "method"
            tuneAcc$Selected[is.na(tuneAcc$Selected)] <- ""
        }
        else optString <- ""
        sdCols <- grep("SD$", colnames(tuneAcc))
        if (showSD) {
            sdCheck <- unlist(lapply(tuneAcc[, sdCols, drop = FALSE], 
                function(u) all(is.na(u))))
            if (any(sdCheck)) {
                rmCols <- names(sdCheck)[sdCheck]
                tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% rmCols)]
            }
        }
        else {
            if (length(sdCols) > 0) 
                tuneAcc <- tuneAcc[, -sdCols, drop = FALSE]
        }
        params <- names(x$bestTune)
        if (!all(params == "parameter")) {
            numVals <- apply(tuneAcc[, params, drop = FALSE], 
                2, function(x) length(unique(x)))
            if (any(numVals < 2)) {
                constString <- NULL
                for (i in seq(along = numVals)) {
                  if (numVals[i] == 1) 
                    constString <- c(constString, paste0("Tuning parameter '", 
                      names(numVals)[i], "' was held constant at a value of ", 
                      stringFunc(tuneAcc[1, names(numVals)[i]])))
                }
                discard <- names(numVals)[which(numVals == 1)]
                tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% discard), 
                  drop = FALSE]
            }
            else constString <- NULL
        }
        else constString <- NULL
        tuneAcc <- tuneAcc[, !grepl("Apparent$|Optimism$", 
            names(tuneAcc)), drop = FALSE]
        colnames(tuneAcc)[colnames(tuneAcc) == ".B"] <- "Resamples"
        nms <- names(tuneAcc)[names(tuneAcc) %in% params]
        sort_args <- vector(mode = "list", length = length(nms))
        for (i in seq(along = nms)) {
            sort_args[[i]] <- tuneAcc[, nms[i]]
        }
        tune_ord <- do.call("order", sort_args)
        if (!is.null(tune_ord)) 
            tuneAcc <- tuneAcc[tune_ord, , drop = FALSE]
        theDots <- list(...)
        theDots$x <- tuneAcc
        printMat <- do.call("format.data.frame", theDots)
        printMat <- as.matrix(printMat)
        rownames(printMat) <- rep("", dim(printMat)[1])
        if (showSD) {
            sdCols <- grep("SD$", colnames(printMat), value = TRUE)
            sd_dat <- printMat[, sdCols, drop = FALSE]
            printMat <- printMat[, !(colnames(printMat) %in% 
                sdCols), drop = FALSE]
            for (col_name in sdCols) {
                not_sd <- gsub("SD$", "", col_name)
                if (any(colnames(printMat) == not_sd)) {
                  printMat[, not_sd] <- paste0(printMat[, not_sd], 
                    " (", sd_dat[, col_name], ")")
                }
            }
        }
        if (!selectCol) 
            printMat <- printMat[, colnames(printMat) != "Selected", 
                drop = FALSE]
        print(printMat, quote = FALSE, print.gap = 2)
        cat("\n")
        if (!is.null(constString)) {
            cat(caret:::truncateText(paste(constString, collapse = "\n")))
            cat("\n")
        }
        if (dim(tuneAcc)[1] > 1) {
            if (is.null(x$update)) {
                met <- paste(x$metric, "was used to select the optimal model using")
                if (is.function(x$control$selectionFunction)) {
                  met <- paste(met, " a custom selection rule.\n")
                }
                else {
                  met <- paste(met, switch(x$control$selectionFunction, 
                    best = paste("the", ifelse(x$maximize, 
                      "largest", "smallest"), "value.\n"), 
                    oneSE = " the one SE rule.\n", tolerance = " a tolerance rule.\n"))
                }
            }
            else {
                met <- paste("The tuning", ifelse(ncol(x$bestTune) > 
                  1, "parameters", "parameter"), 
                  "was set manually.\n")
            }
            cat(caret:::truncateText(met))
        }
        cat(caret:::truncateText(optString))
        if (nzchar(optString)) 
            cat("\n")
    }
    else printMat <- NULL
    if (details) {
        if (!(x$method %in% c("gbm", "treebag", "nb", 
            "lvq", "knn"))) {
            cat("\n----------------------------------------------------------\n")
            cat("\nThe final model:\n\n")
            switch(x$method, lm = , nnet = , multinom = , pls = , 
                earth = , lmStepAIC = , bagEarth = , bagFDA = print(summary(x$finalModel)), 
                rpart = , ctree = , ctree2 = , cforest = , glmboost = , 
                gamboost = , blackboost = , ada = , randomForest = , 
                pcaNNet = , svmradial = , svmpoly = , svmRadial = , 
                svmPoly = , rvmRadial = , rvmPoly = , lssvmRadial = , 
                lssvmPoly = , gaussprRadial = , gaussprPoly = , 
                enet = , lasso = , LMT = , JRip = , lda = , rda = , 
                pamr = , gpls = , J48 = , ppr = print(x$finalModel), 
                fda = {
                  print(x$finalModel)
                  cat("\n Summary of Terms\n\n")
                  print(x$finalModel$fit)
                })
        }
    }
    invisible(printMat)
}


###################################################################################################################

# summary.rpart to modify decision tree model summary
summary.rpart.project = function (object, depvar = NULL, expvar = NULL, cp = 0, digits = getOption("digits"), file, ...) 
{
    if (!inherits(object, "rpart")) 
        stop("Not a legitimate \"rpart\" object")
    x <- object
    if (!missing(file)) {
        sink(file)
        on.exit(sink())
    }
    
    cat("Response variable    :", depvar, "\n")
    cat("Explanatory variables:", paste0(expvar, collapse = ", "), "\n")
    cat("\n")
    
    if(x$method == "class")
      cat("Classification tree\n")
    else
      cat("Regression tree\n")
    cat("Minsplit: ", x$control$minsplit, "\n")
    cat("Maxdepth: ", x$control$maxdepth, "\n")
    cat("cp      : ", x$control$cp, "\n")
    cat("\n")
    
    omit <- x$na.action
    n <- x$frame$n
    if (length(omit)) 
        cat("  n=", n[1L], " (", naprint(omit), ")\n\n", 
            sep = "")
    else cat("  n=", n[1L], "\n\n")
    
    if (!is.null(temp <- x$variable.importance)) {
        temp <- round(100 * temp/sum(temp))
        if (any(temp > 0)) {
            cat("\nVariable importance\n")
            print(temp[temp > 0])
        }
    }
    
    ff <- x$frame
    ylevel <- attr(x, "ylevels")
    id <- as.integer(row.names(ff))
    parent.id <- ifelse(id == 1L, 1L, id%/%2L)
    parent.cp <- ff$complexity[match(parent.id, id)]
    rows <- seq_along(id)[parent.cp > cp]
    rows <- if (length(rows)) 
        rows[order(id[rows])]
    else 1L
    is.leaf <- ff$var == "<leaf>"
    index <- cumsum(c(1L, ff$ncompete + ff$nsurrogate + !is.leaf))
    if (!all(is.leaf)) {
        sname <- rownames(x$splits)
        cuts <- character(nrow(x$splits))
        temp <- x$splits[, 2L]
        for (i in seq_along(cuts)) {
            cuts[i] <- if (temp[i] == -1L) 
                paste("<", format(signif(x$splits[i, 4L], 
                  digits)))
            else if (temp[i] == 1L) 
                paste("<", format(signif(x$splits[i, 4L], 
                  digits)))
            else paste("splits as ", paste(c("L", 
                "-", "R")[x$csplit[x$splits[i, 4L], 
                1:temp[i]]], collapse = "", sep = ""), 
                collapse = "")
        }
        if (any(temp < 2L)) 
            cuts[temp < 2L] <- format(cuts[temp < 2L], justify = "left")
        cuts <- paste0(cuts, ifelse(temp >= 2L, ",", ifelse(temp == 
            1L, " to the right,", " to the left, ")))
    }
    tmp <- if (is.null(ff$yval2)) 
        ff$yval[rows]
    else ff$yval2[rows, , drop = FALSE]
    tprint <- x$functions$summary(tmp, ff$dev[rows], ff$wt[rows], 
        ylevel, digits)
    for (ii in seq_along(rows)) {
        i <- rows[ii]
        nn <- ff$n[i]
        cat("\nNode number ", id[i], ": ", nn, " observations", 
            sep = "")
        if (ff$complexity[i] < cp || is.leaf[i]) 
            cat("\n")
        else cat(",    complexity param=", format(signif(ff$complexity[i], 
            digits)), "\n", sep = "")
        cat(tprint[ii], "\n")
        if (ff$complexity[i] > cp && !is.leaf[i]) {
            sons <- 2L * id[i] + c(0L, 1L)
            sons.n <- ff$n[match(sons, id)]
            cat("  left son=", sons[1L], " (", sons.n[1L], 
                " obs)", " right son=", sons[2L], 
                " (", sons.n[2L], " obs)", sep = "")
            j <- nn - (sons.n[1L] + sons.n[2L])
            if (j > 1L) 
                cat(", ", j, " observations remain\n", 
                  sep = "")
            else if (j == 1L) 
                cat(", 1 observation remains\n")
            else cat("\n")
            cat("  Primary splits:\n")
            j <- seq(index[i], length.out = 1L + ff$ncompete[i])
            temp <- if (all(nchar(cuts[j], "w") < 25L)) 
                format(cuts[j], justify = "left")
            else cuts[j]
            cat(paste("      ", format(sname[j], justify = "left"), 
                " ", temp, " improve=", format(signif(x$splits[j, 
                  3L], digits)), ", (", nn - x$splits[j, 
                  1L], " missing)", sep = ""), sep = "\n")
            if (ff$nsurrogate[i] > 0L) {
                cat("  Surrogate splits:\n")
                j <- seq(1L + index[i] + ff$ncompete[i], length.out = ff$nsurrogate[i])
                agree <- x$splits[j, 3L]
                temp <- if (all(nchar(cuts[j], "w") < 25L)) 
                  format(cuts[j], justify = "left")
                else cuts[j]
                adj <- x$splits[j, 5L]
                cat(paste("      ", format(sname[j], justify = "left"), 
                  " ", temp, " agree=", format(round(agree, 
                    3L)), ", adj=", format(round(adj, 3L)), 
                  ", (", x$splits[j, 1L], " split)", 
                  sep = ""), sep = "\n")
            }
        }
    }
    cat("\n")
    invisible(x)
}


###################################################################################################################
