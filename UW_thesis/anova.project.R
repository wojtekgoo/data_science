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