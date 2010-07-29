pairsDAFS = function(x,...){
    panel.smooth.mod = function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "gray45", span = 2/3, iter = 3, ...)
    {
        points(x, y, pch = pch, col = col, bg = bg, cex = cex)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok))
            lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
                  col = col.smooth, lwd = 2.5, ...)
    }

    panel.hist.mod = function (x, ...){
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5))
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks
        nB <- length(breaks)
        y <- h$counts
        y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col = "gray80", ...)
    }

    pairs(x, upper.panel = panel.smooth.mod, lower.panel = panel.cor,
        diag.panel = panel.hist.mod, ...)
}
