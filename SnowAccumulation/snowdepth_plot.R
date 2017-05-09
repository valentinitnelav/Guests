# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make simple graph for snow accumulation climate data using base R graphic functions 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)

DT <- fread("snowdepth.csv")
str(DT)
range(DT$`snow depth`)
range(DT$date)

# ________________ Function to convert from given cm to lines of text (use it as such)
cm2line <- function(x) {
    lh <- par('cin')[2] * par('cex') * par('lheight')
    inch <- x/2.54
    inch/lh
}

# open the graphic device with desired parameters
pdf(file = "January snowdepth.pdf",
    width = 16.9/2.54, height = 7/2.54, 
    family = "Times", pointsize = 8)
# Since "width" and "height" need to be in inches, one can still use cm 
# and divide by 2.54 to make the conversion to inch.
# family="Times" refers to the font family (here: Times New Roman).
# Check this link for some family fonts: http://www.statmethods.net/advgraphs/parameters.html
# pointsize = 8 refers to the point size used, here (Times New Roman) 8

# storing default par() for reverting later to default values
par.default <- par()

# Adjust plotting region
par(mai = c(0.9/2.54, 1.2/2.54, 0.1/2.54, 0.1/2.54))
# mai  - Adjust margins of plotting region (Bottom, Left, Top, Right) : X[cm]/2.54 = inch (or give directly inch values)

# ___ plot empty frame
plot(x = DT$date , y = DT$`snow depth`,
     xlab="", xlim = c(1950, 2015),
     ylab="", ylim = c(0, 800),
     xaxt="n", yaxt="n", type = "n")

# ___ add a line between 2010 and 2011
abline(v = 1986, lty="dashed", lwd=2, col="gray")
abline(v = 2000, lty="dashed", lwd=2, col="gray")

# ___ add the points
points(x = DT$date , y = DT$`snow depth`,
       xlab="", xlim = c(1950, 2015),
       ylab="", ylim = c(0, 800),
       type = "b", lty="solid", cex=1, pch=21, bg="black",
       las = 1, font.lab=2, cex.lab=10/8, xaxt="n", yaxt="n")

# ___ Create custom OX axis
labels.X <- seq(from=1950, to=2015, by=5)
axis(side=1, at=labels.X, labels=FALSE, tck=0.02)

# position OX labels
text(x=labels.X, y=-40,
     labels = labels.X, pos = 1, xpd = TRUE)
# x & y are graph coordinates (not cm!)
# x- value : adjusting for labels OX positions

# set the title of OX axis at x cm outwards from the plot edge - the one set with par(mai=...) above
title(xlab="Year",line=cm2line(0.55), font.lab=2, cex.lab=10/8)

# ___ Create custom OY axis
labels.Y <- seq(from=0, to=800, by=100)
axis(side=2, at=labels.Y, labels=FALSE, tck=0.02)

# position OY labels
text(x=1945.5, y=labels.Y,
     labels = labels.Y, xpd = TRUE)
# x & y are graph coordinates (not cm!)
# x- value : adjusting for labels OX positions

# set the title of OY axis at x cm outwards from the plot edge - the one set with par(mai=...) above
title(ylab="January snow depth (mm)",line=cm2line(0.85), font.lab=2, cex.lab=10/8)

# return par() to default values (par.default was saved previosly)
par(par.default)

# close the device
dev.off()