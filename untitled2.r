library(ggplot2)
library(nls2)
library(nlme)
subdata=data.frame(`1358024_qog_csd_csv_v6apr11`$gle_gdp, 
                   `1358024_qog_csd_csv_v6apr11`$wdi_hec,
                   `1358024_qog_csd_csv_v6apr11`$wdi_the,
                   `1358024_qog_csd_csv_v6apr11`$wdi_puhegdp,
                   `1358024_qog_csd_csv_v6apr11`$wdi_lifexp)
colnames(subdata)=c("gle_gdp","wdi_hec","wdi_the","wdi_puhegdp","wdi_lifexp")

rhs <- function(x, b0, b1, b2) {
  b0 + exp(-(x-b1)/b2)
}

mod <- nls(wdi_puhegdp ~ A+B*wdi_lifexp , #exp(-(wdi_lifexp-b1)/b2) , 
           data = subdata, 
           start = list(A = 1, B = 1),
           #start = c(b0 = 1, b1 = 20),
           #                          b2 = 1), 
           trace = T,
           alg = "port")
#           , algorithm = "plinear")

subdata[is.na(subdata)] <- 0

mod2 <- nls(wdi_puhegdp ~ A+B*exp(-(wdi_lifexp-b1)) , 
           data = subdata, 
           start = list(A = -2.1, B = 1.5, b1=5),
            alg = "port")
           #start = c(b0 = 1, b1 = 20),
           #                          b2 = 1), 
           #trace = T
            )
glm.fit(subdata$wdi_puhegdp,A+B*exp(-(subdata$wdi_lifexp-b1)))

nlsLM(subdata$wdi_puhegdp,A+B*exp(-(subdata$wdi_lifexp-C)),start=list(A=5.0,B=2.0,C=30))

nls(y~a*log10(x)+b, data=dat, start=list(a=-0.8,b=-2), trace=TRUE)


ggplot() + theme_bw() + 
  geom_point(data=subdata,
             aes(x = wdi_puhegdp, y = wdi_lifexp)) + # , colour  = "Minima",linetype = "Final"),size = 3) +
  geom_smooth(method="nls", formula=y~b*x+c, se=FALSE, 
              start=list(b=0,c=1)) 

  geom_line(aes(x=subdata$wdi_lifexp, y=predict(mod, list(x = subdata$wdi_lifexp)) ) )
#
+ geom_smooth(method="nls", formula=y~b*x+c, se=FALSE, 
            start=list(b=0,c=1)) 


MMODDEL=nlsLM(subdata$wdi_puhegdp ~ 
                a + exp(c*(subdata$wdi_lifexp-b)), data=subdata, start = list(a = 0.12345, b = 50,c=1))
plot(subdata$wdi_lifexp,  subdata$wdi_puhegdp, main = "data")
points(subdata$wdi_lifexp, fitted(MMODDEL), col = 2, lwd = 2)

x <- 0:19
y <- 0:19

# make the line jagged by shifting points
# [down,no move, up] in a repeating group of 3
y <- y + (-1:1)
plot(x,y,type='b') # generate vector of weights that give more
# weight to the unshifted points
w <- rep(0,20)
w <- w + c(1,3,1)

# reconstruct the unshifted straight line
f <- glm.fit(x,y,w)
lines(x,f$fitted.values)


guess = c(tau = 2.2, N0 = 1500, a = 0.25, f0 = 10)

out <- nls.lm(par = guess, fn = fcn, jac = fcn.jac,
              fcall = f, jcall = j,
              TT = TT, N = N, control = nls.lm.control(nprint=1))








if (interactive()) {
  options(device.ask.default = FALSE)
  
  ## Scatterplot (Lattice graphics).
  ## Labels are taken from rownames of data.
  ## Right-click on the plot to identify points.
  playwith(xyplot(Income ~ log(Population / Area),
                  data = data.frame(state.x77), groups = state.region,
                  type = c("p", "smooth"), span = 1, auto.key = TRUE,
                  xlab = "Population density, 1974 (log scale)",
                  ylab = "Income per capita, 1974"))
  
  ## Scatterplot (base graphics); similar.
  ## Note that label style can be set from a menu item.
  urbAss <- USArrests[,c("UrbanPop", "Assault")]
  playwith(plot(urbAss, panel.first = lines(lowess(urbAss)),
                col = "blue", main = "Assault vs urbanisation",
                xlab = "Percent urban population, 1973",
                ylab = "Assault arrests per 100k, 1973"))
  
  ## Time series plot (Lattice).
  ## Date-time range can be entered directly in "time mode"
  ## (supports numeric, Date, POSIXct, yearmon and yearqtr).
  ## Click and drag to zoom in, holding Shift to constrain;
  ## or use the scrollbar to move along the x-axis.
  library(zoo)
  playwith(xyplot(sunspots ~ yearmon(time(sunspots)),
                  xlim = c(1900, 1930), type = "l"),
           time.mode = TRUE)
  
  ## Time series plot (base graphics); similar.
  ## Custom labels are passed directly to playwith.
  tt <- time(treering)
  treeyears <- paste(abs(tt) + (tt <= 0),
                     ifelse(tt > 0, "CE", "BCE"))
  playwith(plot(treering, xlim = c(1000, 1300)),
           labels = treeyears, time.mode = TRUE)
  
  ## Multi-panel Lattice plot.
  ## Need subscripts = TRUE to correctly identify points.
  ## Scales are "same" so zooming applies to all panels.
  ## Use the 'Panel' tool to expand a single panel, then use
  ## the vertical scrollbar to change pages.
  Depth <- equal.count(quakes$depth, number = 3, overlap = 0.1)
  playwith(xyplot(lat ~ long | Depth, data = quakes,
                  subscripts = TRUE, aspect = "iso", pch = ".", cex = 2),
           labels = paste("mag", quakes$mag))
  
  ## Spin and brush for a 3D Lattice plot.
  ## Drag on the plot to rotate in 3D (can be confusing).
  ## Brushing is linked to the previous xyplot (if still open).
  ## Note, brushing 'cloud' requires a recent version of Lattice.
  playwith(cloud(-depth ~ long * lat, quakes, zlab = "altitude"),
           new = TRUE, link.to = playDevCur(), click.mode = "Brush")
  
  ## Set brushed points according to a logical condition.
  playSetIDs(value = which(quakes$mag >= 6))
  
  ## Interactive control of a parameter with a slider.
  xx <- rnorm(50)
  playwith(plot(density(xx, bw = bandwidth), panel.last = rug(xx)),
           parameters = list(bandwidth = seq(0.05, 1, by = 0.01)))
  
  ## The same with a spinbutton (use I() to force spinbutton).
  ## Initial value is set as the first in the vector of values.
  ## This also shows a combobox for selecting text options.
  xx <- rnorm(50)
  kernels <- c("gaussian", "epanechnikov", "rectangular",
               "triangular", "biweight", "cosine", "optcosine")
  playwith(plot(density(xx, bw = bandwidth, kern = kernel), lty = lty),
           parameters = list(bandwidth = I(c(0.1, 1:50/50)),
                             kernel = kernels, lty = 1:6))
  
  ## More parameters (logical, numeric, text).
  playwith(stripplot(yield ~ site, data = barley,
                     jitter = TRUE, type = c("p", "a"),
                     aspect = aspect, groups = barley[[groups]],
                     scales = list(abbreviate = abbrev),
                     par.settings = list(plot.line = list(col = linecol))),
           parameters = list(abbrev = FALSE, aspect = 0.5,
                             groups = c("none", "year", "variety"),
                             linecol = "red"))
  
  ## Looking through 100 time series and comparing to a reference;
  ## Use buttons to save the current series number or its mean value.
  dat <- ts(matrix(cumsum(rnorm(100*100)), ncol = 100), start = 1900)
  colnames(dat) <- paste("Series", 1:100)
  ref <- (dat[,3] + dat[,4]) / 2
  playwith(xyplot(cbind(dat[,i], ref = ref)),
           parameters = list(i = 1:100,
                             print_i = function(playState) print(playState$env$i),
                             print_mean = function(p) print(mean(dat[,p$env$i])),
                             save_to_ii = function(playState)
                               .GlobalEnv$ii <- playState$env$i,
                             append_to_ii = function(playState) {
                               if (!exists("ii")) ii <- c()
                               .GlobalEnv$ii <- c(ii, playState$env$i)
                             })
  )
  
  ## Composite plot (base graphics).
  ## Adapted from an example in help("legend").
  ## In this case, the initial plot() call is detected correctly;
  ## in more complex cases may need e.g. main.function="plot".
  ## Here we also construct data points and labels manually.
  x <- seq(-4*pi, 4*pi, by = pi/24)
  pts <- data.frame(x = x, y = c(sin(x), cos(x), tan(x)))
  labs <- rep(c("sin", "cos", "tan"), each = length(x))
  labs <- paste(labs, round(180 * x / pi) %% 360)
  playwith( {
    plot(x, sin(x), type = "l", xlim = c(-pi, pi),
         ylim = c(-1.2, 1.8), col = 3, lty = 2)
    points(x, cos(x), pch = 3, col = 4)
    lines(x, tan(x), type = "b", lty = 1, pch = 4, col = 6)
    legend("topright", c("sin", "cos", "tan"), col = c(3,4,6),
           lty = c(2, -1, 1), pch = c(-1, 3, 4),
           merge = TRUE, bg = 'gray90')
  }, data.points = pts, labels = labs)
  
  ## A ggplot example.
  ## NOTE: only qplot()-based calls will work.
  ## Labels are taken from rownames of the data.
  if (require(ggplot2)) {
    playwith(qplot(qsec, wt, data = mtcars) + stat_smooth())
  }
  
  ## A minimalist grid plot.
  ## This shows how to get playwith to work with custom plots:
  ## accept xlim/ylim and pass "viewport" to enable zooming.
  myGridPlot <- function(x, y, xlim = NULL, ylim = NULL, ...)
  {
    if (is.null(xlim)) xlim <- extendrange(x)
    if (is.null(ylim)) ylim <- extendrange(y)
    grid.newpage()
    pushViewport(plotViewport())
    grid.rect()
    pushViewport(viewport(xscale = xlim, yscale = ylim,
                          name = "theData"))
    grid.points(x, y, ...)
    grid.xaxis()
    grid.yaxis()
    upViewport(0)
  }
  playwith(myGridPlot(1:10, 11:20, pch = 17), viewport = "theData")
  
  ## Presenting the window as a modal dialog box.
  ## When the window is closed, ask user to confirm.
  confirmClose <- function(playState) {
    if (gconfirm("Close window and report IDs?",
                 parent = playState$win)) {
      cat("Indices of identified data points:\n")
      print(playGetIDs(playState))
      return(FALSE) ## close
    } else TRUE ## don't close
  }
  xy <- data.frame(x = 1:20, y = rnorm(20),
                   row.names = letters[1:20])
  playwith(xyplot(y ~ x, xy, main = "Select points, then close"),
           width = 4, height = 3.5, show.toolbars = FALSE,
           on.close = confirmClose, modal = TRUE,
           click.mode = "Brush")
  
  ## Ask user to save plot to PNG when window is closed:
  saveOnClose <- function(playState) {
    playDevSet(playState)
    if (!gconfirm("Save plot to PNG file? (Cancel = no)")) return(FALSE)
    fname <- gfile("Save PNG file as:", type = "save")
    if (is.na(fname)) return(TRUE) ## cancel
    dev.off(dev.copy(Cairo_png, file = fname,
                     width = dev.size()[1], height = dev.size()[2]))
    FALSE 
  }
  #playwith.options(on.close = saveOnClose)
  
  
  ## Demonstrate cacheing of objects in local environment.
  ## By default, only local variables in the plot call are stored.
  x_global <- rnorm(100)
  doLocalStuff <- function(...) {
    y_local <- rnorm(100)
    angle <- (atan2(y_local, x_global) / (2*pi)) + 0.5
    color <- hsv(h = angle, v = 0.75)
    doRays <- function(x, y, col) {
      segments(0, 0, x, y, col = col)
    }
    playwith(plot(x_global, y_local, pch = 8, col = color,
                  panel.first = doRays(x_global, y_local, color)),
             ...)
  }
  doLocalStuff(title = "locals only") ## eval.args = NA is default
  ## List objects that have been copied and stored:
  ## Note: if you rm(x_global) now, redraws will fail.
  ls(playDevCur()$env)
  ## Next: store all data objects (in a new window):
  doLocalStuff(title = "all stored", eval.args = TRUE, new = TRUE)
  ls(playDevCur()$env)
  ## Now there are two devices open:
  str(playDevList())
  playDevCur()
  playDevOff()
  playDevCur()
  
  ## Not run: 
  ## Big data example, do not try to guess labels or time.mode.
  gc()
  bigobj <- rpois(5000000, 1)
  print(object.size(bigobj), units = "Mb")
  gc()
  playwith(qqmath(~ bigobj, f.value = ppoints(500)),
           data.points = NA, labels = NA, time.mode = FALSE)
  playDevOff()
  gc()
  ## or generate the trellis object first:
  trel <- qqmath(~ bigobj, f.value = ppoints(500))
  playwith(trel)
  rm(trel)
  ## in this case, it is much better to compute the sample first:
  subobj <- quantile(bigobj, ppoints(500), na.rm = TRUE)
  playwith(qqmath(~ subobj))
  rm(subobj)
  rm(bigobj)
  
  ## End(Not run)
  
  ## See demo(package = "playwith") for examples of new tools.
}
