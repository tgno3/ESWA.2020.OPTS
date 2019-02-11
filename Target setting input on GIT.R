#########################################################################################################################
### Project  : Input target setting using City-Gas data
### Script   : Target setting input on GIT.R
### Contents : Operational target setting using city-gas data
#########################################################################################################################

#########################################################################################################################
### Setting up environment
#########################################################################################################################

# Load library
pkgs <- c("ggplot2", "DJL")
sapply(pkgs, require, character.only = T)

# Load data & parameters
price <- read.csv(url("http://bit.ly/2AGUUSF"), header = T)
df.2d <- read.csv(url("http://bit.ly/2VMTUFo"), header = T)
df.3d <- simplify2array(by(df.2d[,-c(1:3)], df.2d$Year, as.matrix))
id.x  <- c(1, 3) #  in: pipe & cost
id.y  <- c(4)    # out: supply
name  <- df.2d$Name[1:33]
loc   <- df.2d$Location[1:33]
rts   <- "crs"
ori   <- "i"


#########################################################################################################################
### Analysis
#########################################################################################################################

# Table 1. Descriptive statistics
table.1 <- data.frame(Min  = unlist(aggregate(df.2d[, c(id.x, id.y) + 3], by = list(rep(loc, 11)), "min")),
                      Med  = unlist(aggregate(df.2d[, c(id.x, id.y) + 3], by = list(rep(loc, 11)), "median")),
                      Mean = unlist(aggregate(df.2d[, c(id.x, id.y) + 3], by = list(rep(loc, 11)), "mean")),
                      Max  = unlist(aggregate(df.2d[, c(id.x, id.y) + 3], by = list(rep(loc, 11)), "max")),
                      Std  = unlist(aggregate(df.2d[, c(id.x, id.y) + 3], by = list(rep(loc, 11)), "sd")))

print(cbind(Loc  = rep(levels(loc), 3), format(round(table.1[-c(1:3),]), big.mark = ",")))


# Figure 1.1. Frontier shift
res.malm.raw <- roc.malmquist(df.3d[,id.x,], df.3d[,id.y,], 2007:2017, "dea", rts, ori)
res.malm.avg <- data.frame(Period = rep(levels(res.malm.raw$cu$Period), 3),
                           Loc    = factor(rep(levels(loc), each = 10), levels = levels(loc)),
                           CU     = aggregate(res.malm.raw$cu$CU, list(res.malm.raw$cu$Period, rep(loc, 10)), mean)$x,
                           FS     = aggregate(res.malm.raw$fs$FS, list(res.malm.raw$fs$Period, rep(loc, 10)), mean)$x,
                           MI     = aggregate(res.malm.raw$mi$MI, list(res.malm.raw$mi$Period, rep(loc, 10)), mean)$x)

ggplot(data = res.malm.raw$fs, aes(x = Period, y = FS, group = rep(loc, 10), colour = rep(loc, 10))) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + geom_hline(yintercept = 1.0, color = "red", size = 0.5) +
  geom_line(data = res.malm.avg, aes(x = Period, y = FS, group = Loc, colour = Loc), size = 1.2) + 
  scale_y_continuous(name = "Frontier Shift (FS)", limits = c(0.6, 1.4), breaks = seq(0.6, 1.4, 0.1)) +
  theme(axis.title.x         = element_text(size = 14, colour = "gray35"),
        axis.title.y         = element_text(size = 14, colour = "gray35"),
        legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))


# Footnote 4. Industrial demand changes / FS ~ Industrial demand changes
IU.2010.2013 <- (df.3d[,6, 7] - df.3d[,6,4])/df.3d[,6,4]*100/3
IU.2014.2016 <- (df.3d[,6,10] - df.3d[,6,8])/df.3d[,6,8]*100/3
aggregate(IU.2014.2016, list(loc), mean, na.rm = T)
aggregate(IU.2010.2013, list(loc), mean, na.rm = T)
IU.annual.c <- c(df.3d[,6,-1] - df.3d[,6,-11])
summary(lm(res.malm.raw$fs$FS ~ IU.annual.c), na.rm = T)


# Figure 2. LPG vs LNG (vs Oil) prices
price.gg <- data.frame(Tick  = rep(1:nrow(price), 3),
                       Price = c(price$Dubai_oil*10, price$LNG, price$LPG),
                       Type  = factor(rep(c("Oil", "LNG", "LPG"), each = nrow(price)), levels = c("Oil", "LNG", "LPG")))

ggplot(data = na.omit(price.gg), aes(x = Tick, y = Price, group = Type, colour = Type)) + 
  geom_line(size = 1.2) + theme_bw() + 
  scale_colour_manual(values = c("lightgrey", "royalblue", "orangered")) +
  scale_x_continuous(name = "Year", breaks = seq(1, nrow(price) + 1, 12), labels = seq(2008, 2019, 1)) +
  scale_y_continuous(name = "Gas price (￦/9,393kcal)", limits = c(200, 1400), breaks = seq(200, 1400, 200),
                     sec.axis = sec_axis(~.*0.1, name = "Oil price - Dubai crude ($/B)", breaks = seq(20, 140, 20))) +
  theme(axis.title.x         = element_text(size = 14, colour = "gray35"),
        axis.title.y         = element_text(size = 14, colour = "gray35"),
        legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))


# Table 2. 2017 SOA
df.ex   <- subset(df.2d[,c(4:9, 1:3)], df.2d$Year %in% c(2015:2017))
res.roc <- roc.dea(df.ex[, id.x], df.ex[, id.y], df.ex$Year, 2017, rts, ori)
id.lroc <- which(res.roc$roc_local > 1)
table.2 <- data.frame(Name     = df.ex$Name[id.lroc],
                      Location = df.ex$Location[id.lroc],
                      Year     = df.ex$Year[id.lroc],
                      df.ex[id.lroc, c(id.x, id.y), drop = F],
                      LocalRoC = res.roc$roc_local[id.lroc])
print(cbind(table.2[,1:2], format(table.2[,4:6], big.mark = ","), round(table.2[,7, drop = F], 4)))


# Table 3. Three gas providers with productivity changes by 2020
id.dmu    <- c(78, 77, 70) # Samchunlly / Busan / Gunsan
delta.t   <- 3
y.limit   <- c()
x_f       <- rbind(df.ex[id.lroc, id.x, drop = F] * ((1/res.roc$roc_local[id.lroc])^delta.t), 
                   df.ex[id.dmu,  id.x, drop = F])
y_f       <- rbind(df.ex[id.lroc, id.y, drop = F], 
                   df.ex[id.dmu,  id.y, drop = F])
res.dea.f <- dm.dea(x_f, y_f, rts, ori)
res.eff.f <- data.frame(df.ex[id.dmu, c(7, 8, id.x, id.y)],
                        Eff_2017 = res.roc$eff_t[id.dmu],
                        Eff_2020 = res.dea.f$eff[(length(id.lroc) + 1):(length(id.lroc) + length(id.dmu))])
for(i in 3:5){
  x_l      <- x_f[c(1:2, i),,drop = F]
  x_l[3,]  <- x_f[i,] * res.eff.f$Eff_2017[i - 2]
  y_l      <- y_f[c(1:2, i),,drop = F]
  y.limit  <- c(y.limit, y_f[i,]*(dm.dea(x_l, y_l, rts, "o", o = 3)$eff[3]))
}
print(cbind(res.eff.f[, 1:2], 
            format(res.eff.f[, 3:5], big.mark = ","), 
            round(res.eff.f[, 6:7], 4),
            SP.Bound = format(round(y.limit), big.mark = ",")))


# Table 4. Operational plans
table.4 <- data.frame(Name               = rep(res.eff.f$Name, each = 2),
                      Eff.2017           = rep(round(res.eff.f$Eff_2017, 4), each = 2),
                      Supply.Target.2020 = rep(c("10% increase", "5% increase"), 3),
                      Beta.SP            = rep(y_f[3:5,,], each = 2) * rep(c(1.10, 1.05), 3),
                      Alpha.PP           = NA,
                      Alpha.OC           = NA,
                      Validation         = NA)

for(i in 1:nrow(table.4)){
  id.dmu.t        <- rep(c(3:5), each = 2)[i]
  m.arg           <- list(xdata = x_f, ydata = y_f, rts = rts, 
                          dmu   = id.dmu.t, 
                          et    = res.roc$eff_t[id.dmu][id.dmu.t - length(id.lroc)], 
                          beta  = table.4$Beta[i])
  res.target      <- do.call("target.spec.dea", m.arg)
  table.4[i, 5:6] <- res.target$alpha
  table.4[i, 7]   <- dm.dea(mapply(c, x_f[1:2,,drop = F], table.4[i, 5:6]), 
                            mapply(c, y_f[1:2,,drop = F], table.4[i, 4]), rts, ori, o = 3)$eff[3]
}

print(cbind(table.4[,1:3], format(round(table.4[,4:6]), big.mark = ","), round(table.4[,7, drop = F], 4)), row.names = F)
