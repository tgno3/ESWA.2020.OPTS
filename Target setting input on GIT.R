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
load("Gas.07.18.Rdata")
df.3d <- simplify2array(by(df.2d[, -c(1:3)], df.2d$Year, as.matrix))
id.x  <- c(1, 3) #  in: pipe & cost
id.y  <- c(4)    # out: supply
loc   <- df.2d$Location[1:nrow(df.3d)]
rts   <- "crs"
ori   <- "i"


#########################################################################################################################
### Analysis
#########################################################################################################################

# Table 1. Descriptive statistics
table.1 <- data.frame(Min  = unlist(aggregate(df.2d[, c(id.x, id.y) + 3], by = list(df.2d$Location), "min")),
                      Med  = unlist(aggregate(df.2d[, c(id.x, id.y) + 3], by = list(df.2d$Location), "median")),
                      Mean = unlist(aggregate(df.2d[, c(id.x, id.y) + 3], by = list(df.2d$Location), "mean")),
                      Max  = unlist(aggregate(df.2d[, c(id.x, id.y) + 3], by = list(df.2d$Location), "max")),
                      Std  = unlist(aggregate(df.2d[, c(id.x, id.y) + 3], by = list(df.2d$Location), "sd")))

print(cbind(Loc  = rep(levels(loc), 3), format(round(table.1[-c(1:3),]), big.mark = ",")))


# Figure 3. Frontier shift
n.period     <- dim(df.3d)[3] - 1
res.malm.raw <- roc.malmquist(df.3d[,id.x,], df.3d[,id.y,], unique(df.2d$Year), "dea", rts, ori)
res.malm.avg <- data.frame(Period = rep(unique(res.malm.raw$cu$Period), 3),
                           Loc    = factor(rep(levels(loc), each = n.period), levels = levels(loc)),
                           CU     = aggregate(res.malm.raw$cu$CU, list(res.malm.raw$cu$Period, rep(loc, n.period)), mean)$x,
                           FS     = aggregate(res.malm.raw$fs$FS, list(res.malm.raw$fs$Period, rep(loc, n.period)), mean)$x,
                           MI     = aggregate(res.malm.raw$mi$MI, list(res.malm.raw$mi$Period, rep(loc, n.period)), mean)$x)

ggplot(data = res.malm.raw$fs, aes(x = Period, y = FS, group = rep(loc, n.period), colour = rep(loc, n.period))) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + geom_hline(yintercept = 1.0, color = "red", size = 0.5) +
  geom_line(data = res.malm.avg, aes(x = Period, y = FS, group = Loc, colour = Loc), size = 1.2) + 
  scale_y_continuous(name = "Frontier Shift (FS)", limits = c(0.6, 1.4), breaks = seq(0.6, 1.4, 0.1)) +
  theme(axis.title.x         = element_text(size = 14, colour = "gray35"),
        axis.title.y         = element_text(size = 14, colour = "gray35"),
        legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(0.95, 0.993))


# Figure 4. LPG vs LNG (vs Oil) prices
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
        legend.justification = c(1, 1), legend.position = c(0.97, 0.993))


# Table 2. Rationale of MI changes
IU.2010.2013 <- (df.3d[,6,  7] - df.3d[,6,  4])/df.3d[,6,  4]*100/3
IU.2014.2016 <- (df.3d[,6, 10] - df.3d[,6,  8])/df.3d[,6,  8]*100/2
IU.2017.2018 <- (df.3d[,6, 12] - df.3d[,6, 11])/df.3d[,6, 11]*100/1

t(matrix(c(round(c(aggregate(IU.2010.2013, list(loc), mean, na.rm = T)$x, mean(IU.2010.2013, na.rm = T)), 2),
           round(c(aggregate(IU.2014.2016, list(loc), mean, na.rm = T)$x, mean(IU.2014.2016, na.rm = T)), 2),
           round(c(aggregate(IU.2017.2018, list(loc), mean, na.rm = T)$x, mean(IU.2017.2018, na.rm = T)), 2)),
         nrow = 4, ncol = 3,
         dimnames = list(c(levels(loc), "Overall"), 
                         c("IU (2010-2013)","IU (2014-2016)", "IU (2017-2018)"))))


# Footnote 5. Regression MI ~ IU
IU.annual.c <- c(df.3d[,6, -1] - df.3d[,6, -12])
summary(lm(res.malm.raw$fs$FS ~ IU.annual.c), na.rm = T)


# Table 3. Validation
table.3 <- data.frame(Period   = c(rep("2009~2013", 6), "2016~2018"),
                      F.Origin = c(rep(2010, 3), rep(2011, 2), 2012, 2017),
                      F.Window = c(1:3, 1:2, 1, 1),
                      MAD.old  = NA,
                      MAD.new  = NA)

for(i in 1:nrow(table.3)){
  from    <- as.numeric(substr(table.3[i, 1], 1, 4))
  origin  <- table.3[i, 2]
  window  <- table.3[i, 2] + table.3[i, 3]
  df.ex   <- df.2d[df.2d$Year %in% c(from:window),]
  res.for <- target.arrival.dea(df.ex[,id.x + 3], df.ex[,id.y + 3], df.ex$Year, origin, rts, ori, anc = F)
  table.3[i, 4] <- mean(abs(df.ex$Year - res.for$arrival_seg), na.rm = T)
  res.for <- target.arrival.dea(df.ex[,id.x + 3], df.ex[,id.y + 3], df.ex$Year, origin, rts, ori, anc = T)
  table.3[i, 5] <- mean(abs(df.ex$Year - res.for$arrival_seg), na.rm = T)
}

print(cbind(table.3[,1:3], round(table.3[,4:5], 4)))


# Table 4. 2018 SOA
df.ex   <- df.2d[df.2d$Year %in% c(2015:2018),]
origin  <- 2018
soa.map <- map.soa.dea(df.ex[,id.x + 3], df.ex[,id.y + 3], df.ex$Year, rts, ori)
res.roc <- roc.dea(df.ex[,id.x + 3], df.ex[,id.y + 3], df.ex$Year, origin, rts, ori)
res.foc <- target.arrival.dea(df.ex[,id.x + 3], df.ex[,id.y + 3], df.ex$Year, origin, rts, ori, anc = T)
id.lroc <- which(res.roc$roc_local > 1)
table.4 <- data.frame(Name     = df.ex$Name[id.lroc],
                      Location = df.ex$Location[id.lroc],
                      Year     = df.ex$Year[id.lroc],
                      df.ex[id.lroc, c(id.x + 3, id.y + 3), drop = F],
                      Ind.p    = df.ex$IU[id.lroc] / df.ex$SP[id.lroc] * 100,
                      LocalRoC = res.foc$roc_local[id.lroc])

print(cbind(table.4[2:1, 1:2], format(table.4[2:1, 4:6], big.mark = ","), 
            round(table.4[2:1, 7, drop = F], 2), round(table.4[2:1, 8, drop = F], 4)))


# Table 5. Three gas providers with productivity changes by 2020
id.dmu    <- c(111, 110, 103) # Samchunlly / Busan / Gunsan
delta.t   <- 2 - mean(table.3$MAD.new[table.3$F.Window == 2])
y.limit   <- c()
x_f       <- rbind(df.ex[id.lroc, id.x + 3, drop = F] * ((1/res.foc$roc_local[id.lroc])^delta.t), 
                   df.ex[id.dmu,  id.x + 3, drop = F])
y_f       <- rbind(df.ex[id.lroc, id.y + 3, drop = F], 
                   df.ex[id.dmu,  id.y + 3, drop = F])
res.dea.f <- dm.dea(x_f, y_f, rts, ori)
res.eff.f <- data.frame(df.ex[id.dmu, c(1, 2, id.x + 3, id.y + 3)],
                        Eff_2018 = res.roc$eff_t[id.dmu],
                        Eff_2020 = res.dea.f$eff[-c(1:length(id.lroc))])
for(i in 3:5){
  x_l      <- x_f[c(1:2, i),,drop = F]
  x_l[3,]  <- x_f[i,] * res.eff.f$Eff_2018[i - 2]
  y_l      <- y_f[c(1:2, i),,drop = F]
  y.limit  <- c(y.limit, y_f[i,]*(dm.dea(x_l, y_l, rts, "o", o = 3)$eff[3]))
}

print(cbind(res.eff.f[, 1:2], 
            format(res.eff.f[, 3:5], big.mark = ","), 
            round(res.eff.f[, 6:7], 4),
            SP.Bound = format(round(y.limit), big.mark = ",")))


# Footnote 8. Kyungdong and Jeonbuk
id.in <- id.lroc[1] # 1 for Jeonbuk / 2 for Kyungdong
x_in  <- df.ex[id.in, id.x + 3, drop = F] * ((1/res.foc$roc_local[id.in])^delta.t)
y_in  <- df.ex[id.in, id.y + 3, drop = F]
df.rf <- subset(df.2d[,c(id.x, id.y) + 3], df.2d$Year == 2013)
df.in <- rbind(data.frame(x_in, y_in), df.rf)
dm.dea(df.in[,1:2], df.in[,3], rts, ori)$eff[1]


# Table 6. Operational plans
table.6 <- data.frame(Name               = rep(res.eff.f$Name, each = 4),
                      Eff.Target         = rep(res.eff.f$Eff_2018, each = 4) * rep(c(1, 1.1), each = 2, 3),
                      Supply.Target.2020 = rep(c("5% increase", "10% increase"), 6),
                      Beta.SP            = rep(y_f[3:5,,], each = 4) * rep(c(1.05, 1.10), 6),
                      Alpha.PP           = NA,
                      Alpha.OC           = NA)

for(i in 1:nrow(table.6)){
  id.dmu.t        <- rep(c(3:5), each = 4)[i]
  delta.t         <- 2 - mean(table.3$MAD.new[table.3$F.Window == 2])
  x_f             <- rbind(df.ex[id.lroc, id.x + 3, drop = F] * ((1/res.foc$roc_local[id.lroc])^delta.t), 
                           df.ex[id.dmu,  id.x + 3, drop = F])
  y_f             <- rbind(df.ex[id.lroc, id.y + 3, drop = F], 
                           df.ex[id.dmu,  id.y + 3, drop = F])
  m.arg           <- list(xdata = x_f, ydata = y_f, rts = rts, dmu = id.dmu.t, 
                          et    = table.6$Eff.Target[i], 
                          beta  = table.6$Beta.SP[i])
  res.target      <- do.call("target.spec.dea", m.arg)
  table.6[i, 5:6] <- res.target$alpha
}

print(cbind(table.6[,1], round(table.6[,2, drop = F], 4), table.6[,3], 
            format(round(table.6[,c(4, 5:6)]), big.mark = ",")), row.names = F)
