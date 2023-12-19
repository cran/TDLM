## ----setup, include=FALSE-----------------------------------------------------

knitr::opts_chunk$set(echo = TRUE, message = TRUE, warning = FALSE,
                      fig.width = 8, fig.height = 8)
# Packages --------------------------------------------------------------------
suppressPackageStartupMessages({
  suppressWarnings({
    library(TDLM)
    library(sf)
  })
})

options(tinytex.verbose = TRUE)


## -----------------------------------------------------------------------------
data(od)

od[1:10, 1:10]

dim(od)

## -----------------------------------------------------------------------------
data(distance)

distance[1:10, 1:10]

dim(distance)

## -----------------------------------------------------------------------------
data(mass)

mass[1:10,]

dim(mass)

mi <- as.numeric(mass[,1])
names(mi) <- rownames(mass)

mj <- mi

Oi <- as.numeric(mass[,2])
names(Oi) <- rownames(mass)

Dj <- as.numeric(mass[,3])
names(Dj) <- rownames(mass)

## -----------------------------------------------------------------------------
library(sf)

data(county)

county[1:10,]

plot(county)

## -----------------------------------------------------------------------------
check_format_names(vectors = list(mi = mi, mj = mj, Oi = Oi, Dj = Dj),
                   matrices = list(od = od, distance = distance),
                   check = "format_and_names")

## -----------------------------------------------------------------------------
spi <- extract_spatial_information(county, id = "ID")

distance2 <- spi$distance

distance2[1:10, 1:10]

## -----------------------------------------------------------------------------
mean(spi$surface)

## -----------------------------------------------------------------------------
sij <- extract_opportunities(opportunity = mi,
                             distance = distance,
                             check_names = TRUE)
sij[1:10, 1:10]

## -----------------------------------------------------------------------------
res <- run_law_model(law = "NGravExp", 
                     mass_origin = mi, 
                     mass_destination = mj, 
                     distance = distance, 
                     opportunity = NULL,
                     param = 0.01,
                     write_proba = TRUE,
                     
                     model = "DCM", 
                     nb_trips = NULL, 
                     out_trips = Oi, 
                     in_trips = Dj,
                     average = FALSE, 
                     nbrep = 3)

## -----------------------------------------------------------------------------
print(res)

str(res)

## -----------------------------------------------------------------------------
res <- run_law_model(law = "NGravExp", 
                     mass_origin = mi, 
                     mass_destination = mj, 
                     distance = distance, 
                     opportunity = NULL,
                     param = c(0.01,0.02),
                     write_proba = TRUE,
                     
                     model = "DCM", 
                     nb_trips = NULL, 
                     out_trips = Oi, 
                     in_trips = Dj,
                     average = FALSE, 
                     nbrep = 3)

## -----------------------------------------------------------------------------
print(res)

str(res)

## -----------------------------------------------------------------------------
res <- run_law_model(law = "Rad", 
                     mass_origin = mi, 
                     mass_destination = mj, 
                     distance = NULL, 
                     opportunity = sij,
                     param = NULL,
                     write_proba = TRUE,
                     
                     model = "DCM", 
                     nb_trips = NULL, 
                     out_trips = Oi, 
                     in_trips = Dj,
                     average = FALSE, 
                     nbrep = 3)

print(res)

## -----------------------------------------------------------------------------
res$replication_1[1:10,1:10]

res <- run_law_model(law = "Rad", 
                     mass_origin = mi, 
                     mass_destination = mj, 
                     distance = NULL, 
                     opportunity = sij,
                     param = NULL,
                     write_proba = TRUE,
                     
                     model = "DCM", 
                     nb_trips = NULL, 
                     out_trips = Oi, 
                     in_trips = Dj,
                     average = TRUE, 
                     nbrep = 3)

print(res)

res$replication_1[1:10,1:10]

## -----------------------------------------------------------------------------
print(calib_param(av_surf = mean(spi$surface), law = "NGravExp"))

## -----------------------------------------------------------------------------
res <- run_law_model(law = "NGravExp", 
                     mass_origin = mi, 
                     mass_destination = mj, 
                     distance = distance, 
                     opportunity = NULL,
                     param = seq(0.05,0.1,0.005),
                     write_proba = TRUE,
                     
                     model = "DCM", 
                     nb_trips = NULL, 
                     out_trips = Oi, 
                     in_trips = Dj,
                     average = FALSE, 
                     nbrep = 3)

calib <- gof(sim = res, obs = od, measures = "all", distance = distance)

print(calib)

## -----------------------------------------------------------------------------
cpc <- aggregate(calib$CPC, list(calib$Parameter_value), mean)[,2]

oldmar <- par()$mar
par(mar = c(4.5, 6, 1, 1))
plot(seq(0.05,0.1,0.005), cpc, type="b", pch=16, cex=2, lty=1, lwd=3, 
     col="steelblue3", axes=FALSE, xlab="", ylab="")
axis(1, cex.axis=1.2)
axis(2, cex.axis=1.2, las=1)
mtext("Parameter value", 1, line = 3.25, cex = 1.75)
mtext("Common Part of Commuters", 2, line = 4, cex = 1.75)
box(lwd=1.5)
par(mar = oldmar)

