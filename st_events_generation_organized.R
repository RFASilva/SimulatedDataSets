# Generation of datasets
library(data.table)
library("stpp")
library("rgl")
library("lgcp")
library("sf")

# Cluster Process Daily
pcp1 <- rpcp(nparents = 100, mc = 500, npoints = 30000, s.region = usaboundaries, t.region = c(1, 525600), discrete.time = TRUE, replace=FALSE, cluster = c("normal", "exponential"), dispersion = c(1, 1440) )
write.table(cbind(pcp1$xyt[, 1:2], trunc(pcp1$xyt[, 3])), file = "poisson_cluster_process_Daily.csv",sep = ",", row.names = F, col.names=T)

# Cluster Process Weekly
pcp1 <- rpcp(nparents = 50, mc = 1000, npoints = 30000, s.region = usaboundaries, t.region = c(1, 525600), discrete.time = TRUE, replace=FALSE, cluster = c("uniform", "uniform"), dispersion = c(4, 10800) )
write.table(cbind(pcp1$xyt[, 1:2], trunc(pcp1$xyt[, 3])), file = "poisson_cluster_process_Weekly2.csv",sep = ",", row.names = F, col.names=T)


# Cluster Process Weekly
pcp1 <- rpcp(nparents = 50, mc = 1000, npoints = 30000, s.region = usaboundaries, t.region = c(1, 525600), discrete.time = TRUE, replace=FALSE, cluster = c("uniform", "uniform"), dispersion = c(0.034, 10800) )
write.table(cbind(pcp1$xyt[, 1:2], trunc(pcp1$xyt[, 3])), file = "poisson_cluster_process_Weekly3.csv",sep = ",", row.names = F, col.names=T)


# Plot data in a Space-time Cube
pcp1 <- cbind(pcp1$xyt[, 1:2], pcp1$xyt[, 3])
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(pcp1[,3], nbcol)
plot3d(pcp1[,1], pcp1[,2], pcp1[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])


# Homogenous Process

hpp1 <- rpp(npoints=30000, s.region = usaboundaries, t.region = c(1, 525600), discrete.time = TRUE, replace = TRUE)
write.table(cbind(hpp1$xyt[, 1:2], trunc(hpp1$xyt[, 3])), file = "homogenous_process.csv",sep = ",", row.names = F, col.names=T)

# Plot data in a Space-time Cube
hpp1 <- cbind(hpp1$xyt[, 1:2], hpp1$xyt[, 3])
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(hpp1[,3], nbcol)
plot3d(hpp1[,1], hpp1[,2], hpp1[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])


# Mixed Cluster and Homogenous Process

pcp_mixed <- rpcp(nparents = 50, mc = 1000, npoints = 30000, s.region = usaboundaries, t.region = c(1, 525600), discrete.time = TRUE, replace=FALSE, cluster = c("uniform", "uniform"), dispersion = c(1, 1440) )
hpp_mixed <- rpp(npoints=5000, s.region = usaboundaries, t.region = c(1, 525600), discrete.time = TRUE, replace = TRUE)

process_mixed <- rbind(pcp_mixed$xyt, hpp_mixed$xyt)
write.table(cbind(process_mixed[, 1:2], trunc(process_mixed[, 3])), file = "poisson_cluster_process_daily_noise.csv",sep = ",", row.names = F, col.names=T)

# Plot data in a Space-time Cube
process_mixed <- cbind(process_mixed[, 1:2], process_mixed[, 3])
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(process_mixed[,3], nbcol)
plot3d(process_mixed[,1], process_mixed[,2], process_mixed[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])

# CONTAGIOUS PROCESSES

cont2 <- rinter(npoints=3000,  
                s.region = usaboundaries, 
                t.region = c(1, 525600),  
                discrete.time = TRUE,
                thetas=0, deltas=1, 
                replace = TRUE,
                thetat=0, deltat=10080, 
                recent=10, inhibition=FALSE)


write.table(cbind(cont2$xyt[, 1:2], trunc(cont2$xyt[, 3])), file = "contagious_2.csv",sep = ",", row.names = F, col.names=T)

cont2 <- rinter(npoints=3000,  
                s.region = usaboundaries, 
                t.region = c(1, 525600),  
                discrete.time = TRUE,
                thetas=0, deltas=5, 
                replace = TRUE,
                thetat=0, deltat=26280, 
                recent=1, inhibition=FALSE)

cont3 <- rinter(npoints=5000,  
                s.region = usaboundaries, 
                t.region = c(1, 525600),  
                discrete.time = TRUE,
                thetas=0, deltas=0.02, 
                replace = TRUE,
                thetat=0, deltat=1440, 
                recent=1, inhibition=FALSE)


cont3teste <- cbind(cont3$xyt[, 1:2], cont3$xyt[, 3])
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(cont3teste[,3], nbcol)
plot3d(cont3teste[,1], cont3teste[,2], cont3teste[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])


write.table(cbind(cont3$xyt[, 1:2], trunc(cont3$xyt[, 3])), file = "contagious_3.csv",sep = ",", row.names = F, col.names=T)


# Log-Gaussian Cox Point Patterns

lgcp4 <- rlgcp(npoints =12000,
               s.region = usaboundaries,
               discrete.time = TRUE,
               scale=c(0.02, 1),
               t.region=c(0,365),
               nx = 20, ny = 20, nt = 365, separable = FALSE,
               model = "cesare", param = c(1, 1, 3, 1, 1, 2), var.grf =1, mean.grf = 20)


lgcp4 <- rlgcp(npoints =5000,
               s.region = usaboundaries,
               discrete.time = TRUE,
               scale=c(1, 10),
               t.region=c(0,365),
               nx = 50, ny = 50, nt = 175, separable = FALSE,
               model = "gneiting", param = c(1, 1, 1, 1, 1, 2), var.grf = 32, mean.grf = 20)

lgcp4 <- rlgcp(npoints = 10000,
               s.region = usaboundaries,
               nx = 50, ny = 50, nt = 50, separable = FALSE,
               model = "gneiting", param = c(1, 1, 1, 1, 1, 2), var.grf = -3, mean.grf = 1)


lgcp4 <- rlgcp(npoints = 10000,
               scale = c(5, 5),
               nx = 60, ny = 60, nt = 50, separable = FALSE,
               model = "gneiting", param = c(1, 1, 1, 1, 1, 2), var.grf = -3, mean.grf = 1)


lgcp4teste <- cbind(lgcp4$xyt[, 1:2], lgcp4$xyt[, 3])
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(lgcp4teste[,3], nbcol)
plot3d(lgcp4teste[,1], lgcp4teste[,2], lgcp4teste[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])




hppbla <- rpp(npoints=5000, s.region = usaboundaries, t.region = c(1, 365), discrete.time = TRUE, replace = TRUE)
write.table(cbind(hppbla$xyt[, 1:2], trunc(hppbla$xyt[, 3])), file = "homogenous_process_log.csv",sep = ",", row.names = F, col.names=T)



write.table(cbind(lgcp4$xyt[, 1:2], ceiling(lgcp4$xyt[, 3]*365000 /1000)), file = "log_gaussian_test.csv",sep = ",", row.names = F, col.names=T)


N <- lgcp4$Lambda[,,1]
for(j in 2:(dim(lgcp4$Lambda)[3])){N <- N + lgcp4$Lambda[, , j]}
image(N, col = grey((1000:1) / 1000)) ; box()
animation(lgcp4$xyt, cex = 0.8, runtime = 10, add = TRUE,
          prevalent = "orange")



write.table(cbind(lgcp4$xyt[, 1:2], trunc(lgcp4$xyt[, 3])), file = "log_gaussian_cox_process3.csv",sep = ",", row.names = F, col.names=T)



lgcp4 <- rlgcp(npoints =10000,
               s.region = usaboundaries,
               discrete.time = TRUE,
               scale=c(20, 365),
               t.region=c(0,730),
               nx = 20, ny = 20, nt = 730, separable = FALSE,
               model = "gneiting", param = c(1, 1, 1, 1, 1, 2), var.grf =5, mean.grf = 20)


write.table(cbind(lgcp4$xyt[, 1:2], trunc(lgcp4$xyt[, 3])), file = "log_gaussian_cox_process3.csv",sep = ",", row.names = F, col.names=T)


lgcp4teste <- cbind(lgcp4$xyt[, 1:2], lgcp4$xyt[, 3])
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(lgcp4teste[,3], nbcol)
plot3d(lgcp4teste[,1], lgcp4teste[,2], lgcp4teste[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])


write.table(cbind(lgcp4$xyt[, 1:2], trunc(lgcp4$xyt[, 3])), file = "log_gaussian_cox_process.csv",sep = ",", row.names = F, col.names=T)



# Just tests

lbd <- function(x,y,t,a) {exp(-4*y) * exp(-2*t)}
pcp_lbda <- rpcp(nparents = 50, mc = 1000, 
                 npoints = 30000, s.region = usaboundaries, 
                 t.region = c(1, 525600), discrete.time = TRUE, 
                 replace=FALSE, cluster = "uniform", lambda = lbd, 
                 dispersion = c(4, 1440) )


# ESTE AQUI CRIA GRUPOS APENAS NUMA PARTE DOS ESTADOS UNIDOS QUE SE CALHAR E ALGO QUE QUERO E POSSO MISTURAR
# COM RUIDO
lbda <- function(x,y,t){ 10 }
pcp2teste <- rpcp(nparents=30, npoints=30000,
                  s.region = usaboundaries, 
                  t.region = c(1, 525600), 
                  discrete.time = TRUE,
                  dispersion = c(2, 1440),
                  cluster = "exponential",
                  )


# Plot data in a Space-time Cube
pcpteste <- cbind(pcp2teste$xyt[, 1:2], pcp2teste$xyt[, 3])
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(pcpteste[,3], nbcol)
plot3d(pcpteste[,1], pcpteste[,2], pcpteste[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])


# TESTES PARA PROCESSO CONTAGIOSO

bla <- rinter(npoints=250, recent=1,
       deltas=7.5, deltat=10,
       inhibition=FALSE)

data(northcumbria)
cont1 <- rinter(npoints=2500, s.region=northcumbria, t.region=c(1,200), 
               thetas=0, deltas=5000, thetat=0, deltat=10, recent=1, inhibition=FALSE)

# 1dia de inibicao
cont1 <- rinter(npoints=2500,  
                s.region = usaboundaries, 
                t.region = c(1, 525600),  
                discrete.time = TRUE,
                thetas=0, deltas=2, 
                thetat=0, deltat=1440, 
                recent=1, inhibition=FALSE)


cont2 <- rinter(npoints=250,  
                s.region = usaboundaries, 
                t.region = c(1, 300),  
                discrete.time = TRUE,
                thetas=0, deltas=1, 
                replace = TRUE,
                thetat=0, deltat=30, 
                recent=1, inhibition=FALSE)


cont2 <- rinter(npoints=50000,  
                s.region = usaboundaries, 
                t.region = c(1, 525600),  
                discrete.time = TRUE,
                thetas=0, deltas=2, 
                replace = TRUE,
                thetat=0, deltat=10080, 
                recent=1, inhibition=FALSE)

cont3 <- rinter(npoints=5000,  
                s.region = usaboundaries, 
                t.region = c(1, 525600),  
                discrete.time = TRUE,
                thetas=0, deltas=0.5, 
                replace = TRUE,
                thetat=0, deltat=10080, 
                recent=1, inhibition=FALSE)

write.table(cbind(cont3$xyt[, 1:2], trunc(cont3$xyt[, 3])), file = "contagious_3.csv",sep = ",", row.names = F, col.names=T)




# Plot data in a Space-time Cube
contteste <- cbind(cont3$xyt[, 1:2], cont3$xyt[, 3])
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(contteste[,3], nbcol)
plot3d(contteste[,1], contteste[,2], contteste[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])


data(northcumbria)
cont1 = rinter(npoints=250, s.region=northcumbria, t.region=c(1,200), 
               thetas=0, deltas=5000, thetat=0, deltat=10, recent=1, inhibition=FALSE)


lgcp1 <- rlgcp(npoints=3000, 
               
                separable=TRUE, 
               model="exponential", param=c(1,1,1,1,1,2), var.grf = 2, mean.grf = -0.5 * 2)

lgcp2 <- rlgcp(npoints=200,
               s.region = usaboundaries, 
               t.region = c(1, 365), 
               separable=TRUE,
               model="exponential", param=c(0.1,0.1,0.1,0.1,0.1,0.2), var.grf=0.02, mean.grf=-0.04)

lgcp4 <- rlgcp(npoints=2000, 
               s.region=northcumbria, t.region=c(1,400), 
               scale=c(1000, 400),
               discrete.time = TRUE,
               nx=50, ny=50, nt=50,
               separable=TRUE, 
               model="exponential", param=c(0.01,0.01,0.01,0.01,0.01,0.02), var.grf=1, mean.grf=0)


lgcp4 <- rlgcp(npoints =12000,
               s.region = usaboundaries, 
              t.region=c(0,1),
              nx = 50, ny = 50, nt = 50, separable = FALSE,
              model = "gneiting", param = c(1, 1, 1, 1, 1, 2), var.grf =0.25, mean.grf = 0)




lgcp4 <- rlgcp(npoints = 200, nx = 50, ny = 50, nt = 50, separable = FALSE,
                model = "gneiting", param = c(1, 1, 1, 1, 1, 2), var.grf = 1, mean.grf = 0)

lgcp4 <- rlgcp(npoints =12000,
               s.region = usaboundaries, 
               scale=c(0.02, 1),
               t.region=c(0,365),
               nx = 20, ny = 20, nt = 365, separable = FALSE,
               model = "cesare", param = c(1, 1, 3, 1, 1, 2), var.grf =1, mean.grf = 20)



N <- lgcp4$Lambda[,,1]
for(j in 2:(dim(lgcp4$Lambda)[3])){N <- N + lgcp4$Lambda[, , j]}
 image(N, col = grey((1000:1) / 1000)) ; box()
 animation(lgcp4$xyt, cex = 0.8, runtime = 10, add = TRUE,
              prevalent = "orange")


 
 lgcp1 <- rlgcp(npoints = 8000, nx = 50, ny = 50, nt = 50, separable = TRUE,
                model = "exponential", param = c(1, 1, 1, 1, 1, 2), var.grf =2, mean.grf = -0.5*2)
 
 
 
 
 lgcp1teste <- cbind(lgcp1$xyt[, 1:2], lgcp1$xyt[, 3])
 nbcol = 100
 color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
 zcol  = cut(lgcp1teste[,3], nbcol)
 plot3d(lgcp1teste[,1], lgcp1teste[,2], lgcp1teste[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])
 

 
 
 lgcp4 <- rlgcp(npoints =10000,
                s.region = usaboundaries,
                discrete.time = TRUE,
                scale=c(0.02, 365),
                t.region=c(0,730),
                nx = 20, ny = 20, nt = 730, separable = FALSE,
                model = "gneiting", param = c(1, 1, 1, 1, 1, 2), var.grf =5, mean.grf = 20)
 
 lgcp4 <- rlgcp(npoints =10000,
                s.region = usaboundaries,
                discrete.time = TRUE,
                scale=c(20, 365),
                t.region=c(0,730),
                nx = 20, ny = 20, nt = 730, separable = FALSE,
                model = "gneiting", param = c(1, 1, 1, 1, 1, 2), var.grf =5, mean.grf = 20)
 

 
 lgcp4teste <- cbind(lgcp4$xyt[, 1:2], lgcp4$xyt[, 3])
 nbcol = 100
 color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
 zcol  = cut(lgcp4teste[,3], nbcol)
 plot3d(lgcp4teste[,1], lgcp4teste[,2], lgcp4teste[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])
 
 
 
 N <- lgcp4$Lambda[,,1]
 for(j in 2:(dim(lgcp4$Lambda)[3])){N <- N + lgcp4$Lambda[, , j]}
 image(N, col = grey((1000:1) / 1000)) ; box()
 animation(lgcp4$xyt, cex = 0.8, runtime = 10, add = TRUE,
           prevalent = "orange")
 
 