require(manipulate)

pos <- array(NA,dim=c(0,2))
psave <- numeric(0)

metapop.setup <- function (p0, npatch = 5) {
  occ <- runif(n=npatch)<p0
  nside <- ceiling(sqrt(9*npatch))
  k <- sample.int(n=nside*nside,size=npatch)
  xx <- expand.grid(x=seq_len(nside),y=seq_len(nside))
  pos <<- xx[k,]+runif(n=2*npatch,min=-0.25,max=0.25)
  psave <<- mean(occ)
  occ
}

metapop.step <- function (x, c, e) {
  p <- mean(x)
  pcol <- 1-exp(-c*p)
  pext <- 1-exp(-e)
  ifelse(
         x,
         ifelse(runif(n=length(x))<pext,F,T),
         ifelse(runif(n=length(x))<pcol,T,F)
         )
}

occ <- metapop.setup(p0=0.5,npatch=20)

manipulate(
           {
             if (reset) {
               occ <- metapop.setup(p0=p0,npatch=size)
             }
             for (k in seq_len(nstep)) {
               occ <- metapop.step(occ,c=c,e=e)
               psave <<- append(psave,mean(occ))
             }
             op <- par(mfrow=c(2,1),mar=c(3,3,0,0))
             plot(pos[,1],pos[,2],
                  pch=ifelse(occ,16,1),
                  cex=min(par("pin")/par("cin"))/ceiling(sqrt(4*length(occ))),
                  ann=F,xaxt='n',yaxt='n',bty='n')
             plot(psave,
                  ylim=c(0,1),
                  ylab="fraction occupied",xlab="time",type='o')
             par(op)
           },
           c=slider(0,0.5,initial=0.2,step=0.01,label='colonization rate'),
           e=slider(0,0.5,initial=0.2,step=0.01,label='extinction rate'),
           p0=slider(0,1,initial=0.5,step=0.1,label='initial occupancy'),
           size=slider(1,100,initial=20,step=1,label='number of patches'),
           nstep=slider(0,100,initial=1,step=1,label='number of steps'),
           run=button("continue"),
           reset=button("rerun")
           )


