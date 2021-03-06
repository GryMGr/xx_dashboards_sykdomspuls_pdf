#' Ukenummer
#' @export Ukenummer
Ukenummer <- c(30:52,1:29)
###############################################################################
###############################################################################
#' Title
#'
#' @param x a
#' @export firstup
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
###############################################################################
###############################################################################
#' CreatePlots1
#'
#' @param d a
#' @param weeknow a
#' @param Ukenummer a
#' @param title a
#' @param yrange a
#' @import data.table
#' @export CreatePlots1
#'
CreatePlots1 <- function (d, weeknow, Ukenummer, title,yrange) {
  par(mfrow=c(1,1),mar=c(2.6,2.6,3,.5), oma=c(0,0,0,0))

  plot(c(d[2,30:52], d[2,1:29]), type="l", col="green", xlim=c(1,52), ylim=c(0,yrange),
       main="", xlab="", ylab="",
       lwd=1.5, cex.lab=0.75,cex.main=2,  axes = F)#, dev.new(width=18, height=14))

  lines(c(d[3,30:52], d[3,1:29]), type="l", col="red", lwd=1.5)
  lines(c(d[4,30:52], d[4,1:29]), type="l", col="orange", lwd=1.5)
  lines(c(d[5,30:52], d[5,1:29]), type="l", col="purple", lwd=1.5)
  lines(c(d[6,30:52], d[6,1:29]), type="l", col="blue",lwd=1.5)
  lines(d[7,30:weeknow],type="l", col="black", lwd=2)


  q<-axis(1, at=1:52, labels=Ukenummer,las=1,cex.axis=0.7)
  axis(2,las=3, cex.axis=0.75)
  box(lwd=1)


  abline(v = c(22, 25), col = "black", lty = 2)
  abline(v = c(34, 40), col = "black", lty = 2)
  text(23.5,0, "Jul/Nytt\u00E5r", col = "black", cex=0.75)
  text(37,0, "P\u00E5ske", col = "black", cex=0.75)

  q <-legend("topright", inset=.02,roundUpNice(yrange), legend=c("2013 / 2014", "2014 / 2015", "2015 / 2016", "2016 / 2017", "2017 / 2018", "2018 / 2019"),
             lty=1, col=c("green","red", "orange", "purple", "blue", "black"),
             lwd=c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5), cex=0.5,box.lty=1, box.lwd=1,text.font=1,seg.len=2)
  mtext(title, outer = F, cex = 1,font=2,line = .5)
  mtext(text = "Ukenummer", side = 1,line = 1.8,cex = .75)
  mtext(text = "Antall konsultasjoner", side = 2,line = 1.8,cex = .75)
  #box("figure", col="blue")

}
###############################################################################
##############################################################################
#' CreatePlots2
#'
#' @param d1 a
#' @param weeknow a
#' @param Ukenummer a
#' @param Fylkename a
#' @param S a
#' @param mytittle a
#' @import data.table
#' @export CreatePlots2
#'
CreatePlots2 <- function (d1, weeknow, Ukenummer, Fylkename,S,mytittle) {

  ageGroups=c("0 - 4 \u00E5r", "5 - 19 \u00E5r", "20 - 64 \u00E5r", "65+ \u00E5r")

  par(mfrow=c(2,2),mar=c(2,2,2,.5), oma=c(0,0,2,0))


  for (i in 1:4) {

    d <- selectAgeGroups(d1,ageG=i,S=S)
    yrange <- max(d,na.rm=T)+(roundUpNice(max(d,na.rm=T))*.25)

    plot(c(d[2,30:52], d[2,1:29]), type="l", col="green", xlim=c(1,52), ylim=c(0,yrange),
         main=ageGroups[i], xlab="Ukenummer", ylab="Antall konsultasjoner",
         lwd=1, cex.lab=0.75,cex.main=0.85, axes = F)

    lines(c(d[3,30:52], d[3,1:29]), type="l", col="red", lwd=1)
    lines(c(d[4,30:52], d[4,1:29]), type="l", col="orange", lwd=1)
    lines(c(d[5,30:52], d[5,1:29]), type="l", col="purple", lwd=1)
    lines(c(d[6,30:52], d[6,1:29]), type="l", col="blue", lwd=1)
    lines(d[7,30:weeknow],type="l", col="black", lwd=2)

    q<-axis(1, at=1:52, labels=Ukenummer,las=1,cex.axis=0.7)
    axis(2,las=3, cex.axis=0.75)
    box(lwd=1)

    abline(v = c(22, 25), col = "black", lty = 2)
    abline(v = c(34, 40), col = "black", lty = 2)
  }
  mtext(paste(mytittle, Fylkename, "aldersfordelt",sep=", "), outer = TRUE, cex = 1,font=2)
  #box("figure", col="blue")
  #box("outer", lty="solid", col="green")

}
###############################################################################
