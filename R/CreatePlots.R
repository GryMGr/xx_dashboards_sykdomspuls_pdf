#' Ukenummer
#' @export Ukenummer
Ukenummer <- c(30:52,1:29)
###############################################################################
###############################################################################
#' Title
#'
#' @param x
#' @return
#' @export
#' @examples
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
###############################################################################
###############################################################################
#' CreatePlots1
#'
#' @param d
#' @param weeknow
#' @param Ukenummer
#' @param title
#' @param yrange
#' @import data.table
#' @return
#' @export
#'
#' @examples
CreatePlots1 <- function (d, weeknow, Ukenummer, title,yrange) {
  par(mfrow=c(1,1),oma = c(0, 0,2, 0))

  plot(c(d[1,30:52], d[2,1:29]), type="l", col="green", xlim=c(1,52), ylim=c(0,yrange),
       main="", xlab="Ukenummer", ylab="Antall konsultasjoner",
       lwd=4, cex.lab=1.6,cex.main=2,  xaxt="n")#, dev.new(width=18, height=14))

  lines(c(d[2,30:52], d[3,1:29]), type="l", col="red", lwd=4)
  lines(c(d[3,30:52], d[4,1:29]), type="l", col="orange", lwd=4)
  lines(c(d[4,30:52], d[5,1:29]), type="l", col="purple", lwd=4)
  lines(c(d[5,30:52], d[6,1:29]), type="l", col="blue", lwd=4)
  lines(c(d[6,30:52], d[7,1:weeknow]), type="l", col="black", lwd=6)

  q <-axis(1, at=1:52, labels=Ukenummer,las=2)
  abline(v = c(22, 25), col = "black", lty = 2)
  abline(v = c(34, 40), col = "black", lty = 2)
  text(23.5,0, "Jul/Nyttår", col = "black", cex=1.4)
  text(37,0, "Påske", col = "black", cex=1.4)

  q <-legend("topright", inset=.02,roundUpNice(yrange), legend=c("2012 / 2013", "2013 / 2014", "2014 / 2015", "2015 / 2016", "2016 / 2017", "2017 / 2018"),
             lty=1, col=c("green","red", "orange", "purple", "blue", "black"),
             lwd=c(4, 4, 4, 4, 4, 4), cex=1,box.lty=1, box.lwd=1,text.font=2,seg.len=2)
  mtext(title, outer = F, cex = 2,font=2,line = 1.5)

}
###############################################################################
##############################################################################
#' CreatePlots2
#'
#' @param d1
#' @param weeknow
#' @param Ukenummer
#' @param Fylkename
#' @import data.table
#' @return
#' @export
#'
#' @examples
CreatePlots2 <- function (d1, weeknow, Ukenummer, Fylkename,S) {

  ageGroups=c("0 - 4 år", "5 - 19 år", "20 - 64 år", "65+ år")

  par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
  for (i in 1:4) {

    d <- selectAgeGroups(d1,ageG=i,S=S)
    yrange <- max(d,na.rm=T)+(roundUpNice(max(d,na.rm=T))*.25)

    plot(c(d[1,30:52], d[2,1:29]), type="l", col="green", xlim=c(1,52), ylim=c(0,yrange),
         main=ageGroups[i], xlab="Ukenummer", ylab="Antall konsultasjoner",
         lwd=3, cex.lab=1.4,cex.main=1.7,  xaxt="n")

    lines(c(d[2,30:52], d[3,1:29]), type="l", col="red", lwd=3)
    lines(c(d[3,30:52], d[4,1:29]), type="l", col="orange", lwd=3)
    lines(c(d[4,30:52], d[5,1:29]), type="l", col="purple", lwd=3)
    lines(c(d[5,30:52], d[6,1:29]), type="l", col="blue", lwd=3)
    lines(c(d[6,30:52], d[7,1:weeknow]), type="l", col="black", lwd=5)

    q<-axis(1, at=1:52, labels=Ukenummer,las=2)
    abline(v = c(22, 25), col = "black", lty = 2)
    abline(v = c(34, 40), col = "black", lty = 2)
  }
  mtext(paste("Mage-tarminfeksjoner", Fylkename, "aldersfordelt",sep=", "), outer = TRUE, cex = 2,font=2)
}
###############################################################################
