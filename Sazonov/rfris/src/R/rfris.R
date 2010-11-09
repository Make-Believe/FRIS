fris_first <- function(Dist, NumObj, ParamR, MaxStolps) {
	a <- .C("fris_cluster", as.double(Dist), as.integer(NumObj), as.double(ParamR), as.integer(MaxStolps), stolps=integer(MaxStolps), QR=double(1), cluster=integer(NumObj), temp=integer(MaxStolps+2), tQR=as.double(-1))
	list(Dist=Dist, NumObj=NumObj, ParamR=ParamR, MaxStolps=MaxStolps, stolps=a$stolps, cluster=a$cluster, QR=a$QR, temp=a$temp, tQR=a$tQR)
}

fris_next <- function(b) {
	a <- .C("fris_cluster", as.double(b$Dist), as.integer(b$NumObj), as.double(b$ParamR), as.integer(b$MaxStolps), stolps=integer(b$MaxStolps), QR=double(1), cluster=integer(b$NumObj), temp=as.integer(b$temp), tQR=as.double(b$tQR))
	list(Dist=b$Dist, NumObj=b$NumObj, ParamR=b$ParamR, MaxStolps=b$MaxStolps, stolps=a$stolps, cluster=a$cluster, QR=a$QR, temp=a$temp, tQR=a$tQR)
}

fris_qr <- function(Dist, NumObj, Classif, Stolps, NumStolps) {
	.C("fris_qr", as.double(Dist), as.integer(NumObj), as.integer(Classif), as.integer(Stolps), as.integer(NumStolps), QR=double(1))$QR
}

fris_class <- function(Dist, NumObj, Classif, Stolps, NumStolps, ParamF, ParamA) {
	.C("fris_class", as.double(Dist), as.integer(NumObj), as.integer(Classif), as.integer(Stolps), as.integer(NumStolps), as.double(ParamF), as.double(ParamA), cluster=integer(NumObj))$cluster
}
