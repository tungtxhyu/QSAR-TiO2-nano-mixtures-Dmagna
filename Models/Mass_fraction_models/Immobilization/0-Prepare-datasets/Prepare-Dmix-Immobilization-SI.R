


# Use this to replicate command for QM descriptors in Dmix 3
for (i in colnames(QMdesc)[-c(1:3)]) {
  print(paste("Dmix3$",i,"mix3 <- ((MeOx$x1/100*QMdesc$",i,"[a1])^2"," + (MeOx$x2/100*QMdesc$",i,"[a2])^2)^0.5", sep = ""))
}

# Use this to replicate command for QM descriptors in Dmix 4
for (i in colnames(QMdesc)[-c(1:3)]) {
  print(paste("Dmix4$",i,"mix4 <- (MeOx$x1/100)^0.5*QMdesc$",i,"[a1]"," + (MeOx$x2/100)^0.5*QMdesc$",i,"[a2]", sep = ""))
}


# Use this to replicate command for QM descriptors in Dmix 5
for (i in colnames(QMdesc)[-c(1:3)]) {
  print(paste("Dmix5$",i,"mix5 <- abs(QMdesc$",i,"[a1])^0.5"," + abs(QMdesc$",i,"[a2])^0.5", sep = ""))
}


# Use this to replicate command for QM descriptors in Dmix 6
for (i in colnames(QMdesc)[-c(1:3)]) {
  print(paste("Dmix6$",i,"mix6 <- MeOx$x1/100*(QMdesc$",i,"[a1])^2"," + MeOx$x2/100*(QMdesc$",i,"[a2])^2", sep = ""))
}


# Use this to replicate command for QM descriptors in Dmix 7
for (i in colnames(QMdesc)[-c(1:3)]) {
  print(paste("Dmix7$",i,"mix7 <- (MeOx$x1/100)^2*QMdesc$",i,"[a1]"," + (MeOx$x2/100)^2*QMdesc$",i,"[a2]", sep = ""))
}


# Use this to replicate command for QM descriptors in Dmix 8
for (i in colnames(QMdesc)[-c(1:3)]) {
  print(paste("Dmix8$",i,"mix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$",i,"[a1]"," + (MeOx$x2/100)^3*QMdesc$",i,"[a2])", sep = ""))
}


# Use this to replicate command for QM descriptors in Dmix 9
for (i in colnames(QMdesc)[-c(1:3)]) {
  print(paste("Dmix9$",i,"mix9 <- sqrt(MeOx$x1/100*abs(QMdesc$",i,"[a1])","*MeOx$x2/100*abs(QMdesc$",i,"[a2]))", sep = ""))
}

