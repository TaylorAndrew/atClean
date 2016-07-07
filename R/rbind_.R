rbind_ <- function(data1, data2) {
  nms1 <- names(data1)
  nms2 <- names(data2)
  if(mean(nms1%in%nms2)==1 & mean(nms2%in%nms1)==1) {
    out <- rbind(data1, data2)
  } else {
    data1[nms2[!nms2%in%nms1]] <- NA
    data2[nms1[!nms1%in%nms2]] <- NA
    out <- rbind(data1, data2)
  }
  return(out)
}
