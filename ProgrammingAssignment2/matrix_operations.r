rowManip <- function(A, row1, row2, action=NA, scalar=1)  {
	#if(action==NA)  {
	#	return(A)
	#}
	if (action == "Switch")  {
		temp1<-A[row1,]
		A[row1,]<-A[row2,]
		A[row2,]<-temp1
		return(A)
	}
	if (action == "Scale")  {
		temp1<-A[row1,]
		A[row1,]<-(temp1*scalar)
		return(A)	
	}
	if (action == "Mult")  {
		temp1<-A[row1,]
		temp2<-A[row2,]
		A[row1,]<-(temp1 + scalar*temp2)
		return(A)	
	}
	else {return(A)
	}
}
