ss.aipe.c <- function(error.variance=NULL, c.weights, width, conf.level=.95, assurance=NULL, certainty=NULL, MSwithin=NULL, 
SD=NULL, ...)
{ 
    
# Checks
####################################################################
if(is.null(error.variance) & is.null(MSwithin) & is.null(SD)) stop ("You must specify the variability of the contrast (e.g., 'error.variance', 'MSwithin', or 'SD'")

if(!is.null(assurance) & !is.null(certainty))
    {if(assurance!=certainty) stop("'assurance' and 'certainty' must have the same value")}
if(!is.null(certainty)) assurance <- certainty 

if(!is.null(error.variance) & !is.null(MSwithin)) {if (error.variance!=MSwithin) stop ("You provided discrepant information about the estimated standard deviation of the contrast")}
if(!is.null(error.variance) & !is.null(SD)) {if (error.variance!=SD^2) stop ("You provided discrepant information about the estimated standard deviation of the contrast")}
if(!is.null(MSwithin) & !is.null(SD)) {if(MSwithin!=SD^2) stop ("You provided discrepant information about the estimated standard deviation of the contrast")}

if(is.null(error.variance)& !is.null(MSwithin)) error.variance<- MSwithin
if(is.null(error.variance)& !is.null(SD)) error.variance<- SD^2

if(identical(sum(c.weights > 1), 0)) stop("Please use fractional values to specify the contrast weights (i.e., no contrast weights above 1.")
if(identical(sum(c.weights < -1), 0)) stop("Please use fractional values to specify the contrast weights (i.e., no contrast weights less than -1.")
if(!identical(round(sum(c.weights), 4), 0)) stop("The sum of the contrast weights ('c.weights') should equal zero.")

#####################################################################
alpha <- 1-conf.level
J <- length(c.weights)
sigma <- sqrt(error.variance) 

n <- (sigma^2 * 4* (qnorm(1-alpha/2))^2* sum(c.weights^2) ) / width^2
tol<- 1e-6
dif<- tol+1

if(is.null(assurance))
    { while (dif > tol)
        {n.p <- n
        n<- (sigma^2 *4* (qt(1-alpha/2, n*J-J))^2 * sum(c.weights^2) ) / width^2
        dif <- abs(n-n.p)
        }
    n<- ceiling(n)
    return(n)
    }

if(!is.null(assurance))
    { while(dif > tol)
        {n.p<-n
        n<- ((sigma^2 *4* (qt(1-alpha/2, n*J-J))^2 * sum(c.weights^2) ) / width^2) * ( qchisq(assurance, n*J-J) / (n*J-J) )
        dif <- abs(n-n.p)
        }
    n<- ceiling(n)
    return(n)
    }

}
