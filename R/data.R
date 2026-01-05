#' cars99 Data Set
#'
#' This data set contains information on 109 vechicles from 1999.
#'
#' @format A tibble with 109 rows and 11 variables: 
#' \describe{
#'   \item{Model}{The vechicle model name.}
#'   \item{CityMPG}{The miles per gallon (MPG) for the vehicle in the city.}
#'   \item{HwyMPG}{The miles per gallon (MPG) for the vehicle on the highway}
#'   \item{FuelCap}{The fuel capacity of the vehicle in gallons.}
#'   \item{Weight}{The weight of the vehicle in lbs.}
#'   \item{FrontWt}{The front weight of the vehicle.}
#'   \item{Accel0_30}{The time it takes, in seconds, for the vehicle to 
#'   accelerate from 0 to 30 mph.}
#'   \item{Accel0_60}{The time it takes, in seconds, for the vehicle to 
#'   accelerate from 0 to 60 mph.}
#'   \item{QtrMile}{The time it takes, in seconds, for the vehicle to travel a 
#'   quarter of a mile.}
#' }
#'
"cars99"

#' ESL Mixture Data Set
#'
#' These are the data used in Figures 2.1-2.3, and elsewhere through the 
#' Elements of Statistical Learning book. 
#'
#' @format A list with 8 components The components are 
#' \describe{
#'   \item{x}{200 x 2 matrix of training predictors.}
#'   \item{y}{ class variable; logical vector of TRUES and FALSES - 100 of each.}
#'   \item{xnew}{matrix 6831 x 2 of lattice points in predictor space.}
#'   \item{prob}{vector of 6831 probabilities (of class TRUE) at each lattice point.}
#'   \item{marginal}{marginal probability at each lattice point.}
#'   \item{px1}{69 lattice coordinates for x.1.}
#'   \item{px2}{99 lattice values for x.2  (69*99=6831).}
#'   \item{means}{20 x 2 matrix of the mixture centers, first ten for one class, next ten for the other}
#' }
#'
"ESL.mixture"