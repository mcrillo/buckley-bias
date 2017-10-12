# general function

## point: vector of two numbers (longitude, latitude) 
## findin: a matrix of 2 columns (first one is longitude, second is latitude) 
## distance: if 0 finds nearest neighbour, if a positive value (in meters) finds neighbours within a circle with the value radius

## Returns a data.frame: 
## "row" = the row number of the neighbour in data.frame
## "distance" = the distance between the points

find_neighbours <- function(point, findin, distance) { # vector, data.frame, numeric
  
  dist_data <- apply(findin, 1, function(x) distCosine(point, x)) #Matrix
  
  if(distance>0) { # find neighbours within radius of distance
    neighb <- data.frame(row_findin = which(dist_data<=distance), distance = dist_data[which(dist_data<=distance)])
    if(length(neighb[,1])==0) distance = 0 
  }  
  
  if(distance==0) { # find nearest neighbour
    neighb <- data.frame(row_findin = which.min(dist_data), distance = min(dist_data))
  }
  
  return(neighb)
  
}

