l = c(10, 1, 10, 10)
h = c(55, 50, 55, 1)

hackers_work <- function(l, h, method = "correct"){
  
  if(length(l) == length(h)) {
    n = length(l)
  } else {
    print("the vectors don't have the same lenght")
  }
  if(method == "correct"){
    output <- list()
    output[[1]] <- max(l[1], h[1])
    output_aux <- which.max(c(l[1] +  l[2], h[1] + l[2],  h[2]))
    if(output_aux == 1){
      output[[2]] <- c(l[1], l[2])
    } else{
      if(output_aux == 2) {
        output[[2]] <- c(h[1], l[2])
      } else{
        output[[2]] <- c(0, h[2])
      }
    }
    
    for(i in 3:n) {
      if(h[i] > output[[i - 1]][length(output[[i - 1]])] + l[i]) {
        output[[i]]   <- c(output[[i - 2]], 0, h[i])
      } else {
        output[[i]] <- c(output[[i - 1]], l[i])
      }
    }
    output <- output[[length(output)]]
  }
  if(method == "incorrect"){
    output = NULL
    l[n + 1] <- 0
    h[n + 1] <- 0
    a <- 0
    for(i in 1:n) {
      if(a == 1) {
        a <- 0
        next
      }
      if(h[i + 1] > l[i] + l[i + 1]) {
        output[i] <- 0
        output[i + 1] <- h[i + 1]
        a <- 1
        next
      } else {
        output[i] <- l[i]
        a <- 0
        next
      }
    }  
  }
  return(output)
}

hackers_work(l, h, "correct")