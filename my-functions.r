## Escreve programas para pessoas e não para computadores

ping <- function(){
  print("PONG")
}

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

sum <- function(listOfNumbers) {
  result <- 0
  for (num in listOfNumbers) {
    result <- result + num
  }
  return (result)
}

avg <- function(listOfNumbers) {
  result <- 0
  for (num in listOfNumbers) {
    result <- result + num
  }
  size <- length(listOfNumbers)
  return (result/size)
}