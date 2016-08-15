stack <- function(){
   it <- list()
   res <- list(
                push=function(x){
                  it[[length(it)+1]] <<- x
                },
                pop=function(){
                  val <- it[[length(it)]]
                  it <<- it[-length(it)]
                  return(val)
                },
                value=function(){
                  return(it)
                },
                lenofstack=function(){
                  return(length(it))
                }
                )
    class(res) <- "stack"
    res
}


print.stack <- function(x,...){
    #print(x$value())
    x$value()
 }
 
lengthstack<-function(x){
    x$lenofstack()
}


push <- function(stack,obj){
    stack$push(obj)
 }


pop <- function(stack){
    stack$pop()
 }
