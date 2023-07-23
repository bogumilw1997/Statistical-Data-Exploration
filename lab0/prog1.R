rm(list = c("a","b"))

a <- 10
a; typeof(a)

b <- 11L
b; typeof(b)

typeof(b+1L)

a <- 128 + 2i;a

sqrt(-1+0i)
1/0;exp(-Inf)
a <- 'Pakiet R'; a

z <- !0;z

a <- c(2, 181, 3.5, 1); a

a <- 1:10; a

a <- seq(2, 10, 2); a

a <- rep(seq(2,10,2), 3); a

a <- rep(c(seq(2,10,2),99, 93), 3); a

wyksztalcenie <- factor(c("podstawowe", "wyzsze", "srednie", "srednie", "wyzsze"))
wyksztalcenie

L <- list(inty = 1:10, x = 2.71, tekst = c("a", "b", "c"), log = rep(T, 5));L

A <- matrix(1:8, 4, 2); A


A <- array(1:20, dim = c(3,3,3)); A

ramka <- data.frame(liczby = 5:1, logiczne = T);ramka

w <- 1:10
w[1]

M <- matrix(1:9, 3, 3);M

M[1,]  

M[,1]

L <- list(inty = 1:10, x = 2.71, tekst = c("a", "b", "c"), log = rep(T, 5))
L$inty
L[[1]][1:2]

ramka$liczby

x <- c(1:5,0,5:1);x
mean(x)

cumsum(x)

y <- c(1, NA, 2, 5, 7)
which(y > 2)

x[x > mean(x)]; mean(x)

sum(x == 3)

x <- 1:10
ifelse(x %% 3, "Nie dzieli sie przez 3", "Dzieli sie przez 3");x

tabliczka_mnozenia <- function(zakres1, zakres2) {
  return(zakres1 %o% zakres2)
}
tabliczka_mnozenia(1:10,1:10)

w <- c(1,2)
v <- c(3,4)
A <- matrix(1:4, 2, 2)
B <- matrix(4:1, 2, 2)
w; v; A; B

apply(A, 1, sum);A

g <- function(x) {
  
  y <- sum(1:x)
  if(y > 2 * mean(x)) return(x)
  else return(0)
  
}
x <- c(1:10);x
sapply(x, g)

x <- 1:10
plot

x <- c(1,1,1,2,2,4,10)
h <- hist(x);h