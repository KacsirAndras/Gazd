% STABILIS E.O
install.packages("stabledist")
library("stabledist")
%cauchy:
rstable(1000, 1, 1, gamma = 1, delta = 0, pm = 0)
%normális:
% alak, szimmertria, skála, hely
rstable(1000, 2, 1, gamma = 1, delta = 0, pm = 0)



% TÖBBDIMENZIÓS GAUSS
install.packages("MASS")
library("MASS")
mv = mvrnorm(100, mu=c(1,2,3), Sigma=diag(c(1,2,3)))



% regresszió
y = c(1,4,8,16,24,36,49,65)
x=c(1,2,3,4,5,6,7,8)
mo = lm(y~x+I(x^2))
summary(mo)

plot(x,y)
nx = seq(1,8,0.1)
p = predict(mo, newdata=data.frame(x=nx))
points(nx, p, type="l")


% likelihood
loglik = function(y, params) {
    l = sum(dweibull(y, scale=params[1], shape=params[2], log=TRUE))
    return (-l)
}

minta = rweibull(1000, scale=2.11, shape=1)

b = nlm(f=loglik, p=c(3,2), y=minta)
b


% LOGNORMAL
rlnorm


% neumann generátor
k = 4
from = k/2+1
to = 2*k-k/2
X = 23432345
Y = toString(X)
z = substr(X, from,to)
x2 = as.numeric(z)
x2

%poisson folyamat 
n = 10
rate = 1
t = rexp(n, rate=rate)
s = cumsum(t)
plot(s,1:n, type="s")
