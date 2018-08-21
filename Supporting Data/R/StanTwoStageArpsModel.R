StanTwoStageArpsModel <- "
data {
int <lower=1> N;
vector[N] t;
vector[N] q;
vector[N] Q;
real <lower=0> cumtodate;
real <lower=0> maxrate;
real <lower=0> qf;
}
parameters {
real <lower=0> q1;
real <lower=0, upper=q1> q2;
real D1;
real D2;
real <lower=0,upper=4> b1;
real <lower=0,upper=b1> b2;
real <lower=100> tBDF;
real <lower=0> s2;
//real <lower=0> s3;
}
transformed parameters {
vector[N] yhat;
//vector[N] Qhat;
for (i in 1:N) {
if (t[i]<tBDF) {
yhat[i] <- q1/((1.0 + b1*D1*t[i])^(inv(b1)));
//  Qhat[i] <- ((q1^b1)/(D1*(1.0-b1)))*(q1^(1.0-b1)-q[i]^(1.0-b1));
}
else {
yhat[i] <- q2/((1.0 + b2*D2*t[i])^(inv(b2)));
//  Qhat[i] <- ((q2^b2)/(D2*(1.0-b2)))*(q2^(1.0-b2)-q[i]^(1.0-b2));
}
}
}
model {
q1 ~ uniform(0,2*maxrate);
q2 ~ uniform(0,2*maxrate);
D1 ~ uniform(0,10);
D2 ~ uniform(0,10);
b1 ~ uniform(0,4);
b2 ~ uniform(0,4);
s2 ~ uniform(0,1e3);
//s3 ~ uniform(0,1e3);
tBDF ~ uniform(0,3650);
q ~ normal(yhat,sqrt(s2));
//Q ~ normal(Qhat,sqrt(s3));
}
generated quantities {
//  real EUR;
//  EUR <- fmax(cumtodate, ((qi^b)/(Di*(1.0-b)))*(qi^(1.0-b)-qf^(1.0-b)));
}
"
