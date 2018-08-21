data {
  int <lower=1> N;
  vector[N] t;
  vector[N] q;
  vector[N] Q;
  real <lower=0> cumtodate;
  real <lower=0> maxrate;
  real <lower=0> qf;
  real <lower=0> t_prior;
  real <lower=0> Q_prior;
}
parameters {
  real <lower=0> qi;
  real <lower=0> Di;
  real <lower=0,upper=4> n;
  //real n;
  real <lower=0> Dinf;
  real <lower=0> s2;
}
transformed parameters {
  //int N_mid;
  //N_mid = round(N/2);
  vector[N] yhat;
  for (i in 1:N)
    yhat[i] = qi*exp(-Di*(t[i]^n) - Dinf*t[i]);
}
  //real <lower=0,upper=4> b;
  //b = -(Di*(n-1)*t[N]^n)/((Dinf*t[N] + Di*t[N]^n)^2);
model {
  qi ~ uniform(0,2*maxrate);
  Di ~ uniform(0,10);
  Dinf ~ uniform(0,10);
  n ~ uniform(0,4);
  //n ~ normal(0.75,4);
  s2 ~ uniform(0,1e3);
  log(q) ~ normal(log(yhat),sqrt(s2));
}
generated quantities {
  //real <lower=0,upper=4> b;
  real b;
  real b_mid;
  b = -(Di*(n-1)*t[N]^n)/((Dinf*t[N] + Di*t[N]^n)^2);
  b_mid = -(Di*(n-1)*t[(N/2)]^n)/((Dinf*t[(N/2)] + Di*t[(N/2)]^n)^2);
}
