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
  real <lower=0> tau;
  real <lower=0,upper=4> n;
  real <lower=0> s2;
}
transformed parameters {
  vector[N] qhat;
  //vector[N] Qhat;
  for (i in 1:N) {
  qhat[i] = qi*exp(-(t[i]/tau)^n);
  //  Qhat[i] = (qi*tau/n)*(tgamma(1.0/n)-tgamma(1.0/n,(t[i]/tau)^n));
  }
}
model {
  qi ~ uniform(0,2*maxrate);
  tau ~ uniform(0,40);
  n ~ uniform(0,4);
  s2 ~ uniform(0,1e3);
  log(q) ~ normal(log(qhat),sqrt(s2));
}
generated quantities {
    real EUR;
    EUR = ((qi^tau)/n)*tgamma(1.0/n)+Q_prior;
}