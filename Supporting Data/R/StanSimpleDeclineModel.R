StanSimpleDeclineModel <- "
  data {
  int <lower=1> N;
  vector[N] t;
  vector[N] q;
  vector[N] Q;
  real cumtodate;
  real qf;
  }
  parameters {
  real Beta[2];
  real Alpha;
  real <lower=0> s2;
  real <lower=0> tBDF;
  }
  transformed parameters {
  vector[N] yhat;
  for (i in 1:N)
  yhat[i] <- Alpha+(log(t[i])-tBDF)*Beta[1+(log(t[i])>tBDF)];
  }
  model {
  log(q) ~ normal(yhat,sqrt(s2));
  s2 ~ uniform(0,1e3);
  tBDF ~ uniform(0,10);
  Alpha ~ normal(0,1000);
  Beta ~ normal(0,1000);
  }
  "