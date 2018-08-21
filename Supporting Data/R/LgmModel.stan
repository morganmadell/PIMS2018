data {
  int <lower=1> N;
  vector[N] t;
  vector[N] q;
  vector[N] Q;
  real <lower=0> maxrate;
  real <lower=0> cumtodate;
  real <lower=0> qf;
  real <lower=0> t_prior;
  real <lower=0> Q_prior;
}
parameters {
  real <lower=0> a;
  real <lower=0> n;
  real <lower=0> K;
  real <lower=0> s2;
}
transformed parameters {
  //vector[N] qhat;
  vector[N] Qhat;
  //vector[N] that;
  //that = t - t_prior;
  for (i in 1:N) {
    //qhat[i] = (K*n*a*that[i]^(n-1))/((a+that[i]^n)^2);
    //Qhat[i] = (K*that[i]^n)/(a+that[i]^n);
    //qhat[i] = (K*n*a*t[i]^(n-1))/((a+t[i]^n)^2);
    //Qhat[i] = (K*t[i]^n)/(a+t[i]^n);
    Qhat[i] = Q_prior + K/((a*t[i]^(-n))+1);
    }
}
model {
  a ~ uniform(0.01, 200.0);
  n ~ uniform(0.01, 4.0);
  K ~ uniform(0.01, 50e6);
  s2 ~ uniform(0.01, 1e4);
  //s3 ~ uniform(0,1e6);
  //log(q) ~ normal(log(qhat),sqrt(s2));
  //log(q) ~ student_t(1,log(qhat),sqrt(s2));
  //(Q-Q_prior) ~ normal(Qhat,sqrt(s2));
  Q ~ normal(Qhat,sqrt(s2));
  //(Q-Q_prior) ~ student_t(1,Qhat,sqrt(s3));
}
generated quantities {
  real EUR;
  EUR = fmax(cumtodate, K+Q_prior);
}
