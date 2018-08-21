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
  real <lower=0, upper=8> b;
  real <lower=0> s2;
  #real <lower=0> s3;
  #real <lower=0> nu1;
  #real <lower=0> nu2;
}
transformed parameters {
  vector[N] qhat;
  #vector[N] Qhat;
  for (i in 1:N) {
    #qhat[i] = qi/((1.0 + b*Di*t[i])^(inv(b)));
    qhat[i] = exp(log(qi^(1-b) - (Q[i]-Q_prior)*Di*(1-b)/(qi^b))/(1-b));
    #Qhat[i] = (qi/(Di*(1.0-b)))*(1.0-((1.0 + b*Di*((t[i])-t_prior))^(1.0-inv(b))));
    #Qhat[i] = ((qi^b)/(Di*(1.0-b)))*(qi^(1.0-b)-q[i]^(1.0-b));
    }
}
model {
  qi ~ uniform(0,2*maxrate);
  Di ~ uniform(0.0, 1.0);
  b ~ uniform(0.01, 8);
  s2 ~ uniform(0, 1e4);
  #s3 ~ uniform(0,1e6);
  log(q) ~ normal(log(qhat),sqrt(s2));
  #log(q) ~ student_t(1,log(qhat),sqrt(s2));
  #(Q-Q_prior) ~ normal(Qhat,sqrt(s3));
  #(Q-Q_prior) ~ student_t(1,Qhat,sqrt(s3));
}
generated quantities {
  real EUR;
  EUR = fmax(cumtodate, ((qi^b)/(Di*(1.0-b)))*(qi^(1.0-b)-qf^(1.0-b))+Q_prior);
  #EUR = ((qi^b)/(Di*(1.0-b)))*(qi^(1.0-b)-qf^(1.0-b));
}

