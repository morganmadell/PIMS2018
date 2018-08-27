function [LL] = intmodel(q,t,params)
D_inf = params(1);
D = params(2);
n = params(3);
e = params(4);
m_0 = params(5);
lambda = params(6);
m = params(7);
%set the M ourselves, maybe 2000 or so
% M = 100;
N = length(t);

ind = 1;
G(ind) = 1/2*log(e^2*2*pi)+(log(q(1))-m_0)^2/(2*pi*e^2);
ind = 2:N;
G(ind) = 1/2*log(lambda^2*2*pi./(1+(t(ind-1)).^m).^2)+(log(q(ind))-log(q(ind-1))+D_inf+...
    D*n*t(ind).^(n-1)).^2.*(1+(t(ind-1)).^m).^2/(2*pi*lambda^2);
% ind = 1:N;
% G(ind) = ((m_0-D_inf*t(ind)-D*t(ind).^n-log(q(ind))).^2+e^2+(t(ind)*lambda^2)./((1+t(ind).^2).^2))...
%     ./((m_0-D_inf*t(ind)-D*t(ind).^n).^2+e^2+(t(ind)*lambda^2)./((1+t(ind).^2).^2)+(log(q(ind))).^2);
LL = sum(G);
end