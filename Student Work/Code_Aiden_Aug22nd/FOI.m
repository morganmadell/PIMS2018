function[res] = FOI(x, cens, freq, data)

    m_0 = x(1);
    D_inf = x(2);
    D = x(3);
    n = x(4);
    epsilon = x(5);
    lambda = x(6);

    t = (1:length(data))-1;

    q = m_0 - D_inf.*t-(D.*(t).^n)./n;
    
    res = sum(((transpose(log(data))-q).^2 + epsilon^2 + (t.*lambda.^2)./(1+t.^2).^2)./...
        (q.^2 + epsilon.^2 + (t.*lambda.^2)./(1+t.^2).^2));

end