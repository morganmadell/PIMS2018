function[res] = FOI(x, cens, freq, data)

    m_0 = x(1);
    D_inf = x(2);
    D = x(3);
    n = x(4);
    lambda = x(5);
    alpha = x(6);
    
    data = log(data);
    
    delta = data(2:end)-data(1:end-1);
    t = transpose(1:length(data));
    
    res = sum(delta.^2 + 2.*delta.*(D_inf + D .* (t(2:end).^(n-1)-t(1:end-1).^(n-1))-lambda.*alpha) +...
        (D_inf+ D .*(t(2:end).^(n-1)-t(1:end-1).^(n-1))).^2 - 2.*lambda.*alpha.*...
        (D_inf + D .* (t(2:end).^(n-1)-t(1:end-1).^(n-1))) + lambda.^2.*alpha);
    
    m = sum(( - D_inf + m_0 -...
        D.*t(2:end).^n./n + lambda.*alpha.*t(2:end)-data(2:end)).^2);
    res = res+1000*m;
    
    if res < 0
        disp("Oh no!")
    end

end