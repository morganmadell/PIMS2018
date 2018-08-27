function [res] = ClassicalFOI(x, cens, freq, data)

    m_0 = x(1);
    D_inf = x(2);
    D = x(3);
    n = x(4);
    
    t = (1:length(data))-1;
    
    q = m_0 - D_inf.*t-(D.*(t).^n);
    
    res = sum((q-transpose(log(data))).^2);

end