for i = 1:length(loc)
    
    figure()
    
    X = T{loc==i,4};
    data = T{loc==i,20};
    t = 0:length(X)-1;
    
    params = PSONonClassical(data);

    m_0 = params(1);
    D_inf = params(2);
    D = params(3);
    n = params(4);
    epsilon = params(5);
    lambda = params(6);
    
    BM = bm(0,1);
    
    hold on
    scatter(X, log(data), 'd')
    
    for j = 1:4
        
        if length(t)-1 ~= 0
            path = simByEuler(BM,length(t)-1);
            path = transpose(path);
    
        r = normrnd(0,1);

        q = m_0 + epsilon.*r - D_inf.*t - D.*t.^n + (lambda.*path)./(1+t.^2);
    
    
        plot(X, q)
        end
    
    end
    
    drawnow()
    
    pause(1.5)
    close()
    
end