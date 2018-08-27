for i = 1:length(loc)
    
    X = T{loc==i,4};
    data = T{loc==i,19};
    
    X = X(data~=0);
    data = data(data~=0);
    t = 0:length(X)-1;
    
    if length(data) < 100
        continue
    end
    
    figure()
    
    params = PSONonClassical(data);

    m_0 = params(1);
    D_inf = params(2);
    D = params(3);
    n = params(4);
    lambda = params(5);
    alpha = params(6);
    
    disp("Well: " + num2str(i))
    disp("# of Points: " + num2str(length(data)))
    disp("m_0: " + num2str(m_0))
    disp("D_inf: " + num2str(D_inf))
    disp("D: " + num2str(D))
    disp("n: " + num2str(n))
    disp("lambda: " + num2str(lambda))
    disp("alpha: " + num2str(alpha))
    
    i;
    
    BM = bm(0,1);
    
    hold on
    scatter(X, log(data), 'd')
    
    for j = 1:10
        
        if length(t)-1 > 0
            poi = poissrnd(alpha, 1, length(data)-1);
            poi = [0, poi];
            N = [];
            for k = 1:length(poi)
                N = [N,sum(poi(1:k))];
            end

        q = m_0 - D_inf.*t - D.*t.^n./n + lambda.*N;
    
    
        plot(X, q)
        end
    
    end
    
    drawnow()
    
    saveas(gcf, "Pictures/Well"+num2str(i)+"-10paths.png")
    
    pause(1)
    close()
    
end