%%

T = readtable('Viking/viking_prod_data.csv', 'Delimiter', ',');
%T.X_TD_TVD{i} = str2num(T.X_TD_TVD{i});

%%
    
[names, ~, loc] = unique(T.X_UWI_DISPLAY);

%%

pause on
figure
hold on

for i = 1:max(loc)
    
    X = T{loc==i,4};
    Y = T{loc==i,5:23};
    scatter(X,Y(:,6));
    
    pause(0.5)
    
    if mod(i, 10) == 0
        close()
        figure
        hold on
    end
    
end

%%

observances = [];

for i = 1:max(loc)
    
    observances = [observances,length(T{loc==i, 4})];
    
end

%%

X = T{loc==d+3,4};
data = T{loc==d+3,20};
t = 1:length(X);

params = PSORes(data);

m_0 = params(1);
D_inf = params(2);
D = params(3);
n = params(4);
epsilon = params(5);
lambda = params(6);

BM = bm(0,1);

%%

hold on

scatter(X, log(data), 'd')

for k = 1:3
    
    r = normrnd(0,1, 1);

    path = simByEuler(BM, length(data)-1);
    path = transpose(path);

    q = m_0 + epsilon.*r - D_inf.*t - D.*t.^n + lambda.*(path);
    
    scatter(X, q)
    
end

%%

for i = 1:length(loc)
    
    figure()
    
    X = T{loc==i,4};
    data = T{loc==i,20};
    t = 0:length(X)-1;
    
    params = PSORes(data);

    m_0 = params(1);
    D_inf = params(2);
    D = params(3);
    n = params(4);
    epsilon = params(5);
    lambda = params(6);
    
    scatter(t, log(data), 'd')
    
    BM = bm(0,1);
    
    hold on
    scatter(X, log(data), 'd')
    
    for j = 1:4
        
        path = simByEuler(BM,length(t)-1);
    
        r = normrnd(0,1);

        q = m_0 + epsilon.*r - D_inf.*t - D.*t.^n + (lambda.*path)./(1+t.^2);
    
    
        scatter(X, q)
    
        drawnow()
    end
    
    pause(1)
    close()
    
end
%%

for i = 1:length(loc)
    
    figure()
    
    X = T{loc==i,4};
    data = T{loc==i,20};
    X = X(data~=0);
    data = data(data~=0);
    
    params = PSORes(data);
    
    m_0 = params(1);
    D_inf = params(2);
    D = params(3);
    n = params(4);
    
    t = (1:length(data))-1;
    
    q = m_0 - D_inf.*t - D.*t.^n;
    
    hold on
    plot(X, q)
    scatter(X, log(data), 'd')
    drawnow()
    pause(1)
    close()
    
end

%%

