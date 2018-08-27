function [params] = PSONonClassical(data)

    ResFunc = @(x) FOI(x, 0, 0, data);

    % options for the particle swarm
    options = optimoptions('particleswarm','SwarmSize', 512,...
        'HybridFcn',@fmincon);
    
    % begin optimizing
    params = particleswarm(ResFunc, 6, [-max(abs(log(data))), 0,0,10^-3,0, 0],...
        [max(abs(log(data))), 100,100,0.9,100, 1], options);

end