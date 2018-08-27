function [params] = PSONonClassical(data)

    ResFunc = @(x) FOI(x, 0, 0, data);

    % options for the particle swarm
    options = optimoptions('particleswarm','SwarmSize', 512,...
        'HybridFcn',@fmincon);
    
    % begin optimizing
    params = particleswarm(ResFunc, 6, [0,0,0,10^-3,0,0],...
        [max(log(data)),100,100,1,10, 10], options);

end