function [params] = PSORes(data)

    ResFunc = @(x) ClassicalFOI(x, 0, 0, data);

    % options for the particle swarm
    options = optimoptions('particleswarm','SwarmSize', 512,...
        'HybridFcn',@fmincon);
    
    % begin optimizing
    params = particleswarm(ResFunc, 4, [0,0,0,10^-3],...
        [inf,inf,inf,1], options);

end