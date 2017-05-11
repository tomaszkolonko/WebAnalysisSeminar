function [ theta ] = linear_sgd( data, labels )

nrSamples = size(data, 2);
% iterations
n = nrSamples * 10;
% learning rate
alpha = 0.01;

dim = size(data, 1);

theta = zeros(dim, 1);
errors = zeros(1, n);

% iterate
for i = 1:n
    % pick random sample
    index = floor( nrSamples * rand(1)) + 1;
    
    % calculate error
    tx = theta' * data(: , index);
    error = labels(index) - tx;
    
    theta =  theta + alpha * error * data( : , index);
    
%     %  calculate cost (all samples)
%     tx = theta' * data;
%     cost = sum((tx - labels).^2) / nrSamples;
%     errors(i) = cost;
end

% % plot objective function
% figure(6);
% plot(1:n, errors);
% title('number of iterations with cost');

end