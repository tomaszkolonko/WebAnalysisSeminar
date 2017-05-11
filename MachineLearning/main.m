[~,~,sEXT,sNEU,sAGR,sCON,sOPN,cEXT ...
    ,cNEU,cAGR,cCON,cOPN,~,networkSize,betweenness ...
    ,nBetweenness,density,brokerage,nBrokerage,transitivity] = import_dataset('../Dataset/datasetExtracted/mypersonality_final_utf8_ok_cleaned.csv');
addpath('lib/libsvm-3.22/windows/');

% preprocess data
cOPN = convert_to_labels(cOPN);
cNEU = convert_to_labels(cNEU);
cEXT = convert_to_labels(cEXT);
cCON = convert_to_labels(cCON);
cAGR = convert_to_labels(cAGR);

% experiment iterations
iterations = 50;

% select independent variables
data = [networkSize nBetweenness density nBrokerage transitivity];

% remove invalid data
data(isnan(data)) = 0;

% select dependent variable
labels = cAGR;
contLabels = sAGR;

% remove duplicate samples
[data,ia,~] = unique(data(:, :),'rows');
labels = labels(ia);
contLabels = contLabels(ia);

% repeat several times
linCosts = zeros(iterations, 1);
svmAccuracies = zeros(iterations, 1);
tic;
for k=1:iterations
    fprintf('\n\nIteration %d\n', k);
    % split up into train and test
    fraction = 0.7;
    nrSamples = size(data, 1);
    dim = size(data, 2);
    trainIndices = rand(nrSamples, 1) < fraction;
    trainIndex = floor(fraction * nrSamples);
    trainData = data(trainIndices, :);
    testData = data(~trainIndices, :);
    trainLabels = labels(trainIndices);
    testLabels = labels(~trainIndices);
    contTrainLabels = contLabels(trainIndices);
    contTestLabels = contLabels(~trainIndices);
    
    % Apply normalization
    shift = min(trainData);
    scale = 1 ./ (max(trainData) - shift);
    
    trainData = bsxfun(@minus, trainData, shift);
    trainData = bsxfun(@times, trainData, scale);
    testData = bsxfun(@minus, testData, shift);
    testData = bsxfun(@times, testData, scale);
    
    %% Linear regression
    linData = [trainData ones(size(trainData, 1), 1)]; % add intercept term
    theta = linear_sgd( linData', contTrainLabels' );
    W = theta(1:dim); B = theta(dim+1);
    
    % make predictions
    predictions = W'* testData' + B;
    
    % calculate global cost
    linTestData = [testData ones(size(testData, 1), 1)];
    tx = theta' * linTestData';
    cost = sum((tx - contTestLabels').^2) / size(linTestData, 1);
    linCosts(k) = cost;
    
    %% SVM
    K = 10;
    gridResolution = 6;
    
    Cs = 2 .^ linspace(-3, 14, gridResolution);
    gammas = 2 .^ linspace(-10, 2, gridResolution);
    
    o = get_supported_options();
    options = [o.kernel.rbf, o.quiet, o.cachesize(8000)];
    
    [ bestC, bestGamma, ~ ] = grid_search_svm(trainData, trainLabels, options, Cs, gammas, K);
    
    % Train again with selected hyperparameters
    o = get_supported_options();
    options = [o.kernel.rbf, o.quiet, o.cachesize(8000), o.cost(bestC), o.kernel.gamma(bestGamma)];
    model = svmtrain(trainLabels, trainData, options);
    
    % Run svm on the unseen test set
    [~, testAccuracy, ~] = svmpredict(testLabels, testData, model, []);
    svmAccuracies(k) = testAccuracy(1);
    %fprintf('SVM Accuracy on test set using RBF kernel with C = %f and gamma = %f: %f\n', bestC, bestGamma, testAccuracy(1));
    toc;
    
end

figure();
plot(1:k, linCosts);
title('Linear regresssion costs over iterations');
xlabel('Iterations');
ylabel('Cost');

figure();
plot(1:k, svmAccuracies);
title('SVM accuracies over iterations');
xlabel('Iterations');
ylabel('Accuracy');


fprintf('Median linear regression cost per sample: %f\n', median(linCosts));

fprintf('Median SVM Accuracy on test set using RBF kernel: %f\n', median(svmAccuracies));
