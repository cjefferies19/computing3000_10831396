% Clear and close previous results
clear
clc
close all

% Sets the centre points and covariance for each cluster
mean1 = [0, 2]; 
sigma1 = 0.8; 

mean2 = [1, -1];
sigma2 = 1;

mean3 = [-3, 4];
sigma3 = 1.4;

mean4 = [-1, -2];
sigma4 = 0.7;

% Determines the number of points each cluster has
pointNumbers = [1000, 1000, 1000, 1000]; 

% Calculating data clusters
data1 = (randn(pointNumbers(1), 2) .* sigma1) + mean1; 
data2 = (randn(pointNumbers(2), 2) .* sigma2) + mean2;
data3 = (randn(pointNumbers(3), 2) .* sigma3) + mean3;
data4 = (randn(pointNumbers(4), 2) .* sigma4) + mean4;

% Adding raw data to the first figure
figure(1); 
hold off;

% Plotting previously determined clusters into the same graph
plot(data1(:,1), data1(:,2), 'r.'); 
hold on;
plot(data2(:,1), data2(:,2), 'b.');
plot(data3(:,1), data3(:,2), 'g.');
plot(data4(:,1), data4(:,2), 'y.');

% 100x100 grid
gridSize = 100; 

% Determines the borders for the data
bor = linspace(-8, 8, gridSize); 

% Using the grid to create 2 matrices
[A, B] = meshgrid(bor, bor); 
mGrid = [A(:), B(:)]; 

% Calculates the density using gaussianProb
dense1 = gaussianProb(mGrid, mean1, sigma1); 
dense2 = gaussianProb(mGrid, mean2, sigma2); 
dense3 = gaussianProb(mGrid, mean3, sigma3); 
dense4 = gaussianProb(mGrid, mean4, sigma4); 

% Plots previous density values as contours
con1 = reshape(dense1, gridSize, gridSize);
con2 = reshape(dense2, gridSize, gridSize); 
con3 = reshape(dense3, gridSize, gridSize);
con4 = reshape(dense4, gridSize, gridSize);

% Contour lines plotted
contour(bor, bor, con1); 
contour(bor, bor, con2); 
contour(bor, bor, con3);
contour(bor, bor, con4);

% Axis limits and title
title('Starting Data');
axis([-8 8 -8 8])

% Combining all data points into one array
dataA = [data1; data2; data3; data4]; 
dataPoints = size(dataA, 1); 

% 4 clusters
clu = 4;
N = size(dataA, 1);
D = size(dataA, 2);

% Integers with random permutation
indices = randperm(dataPoints); 

% Matrixes and storing of original clusters
mu = dataA(indices(1:clu), :);
sigma = cell(1, clu); 


for j = 1:clu
    sigma{j} = cov(dataA); 
end

% Cluster weight
phi = ones(1, clu) * (1 / clu); 
W = zeros(dataPoints, clu); 

% 1000 iteration for loop
for iter = 1:1000

    % Probability Density Function store location
    pdf = zeros(dataPoints, clu); 
    
    
    for j = 1:clu

        % Calculating the probability of each data point
        pdf(:, j) = gaussianProb(dataA, mu(j, :), sigma{j}); 
    end
    
    pd_w = bsxfun(@times, pdf, phi); 
    W = bsxfun(@rdivide, pd_w, sum(pd_w, 2)); 

    % Calculating performance metrics log possibility
    performance = sum(log(sum(pd_w, 2)));

    % Stores the log
    performanceStore(iter) = performance;
    
    % Stores original means
    previousMu = mu; 
    
    
    for j = 1:clu

        % Calculates probability
        phi(j) = sum(W(:, j)) / dataPoints; 
        
        % Calculates mean
        mu(j, :) = sum(bsxfun(@times, W(:, j), dataA)) / sum(W(:, j)); 
        sigma_k = zeros(D, D); 

        % Calculates standard deviation
        data_m = bsxfun(@minus, dataA, mu(j, :)); 
        
        for i = 1:dataPoints
            % Calculates covariance matrix
            sigma_k = sigma_k + W(i, j) * (data_m(i, :)' * data_m(i, :)); 
        end
        sigma{j} = sigma_k / sum(W(:, j)); 
    end
    
    if norm(mu - previousMu) < 1e-6 
        break;
    end
end

% Graph to showcase performance
figure(2);
plot(performanceStore(1:iter));
ylabel('Log Likelihood');
title('Convergence');
xlabel('Iteration');
grid on;

% Creates new figure post GMM
figure(3); 
hold off;
plot(data1(:,1), data1(:,2), 'r.');
hold on;
plot(data2(:,1), data2(:,2), 'b.');
plot(data3(:,1), data3(:,2), 'g.');
plot(data4(:,1), data4(:,2), 'y.');

% Plots original clusters
plot(mu(1,1), mu(1,2), 'kx'); 
plot(mu(2,1), mu(2,2), 'kx');
plot(mu(3,1), mu(3,2), 'kx');
plot(mu(4,1), mu(4,2), 'kx');

% 100x100 grid 
gridSize = 100; 
bor = linspace(-8, 8, gridSize);
[A, B] = meshgrid(bor, bor);
mGrid = [A(:), B(:)];

% Using updated covariance values new contours are created
dense1 = gaussianProb(mGrid, mu(1, :), sigma{1}); 
dense2 = gaussianProb(mGrid, mu(2, :), sigma{2}); 
dense3 = gaussianProb(mGrid, mu(3, :), sigma{3}); 
dense4 = gaussianProb(mGrid, mu(4, :), sigma{4});

% Reshaped to prepare for contour plots
con1 = reshape(dense1, gridSize, gridSize); 
con2 = reshape(dense2, gridSize, gridSize);
con3 = reshape(dense3, gridSize, gridSize);
con4 = reshape(dense4, gridSize, gridSize);

% Plotting contours
contour(bor, bor, con1); 
contour(bor, bor, con2);
contour(bor, bor, con3);
contour(bor, bor, con4);

axis([-8 8 -8 8])
title('New Data');

% Calculating probability density
function pd = gaussianProb(data, mu, Sigma) 
    % n = data matrix column amount
    n = size(data, 2); 
    % Difference between mean and data point
    meanDiff = bsxfun(@minus, data, mu); 
    % Calculates the probability density
    pd = (1 / sqrt((2 * pi)^n * det(Sigma))) * exp(-0.5 * sum((meanDiff / Sigma) .* meanDiff, 2)); 
end