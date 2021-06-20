function visualiseConvergence1(f, df, TOL, N0)
x = linspace(-10,10,1000);

c1 = zeros(1,1000);
c2 = zeros(1,1000);

func = zeros(1,1000); 
for i = 1:length(x)

    [~, temp_c1] = Newton(f, df, x(i), TOL, N0);
    [~, temp_c2] = Ostrowski(f, df, x(i), TOL, N0);
    % c1/c2 checks if convergence occuured for a specific p0 value
    
    func(i) = f(x(i));
    % func is a sample of the function to plot later
    
    c1(i) = temp_c1;
    c2(i) = temp_c2;
end

% Plotting the first graph
subplot(1,2,1)
hold on
scatter(x(c1==true),func(c1==true), 'b');
scatter(x(c1~=true),func(c1~=true), 'r');

% Plotting the second graph
subplot(1,2,2)
hold on
scatter(x(c2==true),func(c2==true), 'b');
scatter(x(c2~=true),func(c2~=true), 'r');