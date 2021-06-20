function p = RootFindingImproved(f, df, xMin, xMax, TOL, N0)

% run Bisection initalise
[xMin, xMax] = BisectionInitialise(f, xMin, xMax);

% run bisection 5 times
p = Bisection(f, xMin, xMax, TOL, 5);

% refine using Ostrowski
p = Ostrowski(f, df, p, TOL, N0);
end