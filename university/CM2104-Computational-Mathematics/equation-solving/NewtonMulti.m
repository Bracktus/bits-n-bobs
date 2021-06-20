function roots = NewtonMulti(f, df, p0, TOL, N0)
    syms x;
    
    % symbolic version of f 
    symbolic_f = sym(f);
    deg = polynomialDegree(symbolic_f);
    roots = zeros(1,deg);
    
    new_f = f;
    new_df = df;
     
    % for every power in the polynomial
    for i = 1:10
        root = Newton(new_f, new_df, p0, TOL, N0);
        roots(i) = root;
        p0 = root;
        
        % do the division of polynomial and get coeffiecients
        [poly_f, ~] = deconv(sym2poly(symbolic_f), [1 -root]);
        
        symbolic_f = poly2sym(poly_f);
        symbolic_df = simplify(diff(symbolic_f));
        if polynomialDegree(symbolic_f) == 0
            disp("deg = 0");
            break
        end
        
        new_f = matlabFunction(symbolic_f, 'Vars', x);
        new_df = matlabFunction(symbolic_df, 'Vars', x);
    end 
    
    points = linspace(10,-10,1000);
    func = zeros(1,1000);
    for i = 1:length(points)
        func(i) = f(points(i));
    end
    
    hold on
    plot(points, func);
    plot([-10 10], [0 0]);
    scatter(roots, zeros(1, length(roots)));
    xlim([-10 10]);
    hold off
end