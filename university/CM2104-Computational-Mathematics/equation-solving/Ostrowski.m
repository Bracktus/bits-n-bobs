% CM2208 Otrowski's Method
% Input: function f, df (derivative of f), initial guess p0, 
% tolerance (as relative error)
% N0 (max. iterations)
% Output: the value p
function [p, converged, iterations] = Ostrowski(f, df, p0, TOL, N0)
%fprintf('%3d:%16.9f\n', 0, p0); 
%Step 1:
i = 1;
%Step 2:
while i <= N0
   %Step 3:
   q = p0 - f(p0)/df(p0);
   term = (f(p0)-f(q))/(f(p0)-2*f(q));
   p = p0 - (f(p0)/df(p0) * term);
   %fprintf('%3d:%16.9f\n', i, p);  
   %Step 4:
   if abs(q - p) < TOL
       converged = true;
       iterations = i;
       fprintf('Solution found p = %g\n', p);       
       return
   end
   %Step 5:
   i = i + 1;
   %Step 6:
   p0 = p; 
end
iterations = N0;
converged = false;
%fprintf('Method failed after %d iterations\n', N0);
end