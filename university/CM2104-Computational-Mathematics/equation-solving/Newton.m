% CM2208 Newton's Method
% Input: function f, df (derivative of f), initial guess p0, 
% tolerance (as relative error)
% N0 (max. iterations)
% Output: the value p
function [p, converged, iterations] = Newton(f, df, p0, TOL, N0)
%fprintf('%3d:%16.9f\n', 0, p0);
%Step 1:
i = 1;
%Step 2:
while i <= N0
   %Step 3:
   p = p0 - f(p0)/df(p0);
   %fprintf('%3d:%16.9f\n', i, p); 
   %Step 4:
   if abs(p0 - p) < TOL
       %fprintf('Solution found p = %g\n', p);
       converged = true;
       iterations = i;
       return
   end
   %Step 5:
   i = i + 1;
   %Step 6:
   p0 = p;  
end
%fprintf('Method failed after %d iterations\n', N0);
converged = false;
iterations = N0;
end