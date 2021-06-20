function [xMin, xMax] = BisectionInitialise(f, xMin, xMax)

if (xMin >= xMax)
    error('Incorrect interval. Ensure xMin < xMax');
end

smallRange = abs(xMax - xMin) / 2^20;
i = 0;
found = false;

while ~found
    minSign = f(xMin);
    maxSign = f(xMax);
            
    % if different sign
    if minSign * maxSign < 0
        found = true;
        fprintf('%d:%16.9f,%16.9f\n', i, xMin, xMax);
    else
        fprintf('%d:%16.9f,%16.9f\n', i, xMin, xMax);
        % change midpoint
        xMin = (xMin + xMax)/2;
        
        % if the range is 1/2^20 th of the original range.
        if abs(xMax - xMin) < smallRange
            fprintf("Couldn't find suitable range\n");
            break
        end
    end
    i = i + 1;
end

end