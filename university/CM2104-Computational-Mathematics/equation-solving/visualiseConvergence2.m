function visualiseConvergence2(f, df, TOL, N0)
arr_size = 400;

unique_roots = [];
root_type_n = zeros(arr_size);
root_type_o = zeros(arr_size);
% rootType is used to map each value to a unique root

iterations_n_img = zeros(arr_size);
iterations_o_img = zeros(arr_size);
% iterations stores the number of iterations required to converge

a = linspace(-1,1,arr_size);
b = linspace(-1,1,arr_size);

for i = 1:arr_size
    for j = 1:arr_size
        x = complex(a(i), b(j));
        [value_n, converged_n, iterations_n] = Newton(f, df, x, TOL, N0);
        [value_o, converged_o, iterations_o] = Ostrowski(f, df, x, TOL, N0);
        
        iterations_n_img(i,j) = iterations_n;
        iterations_o_img(i,j) = iterations_o;
        
        [in_n, pos_n] = inArray(unique_roots, value_n, 1e-9);
        [in_o, pos_o] = inArray(unique_roots, value_o, 1e-9);
        % if it converged and it's a old root
        if converged_n && in_n
            root_type_n(i,j) = pos_n;
        % if it converged and it's a new root
        elseif converged_n && ~in_n
            unique_roots = [unique_roots value_n];
            root_type_n(i,j) = length(unique_roots);
        % if it didn't converge
        else
            root_type_n(i,j) = -1;
        end
        
        if converged_o && in_o
            root_type_o(i,j) = pos_o;
        elseif converged_o && ~in_o
            unique_roots = [unique_roots value_o];
            root_type_o(i,j) = length(unique_roots);
        else
            root_type_o(i,j) = -1;
        end
    end
end

[r, g, b] = nDistinctColours(length(unique_roots));

r_n = zeros(arr_size);
b_n = zeros(arr_size);
g_n = zeros(arr_size);

r_o = zeros(arr_size);
b_o = zeros(arr_size);
g_o = zeros(arr_size);

for i = 1:arr_size
    for j = 1:arr_size

        colour_index_n = root_type_n(i,j);
        colour_index_o = root_type_o(i,j);
        
        % if the colour index != 1 then it converges
        if colour_index_n ~= -1
             r_n(i,j) = r(colour_index_n);
             g_n(i,j) = g(colour_index_n);
             b_n(i,j) = b(colour_index_n);     
        end
        
        if colour_index_o ~= -1
             r_o(i,j) = r(colour_index_o);
             g_o(i,j) = g(colour_index_o);
             b_o(i,j) = b(colour_index_o);     
        end
    end
end

im_n = genImage(r_n, g_n, b_n, iterations_n_img, root_type_n);
im_o = genImage(r_o, g_o, b_o, iterations_o_img, root_type_o);

subplot(1,2,1);
imshow(im_n);
subplot(1,2,2);
imshow(im_o);

end

function image_final = genImage(r, g, b, iterations, roots)
    % generates an image from rgb values, number of iterations and whether
    % the function converged or not.
    
    image_roots = cat(3, r, g, b);
    image_iters = rescale(log(iterations));
    image_iters = cat(3, image_iters, image_iters, image_iters);

    mask = (roots == -1);
    image_final = (image_roots + image_iters)/2;
    image_final = imoverlay(image_final, mask, [0.5, 0.5, 0.5]);
end

function [inArray, pos] = inArray(array, value, tol)
    % Checks if the value is already in the array
    % Checks if the value is similar to other roots
    inArray = false;
    pos = 0;
    for i = 1:length(array)
        arrVal = array(i);
        if abs(value - arrVal) < tol
            inArray = true;
            pos = i;
            return
        end
    end
end

function [r,g,b] = nDistinctColours(n)
    %adapted from:
    %https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
    rgb = zeros(1,3,n);
    % generate a random start value between 0 and 1
    for i = 1:n
        h = rand(1,1);
        hsv = [h, 0.9, 1];
        rgb(:,:,i) = hsv2rgb(hsv);
    end
    
    r = reshape(rgb(:,1,:), [1,n]);
    g = reshape(rgb(:,2,:), [1,n]);
    b = reshape(rgb(:,3,:), [1,n]);
end

