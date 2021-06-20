classdef coursework_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                        matlab.ui.Figure
        GridLayout                      matlab.ui.container.GridLayout
        LeftPanel                       matlab.ui.container.Panel
        SquaresButton                   matlab.ui.control.Button
        LinesButton                     matlab.ui.control.Button
        Squaresroot2Button              matlab.ui.control.Button
        NumberEditFieldLabel            matlab.ui.control.Label
        NumberEditField                 matlab.ui.control.NumericEditField
        LineSizeEditFieldLabel          matlab.ui.control.Label
        LineSizeEditField               matlab.ui.control.NumericEditField
        SquareSizeEditFieldLabel        matlab.ui.control.Label
        SquareSizeEditField             matlab.ui.control.NumericEditField
        PlankDistanceEditFieldLabel     matlab.ui.control.Label
        PlankDistanceEditField          matlab.ui.control.NumericEditField
        PlankNumberEditFieldLabel       matlab.ui.control.Label
        PlankNumberEditField            matlab.ui.control.NumericEditField
        LinesGridButton                 matlab.ui.control.Button
        SimilarLinesLabel               matlab.ui.control.Label
        SimilarNumEditField             matlab.ui.control.NumericEditField
        ClosestLinesLabel               matlab.ui.control.Label
        ClosestLinesEditField           matlab.ui.control.NumericEditField
        EstimationLabel                 matlab.ui.control.Label
        TrianglesButton                 matlab.ui.control.Button
        PlotCheckBox                    matlab.ui.control.CheckBox
        PlotAverageEstimationsButtonGroup  matlab.ui.container.ButtonGroup
        PiButton                        matlab.ui.control.RadioButton
        Root2Button                     matlab.ui.control.RadioButton
        PlotButton                      matlab.ui.control.Button
        NumberEditField_2Label          matlab.ui.control.Label
        plotNumEditField                matlab.ui.control.NumericEditField
        TurnplotoffforNumber10000Label  matlab.ui.control.Label
        RightPanel                      matlab.ui.container.Panel
        UIAxes                          matlab.ui.control.UIAxes
    end

    % Properties that correspond to apps with auto-reflow
    properties (Access = private)
        onePanelWidth = 576;
    end

    methods (Access = private)
        function [x1,x2,x3,x4,y1,y2,y3,y4] = genSquare(app,xMax, yMax, numThrows,hypLength)
            % Generates a square within the bounds from 2 corners.
            
            [x1,x2,y1,y2] = genLines(app,xMax,yMax,numThrows,hypLength);
            % get a load of random lines
            
            xMid = (x1+x2)/2;
            yMid = (y1+y2)/2;

            tmpx1 = x1 - xMid;
            tmpx2 = x2 - xMid;
            tmpy1 = y1 - yMid;
            tmpy2 = y2 - yMid;
            % moves the origin to the midpoint (temporarily)
            
            p3 = [0 -1; 1 0] * [tmpx1' tmpy1']';
            p4 = [0 -1; 1 0] * [tmpx2' tmpy2']';
            % rotate the vectors by 90 degrees (around the midpoint)
            
            x3 = p3(1,:) + xMid;
            y3 = p3(2,:) + yMid;
            x4 = p4(1,:) + xMid;
            y4 = p4(2,:) + yMid;
            % move the origin back to it's original positions
            setappdata(app.UIAxes, 'points', [x1;x2;x3;x4;y1;y2;y3;y4]);
        end
        
        function [x1,x2,y1,y2] = genLines(app,xMax,yMax,numThrows,length)
            % Generates a vector of random lines within the range
            
            needleSpawnLB = length;
            needleSpawnXUB = xMax - length;
            needleSpawnYUB = yMax - length;
            % Setting upper and lower bounds for needle generation
            
            x1 = (needleSpawnXUB - needleSpawnLB).*rand(1,numThrows) + needleSpawnLB;
            % generates inital x values within the range:
            y1 = (needleSpawnYUB - needleSpawnLB).*rand(1,numThrows) + needleSpawnLB;
            % same for y values
            
            angle = rand(1,numThrows)*2*pi;
            x2 = x1 + length*cos(angle);
            y2 = y1 + length*sin(angle);
            % picks a random angle and puts a point at the correct distance
            setappdata(app.UIAxes, 'points', [x1;x2;y1;y2]);
            setappdata(app.UIAxes, 'angles', angle);
        end
        
        function [num, xMax, yMax, plankDistance, squareDiagLen, lineLen, plotOn] = loadValues(app)
            %helper function for loading values
            num = app.NumberEditField.Value;
            lineLen = app.LineSizeEditField.Value;
            xMax = app.PlankDistanceEditField.Value * app.PlankNumberEditField.Value;
            yMax = app.PlankDistanceEditField.Value * app.PlankNumberEditField.Value;
            plankDistance = app.PlankDistanceEditField.Value;
            squareDiagLen = app.SquareSizeEditField.Value * sqrt(2);
            plotOn = app.PlotCheckBox.Value;
            
            % fixing the Xlim and Ylim
            xlim(app.UIAxes ,[0, xMax]);
            ylim(app.UIAxes, [0, yMax]);
        end
        
        function lengths = distfrompoint2line(~,p0,p1,p2)
            % gets the distance from p0 and the line defined by p1 and p2

            %moving vectors to the origin
            v1 = p0 - p1;
            v2 = p2 - p1;
            
            dots = dot(v1,v2);
            normsqrd = vecnorm(v2).^2;

            proport = dots./normsqrd;
            %when v1 is projected onto v2
            %the point at which it lands is a proportion of the vector
            %https://www.desmos.com/calculator/22jx7gdeym
            closest = zeros(2,length(p1));

            beforeLines = proport < 0;
            afterLines = proport > 1;
            betweenLines = (proport > 0) & (proport < 1);
            %Generating Logical Vectors

            closest(:,beforeLines) = p1(:,beforeLines);
            closest(:,afterLines) = p2(:,afterLines);
            closest(:,betweenLines) = p1(:,betweenLines) + v2(:,betweenLines) .* proport(betweenLines);

            shortestPath = p0 - closest;
            lengths = sqrt(dot(shortestPath,shortestPath));

%             for i = 1:length(closest)
%                 hold(app.UIAxes);
%                 plot(app.UIAxes,[closest(1,i),p0(1)],[closest(2,i),p0(2)], 'c');
%                 hold(app.UIAxes);
%             end
%             FOR VISUALISING DISTANCE VECTORS
        end

        function linesArr = getNSimilarLines(app,p1,p2)
            points = getappdata(app.UIAxes, 'points');
            angles = getappdata(app.UIAxes, 'angles');
            n = app.SimilarNumEditField.Value;
            % importing all of our points and angles
            x1 = points(1,:);
            
            index = p1(1) == x1;
            myAng = angles(index);
%           get the absolute difference between my angle and all the
%           other angles
            compAngles = abs(angles - myAng);
            [~, idx] = sort(compAngles);
            indexes = idx(2:n+1);
            linesArr = points(:,indexes);
        end
        
        function lengths = distfrompoint2ray(app, p0, p1, p2)
            %gets distance from point to a line's infinite ray
            v1 = p0 - p1;
            v2 = p2 - p1;
            
            dots = dot(v1,v2);
            normsqrd = vecnorm(v2).^2;
            proport = dots./normsqrd;
            
            closest = p1 + (v2 .* proport);
            shortestPath = p0 - closest;
            lengths = sqrt(dot(shortestPath,shortestPath));
            
%             for i = 1:length(closest)
%                 hold(app.UIAxes);
%                 plot(app.UIAxes,[closest(1,i),p0(1)],[closest(2,i),p0(2)], 'c');
%                 hold(app.UIAxes);
%             end
%             FOR VISUALISING DISTANCE VECTORS
        end
        
        function extendVectors(app,p1,p2)
            [~, xMax, ~, ~, ~, ~] = loadValues(app);
            lastPlottedRay = getappdata(app.UIAxes, 'lastExtended');
            delete(lastPlottedRay);
            
            v1 = p2 - p1;
            v2 = p1 - p2;
            
            newV1 = p1 + v1*xMax;
            newV2 = p2 + v2*xMax;
            
            hold(app.UIAxes);
            plotted = plot(app.UIAxes, [newV1(1,:);newV2(1,:)], [newV1(2,:);newV2(2,:)], 'black');
            hold(app.UIAxes);
            
            setappdata(app.UIAxes, 'lastExtended', plotted);
        end
        
        function [x1,x2,x3,y1,y2,y3]= genTriangle(app,xMax,numThrows,length)
            [x1,x2,y1,y2] = genLines(app,xMax,xMax,numThrows,length);
            p1 = [x1;y1];
            p2 = [x2;y2];
            v1 = p2 - p1;
            
            rotMat = [cosd(60) -sind(60); sind(60) cosd(60)];
            v2 = rotMat * v1;
            
            x3 = v2(1,:) + p1(1,:);
            y3 = v2(2,:) + p1(2,:);
            
            setappdata(app.UIAxes, 'points', [x1;x2;x3;y1;y2;y3]);
            
        end
        
        function piEst = estPiNoPlot(app, numThrows)
            cla(app.UIAxes);
            lineLen = 10;
            xMax = 100;
            plankDistance = 20;
            [x1,x2,~,~] = genLines(app, xMax, xMax, numThrows, lineLen);
            bigX = max([x1;x2]);
            smallX = min([x1;x2]);
            % get the start xVal and end xVal 
            intersects = zeros(length(numThrows));
            
            verts = 0:plankDistance:xMax;
            for i = 1:length(verts)
                condit1 = bigX > verts(i); % array of 1 or 0 if true or false
                condit2 = smallX < verts(i); % same as above
                intersects = intersects + (condit1 == condit2);
                % If the vertical line is between the start and x values count+= 1
                xline(app.UIAxes, verts(i)); 
            end
            count = nnz(intersects);
            prob = count/numThrows;
            piEst = (2 * lineLen)/(plankDistance*prob);
        end
        
        function root2Est = estRoot2NoPlot(app,num)
            squareDiagLen = 10;
            xMax = 100;
            plankDistance = 20;
            [x1,x2,x3,x4,~,~,~,~] = genSquare(app, xMax, xMax, num, squareDiagLen);
            
            xValues = [x1;x2;x3;x4];
            xValues = sort(xValues);
            % Matrix of xValues, Each column represents a square's x-cords
            countA = 0;
            countB = 0;
            intersects = zeros(length(num));
            
            % need to see how many consective lines are crossed.
            verts = 0:plankDistance:xMax;
            for i = 1:length(verts)
                con1 = xValues(1,:) > verts(i); % array of 1 or 0 if true or false
                con4 = xValues(4,:) < verts(i); % same
                countA = countA + nnz((con1 == con4)); % if 1 in both cols += 1
                intersects = intersects + (con1 == con4);
                % if it's touching a line in any way 
            
                con2 = xValues(2,:) > verts(i);
                con3 = xValues(3,:) < verts(i);
                countB = countB + nnz((con2 == con3));
                % if it's inbetween the 2 middle points
                % then the line wouldn't be touching consecutive sides
                % instead it's touching opposite sides         
                xline(app.UIAxes, verts(i));
            end
            probA = countA/num;
            probB = (countA - countB)/num;
            root2Est = 2 - probB/probA;
        end
    end
    
    methods (Access = public)
        function lineCallback(app,src,~)
            points = getappdata(app.UIAxes, 'points');
            %Triggers when a line is clicked
            dataX = src.XData;
            dataY = src.YData;
   
            lastPlottedLine = getappdata(app.UIAxes, 'lastPlottedLine');
            delete(lastPlottedLine);
            lastsimilarLine = getappdata(app.UIAxes, 'lastSimilarLine');
            delete(lastsimilarLine);
            lastClosestRays = getappdata(app.UIAxes, 'lastClosestLine');
            delete(lastClosestRays);
            %Deleting the latest plots

            hold(app.UIAxes);
            plotted = plot(app.UIAxes, [dataX(1);dataX(2)], ... 
                                       [dataY(1);dataY(2)],'blue');
            hold(app.UIAxes);
            %Higlighting our selected line
            
            similar = getNSimilarLines(app, [dataX(1);dataY(1)],[dataX(2);dataY(2)]);
            simx1 = similar(1,:);
            simx2 = similar(2,:);
            simy1 = similar(3,:);
            simy2 = similar(4,:);
            hold(app.UIAxes);
            plotted2 = plot(app.UIAxes,[simx1;simx2],[simy1;simy2], 'magenta');
            hold(app.UIAxes);
            %Higlighting our N most similar lines
            
            x1 = points(1,:);
            x2 = points(2,:);
            y1 = points(3,:);
            y2 = points(4,:);
            
            distances1 = distfrompoint2ray(app,[dataX(1);dataY(1)],[x1;y1],[x2;y2]);
            distances2 = distfrompoint2ray(app,[dataX(2);dataY(2)],[x1;y1],[x2;y2]);
            
            rayDistances = min([distances1;distances2]);
            [~, rayIndexes] = sort(rayDistances);
            rayNum = app.ClosestLinesEditField.Value + 1;
            rayIndexes = rayIndexes(2:rayNum);
            rayp1 = [x1(rayIndexes);y1(rayIndexes)];
            rayp2 = [x2(rayIndexes);y2(rayIndexes)];
            
            extendVectors(app,rayp1,rayp2);
            %Coordinates of the closest N ray lines
            
            hold(app.UIAxes);
            plotted3 = plot(app.UIAxes, [rayp1(1,:);rayp2(1,:)], [rayp1(2,:);rayp2(2,:)],'yellow');
            hold(app.UIAxes);
            setappdata(app.UIAxes, 'lastClosestLine', plotted3);
            setappdata(app.UIAxes, 'lastSimilarLine', plotted2);
            setappdata(app.UIAxes, 'lastPlottedLine', plotted);
        end
    end

    % Callbacks that handle component events
    methods (Access = private)

        % Button pushed function: SquaresButton
        function SquaresButtonPushed(app, event)
            app.EstimationLabel.Text = "Estimating pi...";
            [num, xMax, yMax,plankDistance,squareDiagLen, ~, plotOn] = loadValues(app);
            [x1,x2,x3,x4,y1,y2,y3,y4] = genSquare(app, xMax, yMax, num, squareDiagLen);
            cla(app.UIAxes);
            bigX = max(max(x1,x2),max(x3,x4));
            smallX = min(min(x1,x2),min(x3,x4));
            % Getting the smallest and largest x values
            line1 = [max(x1,x3); min(x1,x3)];
            line2 = [max(x3,x2); min(x3,x2)];
            line3 = [max(x4,x2); min(x4,x2)];
            line4 = [max(x4,x1); min(x4,x1)];
            % long needle case checks lines instead of square so need to extract lines
            
            intersects = zeros(length(num));
            %preallocating short needle case
            
            intersects1 = zeros(length(num));
            intersects2 = zeros(length(num));
            intersects3 = zeros(length(num));
            intersects4 = zeros(length(num));
            %preallocating long needle case
            
            verts = 0:plankDistance:xMax;
            squareLen = squareDiagLen/sqrt(2);
            for i = 1:length(verts)
                if squareLen < plankDistance
                    %short needle (treat square as square)
                    condit1 = bigX > verts(i); % array of 1 or 0 if true or false
                    condit2 = smallX < verts(i); % same as above
                    intersects =  intersects + (condit1 == condit2);
                    % If the vertical line is between the start and end x values then count+= 1
                else 
                    %long needle (treat square as 4 lines)
                    line1Con1 = line1(1,:) > verts(i);
                    line1Con2 = line1(2,:) < verts(i);
                    intersects1 = intersects1 + (line1Con1 == line1Con2);
                    
                    line2Con1 = line2(1,:) > verts(i);
                    line2Con2 = line2(2,:) < verts(i);
                    intersects2 = intersects2 + (line2Con1 == line2Con2);
                    
                    line3Con1 = line3(1,:) > verts(i);
                    line3Con2 = line3(2,:) < verts(i);
                    intersects3 = intersects3 + (line3Con1 == line3Con2);
                    
                    line4Con1 = line4(1,:) > verts(i);
                    line4Con2 = line4(2,:) < verts(i);
                    intersects4 = intersects4 + (line4Con1 == line4Con2);
                    
                    intersects = nnz(intersects1 + intersects2 + intersects3 + intersects4);
                end
                xline(app.UIAxes, verts(i));
            end
            intersects = logical(intersects);
            
            if plotOn
                %converting into logical array
                plot1x1 = x1(intersects);
                plot1x2 = x2(intersects);
                plot1x3 = x3(intersects);
                plot1x4 = x4(intersects);
                
                plot1y1 = y1(intersects);
                plot1y2 = y2(intersects);
                plot1y3 = y3(intersects);
                plot1y4 = y4(intersects);
                %intersecting squares
                
                plot2x1 = x1(~intersects);
                plot2x2 = x2(~intersects);
                plot2x3 = x3(~intersects);
                plot2x4 = x4(~intersects);
                
                plot2y1 = y1(~intersects);
                plot2y2 = y2(~intersects);
                plot2y3 = y3(~intersects);
                plot2y4 = y4(~intersects);
                %non-intersecting squares
    
                hold(app.UIAxes);
                plot(app.UIAxes,[plot1x1;plot1x4;plot1x2;plot1x3;plot1x1], ...
                                [plot1y1;plot1y4;plot1y2;plot1y3;plot1y1], 'r');
                
                plot(app.UIAxes,[plot2x1;plot2x4;plot2x2;plot2x3;plot2x1], ...
                                [plot2y1;plot2y4;plot2y2;plot2y3;plot2y1],'g'); 
                hold(app.UIAxes);
            end

            if squareLen < plankDistance
                %short needle
                count = nnz(intersects);
                prob = count/num;
                piEst = (4 * squareLen)/(plankDistance*prob);
            else
                %long needle
                count = nnz(intersects1) + nnz(intersects2) + nnz(intersects3) + nnz(intersects4);
                prob = count/(num * 4);
                l = squareLen;
                t = plankDistance;
                x = l/t;
                piEst = (2 * (x - sqrt(x^2 -1) + asec(x)))/ prob;
            end
                string = sprintf("PI Estimate: %f", piEst);
                %string = sprintf("count: %f", count);
                %msgbox(string, 'PI estimate');
                app.EstimationLabel.Text = string;
        end

        % Button pushed function: LinesButton
        function LinesButtonPushed(app, event)
            app.EstimationLabel.Text = "Estimating pi...";
            [num, xMax, yMax, plankDistance, ~, lineLen, plotOn] = loadValues(app);
            [x1,x2,y1,y2] = genLines(app, xMax, yMax, num, lineLen);
            % plotting stuff
            cla(app.UIAxes);
            hold(app.UIAxes);
            
            bigX = max([x1;x2]);
            smallX = min([x1;x2]);
            % get the start xVal and end xVal 
            intersects = zeros(length(num));
            
            verts = 0:plankDistance:xMax;
            for i = 1:length(verts)
                condit1 = bigX > verts(i); % array of 1 or 0 if true or false
                condit2 = smallX < verts(i); % same as above
                intersects = intersects + (condit1 == condit2);
                % If the vertical line is between the start and x values count+= 1
                xline(app.UIAxes, verts(i)); 
            end
            count = nnz(intersects);
            intersects = logical(intersects);
            
            if plotOn
                plot1x1 = x1(intersects);
                plot1x2 = x2(intersects);
                plot1y1 = y1(intersects);
                plot1y2 = y2(intersects);
                %if they intersect
                
                plot2x1 = x1(~intersects);
                plot2x2 = x2(~intersects);
                plot2y1 = y1(~intersects);
                plot2y2 = y2(~intersects);
                %if they don't
                
                plot(app.UIAxes,[plot2x1;plot2x2],[plot2y1;plot2y2], 'g','ButtonDownFcn',@app.lineCallback);
                plot(app.UIAxes,[plot1x1;plot1x2],[plot1y1;plot1y2], 'r','ButtonDownFcn',@app.lineCallback);
            end
            
            hold(app.UIAxes);
            prob = count/num;

            if lineLen <= plankDistance
                piEst = (2 * lineLen)/(plankDistance*prob);
            else
                l = lineLen;
                t = plankDistance;
                x = l/t;
                piEst = (2 * (x - sqrt(x^2 -1) + asec(x)))/ prob;
            end
                string = sprintf("PI Estimate: %f", piEst);
                %msgbox(string, 'PI estimate');
                app.EstimationLabel.Text = string;
        end

        % Button pushed function: Squaresroot2Button
        function Squaresroot2ButtonPushed(app, event)
            app.EstimationLabel.Text = "Estimating Sqrt(2)...";
            [num, xMax, yMax, plankDistance, squareDiagLen, ~, plotOn] = loadValues(app);
            [x1,x2,x3,x4,y1,y2,y3,y4] = genSquare(app, xMax, yMax, num, squareDiagLen);
            if squareDiagLen >= plankDistance
                errordlg('Square is too large, unable to compute root 2', 'Error');
                return;
            end
            hold(app.UIAxes);
            cla(app.UIAxes);
            
            xValues = [x1;x2;x3;x4];
            xValues = sort(xValues);
            % Matrix of xValues, Each column represents a square's x-cords
            countA = 0;
            countB = 0;
            intersects = zeros(length(num));
            
            % need to see how many consective lines are crossed.
            verts = 0:plankDistance:xMax;
            for i = 1:length(verts)
                
                con1 = xValues(1,:) > verts(i); % array of 1 or 0 if true or false
                con4 = xValues(4,:) < verts(i); % same
                countA = countA + nnz((con1 == con4)); % if 1 in both cols += 1
                intersects = intersects + (con1 == con4);
                % if it's touching a line in any way 
            
                con2 = xValues(2,:) > verts(i);
                con3 = xValues(3,:) < verts(i);
                countB = countB + nnz((con2 == con3));
                % if it's inbetween the 2 middle points
                % then the line wouldn't be touching consecutive sides
                % instead it's touching opposite sides         
                xline(app.UIAxes, verts(i));
            end
            
            intersects = logical(intersects);
            if plotOn
                %converting into logical array
                plot1x1 = x1(intersects);
                plot1x2 = x2(intersects);
                plot1x3 = x3(intersects);
                plot1x4 = x4(intersects);
                
                plot1y1 = y1(intersects);
                plot1y2 = y2(intersects);
                plot1y3 = y3(intersects);
                plot1y4 = y4(intersects);
                %intersecting squares
                
                plot2x1 = x1(~intersects);
                plot2x2 = x2(~intersects);
                plot2x3 = x3(~intersects);
                plot2x4 = x4(~intersects);
                
                plot2y1 = y1(~intersects);
                plot2y2 = y2(~intersects);
                plot2y3 = y3(~intersects);
                plot2y4 = y4(~intersects);
                %non-intersecting squares
    
                plot(app.UIAxes,[plot1x1;plot1x4;plot1x2;plot1x3;plot1x1], ...
                                [plot1y1;plot1y4;plot1y2;plot1y3;plot1y1], 'r');
                
                plot(app.UIAxes,[plot2x1;plot2x4;plot2x2;plot2x3;plot2x1], ...
                                [plot2y1;plot2y4;plot2y2;plot2y3;plot2y1],'g');
            end
            hold(app.UIAxes);
            probA = countA/num;
            probB = (countA - countB)/num;
            root2Est = 2 - probB/probA;
            string = sprintf("SQRT(2) Estimate: %f", root2Est);
            %msgbox(string, 'SQRT estimate');
            app.EstimationLabel.Text = string;
        end

        % Button pushed function: LinesGridButton
        function LinesGridButtonPushed(app, event)
            app.EstimationLabel.Text = "Estimating pi...";
            [num, xMax, yMax, plankDistance, ~, lineLen, plotOn] = loadValues(app);
            if lineLen >= plankDistance
                errordlg('Line is too large, unable to compute pi', 'Error');
                return;
            end
            cla(app.UIAxes);
            hold(app.UIAxes);
            
            [x1,x2,y1,y2] = genLines(app, xMax, yMax, num, lineLen);
            bigX = max([x1;x2]);
            smallX = min([x1;x2]);
            bigY = max([y1;y2]);
            smallY = min([y1;y2]);

            verts = 0:plankDistance:xMax;
            Xintersects = zeros(length(num));
            Yintersects = zeros(length(num));
            for i = 1:length(verts)
                condit1 = bigX > verts(i); % array of 1 or 0 if true or false
                condit2 = smallX < verts(i); % same as above
                Xintersects = Xintersects + (condit1 == condit2);
                % If the horizontal line is between the start and end X values count+= 1
                
                condit3 = bigY > verts(i);
                condit4 = smallY < verts(i);
                Yintersects = Yintersects + (condit3 == condit4);
                % If the vertical line is between the start and end Y values count+= 1
                
                xline(app.UIAxes, verts(i)); 
                yline(app.UIAxes, verts(i));
            end
            totalIntersects = Xintersects + Yintersects;
            intersects = logical(totalIntersects);
            if plotOn
                plot1x1 = x1(intersects);
                plot1x2 = x2(intersects);
                plot1y1 = y1(intersects);
                plot1y2 = y2(intersects);
                
                plot2x1 = x1(~intersects);
                plot2x2 = x2(~intersects);
                plot2y1 = y1(~intersects);
                plot2y2 = y2(~intersects);
                
                plot(app.UIAxes,[plot2x1;plot2x2],[plot2y1;plot2y2], 'g', 'ButtonDownFcn',@app.lineCallback);
                plot(app.UIAxes,[plot1x1;plot1x2],[plot1y1;plot1y2], '-r', 'ButtonDownFcn',@app.lineCallback);
            end
            hold(app.UIAxes);
            
            count = nnz(totalIntersects);
            prob = count/num;
            piEst = ((4*lineLen*plankDistance) - lineLen^2)/(prob * plankDistance^2);
            string = sprintf("PI Estimate: %f", piEst);
            %string = sprintf("count: %f", count);
            %msgbox(string, 'PI estimate');
            app.EstimationLabel.Text = string;
            
        end

        % Button down function: UIAxes
        function UIAxesButtonDown(app, event)
            points = getappdata(app.UIAxes, 'points');
            lastPlottedLine = getappdata(app.UIAxes, 'lastPlottedLine');
            delete(lastPlottedLine);
            lastsimilarLine = getappdata(app.UIAxes, 'lastSimilarLine');
            delete(lastsimilarLine);
            lastClosestRays = getappdata(app.UIAxes, 'lastClosestLine');
            delete(lastClosestRays);
            
            cords = get(app.UIAxes, 'CurrentPoint');
            x = ones(1, width(points)) * cords(1);
            y = ones(1, width(points)) * cords(3);

            if height(points) == 4
                %if lines are being plotted
                x1 = points(1,:);
                x2 = points(2,:);
                y1 = points(3,:);
                y2 = points(4,:);
                
                p0 = [x;y];
                p1 = [x1;y1];
                p2 = [x2;y2];

                distances = distfrompoint2line(app,p0,p1,p2);

                %you can return indexes when sorting 
                [~, indexes] = sort(distances);
                
                myLinep1 = [x1(indexes(1));y1(indexes(1))];
                myLinep2 = [x2(indexes(1));y2(indexes(1))];
                %Coordinates of the closest line
                hold(app.UIAxes);
                
                plotted = plot(app.UIAxes, [x1(indexes(1));x2(indexes(1))], ... 
                                           [y1(indexes(1));y2(indexes(1))],'blue');
                hold(app.UIAxes);
                %Plotting the closest line
                
                rayDistances1 = distfrompoint2ray(app,myLinep1,p1,p2);
                rayDistances2 = distfrompoint2ray(app,myLinep2,p1,p2);
                rayDistances = min([rayDistances1;rayDistances2]);
                [~, rayIndexes] = sort(rayDistances);
                rayNum = app.ClosestLinesEditField.Value + 1;
                rayIndexes = rayIndexes(2:rayNum);
                rayp1 = [x1(rayIndexes);y1(rayIndexes)];
                rayp2 = [x2(rayIndexes);y2(rayIndexes)];
                % Getting Coordinates of the closest N rays
                
                extendVectors(app,rayp1,rayp2);
                hold(app.UIAxes);
                plotted3 = plot(app.UIAxes, [rayp1(1,:);rayp2(1,:)], [rayp1(2,:);rayp2(2,:)],'yellow');
                %plotting N closest rays
                hold(app.UIAxes);
                
                similar = getNSimilarLines(app, myLinep1, myLinep2);
                simx1 = similar(1,:);
                simx2 = similar(2,:);
                simy1 = similar(3,:);
                simy2 = similar(4,:);
                %obtaining the N most similar lines 
                hold(app.UIAxes);
                plotted2 = plot(app.UIAxes,[simx1;simx2],[simy1;simy2], 'magenta');
                hold(app.UIAxes);
                %Plotting the N most similar lines
                setappdata(app.UIAxes, 'lastSimilarLine', plotted2);
                setappdata(app.UIAxes, 'lastPlottedLine', plotted);
                setappdata(app.UIAxes, 'lastClosestLine', plotted3);
                
            end
        end

        % Button pushed function: TrianglesButton
        function TrianglesButtonPushed(app, event)
            app.EstimationLabel.Text = "Estimating pi...";
            [num, xMax, ~, plankDistance, ~, lineLen, plotOn] = loadValues(app);
            if lineLen >= plankDistance
                errordlg('Triangle is too large, unable to compute pi', 'Error');
                return;
            end
            [x1,x2,x3,y1,y2,y3] = genTriangle(app, xMax, num, lineLen);
            % plotting stuff
            cla(app.UIAxes);
            hold(app.UIAxes);
            
            bigX = max([x1;x2;x3]);
            smallX = min([x1;x2;x3]);
            % get the start xVal and end xVal 
            intersects = zeros(length(num));
            
            verts = 0:plankDistance:xMax;
            for i = 1:length(verts)
                condit1 = bigX > verts(i); % array of 1 or 0 if true or false
                condit2 = smallX < verts(i); % same as above
                intersects = intersects + (condit1 == condit2);
                % If the vertical line is between the start and x values count+= 1
                xline(app.UIAxes, verts(i)); 
            end
            count = nnz(intersects);
            intersects = logical(intersects);
            
            if plotOn
                plot1x1 = x1(intersects);
                plot1x2 = x2(intersects);
                plot1x3 = x3(intersects);
                plot1y1 = y1(intersects);
                plot1y2 = y2(intersects);
                plot1y3 = y3(intersects);
                %if they intersect
                
                plot2x1 = x1(~intersects);
                plot2x2 = x2(~intersects);
                plot2x3 = x3(~intersects);
                plot2y1 = y1(~intersects);
                plot2y2 = y2(~intersects);
                plot2y3 = y3(~intersects);
                %if they don't
                
                plot(app.UIAxes,[plot2x1;plot2x2;plot2x3;plot2x1],[plot2y1;plot2y2;plot2y3;plot2y1], 'g');
                plot(app.UIAxes,[plot1x1;plot1x2;plot1x3;plot1x1],[plot1y1;plot1y2;plot1y3;plot1y1], 'r');
            end
            hold(app.UIAxes);
            prob = count/num;

            if lineLen <= plankDistance
                piEst = (3 * lineLen)/(plankDistance*prob);
            end
                string = sprintf("PI Estimate: %f", piEst);
                app.EstimationLabel.Text = string;
        end

        % Button pushed function: PlotButton
        function PlotButtonPushed(app, event)
            c1 = app.PiButton.Value;
            c2 = app.Root2Button.Value;

            choices = find(1==[c1 c2]);

            numThrows = app.plotNumEditField.Value;
            estimates = zeros(1, numThrows/10);
            throwCount = zeros(1, numThrows/10);
            count = 1;
            
            switch choices
                case 1
                    for i=1:5:numThrows
                        throwCount(count) = i;
                        estimates(count) = estPiNoPlot(app,i);
                        count = count + 1;
                    end
                case 2
                    for i=1:5:numThrows
                        throwCount(count) = i;
                        estimates(count) = estRoot2NoPlot(app,i);
                        count = count + 1;
                    end
            end
            ax = axes;
            plot(ax,throwCount, estimates);
            axis square;
        end

        % Changes arrangement of the app based on UIFigure width
        function updateAppLayout(app, event)
            currentFigureWidth = app.UIFigure.Position(3);
            if(currentFigureWidth <= app.onePanelWidth)
                % Change to a 2x1 grid
                app.GridLayout.RowHeight = {745, 745};
                app.GridLayout.ColumnWidth = {'1x'};
                app.RightPanel.Layout.Row = 2;
                app.RightPanel.Layout.Column = 1;
            else
                % Change to a 1x2 grid
                app.GridLayout.RowHeight = {'1x'};
                app.GridLayout.ColumnWidth = {295, '1x'};
                app.RightPanel.Layout.Row = 1;
                app.RightPanel.Layout.Column = 2;
            end
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.AutoResizeChildren = 'off';
            app.UIFigure.Position = [100 100 1101 745];
            app.UIFigure.Name = 'MATLAB App';
            app.UIFigure.SizeChangedFcn = createCallbackFcn(app, @updateAppLayout, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.UIFigure);
            app.GridLayout.ColumnWidth = {295, '1x'};
            app.GridLayout.RowHeight = {'1x'};
            app.GridLayout.ColumnSpacing = 0;
            app.GridLayout.RowSpacing = 0;
            app.GridLayout.Padding = [0 0 0 0];
            app.GridLayout.Scrollable = 'on';

            % Create LeftPanel
            app.LeftPanel = uipanel(app.GridLayout);
            app.LeftPanel.Layout.Row = 1;
            app.LeftPanel.Layout.Column = 1;

            % Create SquaresButton
            app.SquaresButton = uibutton(app.LeftPanel, 'push');
            app.SquaresButton.ButtonPushedFcn = createCallbackFcn(app, @SquaresButtonPushed, true);
            app.SquaresButton.Position = [176 714 100 22];
            app.SquaresButton.Text = 'Squares';

            % Create LinesButton
            app.LinesButton = uibutton(app.LeftPanel, 'push');
            app.LinesButton.ButtonPushedFcn = createCallbackFcn(app, @LinesButtonPushed, true);
            app.LinesButton.Position = [175 656 100 22];
            app.LinesButton.Text = 'Lines';

            % Create Squaresroot2Button
            app.Squaresroot2Button = uibutton(app.LeftPanel, 'push');
            app.Squaresroot2Button.ButtonPushedFcn = createCallbackFcn(app, @Squaresroot2ButtonPushed, true);
            app.Squaresroot2Button.Position = [175 686 100 22];
            app.Squaresroot2Button.Text = 'Squares (root2)';

            % Create NumberEditFieldLabel
            app.NumberEditFieldLabel = uilabel(app.LeftPanel);
            app.NumberEditFieldLabel.HorizontalAlignment = 'right';
            app.NumberEditFieldLabel.Position = [176 555 55 22];
            app.NumberEditFieldLabel.Text = 'Number';

            % Create NumberEditField
            app.NumberEditField = uieditfield(app.LeftPanel, 'numeric');
            app.NumberEditField.Limits = [0 Inf];
            app.NumberEditField.Position = [245 555 35 22];
            app.NumberEditField.Value = 100;

            % Create LineSizeEditFieldLabel
            app.LineSizeEditFieldLabel = uilabel(app.LeftPanel);
            app.LineSizeEditFieldLabel.HorizontalAlignment = 'right';
            app.LineSizeEditFieldLabel.Position = [177 523 55 22];
            app.LineSizeEditFieldLabel.Text = 'Line Size';

            % Create LineSizeEditField
            app.LineSizeEditField = uieditfield(app.LeftPanel, 'numeric');
            app.LineSizeEditField.Position = [246 523 35 22];
            app.LineSizeEditField.Value = 10;

            % Create SquareSizeEditFieldLabel
            app.SquareSizeEditFieldLabel = uilabel(app.LeftPanel);
            app.SquareSizeEditFieldLabel.HorizontalAlignment = 'right';
            app.SquareSizeEditFieldLabel.Position = [161 492 71 22];
            app.SquareSizeEditFieldLabel.Text = 'Square Size';

            % Create SquareSizeEditField
            app.SquareSizeEditField = uieditfield(app.LeftPanel, 'numeric');
            app.SquareSizeEditField.Position = [246 492 35 22];
            app.SquareSizeEditField.Value = 13;

            % Create PlankDistanceEditFieldLabel
            app.PlankDistanceEditFieldLabel = uilabel(app.LeftPanel);
            app.PlankDistanceEditFieldLabel.HorizontalAlignment = 'right';
            app.PlankDistanceEditFieldLabel.Position = [146 461 86 22];
            app.PlankDistanceEditFieldLabel.Text = 'Plank Distance';

            % Create PlankDistanceEditField
            app.PlankDistanceEditField = uieditfield(app.LeftPanel, 'numeric');
            app.PlankDistanceEditField.Position = [246 461 35 22];
            app.PlankDistanceEditField.Value = 20;

            % Create PlankNumberEditFieldLabel
            app.PlankNumberEditFieldLabel = uilabel(app.LeftPanel);
            app.PlankNumberEditFieldLabel.HorizontalAlignment = 'right';
            app.PlankNumberEditFieldLabel.Position = [150 434 82 22];
            app.PlankNumberEditFieldLabel.Text = 'Plank Number';

            % Create PlankNumberEditField
            app.PlankNumberEditField = uieditfield(app.LeftPanel, 'numeric');
            app.PlankNumberEditField.Position = [246 434 35 22];
            app.PlankNumberEditField.Value = 5;

            % Create LinesGridButton
            app.LinesGridButton = uibutton(app.LeftPanel, 'push');
            app.LinesGridButton.ButtonPushedFcn = createCallbackFcn(app, @LinesGridButtonPushed, true);
            app.LinesGridButton.Position = [175 625 100 22];
            app.LinesGridButton.Text = 'Lines (Grid)';

            % Create SimilarLinesLabel
            app.SimilarLinesLabel = uilabel(app.LeftPanel);
            app.SimilarLinesLabel.HorizontalAlignment = 'right';
            app.SimilarLinesLabel.Position = [100 400 136 22];
            app.SimilarLinesLabel.Text = 'Similar Lines';

            % Create SimilarNumEditField
            app.SimilarNumEditField = uieditfield(app.LeftPanel, 'numeric');
            app.SimilarNumEditField.Position = [246 400 36 22];
            app.SimilarNumEditField.Value = 3;

            % Create ClosestLinesLabel
            app.ClosestLinesLabel = uilabel(app.LeftPanel);
            app.ClosestLinesLabel.HorizontalAlignment = 'right';
            app.ClosestLinesLabel.Position = [100 370 136 22];
            app.ClosestLinesLabel.Text = 'Closest Lines';

            % Create ClosestLinesEditField
            app.ClosestLinesEditField = uieditfield(app.LeftPanel, 'numeric');
            app.ClosestLinesEditField.Position = [246 370 36 22];
            app.ClosestLinesEditField.Value = 3;

            % Create EstimationLabel
            app.EstimationLabel = uilabel(app.LeftPanel);
            app.EstimationLabel.Position = [8 714 204 22];
            app.EstimationLabel.Text = 'Estimation:';

            % Create TrianglesButton
            app.TrianglesButton = uibutton(app.LeftPanel, 'push');
            app.TrianglesButton.ButtonPushedFcn = createCallbackFcn(app, @TrianglesButtonPushed, true);
            app.TrianglesButton.Position = [176 593 100 22];
            app.TrianglesButton.Text = 'Triangles';

            % Create PlotCheckBox
            app.PlotCheckBox = uicheckbox(app.LeftPanel);
            app.PlotCheckBox.Text = 'Plot';
            app.PlotCheckBox.Position = [4 534 43 22];
            app.PlotCheckBox.Value = true;

            % Create PlotAverageEstimationsButtonGroup
            app.PlotAverageEstimationsButtonGroup = uibuttongroup(app.LeftPanel);
            app.PlotAverageEstimationsButtonGroup.Title = 'Plot Average Estimations';
            app.PlotAverageEstimationsButtonGroup.Position = [4 6 211 106];

            % Create PiButton
            app.PiButton = uiradiobutton(app.PlotAverageEstimationsButtonGroup);
            app.PiButton.Text = 'Pi';
            app.PiButton.Position = [11 60 33 22];
            app.PiButton.Value = true;

            % Create Root2Button
            app.Root2Button = uiradiobutton(app.PlotAverageEstimationsButtonGroup);
            app.Root2Button.Text = 'Root 2';
            app.Root2Button.Position = [11 38 65 22];

            % Create PlotButton
            app.PlotButton = uibutton(app.PlotAverageEstimationsButtonGroup, 'push');
            app.PlotButton.ButtonPushedFcn = createCallbackFcn(app, @PlotButtonPushed, true);
            app.PlotButton.Position = [104 59 100 22];
            app.PlotButton.Text = 'Plot';

            % Create NumberEditField_2Label
            app.NumberEditField_2Label = uilabel(app.PlotAverageEstimationsButtonGroup);
            app.NumberEditField_2Label.HorizontalAlignment = 'right';
            app.NumberEditField_2Label.Position = [19 8 48 22];
            app.NumberEditField_2Label.Text = 'Number';

            % Create plotNumEditField
            app.plotNumEditField = uieditfield(app.PlotAverageEstimationsButtonGroup, 'numeric');
            app.plotNumEditField.Position = [82 8 100 22];
            app.plotNumEditField.Value = 500;

            % Create TurnplotoffforNumber10000Label
            app.TurnplotoffforNumber10000Label = uilabel(app.LeftPanel);
            app.TurnplotoffforNumber10000Label.Position = [4 555 179 22];
            app.TurnplotoffforNumber10000Label.Text = 'Turn plot off for Number > 10000';

            % Create RightPanel
            app.RightPanel = uipanel(app.GridLayout);
            app.RightPanel.Layout.Row = 1;
            app.RightPanel.Layout.Column = 2;

            % Create UIAxes
            app.UIAxes = uiaxes(app.RightPanel);
            title(app.UIAxes, 'Buffon''s Simulation')
            xlabel(app.UIAxes, 'X')
            ylabel(app.UIAxes, 'Y')
            zlabel(app.UIAxes, 'Z')
            app.UIAxes.DataAspectRatio = [1 1 1];
            app.UIAxes.PlotBoxAspectRatio = [1 1 1];
            app.UIAxes.ButtonDownFcn = createCallbackFcn(app, @UIAxesButtonDown, true);
            app.UIAxes.Position = [1 6 799 730];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = coursework_exported

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.UIFigure)

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.UIFigure)
        end
    end
end