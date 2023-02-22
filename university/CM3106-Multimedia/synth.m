classdef synth < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                   matlab.ui.Figure
        TabGroup                   matlab.ui.container.TabGroup
        PlaybackTab                matlab.ui.container.Tab
        PitchSlider                matlab.ui.control.Slider
        PitchSliderLabel           matlab.ui.control.Label
        TempoSilder                matlab.ui.control.Slider
        TempoLabel                 matlab.ui.control.Label
        LoadAudioButton            matlab.ui.control.Button
        PlayAudioButton            matlab.ui.control.Button
        LayersTab                  matlab.ui.container.Tab
        GranWarningLabel           matlab.ui.control.Label
        GranularConvolutionButton  matlab.ui.control.Button
        SelectedlayerButtonGroup   matlab.ui.container.ButtonGroup
        Layer2Button               matlab.ui.control.ToggleButton
        Layer1Button               matlab.ui.control.ToggleButton
        EditTab                    matlab.ui.container.Tab
        SelfConvolutionButton      matlab.ui.control.Button
        CloneSectionButton         matlab.ui.control.Button
        ReverseSectionButton       matlab.ui.control.Button
        BlockStrengthSlider        matlab.ui.control.Slider
        MultiplierSliderLabel      matlab.ui.control.Label
        EditSectionButton          matlab.ui.control.Button
        VolumeShapingTab           matlab.ui.container.Tab
        ShapingTimeSlider          matlab.ui.control.Slider
        TSliderLabel               matlab.ui.control.Label
        ShapingAmpSlider           matlab.ui.control.Slider
        ASliderLabel               matlab.ui.control.Label
        EnvelopeButtonGroup        matlab.ui.container.ButtonGroup
        ReleaseButton              matlab.ui.control.ToggleButton
        SustainButton              matlab.ui.control.ToggleButton
        DecayButton                matlab.ui.control.ToggleButton
        AttackButton               matlab.ui.control.ToggleButton
        EnvelopeAxis               matlab.ui.control.UIAxes
        AudioProcessingTab         matlab.ui.container.Tab
        FrequencySlider            matlab.ui.control.Slider
        FrequencySliderLabel       matlab.ui.control.Label
        FlangerButton              matlab.ui.control.Button
        DelaymsSlider              matlab.ui.control.Slider
        DelaymsSliderLabel         matlab.ui.control.Label
        WahWahButton               matlab.ui.control.Button
        EqualiserTab               matlab.ui.container.Tab
        HzLabel_3                  matlab.ui.control.Label
        EqualiseButton             matlab.ui.control.Button
        KHzLabel_5                 matlab.ui.control.Label
        KHzLabel_4                 matlab.ui.control.Label
        KHzLabel_3                 matlab.ui.control.Label
        KHzLabel_2                 matlab.ui.control.Label
        KHzLabel                   matlab.ui.control.Label
        HzLabel_5                  matlab.ui.control.Label
        HzLabel_4                  matlab.ui.control.Label
        HzLabel_2                  matlab.ui.control.Label
        HzLabel                    matlab.ui.control.Label
        KHzSlider_5                matlab.ui.control.Slider
        KHzSlider_4                matlab.ui.control.Slider
        KHzSlider_3                matlab.ui.control.Slider
        KHzSlider_2                matlab.ui.control.Slider
        KHzSlider                  matlab.ui.control.Slider
        HzSlider_5                 matlab.ui.control.Slider
        HzSlider_4                 matlab.ui.control.Slider
        HzSlider_3                 matlab.ui.control.Slider
        HzSlider_2                 matlab.ui.control.Slider
        HzSlider                   matlab.ui.control.Slider
        UIAxes                     matlab.ui.control.UIAxes
    end

    %TODO refactor getting/setting state for layers
    
    %Global state
    properties (Access = private)
        frame_size % The frame size for the stft
        hop_size % The hop size for the stft

        signal % The sound signal
        sample_rate % The sample rate
        stft_signal % The sound signal after it's been put through the stft
        patch_list % Blocks to draw over the plot

        signal_2 % The sound signal (Second Layer)
        sample_rate_2 % The sample rate (Second Layer)
        stft_signal_2 % The sound signal after it's been put through the stft (Second Layer)
        patch_list_2 % Blocks to draw over the plot

        attack  = [0.0, 1] % The xy pair for the attack
        decay   = [0.50, 1] %  The xy pair for the decay
        sustain = [0.75, 1] % The xy pair for sustain
        release = [1.00, 1] % The xy pair for release
    end
    
    methods (Access = private)

        function D = my_stft(app, x, f, w, h, sr)
            % call this my_stft to prevent name conflicts w/ matlab
            % D = stft(X, F, W, H, SR)                       
            % Short-time Fourier transform.
            %	Returns some frames of short-term Fourier transform of x.  Each 
            %	column of the result is o1 ne F-point fft (default 256); each
            %	successive frame is offset by H points (W/2) until X is exhausted.  
            %       Data is hann-windowed at W pts (F), or rectangular if W=0, or 
            %       with W if it is a vector.
            %       Without output arguments, will plot like sgram (SR will get
            %       axes right, defaults to 8000).
            %	See also 'istft.m'.
            % dpwe 1994may05.  Uses built-in 'fft'
            % $Header: /home/empire6/dpwe/public_html/resources/matlab/pvoc/RCS/stft.m,v 1.4 2010/08/13 16:03:14 dpwe Exp $
            
            if nargin < 2;  f = 256; end
            if nargin < 3;  w = f; end
            if nargin < 4;  h = 0; end
            if nargin < 5;  sr = 8000; end
            
            % expect x as a row
            if size(x,1) > 1
              x = x';
            end
            
            s = length(x);
            
            if length(w) == 1
              if w == 0
                % special case: rectangular window
                win = ones(1,f);
              else
                if rem(w, 2) == 0   % force window to be odd-len
                  w = w + 1;
                end
                halflen = (w-1)/2;
                halff = f/2;   % midpoint of win
                halfwin = 0.5 * ( 1 + cos( pi * (0:halflen)/halflen));
                win = zeros(1, f);
                acthalflen = min(halff, halflen);
                win((halff+1):(halff+acthalflen)) = halfwin(1:acthalflen);
                win((halff+1):-1:(halff-acthalflen+2)) = halfwin(1:acthalflen);
              end
            else
              win = w;
            end
            
            w = length(win);
            % now can set default hop
            if h == 0
              h = floor(w/2);
            end
            
            c = 1;
            
            % pre-allocate output array
            d = zeros((1+f/2),1+fix((s-f)/h));
            
            for b = 0:h:(s-f)
              u = win.*x((b+1):(b+f));
              t = fft(u);
              d(:,c) = t(1:(1+f/2))';
              c = c+1;
            end;
            
            % If no output arguments, plot a spectrogram
            if nargout == 0
              tt = [0:size(d,2)]*h/sr;
              ff = [0:size(d,1)]*sr/f;
              imagesc(tt,ff,20*log10(abs(d)));
              axis('xy');
              xlabel('time / sec');
              ylabel('freq / Hz');
              % leave output variable D undefined
            else
              % Otherwise, no plot, but return STFT
              D = d;
            end
        end

        function c = pvsample(app, b, t, hop)
            % c = pvsample(b, t, hop)   Interpolate an STFT array according to the 'phase vocoder'
            %     b is an STFT array, of the form generated by 'specgram'.
            %     t is a vector of (real) time-samples, which specifies a path through 
            %     the time-base defined by the columns of b.  For each value of t, 
            %     the spectral magnitudes in the columns of b are interpolated, and 
            %     the phase difference between the successive columns of b is 
            %     calculated; a new column is created in the output array c that 
            %     preserves this per-step phase advance in each bin.
            %     hop is the STFT hop size, defaults to N/2, where N is the FFT size
            %     and b has N/2+1 rows.  hop is needed to calculate the 'null' phase 
            %     advance expected in each bin.
            %     Note: t is defined relative to a zero origin, so 0.1 is 90% of 
            %     the first column of b, plus 10% of the second.
            % 2000-12-05 dpwe@ee.columbia.edu
            % $Header: /homes/dpwe/public_html/resources/matlab/dtw/../RCS/pvsample.m,v 1.3 2003/04/09 03:17:10 dpwe Exp $
            
            if nargin < 3
              hop = 0;
            end
            
            [rows,cols] = size(b);
            
            N = 2*(rows-1);
            
            if hop == 0
              % default value
              hop = N/2;
            end
            
            % Empty output array
            c = zeros(rows, length(t));
            
            % Expected phase advance in each bin
            dphi = zeros(1,N/2+1);
            dphi(2:(1 + N/2)) = (2*pi*hop)./(N./(1:(N/2)));
            
            % Phase accumulator
            % Preset to phase of first frame for perfect reconstruction
            % in case of 1:1 time scaling
            ph = angle(b(:,1));
            
            % Append a 'safety' column on to the end of b to avoid problems 
            % taking *exactly* the last frame (i.e. 1*b(:,cols)+0*b(:,cols+1))
            b = [b,zeros(rows,1)];
            
            ocol = 1;
            for tt = t
              % Grab the two columns of b
              bcols = b(:,floor(tt)+[1 2]);
              tf = tt - floor(tt);
              bmag = (1-tf)*abs(bcols(:,1)) + tf*(abs(bcols(:,2)));
              % calculate phase advance
              dp = angle(bcols(:,2)) - angle(bcols(:,1)) - dphi';
              % Reduce to -pi:pi range
              dp = dp - 2 * pi * round(dp/(2*pi));
              % Save the column
              c(:,ocol) = bmag .* exp(j*ph);
              % Cumulate phase, ready for next frame
              ph = ph + dphi' + dp;
              ocol = ocol+1;
            end
            
        end
        
        function y = pvoc(app, x, r, n)
            % y = pvoc(x, r, n)  Time-scale a signal to r times faster with phase vocoder
            %      x is an input sound. n is the FFT size, defaults to 1024.  
            %      Calculate the 25%-overlapped STFT, squeeze it by a factor of r, 
            %      inverse spegram.
            % 2000-12-05, 2002-02-13 dpwe@ee.columbia.edu.  Uses pvsample, stft, istft
            % $Header: /home/empire6/dpwe/public_html/resources/matlab/pvoc/RCS/pvoc.m,v 1.3 2011/02/08 21:08:39 dpwe Exp $
            
            if nargin < 3
              n = 1024;
            end
            
            % With hann windowing on both input and output, 
            % we need 25% window overlap for smooth reconstruction
            hop = n/4;
            % Effect of hanns at both ends is a cumulated cos^2 window (for
            % r = 1 anyway); need to scale magnitudes by 2/3 for
            % identity input/output
            %scf = 2/3;
            % 2011-02-07: this factor is now included in istft.m
            scf = 1.0;
            
            % Calculate the basic STFT, magnitude scaled
            X = scf * my_stft(app, x', n, n, hop);
            
            % Calculate the new timebase samples
            [rows, cols] = size(X);
            t = 0:r:(cols-2);
            % Have to stay two cols off end because (a) counting from zero, and 
            % (b) need col n AND col n+1 to interpolate
            
            % Generate the new spectrogram
            X2 = pvsample(app, X, t, hop);
            
            % Invert to a waveform
            y = my_istft(app, X2, n, n, hop)';
        end

        function x = my_istft(app, d, ftsize, w, h)
            % X = istft(D, F, W, H)                   Inverse short-time Fourier transform.
            %	Performs overlap-add resynthesis from the short-time Fourier transform 
            %	data in D.  Each column of D is taken as the result of an F-point 
            %	fft; each successive frame was offset by H points. Data is 
            %	hamm-windowed at W pts.  
            %       W = 0 gives a rectangular window; W as a vector uses that as window.
            % dpwe 1994may24.  Uses built-in 'ifft' etc.
            % $Header: /homes/dpwe/public_html/resources/matlab/pvoc/RCS/istft.m,v 1.4 2009/01/07 04:20:00 dpwe Exp $
            
            s = size(d);
            %if s(1) != (ftsize/2)+1
            %  error('number of rows should be fftsize/2+1')
            %end
             
            cols = s(2);
            xlen = ftsize + (cols-1)*h;
            x = zeros(1,xlen);
            
            if length(w) == 1
              if w == 0
                % special case: rectangular window
                win = ones(1,ftsize);
              else
                if rem(w, 2) == 0   % force window to be odd-len
                  w = w + 1;
                end
                halflen = (w-1)/2;
                halff = ftsize/2;
                halfwin = 0.5 * ( 1 + cos( pi * (0:halflen)/halflen));
                win = zeros(1, ftsize);
                acthalflen = min(halff, halflen);
                win((halff+1):(halff+acthalflen)) = halfwin(1:acthalflen);
                win((halff+1):-1:(halff-acthalflen+2)) = halfwin(1:acthalflen);
                % 2009-01-06: Make stft-istft loop be identity
                win = 2/3*win;
              end
            else
              win = w;
              w = length(win);
            end
               
            for b = 0:h:(h*(cols-1))
              ft = d(:,1+b/h)';
              ft = [ft, conj(ft([((ftsize/2)):-1:2]))];
              px = real(ifft(ft));
              x((b+1):(b+ftsize)) = x((b+1):(b+ftsize))+px.*win;
            end
        end

        function plotSignal(app, stft_signal)
            % https://www.mathworks.com/matlabcentral/answers/360670-imshow-in-app-designer-image-size-doesn-t-fit
            specy = abs(stft_signal)/app.frame_size;
            I = imshow(flipud(255*specy), 'Parent', app.UIAxes, 'XData', [1 app.UIAxes.Position(3)], 'YData', [1 app.UIAxes.Position(4)]);
            app.UIAxes.XLim = [0 I.XData(2)];
            app.UIAxes.YLim = [0 I.YData(2)];
            colormap(app.UIAxes, jet); %color display
        end

        function [num, den] = ratApprox(app, val, max_denom)
            % https://www.johndcook.com/blog/2010/10/20/best-rational-approximation/
            % only works on numbers in range of [0 - 1]
            a = 0;
            b = 1;
            c = 1;
            d = 1;
        
            while b <= max_denom && d <= max_denom
                mediant = (a+c)/(b+d);
                if val == mediant
                    if b + d <= max_denom
                        num = a + c;
                        den = b + d;
                        return
                    elseif d > b
                        num = c;
                        den = d;
                        return
                    else 
                        num = a;
                        den = b;
                        return
                    end
                elseif val > mediant
                    a = a + c;
                    b = b + d;
                else
                    c = a + c;
                    d = b + d;
                end
            end
        
            if b > max_denom
                num = c;
                den = d;
            else
                num = a;
                den = b;
            end
        end
        
        function plotADSR(app)
            xy = [[0, 0]; app.attack; app.decay; app.sustain; app.release];
            plot(app.EnvelopeAxis, xy(:,1), xy(:,2));
            xlim(app.EnvelopeAxis, [0 1]);
            ylim(app.EnvelopeAxis, [0 1]);
        end
        
        function constrainSliders(app)
            % Set the limits of the sliders for ADSR Enveloping based on 
            % the one in front, and the one behind.
            % Also, load up value for slider

            selectedButton = app.EnvelopeButtonGroup.SelectedObject.Text;
            switch selectedButton
                case "Attack"
                    curr_point = app.attack;
                    min_x = 0;
                    max_x = app.decay(1);
                case "Decay"
                    curr_point = app.decay;
                    min_x = app.attack(1);
                    max_x = app.sustain(1);
                case "Sustain"
                    curr_point = app.sustain;
                    min_x = app.decay(1);
                    max_x = app.release(1);
                case "Release"
                    curr_point = app.release;
                    min_x = app.sustain(1);
                    max_x = 1;
            end

            curr_x = curr_point(1);
            curr_y = curr_point(2);
            
            app.ShapingTimeSlider.Limits = [min_x, max_x];
            app.ShapingTimeSlider.Value = curr_x;
            app.ShapingAmpSlider.Value = curr_y;
        end
        
        function mask = generateMask(app, len)
            x1 = 0;
            y1 = 0;

            x2 = floor(app.attack(1)*len);
            y2 = app.attack(2);

            x3 = floor(app.decay(1)*len);
            y3 = app.decay(2);

            x4 = floor(app.sustain(1)*len);
            y4 = app.sustain(2);

            x5 = floor(app.release(1)*len);
            y5 = app.release(2);

            attack_grad = y2/x2;
            attack_mask = 0 : x2;
            attack_mask = attack_mask * attack_grad;

            decay_grad = (y3 - y2)/(x3 - x2);
            decay_mask = 0:(x3 - x2);
            decay_mask = (decay_mask * decay_grad) + y2;
            
            sustain_grad = (y4 - y3)/(x4 - x3);
            sustain_mask = 0 : (x4 - x3);
            sustain_mask = (sustain_mask * sustain_grad) + y3;
            
            release_grad = (y5 - y4)/(x5 - x4);
            release_mask = 0 : (x5 - x4);
            release_mask = (release_mask * release_grad) + y4;
            
            %reshape the mask to match the size of original signal.
            %then transpose to make it able to do element wise
            %multiplication
            mask = [attack_mask decay_mask sustain_mask release_mask];
            [~, mlen] = size(mask);
            diff = abs(len - mlen);
            if mlen > len
                mask = mask(1:end - diff);
            else
                mask = [mask zeros(1, diff)];
            end
            mask = mask';
        end
        
        function y = flanger(app, x, Fs, freq, delay)
            %y(n) = x(n) + gx[x - M(n)]
            %M(n) = M0 * [1 + A*sin(2*pi*n*(f/Fs))]
            %f = flang freq (can be from 0.1 to 1hz)
            %A = sweep/excursion 
            %M0 = max delay length
            %g = depth control (should be set to 1 for maximum effect)
            
            %delay is in milliseconds
            g = 0.8;
            M0 = round((delay/1000)*Fs);
            y = zeros(1, length(x) + M0);
            
            for n = M0:length(x)
                M = abs(M0 * sin(2*pi*n*(freq/Fs)));
                M = round(M);
                y(n) = g*x(n) + g*x(n - M);
            end
        end

        function yb = wahwah(app, x, Fs)
            % wah_wah.m   state variable band pass
            % written by Ronan O'Malley
            % October 2nd 2005
            
            % damping factor
            % lower the damping factor the smaller the pass band
            damp = 0.05;

            % min and max centre cutoff frequency of variable bandpass filter
            minf=500;
            maxf=3000;
            
            % wah frequency, how many Hz per second are cycled through
            Fw = 2000;
            delta = Fw/Fs;
            
            % create triangle wave of centre frequency values
            Fc=minf:delta:maxf;
            while(length(Fc) < length(x))
                Fc= [ Fc (maxf:-delta:minf) ];
                Fc= [ Fc (minf:delta:maxf) ];
            end
            
            % trim tri wave to size of input
            Fc = Fc(1:length(x));

            % difference equation coefficients
            F1 = 2*sin((pi*Fc(1))/Fs);  % must be recalculated each time Fc changes
            Q1 = 2*damp;                % this dictates size of the pass bands
            
            yh=zeros(size(x));          % create emptly out vectors
            yb=zeros(size(x));
            yl=zeros(size(x));
            
            % first sample, to avoid referencing of negative signals
            yh(1) = x(1);
            yb(1) = F1*yh(1);
            yl(1) = F1*yb(1);

            % apply difference equation to the sample
            for n=2:length(x),
                yh(n) = x(n) - yl(n-1) - Q1*yb(n-1);
                yb(n) = F1*yh(n) + yb(n-1);
                yl(n) = F1*yb(n) + yl(n-1);
                F1 = 2*sin((pi*Fc(n))/Fs);
            end

            %normalise
            maxyb = max(abs(yb));
            yb = yb/maxyb;
        end
        
        function y = grainLn(app, x, init, L, Lw)
            % taken from lab5wk6
            % extract a long grain
            % x    input signal
            % init first sample
            % L    grain length (in samples)
            % Lw   length fade-in and fade-out (in samples)
            
            if length(x) <= init+L , error('length(x) too short.'),  end
            
            y = x(init:init+L-1);                      % extract segment
            w = hanning(2*Lw+1);
            
            w = w(:); y = y(:);
            
            y(1:Lw)     = y(1:Lw).*w(1:Lw);            % fade-in
            y(L-Lw+1:L) = y(L-Lw+1:L).*w(Lw+2:2*Lw+1); % fade-out
        end

        function y = graunlarise(app, x, fs)
            % Taken from lab5wk6
            nEv = 400; 
            maxL = round(fs*0.02);
            minL = round(fs*0.01);
            Lw = round(fs*0.01);
           
            Ly=length(x);
            L = round((maxL - minL) * rand(nEv, 1) + minL);
            initIn = ceil((Ly - maxL) * rand(nEv, 1));
            initOut = ceil((Ly - maxL) * rand(nEv, 1));
            endOut = initOut + L - 1;
            
            y=zeros(Ly,1); 
            % Synthesis
            for k=1:nEv
              grain=grainLn(app, x,initIn(k),L(k),Lw);
              y(initOut(k):endOut(k))= y(initOut(k):endOut(k)) + grain;
            end
        end
        
        function y = peakfilt(~, x, Wc, Wb, G)
            % y = peakfilt (x, Wc, Wb, G)
            % Author: M. Holters
            % Applies a peak filter to the input signal x.
            % Wc is the normalized center frequency 0<Wc<1, i.e. 2*fc/fS.
            % Wb is the normalized bandwidth 0<Wb<1, i.e. 2*fb/fS.
            % G is the gain in dB.
            
            V0 = 10^(G/20); H0 = V0 - 1;
            if G >= 0
              c = (tan(pi*Wb/2)-1) / (tan(pi*Wb/2)+1);     % boost
            else
              c = (tan(pi*Wb/2)-V0) / (tan(pi*Wb/2)+V0);   % cut
            end
            d = -cos(pi*Wc);
            xh = [0, 0];
            for n = 1:length(x)
              xh_new = x(n) - d*(1-c)*xh(1) + c*xh(2);
              ap_y = -c * xh_new + d*(1-c)*xh(1) + xh(2);
              xh = [xh_new, xh(1)];
              y(n) = 0.5 * H0 * (x(n) - ap_y) + x(n);
            end
        end

        function y = lowshelving (~, x, Wc, G)
            % y = lowshelving (x, Wc, G)
            % Author: M. Holters
            % Applies a low-frequency shelving filter to the input signal x.
            % Wc is the normalized cut-off frequency 0<Wc<1, i.e. 2*fc/fS.
            % G is the gain in dB
            
            V0 = 10^(G/20); H0 = V0 - 1;
            if G >= 0
                c = (tan(pi*Wc/2)-1) / (tan(pi*Wc/2)+1);     % boost
            else
                c = (tan(pi*Wc/2)-V0) / (tan(pi*Wc/2)+V0);   % cut
            end
            xh = 0;
            
            for n = 1:length(x)
                xh_new = x(n) - c*xh;
                ap_y = c * xh_new + xh;
                xh = xh_new;
                y(n) = 0.5 * H0 * (x(n) - ap_y) + x(n);  % change to minus for HS
            end
        end
        
        function y = highshelving (~, x, Wc, G)
            % y = highshelving (x, Wc, G)
            % Author: M. Holters
            % Applies a low-frequency shelving filter to the input signal x.
            % Wc is the normalized cut-off frequency 0<Wc<1, i.e. 2*fc/fS
            % G is the cut in dB
            
            V0 = 10^(G/20); H0 = V0 - 1;
            if G >= 0
                c = (tan(pi*Wc/2)-1) / (tan(pi*Wc/2)+1);     % boost
            else
                c = (V0*tan(pi*Wc/2)-1) / (V0*tan(pi*Wc/2)+1);   % cut
            end
            xh = 0;
            
            for n = 1:length(x)
                xh_new = x(n) - c*xh;
                ap_y = c * xh_new + xh;
                xh = xh_new;
                y(n) = 0.5 * H0 * (x(n) + ap_y) + x(n);  % changed to minus for HS
            end
        end

        function [curr_signal, curr_stft] = getCurrentLayer(app)
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            switch selectedLayer
                case "Layer 1"
                    curr_stft = app.stft_signal;
                    curr_signal = app.signal;
                case "Layer 2"
                    curr_stft = app.stft_signal_2;
                    curr_signal = app.signal_2;
            end
        end

        function setCurrentSTFT(app, stft_signal)
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            switch selectedLayer
                case "Layer 1"
                    app.stft_signal = stft_signal;
                case "Layer 2"
                    app.stft_signal_2 = stft_signal;
            end
            plotSignal(app, stft_signal);
        end

        function setCurrentSignal(app, signal)
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            switch selectedLayer
                case "Layer 1"
                    app.signal = signal;
                case "Layer 2"
                    app.signal_2 = signal;
            end
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Button pushed function: LoadAudioButton
        function LoadAudioButtonPushed(app, event)
            [file, path] = uigetfile(["*.mp3; *.wav;*.acc;*.flac", "Audio Files (*.mp3, *.wav, *.acc, *.flac"], "Select a sound file.");
            frame_size = 512;
            hop_size = 512/4;
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;

            if (file ~= 0)
                [y, Fs] = audioread(strcat(path, file));
                [~, n] = size(y);   
                if n > 1
                    y = (y(:,1)+y(:,2))/2;
                end

                app.frame_size = frame_size;
                app.hop_size = hop_size;

                switch selectedLayer
                    case "Layer 1"
                        app.signal = y;
                        app.sample_rate = Fs;
                        app.stft_signal = my_stft(app, y, frame_size, frame_size, hop_size);
                        plotSignal(app, app.stft_signal);
                    case "Layer 2"
                        app.signal_2 = y;
                        app.sample_rate_2 = Fs;
                        app.stft_signal_2 = my_stft(app, y, frame_size, frame_size, hop_size);
                        plotSignal(app, app.stft_signal_2);
                end
            end
        end

        % Button pushed function: PlayAudioButton
        function PlayAudioButtonPushed(app, event)
            [~, stft_signal] = getCurrentLayer(app);
            pitch_value = max(app.PitchSlider.Value, 0.05);
            tempo_value = max(app.TempoSilder.Value, 0.05);
            
            %Get the ratio for pitch adjustment
            if pitch_value == 1
                a = 1;
                b = 1;
            elseif pitch_value > 1
                [a, b] = ratApprox(app, pitch_value - 1, 300);
                a = a + b;
            else
                [a, b] = ratApprox(app, pitch_value, 300);
            end
            
            y = my_istft(app, stft_signal, app.frame_size, app.frame_size, app.hop_size);

            % Pitch changes
            ypvoc = pvoc(app, y, pitch_value, 1024);
            ypitch = resample(ypvoc, a, b);
            
            % Tempo changes
            yslow = pvoc(app, ypitch, tempo_value, 1024);
            
            % Apply ADSR Envelope
            [len, ~] = size(yslow);
            mask = generateMask(app, len);
            ymasked = yslow .* mask;
          
            sound(ymasked, app.sample_rate);
        end

        % Button pushed function: EditSectionButton
        function EditSectionButtonPushed(app, event)
            %https://www.mathworks.com/matlabcentral/answers/392617-how-can-i-use-ginput-in-app-designer
            % Set up figure handle visibility, run ginput, and return state
            fhv = app.UIFigure.HandleVisibility;        % Current status
            app.UIFigure.HandleVisibility = 'callback'; % Temp change (or, 'on')
            set(0, 'CurrentFigure', app.UIFigure);       % Make fig current
            [x,y] = ginput(2);
            app.UIFigure.HandleVisibility = fhv;        % return original state

            %Obtain the selected signal
            [~, stft_signal] = getCurrentLayer(app);
            
            %Check if user selection is out of bounds
            pos = app.UIAxes.Position;
            axes_width = pos(3);
            axes_height = pos(4);

            oob = any([x(1) > axes_width, x(2) > axes_width, ...
                       x(1) < 0, x(2) < 0, ...
                       y(1) > axes_height, y(2) > axes_height, ...
                       y(1) < 0, y(2) < 0]);
            if oob
                errordlg("Mask is out of bounds");
                return
            end
            
            % Generate mask
            strength = app.BlockStrengthSlider.Value;
            [stft_y, stft_x] = size(stft_signal);
            remap_x = @(x_val) max(round((x_val/axes_width) * stft_x), 1);
            remap_y = @(y_val) max(round((y_val/axes_height) * stft_y), 1);
            
            x1 = remap_x(min(x(1), x(2)));
            x2 = remap_x(max(x(1), x(2)));

            y1 = remap_y(min(y(1), y(2)));
            y2 = remap_y(max(y(1), y(2)));
            
            mask = ones(stft_y, stft_x);
            mask(y1:y2, x1:x2) = strength;
            %because we do flipud when we plot, we must flipud the mask
            stft_signal = stft_signal .* flipud(mask);

            setCurrentSTFT(app, stft_signal);
            setCurrentSignal(app, my_istft(app, stft_signal, app.frame_size, app.frame_size, app.hop_size));    
        end

        % Button down function: VolumeShapingTab
        function VolumeShapingTabButtonDown(app, event)
            % When we open up the volume shaping tab, we should auto plot
            % our envelope
            constrainSliders(app);
            plotADSR(app);
        end

        % Selection changed function: EnvelopeButtonGroup
        function EnvelopeButtonGroupSelectionChanged(app, event)
            constrainSliders(app);
        end

        % Value changed function: ShapingAmpSlider
        function ShapingAmpSliderValueChanged(app, event)
            value = app.ShapingAmpSlider.Value;
            selectedButton = app.EnvelopeButtonGroup.SelectedObject.Text;
             switch selectedButton
                case "Attack"
                    app.attack(2) = value;
                case "Decay"
                    app.decay(2) = value;
                case "Sustain"
                    app.sustain(2) = value;
                case "Release"
                    app.release(2) = value;
             end
             plotADSR(app);
        end

        % Value changed function: ShapingTimeSlider
        function ShapingTimeSliderValueChanged(app, event)
            value = app.ShapingTimeSlider.Value;
            selectedButton = app.EnvelopeButtonGroup.SelectedObject.Text;
             switch selectedButton
                case "Attack"
                    app.attack(1) = value;
                case "Decay"
                    app.decay(1) = value;
                case "Sustain"
                    app.sustain(1) = value;
                case "Release"
                    app.release(1) = value;
             end
             plotADSR(app);
        end

        % Selection changed function: SelectedlayerButtonGroup
        function SelectedlayerButtonGroupSelectionChanged(app, event)
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            switch selectedLayer
                case "Layer 1"
                    plotSignal(app, app.stft_signal);
                case "Layer 2"
                    plotSignal(app, app.stft_signal_2);
            end
        end

        % Button pushed function: WahWahButton
        function WahWahButtonPushed(app, event)
            [signal, ~] = getCurrentLayer(app);
            new_signal = wahwah(app, signal, app.sample_rate);
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            switch selectedLayer
                case "Layer 1"
                    app.signal = wahwah(app, app.signal, app.sample_rate);
                    app.stft_signal = my_stft(app, app.signal, app.frame_size, app.frame_size, app.hop_size);
                    plotSignal(app, app.stft_signal);    
                case "Layer 2"
                    app.signal_2 = wahwah(app, app.signal_2, app.sample_rate_2);
                    app.stft_signal_2 = my_stft(app, app.signal_2, app.frame_size, app.frame_size, app.hop_size);
                    plotSignal(app, app.stft_signal_2);   
            end
        end

        % Button pushed function: FlangerButton
        function FlangerButtonPushed(app, event)
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            freq = app.FrequencySlider.Value;
            delay = app.DelaymsSlider.Value;
            switch selectedLayer
                case "Layer 1"
                    app.signal = flanger(app, app.signal, app.sample_rate, freq, delay);
                    app.stft_signal = my_stft(app, app.signal, app.frame_size, app.frame_size, app.hop_size);
                    plotSignal(app, app.stft_signal);   
                case "Layer 2"
                    app.signal_2 = flanger(app, app.signal_2, app.sample_rate_2, freq, delay);
                    app.stft_signal_2 = my_stft(app, app.signal_2, app.frame_size, app.frame_size, app.hop_size);
                    plotSignal(app, app.stft_signal_2);   
            end
        end

        % Button pushed function: GranularConvolutionButton
        function GranularConvolutionButtonPushed(app, event)
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            switch selectedLayer
                case "Layer 1"
                    signal_1 = app.signal;
                    curr_stft = app.stft_signal;
                    signal_2 = app.signal_2;
                    fs = app.sample_rate_2;
                case "Layer 2"
                    signal_1 = app.signal_2;
                    curr_stft = app.stft_signal_2;
                    signal_2 = app.signal;
                    fs = app.sample_rate;
            end

            % Apply granular synthesis to second signal
            grains = graunlarise(app, signal_2, fs);

            % Trunchate/Pad the 2nd signal to be same length as first
            gLen = length(signal_1);
            y = ones(1, gLen);
            for i=1:gLen
                idx = max(1, mod(i, length(grains)));
                y(i) = grains(idx);
            end

            % get ffts
            s1 = fft(signal_1);
            s2 = fft(y);

            % Do convolution with the current signal
            Y = s1 .* s2';
            Y = real(ifft(Y));
            Y = Y/max(abs(Y));  

            switch selectedLayer
                case "Layer 1"
                    app.signal = Y;
                    app.stft_signal = my_stft(app, app.signal, app.frame_size, app.frame_size, app.hop_size);
                    plotSignal(app, app.stft_signal);   
                case "Layer 2"
                    app.signal_2 = Y;
                    app.stft_signal_2 = my_stft(app, app.signal_2, app.frame_size, app.frame_size, app.hop_size);
                    plotSignal(app, app.stft_signal_2);   
            end
        end

        % Button pushed function: ReverseSectionButton
        function ReverseSectionButtonPushed(app, event)
            %https://www.mathworks.com/matlabcentral/answers/392617-how-can-i-use-ginput-in-app-designer
            % Set up figure handle visibility, run ginput, and return state
            fhv = app.UIFigure.HandleVisibility;        % Current status
            app.UIFigure.HandleVisibility = 'callback'; % Temp change (or, 'on')
            set(0, 'CurrentFigure', app.UIFigure);       % Make fig current
            [x,y] = ginput(2);
            app.UIFigure.HandleVisibility = fhv;        % return original state

            %Obtain the selected signal
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            switch selectedLayer
                case "Layer 1"
                    stft_signal = app.stft_signal;
                case "Layer 2"
                    stft_signal = app.stft_signal_2;
            end
            
            %Check if user selection is out of bounds
            pos = app.UIAxes.Position;
            axes_width = pos(3);
            axes_height = pos(4);

            %Flip on y axis
            y(1) = axes_height - y(1);
            y(2) = axes_height - y(2);

            oob = any([x(1) > axes_width, x(2) > axes_width, ...
                       x(1) < 0, x(2) < 0, ...
                       y(1) > axes_height, y(2) > axes_height, ...
                       y(1) < 0, y(2) < 0]);
            if oob
                errordlg("Mask is out of bounds");
                return
            end
            
            % Generate reversal area
            [stft_y, stft_x] = size(stft_signal);
            remap_x = @(x_val) max(round((x_val/axes_width) * stft_x), 1);
            remap_y = @(y_val) max(round((y_val/axes_height) * stft_y), 1);
            
            x1 = remap_x(min(x(1), x(2)));
            x2 = remap_x(max(x(1), x(2)));

            y1 = remap_y(min(y(1), y(2)));
            y2 = remap_y(max(y(1), y(2)));
            

            % Flip on x axis
            reversed_block = stft_signal(y1:y2, x1:x2);
            reversed_block = fliplr(reversed_block);

            stft_signal(y1:y2, x1:x2) = reversed_block;
            new_signal = my_istft(app, stft_signal, app.frame_size, app.frame_size, app.hop_size);

            switch selectedLayer
                case "Layer 1"
                    app.stft_signal = stft_signal;
                    app.signal = new_signal;
                case "Layer 2"
                    app.stft_signal_2 = stft_signal;
                    app.signal_2 = new_signal;
            end
            
            plotSignal(app, stft_signal);
        end

        % Button pushed function: EqualiseButton
        function EqualiseButtonPushed(app, event)
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            switch selectedLayer
                case "Layer 1"
                    y = app.signal;
                    Fs = app.sample_rate;
                case "Layer 2"
                    y = app.signal_2;
                    Fs = app.sample_rate_2;
            end
            
            %low shelving
            y1 = lowshelving(app, y, (2*32)/Fs, app.HzSlider.Value);
            
            %peaking
            y2 = peakfilt(app, y1, 2*(64)/Fs, (2*64)/Fs, app.HzSlider_2.Value);
            y3 = peakfilt(app, y2, 2*(128)/Fs, (2*128)/Fs, app.HzSlider_3.Value); 
            y4 = peakfilt(app, y3, 2*(256)/Fs, (2*256)/Fs, app.HzSlider_4.Value); 
            y5 = peakfilt(app, y4, 2*(512)/Fs, (2*512)/Fs, app.HzSlider_5.Value);
            y6 = peakfilt(app, y5, 2*(1024)/Fs, (2*1024)/Fs, app.KHzSlider.Value);
            y7 = peakfilt(app, y6, 2*(2048)/Fs, (2*2048)/Fs, app.KHzSlider_2.Value);
            y8 = peakfilt(app, y7, 2*(4096)/Fs, (2*4096)/Fs, app.KHzSlider_3.Value);
            y9 = peakfilt(app, y8, 2*(8192)/Fs, (2*8192)/Fs, app.KHzSlider_4.Value);
            
            %high shelving
            y10 = highshelving(app, y9, (2*16348)/Fs, app.KHzSlider_5.Value);
            
            switch selectedLayer
                case "Layer 1"
                    app.signal = y10;
                    app.stft_signal = my_stft(app, app.signal, app.frame_size, app.frame_size, app.hop_size);
                    plotSignal(app, app.stft_signal);   
                case "Layer 2"
                    app.signal_2 = y10;
                    app.stft_signal_2 = my_stft(app, app.signal_2, app.frame_size, app.frame_size, app.hop_size);
                    plotSignal(app, app.stft_signal_2);   
            end
        end

        % Button pushed function: CloneSectionButton
        function CloneSectionButtonPushed(app, event)
            %https://www.mathworks.com/matlabcentral/answers/392617-how-can-i-use-ginput-in-app-designer
            % Set up figure handle visibility, run ginput, and return state
            fhv = app.UIFigure.HandleVisibility;        % Current status
            app.UIFigure.HandleVisibility = 'callback'; % Temp change (or, 'on')
            set(0, 'CurrentFigure', app.UIFigure);       % Make fig current
            [x,y] = ginput(2);
            app.UIFigure.HandleVisibility = fhv;        % return original state

            %Obtain the selected signal
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            switch selectedLayer
                case "Layer 1"
                    stft_signal = app.stft_signal;
                case "Layer 2"
                    stft_signal = app.stft_signal_2;
            end
            
            %Check if user selection is out of bounds
            pos = app.UIAxes.Position;
            axes_width = pos(3);
            axes_height = pos(4);

            %Flip on y axis
            y(1) = axes_height - y(1);
            y(2) = axes_height - y(2);

            oob = any([x(1) > axes_width, x(2) > axes_width, ...
                       x(1) < 0, x(2) < 0, ...
                       y(1) > axes_height, y(2) > axes_height, ...
                       y(1) < 0, y(2) < 0]);
            if oob
                errordlg("Mask is out of bounds");
                return
            end
            
            % Generate clone area
            [stft_y, stft_x] = size(stft_signal);
            remap_x = @(x_val) max(floor((x_val/axes_width) * stft_x), 1);
            remap_y = @(y_val) max(floor((y_val/axes_height) * stft_y), 1);
            
            x1 = remap_x(min(x(1), x(2)));
            x2 = remap_x(max(x(1), x(2)));

            y1 = remap_y(min(y(1), y(2)));
            y2 = remap_y(max(y(1), y(2)));
         
            cloned_block = stft_signal(y1:y2, x1:x2);

            % Get paste location
            fhv = app.UIFigure.HandleVisibility;        % Current status
            app.UIFigure.HandleVisibility = 'callback'; % Temp change (or, 'on')
            set(0, 'CurrentFigure', app.UIFigure);       % Make fig current
            [px,py] = ginput(1);
            app.UIFigure.HandleVisibility = fhv;        % return original state

            py = axes_height - py;
            
            % Height and width of our selection. (In stft space)
            w = int32(x2 - x1);
            h = int32(y2 - y1);

            % The center coordinates of the rectangular paste area (stft space)
            center_x = remap_x(px); 
            center_y = remap_y(py);

            half_w = idivide(w,2);
            half_h = idivide(h,2);

            left  = w - half_w;
            right = half_w;
            down  = h - half_h;
            up    = half_h;

            xp1 = center_x - left;
            xp2 = center_x + right;
            yp1 = center_y - down;
            yp2 = center_y + up;

            oob = any([xp1 < 0, xp2 > stft_x, ...
                       yp1 < 0, yp2 > stft_y]);
            if oob
                errordlg("Invalid paste location");
                return
            end

            stft_signal(yp1:yp2, xp1:xp2) = cloned_block;
            new_signal = my_istft(app, stft_signal, app.frame_size, app.frame_size, app.hop_size);

            switch selectedLayer
                case "Layer 1"
                    app.stft_signal = stft_signal;
                    app.signal = new_signal;
                case "Layer 2"
                    app.stft_signal_2 = stft_signal;
                    app.signal_2 = new_signal;
            end
            
            plotSignal(app, stft_signal);
        end

        % Button pushed function: SelfConvolutionButton
        function SelfConvolutionButtonPushed(app, event)
            % https://www.mathworks.com/matlabcentral/answers/392617-how-can-i-use-ginput-in-app-designer
            % Set up figure handle visibility, run ginput, and return state
            fhv = app.UIFigure.HandleVisibility;        % Current status
            app.UIFigure.HandleVisibility = 'callback'; % Temp change (or, 'on')
            set(0, 'CurrentFigure', app.UIFigure);       % Make fig current
            [x,y] = ginput(2);
            app.UIFigure.HandleVisibility = fhv;        % return original state

            %Obtain the selected signal
            selectedLayer = app.SelectedlayerButtonGroup.SelectedObject.Text;
            switch selectedLayer
                case "Layer 1"
                    stft_signal = app.stft_signal;
                case "Layer 2"
                    stft_signal = app.stft_signal_2;
            end
            
            %Check if user selection is out of bounds
            pos = app.UIAxes.Position;
            axes_width = pos(3);
            axes_height = pos(4);

            %Flip on y axis
            y(1) = axes_height - y(1);
            y(2) = axes_height - y(2);

            oob = any([x(1) > axes_width, x(2) > axes_width, ...
                       x(1) < 0, x(2) < 0, ...
                       y(1) > axes_height, y(2) > axes_height, ...
                       y(1) < 0, y(2) < 0]);
            if oob
                errordlg("Mask is out of bounds");
                return
            end
            
            % Generate clone area
            [stft_y, stft_x] = size(stft_signal);
            remap_x = @(x_val) max(floor((x_val/axes_width) * stft_x), 1);
            remap_y = @(y_val) max(floor((y_val/axes_height) * stft_y), 1);
            
            x1 = remap_x(min(x(1), x(2)));
            x2 = remap_x(max(x(1), x(2)));

            y1 = remap_y(min(y(1), y(2)));
            y2 = remap_y(max(y(1), y(2)));
         
            orig_block = stft_signal(y1:y2, x1:x2);

            % Get conv location
            fhv = app.UIFigure.HandleVisibility;        % Current status
            app.UIFigure.HandleVisibility = 'callback'; % Temp change (or, 'on')
            set(0, 'CurrentFigure', app.UIFigure);       % Make fig current
            [px,py] = ginput(1);
            app.UIFigure.HandleVisibility = fhv;        % return original state

            py = axes_height - py;
            
            % Height and width of our selection. (In stft space)
            w = int32(x2 - x1);
            h = int32(y2 - y1);

            % The center coordinates of the rectangular conv area (stft space)
            center_x = remap_x(px); 
            center_y = remap_y(py);

            half_w = idivide(w,2);
            half_h = idivide(h,2);

            left  = w - half_w;
            right = half_w;
            down  = h - half_h;
            up    = half_h;

            xp1 = center_x - left;
            xp2 = center_x + right;
            yp1 = center_y - down;
            yp2 = center_y + up;

            oob = any([xp1 < 0, xp2 > stft_x, ...
                       yp1 < 0, yp2 > stft_y]);
            if oob
                errordlg("Invalid convolution location");
                return
            end

            conv_block = stft_signal(yp1:yp2, xp1:xp2);
            new_block = conv_block .* orig_block;
            stft_signal(yp1:yp2, xp1:xp2) = new_block;

            new_signal = my_istft(app, stft_signal, app.frame_size, app.frame_size, app.hop_size);

            switch selectedLayer
                case "Layer 1"
                    app.stft_signal = stft_signal;
                    app.signal = new_signal;
                case "Layer 2"
                    app.stft_signal_2 = stft_signal;
                    app.signal_2 = new_signal;
            end
            
            plotSignal(app, stft_signal);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1000 600];
            app.UIFigure.Name = 'MATLAB App';

            % Create UIAxes
            app.UIAxes = uiaxes(app.UIFigure);
            app.UIAxes.XTick = [];
            app.UIAxes.YTick = [];
            app.UIAxes.ZTick = [];
            app.UIAxes.Position = [2 1 998 450];

            % Create TabGroup
            app.TabGroup = uitabgroup(app.UIFigure);
            app.TabGroup.Position = [1 451 1000 150];

            % Create PlaybackTab
            app.PlaybackTab = uitab(app.TabGroup);
            app.PlaybackTab.Title = 'Playback';

            % Create PlayAudioButton
            app.PlayAudioButton = uibutton(app.PlaybackTab, 'push');
            app.PlayAudioButton.ButtonPushedFcn = createCallbackFcn(app, @PlayAudioButtonPushed, true);
            app.PlayAudioButton.Position = [11 92 100 22];
            app.PlayAudioButton.Text = 'Play Audio';

            % Create LoadAudioButton
            app.LoadAudioButton = uibutton(app.PlaybackTab, 'push');
            app.LoadAudioButton.ButtonPushedFcn = createCallbackFcn(app, @LoadAudioButtonPushed, true);
            app.LoadAudioButton.Position = [12 41 100 22];
            app.LoadAudioButton.Text = 'Load Audio';

            % Create TempoLabel
            app.TempoLabel = uilabel(app.PlaybackTab);
            app.TempoLabel.HorizontalAlignment = 'right';
            app.TempoLabel.Position = [120 41 42 22];
            app.TempoLabel.Text = 'Tempo';

            % Create TempoSilder
            app.TempoSilder = uislider(app.PlaybackTab);
            app.TempoSilder.Limits = [0 2];
            app.TempoSilder.Position = [183 50 769 3];
            app.TempoSilder.Value = 1;

            % Create PitchSliderLabel
            app.PitchSliderLabel = uilabel(app.PlaybackTab);
            app.PitchSliderLabel.HorizontalAlignment = 'right';
            app.PitchSliderLabel.Position = [133 93 32 22];
            app.PitchSliderLabel.Text = {'Pitch'; ''};

            % Create PitchSlider
            app.PitchSlider = uislider(app.PlaybackTab);
            app.PitchSlider.Limits = [0 2];
            app.PitchSlider.Position = [186 102 767 3];
            app.PitchSlider.Value = 1;

            % Create LayersTab
            app.LayersTab = uitab(app.TabGroup);
            app.LayersTab.Title = 'Layers';

            % Create SelectedlayerButtonGroup
            app.SelectedlayerButtonGroup = uibuttongroup(app.LayersTab);
            app.SelectedlayerButtonGroup.SelectionChangedFcn = createCallbackFcn(app, @SelectedlayerButtonGroupSelectionChanged, true);
            app.SelectedlayerButtonGroup.Title = 'Selected layer';
            app.SelectedlayerButtonGroup.Position = [13 7 123 106];

            % Create Layer1Button
            app.Layer1Button = uitogglebutton(app.SelectedlayerButtonGroup);
            app.Layer1Button.Text = 'Layer 1';
            app.Layer1Button.Position = [11 53 100 22];
            app.Layer1Button.Value = true;

            % Create Layer2Button
            app.Layer2Button = uitogglebutton(app.SelectedlayerButtonGroup);
            app.Layer2Button.Text = 'Layer 2';
            app.Layer2Button.Position = [9 13 100 22];

            % Create GranularConvolutionButton
            app.GranularConvolutionButton = uibutton(app.LayersTab, 'push');
            app.GranularConvolutionButton.ButtonPushedFcn = createCallbackFcn(app, @GranularConvolutionButtonPushed, true);
            app.GranularConvolutionButton.Position = [148 10 139 102];
            app.GranularConvolutionButton.Text = 'Granular Convolution';

            % Create GranWarningLabel
            app.GranWarningLabel = uilabel(app.LayersTab);
            app.GranWarningLabel.Position = [295 58 232 22];
            app.GranWarningLabel.Text = 'Convole the selected signal with the other.';

            % Create EditTab
            app.EditTab = uitab(app.TabGroup);
            app.EditTab.Title = 'Edit';

            % Create EditSectionButton
            app.EditSectionButton = uibutton(app.EditTab, 'push');
            app.EditSectionButton.ButtonPushedFcn = createCallbackFcn(app, @EditSectionButtonPushed, true);
            app.EditSectionButton.Position = [11 79 100 22];
            app.EditSectionButton.Text = 'Edit Section';

            % Create MultiplierSliderLabel
            app.MultiplierSliderLabel = uilabel(app.EditTab);
            app.MultiplierSliderLabel.HorizontalAlignment = 'right';
            app.MultiplierSliderLabel.Position = [123 78 54 22];
            app.MultiplierSliderLabel.Text = 'Multiplier';

            % Create BlockStrengthSlider
            app.BlockStrengthSlider = uislider(app.EditTab);
            app.BlockStrengthSlider.Limits = [0 1];
            app.BlockStrengthSlider.Position = [198 87 760 3];

            % Create ReverseSectionButton
            app.ReverseSectionButton = uibutton(app.EditTab, 'push');
            app.ReverseSectionButton.ButtonPushedFcn = createCallbackFcn(app, @ReverseSectionButtonPushed, true);
            app.ReverseSectionButton.Position = [9 20 104 22];
            app.ReverseSectionButton.Text = 'Reverse Section';

            % Create CloneSectionButton
            app.CloneSectionButton = uibutton(app.EditTab, 'push');
            app.CloneSectionButton.ButtonPushedFcn = createCallbackFcn(app, @CloneSectionButtonPushed, true);
            app.CloneSectionButton.Position = [124 19 100 22];
            app.CloneSectionButton.Text = 'Clone Section';

            % Create SelfConvolutionButton
            app.SelfConvolutionButton = uibutton(app.EditTab, 'push');
            app.SelfConvolutionButton.ButtonPushedFcn = createCallbackFcn(app, @SelfConvolutionButtonPushed, true);
            app.SelfConvolutionButton.Position = [231 19 104 22];
            app.SelfConvolutionButton.Text = 'Self-Convolution';

            % Create VolumeShapingTab
            app.VolumeShapingTab = uitab(app.TabGroup);
            app.VolumeShapingTab.Title = 'Volume Shaping';
            app.VolumeShapingTab.ButtonDownFcn = createCallbackFcn(app, @VolumeShapingTabButtonDown, true);

            % Create EnvelopeAxis
            app.EnvelopeAxis = uiaxes(app.VolumeShapingTab);
            title(app.EnvelopeAxis, 'ADSR Envolope')
            xlabel(app.EnvelopeAxis, 'Time')
            ylabel(app.EnvelopeAxis, 'Amplitude')
            zlabel(app.EnvelopeAxis, 'Z')
            app.EnvelopeAxis.Position = [520 1 479 125];

            % Create EnvelopeButtonGroup
            app.EnvelopeButtonGroup = uibuttongroup(app.VolumeShapingTab);
            app.EnvelopeButtonGroup.SelectionChangedFcn = createCallbackFcn(app, @EnvelopeButtonGroupSelectionChanged, true);
            app.EnvelopeButtonGroup.Title = 'Envelope';
            app.EnvelopeButtonGroup.Position = [11 10 230 106];

            % Create AttackButton
            app.AttackButton = uitogglebutton(app.EnvelopeButtonGroup);
            app.AttackButton.Text = 'Attack';
            app.AttackButton.Position = [11 53 100 22];
            app.AttackButton.Value = true;

            % Create DecayButton
            app.DecayButton = uitogglebutton(app.EnvelopeButtonGroup);
            app.DecayButton.Text = 'Decay';
            app.DecayButton.Position = [115 52 100 22];

            % Create SustainButton
            app.SustainButton = uitogglebutton(app.EnvelopeButtonGroup);
            app.SustainButton.Text = 'Sustain';
            app.SustainButton.Position = [11 30 100 22];

            % Create ReleaseButton
            app.ReleaseButton = uitogglebutton(app.EnvelopeButtonGroup);
            app.ReleaseButton.Text = 'Release';
            app.ReleaseButton.Position = [115 30 100 22];

            % Create ASliderLabel
            app.ASliderLabel = uilabel(app.VolumeShapingTab);
            app.ASliderLabel.HorizontalAlignment = 'right';
            app.ASliderLabel.Position = [262 91 25 22];
            app.ASliderLabel.Text = 'A';

            % Create ShapingAmpSlider
            app.ShapingAmpSlider = uislider(app.VolumeShapingTab);
            app.ShapingAmpSlider.Limits = [0 1];
            app.ShapingAmpSlider.ValueChangedFcn = createCallbackFcn(app, @ShapingAmpSliderValueChanged, true);
            app.ShapingAmpSlider.Position = [308 100 150 3];

            % Create TSliderLabel
            app.TSliderLabel = uilabel(app.VolumeShapingTab);
            app.TSliderLabel.HorizontalAlignment = 'right';
            app.TSliderLabel.Position = [262 41 25 22];
            app.TSliderLabel.Text = 'T';

            % Create ShapingTimeSlider
            app.ShapingTimeSlider = uislider(app.VolumeShapingTab);
            app.ShapingTimeSlider.Limits = [0 1];
            app.ShapingTimeSlider.ValueChangedFcn = createCallbackFcn(app, @ShapingTimeSliderValueChanged, true);
            app.ShapingTimeSlider.Position = [308 50 150 3];

            % Create AudioProcessingTab
            app.AudioProcessingTab = uitab(app.TabGroup);
            app.AudioProcessingTab.Title = 'Audio Processing';

            % Create WahWahButton
            app.WahWahButton = uibutton(app.AudioProcessingTab, 'push');
            app.WahWahButton.ButtonPushedFcn = createCallbackFcn(app, @WahWahButtonPushed, true);
            app.WahWahButton.Position = [13 90 100 22];
            app.WahWahButton.Text = 'Wah Wah';

            % Create DelaymsSliderLabel
            app.DelaymsSliderLabel = uilabel(app.AudioProcessingTab);
            app.DelaymsSliderLabel.HorizontalAlignment = 'right';
            app.DelaymsSliderLabel.Position = [148 91 64 22];
            app.DelaymsSliderLabel.Text = 'Delay (ms)';

            % Create DelaymsSlider
            app.DelaymsSlider = uislider(app.AudioProcessingTab);
            app.DelaymsSlider.Limits = [0 50];
            app.DelaymsSlider.Position = [233 100 740 3];
            app.DelaymsSlider.Value = 25;

            % Create FlangerButton
            app.FlangerButton = uibutton(app.AudioProcessingTab, 'push');
            app.FlangerButton.ButtonPushedFcn = createCallbackFcn(app, @FlangerButtonPushed, true);
            app.FlangerButton.Position = [13 60 100 22];
            app.FlangerButton.Text = 'Flanger';

            % Create FrequencySliderLabel
            app.FrequencySliderLabel = uilabel(app.AudioProcessingTab);
            app.FrequencySliderLabel.HorizontalAlignment = 'right';
            app.FrequencySliderLabel.Position = [148 29 62 22];
            app.FrequencySliderLabel.Text = 'Frequency';

            % Create FrequencySlider
            app.FrequencySlider = uislider(app.AudioProcessingTab);
            app.FrequencySlider.Limits = [0 6.28318530717959];
            app.FrequencySlider.Position = [231 38 742 3];
            app.FrequencySlider.Value = 3.14159265358979;

            % Create EqualiserTab
            app.EqualiserTab = uitab(app.TabGroup);
            app.EqualiserTab.Title = 'Equaliser';

            % Create HzSlider
            app.HzSlider = uislider(app.EqualiserTab);
            app.HzSlider.Limits = [-6 6];
            app.HzSlider.Orientation = 'vertical';
            app.HzSlider.Position = [13 27 3 77];

            % Create HzSlider_2
            app.HzSlider_2 = uislider(app.EqualiserTab);
            app.HzSlider_2.Limits = [-6 6];
            app.HzSlider_2.Orientation = 'vertical';
            app.HzSlider_2.Position = [73 28 3 77];

            % Create HzSlider_3
            app.HzSlider_3 = uislider(app.EqualiserTab);
            app.HzSlider_3.Limits = [-6 6];
            app.HzSlider_3.Orientation = 'vertical';
            app.HzSlider_3.Position = [139 26 3 77];

            % Create HzSlider_4
            app.HzSlider_4 = uislider(app.EqualiserTab);
            app.HzSlider_4.Limits = [-6 6];
            app.HzSlider_4.Orientation = 'vertical';
            app.HzSlider_4.Position = [201 28 3 77];

            % Create HzSlider_5
            app.HzSlider_5 = uislider(app.EqualiserTab);
            app.HzSlider_5.Limits = [-6 6];
            app.HzSlider_5.Orientation = 'vertical';
            app.HzSlider_5.Position = [257 28 3 77];

            % Create KHzSlider
            app.KHzSlider = uislider(app.EqualiserTab);
            app.KHzSlider.Limits = [-6 6];
            app.KHzSlider.Orientation = 'vertical';
            app.KHzSlider.Position = [311 28 3 77];

            % Create KHzSlider_2
            app.KHzSlider_2 = uislider(app.EqualiserTab);
            app.KHzSlider_2.Limits = [-6 6];
            app.KHzSlider_2.Orientation = 'vertical';
            app.KHzSlider_2.Position = [371 29 3 77];

            % Create KHzSlider_3
            app.KHzSlider_3 = uislider(app.EqualiserTab);
            app.KHzSlider_3.Limits = [-6 6];
            app.KHzSlider_3.Orientation = 'vertical';
            app.KHzSlider_3.Position = [428 28 3 77];

            % Create KHzSlider_4
            app.KHzSlider_4 = uislider(app.EqualiserTab);
            app.KHzSlider_4.Limits = [-6 6];
            app.KHzSlider_4.Orientation = 'vertical';
            app.KHzSlider_4.Position = [486 28 3 77];

            % Create KHzSlider_5
            app.KHzSlider_5 = uislider(app.EqualiserTab);
            app.KHzSlider_5.Limits = [-6 6];
            app.KHzSlider_5.Orientation = 'vertical';
            app.KHzSlider_5.Position = [542 28 3 77];

            % Create HzLabel
            app.HzLabel = uilabel(app.EqualiserTab);
            app.HzLabel.Position = [9 1 50 22];
            app.HzLabel.Text = '31.25Hz';

            % Create HzLabel_2
            app.HzLabel_2 = uilabel(app.EqualiserTab);
            app.HzLabel_2.Position = [73 1 44 22];
            app.HzLabel_2.Text = '62.5Hz';

            % Create HzLabel_4
            app.HzLabel_4 = uilabel(app.EqualiserTab);
            app.HzLabel_4.Position = [203 1 40 22];
            app.HzLabel_4.Text = '250Hz';

            % Create HzLabel_5
            app.HzLabel_5 = uilabel(app.EqualiserTab);
            app.HzLabel_5.Position = [262 1 40 22];
            app.HzLabel_5.Text = '500Hz';

            % Create KHzLabel
            app.KHzLabel = uilabel(app.EqualiserTab);
            app.KHzLabel.Position = [308 1 35 22];
            app.KHzLabel.Text = '1KHz';

            % Create KHzLabel_2
            app.KHzLabel_2 = uilabel(app.EqualiserTab);
            app.KHzLabel_2.Position = [366 1 35 22];
            app.KHzLabel_2.Text = '2KHz';

            % Create KHzLabel_3
            app.KHzLabel_3 = uilabel(app.EqualiserTab);
            app.KHzLabel_3.Position = [428 1 35 22];
            app.KHzLabel_3.Text = '4KHz';

            % Create KHzLabel_4
            app.KHzLabel_4 = uilabel(app.EqualiserTab);
            app.KHzLabel_4.Position = [483 1 35 22];
            app.KHzLabel_4.Text = '8KHz';

            % Create KHzLabel_5
            app.KHzLabel_5 = uilabel(app.EqualiserTab);
            app.KHzLabel_5.Position = [542 1 42 22];
            app.KHzLabel_5.Text = '16KHz';

            % Create EqualiseButton
            app.EqualiseButton = uibutton(app.EqualiserTab, 'push');
            app.EqualiseButton.ButtonPushedFcn = createCallbackFcn(app, @EqualiseButtonPushed, true);
            app.EqualiseButton.Position = [603 15 100 96];
            app.EqualiseButton.Text = 'Equalise';

            % Create HzLabel_3
            app.HzLabel_3 = uilabel(app.EqualiserTab);
            app.HzLabel_3.Position = [135 1 40 22];
            app.HzLabel_3.Text = '125Hz';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = synth

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
