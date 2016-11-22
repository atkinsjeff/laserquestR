function [ R ] = Rugosity(el,Z)
%R = Rugosity(el,Z)
%Written by Tim Morn, 2014
%R  = Rugosity
%el = VAI
%Z  = Effective leaf height

maxel=max(el,[],1);
stdStd=0;meanStd=0;

for i=1:40
    heightBin = sum(el(:,i).*Z(:,i))/sum(el(:,i));                         %Mean column leaf height
    stdBin    = sum(el(:,i).*((Z(:,i)-heightBin).^2))/sum(el(:,i));        %Std of leaf heights in column
    stdStd    = nansum([stdStd stdBin*stdBin/length(maxel)]);              %Accums the square of std (variance) of leaf heights in plot
    meanStd   = nansum([meanStd stdBin/length(maxel)]);                    %Accums the mean of std dev of leaf heights in plot
end

% stdStd(current_plot) = stdStd+stdBin*stdBin/length(maxel); 		   %accumulates the square of std (variance) of leaf heights in plot
% meanStd(current_plot) = meanStd(current_plot)+stdBin/length(maxel);      %accumulates the mean of std dev of leaf heights in plot
% stdStd = (stdStd-meanStd.*meanStd).^0.5;

R = (stdStd-meanStd.*meanStd).^0.5; tandard DEviation                      %Horizontal std dev of vertical (column) std dev of leaf height. Rugosity