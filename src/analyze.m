#!/usr/bin/octave -qf

clear all
format long

if (nargin() < 3)
    error("Usage: $analyze.m baseFileName ibegin iend [mode errbar_pos...]")
endif

baseFileName = argv(){1}
ibegin = str2num(argv(){2})
iend = str2num(argv(){3})
indexes = [ibegin:iend]

if (nargin() > 3)
    mode = argv(){4} #even or odd or custom
    if (strcmp(mode, "custom"))
        if (nargin() > 4)
            errbarXPos_cell = argv()(5:end)
            for i = [1:length(errbarXPos_cell)]
                errbarXPos(i) = str2num(errbarXPos_cell{i})
            endfor
        else
            error("When mode = custom, there should be values of errbarXPos following behind.\n\
                   Usage: $analyze.m baseFileName ibegin iend [mode errbar_pos...]")
        endif
    endif
endif

#k = 0;
#for i = indexes
#    fileName = strcat(baseFileName, num2str(i, "%02d"))
#    datafid = fopen(fileName, 'r');
#    j = 0;
#    k++;
#    while(!feof(datafid))
#        j++;
#        [r(j), g(j,k)] = fscanf(datafid, "%f %f", "C");
#    endwhile
#    fclose(datafid);
#endfor

for i = indexes
    fileName = strcat(baseFileName, num2str(i, "%02d"))
    data = load(fileName)
    if (!exist("xdata"))
        xdata= data(:,1)
        ydata = data(:,2)
    else
#        if (size(xdata) != size(data) || xdata != data(:,1))
#            error("The xdata indexes of file %s is different", fileName)
#        endif
    # don't check the consistency of xdata
    # use the first xdata
        minSize = min(size(ydata, 1), size(data, 1))
        ydata(1:minSize, end+1) = data(1:minSize, 2)
    endif
endfor

ydata_mean = mean(ydata, 2);
ydata_std = std(ydata, 0, 2);

if (nargin() > 3)
    switch (mode)
    case "even"
        ydata_std(1:2:j) = 0
    case "odd"
        ydata_std(2:2:j) = 0
    case "custom"
        #find the closest xdata, both its value and index
        for i = [1:length(errbarXPos)]
            [m, mi] = min(abs(errbarXPos(i) - xdata))
            errbarIndexes(i) = mi
        endfor
        #assign values of OTHER indexes as 0
        for i = [1:length(xdata)]
            if (all(errbarIndexes != i))
                ydata_std(i) = 0
            endif
        endfor
    otherwise
        error("Unknown mode: %s", mode)
    endswitch
endif

out = [xdata, ydata_mean, ydata_std]
outFileName = strcat("rdf_std_", num2str(ibegin), "-", num2str(iend))
save(outFileName,"out")

