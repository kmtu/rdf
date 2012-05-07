#!/usr/bin/octave -q

clear all
format long

baseFileName = argv(){1}
imin = str2num(argv(){2})
imax = str2num(argv(){3})
indexes = [imin:imax]

if (nargin() > 3)
    opt = argv(){4} #even or odd
endif

k = 0;
for i = indexes
    fileName = strcat(baseFileName, num2str(i, "%02d"))
    datafid = fopen(fileName, 'r');
    j = 0;
    k++;
    while(!feof(datafid))
        j++;
        [r(j), g(j,k)] = fscanf(datafid, "%f %f", "C");
    endwhile
    fclose(datafid);
endfor

#gout = g'(imin:imax, 100:110)
#outFileName = strcat("rdf_", num2str(imin), "-", num2str(imax))
#save("-ascii", outFileName,"gout")


for i = [1:length(r)]
    g_mean(i) = mean(g(i,:));
    g_std(i) = std(g(i,:));
endfor

if (nargin() > 3)
    #even
    if (strcmp(opt, "even"))
        g_std(1:2:j) = 0
    endif

    #odd
    if (strcmp(opt, "odd"))
        g_std(2:2:j) = 0
    endif
endif

out = [r', g_mean', g_std']
outFileName = strcat("rdf_std_", num2str(imin), "-", num2str(imax))
save(outFileName,"out")

