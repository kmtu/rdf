#!/home/kmtu/bin/python3.2
import sys
import argparse
import subprocess
import math

parser = argparse.ArgumentParser(description='Process data to RDF')
parser.add_argument('dataIndexFile', type=argparse.FileType('r'))
parser.add_argument('window', type=int, help="Window size in ns")
parser.add_argument('outputBaseName', 
                     help = "Output file name = outputBaseName01,02,03...")
parser.add_argument('-r', '--dataRange', nargs=2, type=int, metavar=('BEGIN', 'END'),
                     help = "The data range to be processed. ex. -r 6 10, means 6ns to 10ns.")
parser.add_argument('--correlated', action='store_true', default=False,
                     help = "Process data with the window in a correlated way: 1 2 3, 2 3 4, 3 4 5...\n"+
                            "Default is False, i.e. uncorrelated: 1 2 3, 4 5 6,...")
parser.add_argument('--beginIndex', type=int, default=1,
                     help = "Only takes effect for uncorrelated rdf.")
parser.add_argument('-c', '--controlFile', default="rdf_control",
                     help = "rdf control file")
args = parser.parse_args()

dataIndexFile = args.dataIndexFile
window = args.window
control = args.controlFile
outputBaseName = args.outputBaseName
isCorrelated = args.correlated
beginIndex = args.beginIndex

if args.dataRange == None:
    dataRange = [0, 0]
else:
    dataRange = [i-1 for i in args.dataRange]

currentTime = -1 #begin from 0
nextTime = 1
dataFileList = []
isWithinRange = False
for (i, line) in enumerate(dataIndexFile):
    line = line.strip()
    if i == 0:
        dataDir = line
    elif line == "":
        pass
    else:
        try:
            currentTime = int(line) - 1
        except ValueError:
            if isWithinRange:
                dataFileList[currentTime - dataRange[0]].append(dataDir + "/" + line)
        else:
            if currentTime == nextTime - 1:
                nextTime += 1
                if args.dataRange == None or (currentTime >= dataRange[0] and currentTime <= dataRange[1]):
                    dataFileList.append([])
                    isWithinRange = True
                else:
                    isWithinRange = False
            else:
                sys.exit("Data index file error: " +
                         "data time is wrong or discontinued")

print("Window size: " + str(window))
for i in dataFileList:
    print(i)


if isCorrelated:
    for i in range(len(dataFileList) - window + 1):
        files = []
        for j in range(window):
            files += dataFileList[i+j]
    #    print("rdf_dlchau -c " + control + " -o " + outputBaseName+"-%02d"%i + " -f " + " ".join(files))
        subprocess.check_call(["rdf_dlchau", "-c", control, 
                               "-o", outputBaseName + "%02d"%(i+dataRange[0]+1),
                               "-f"] + files)
else:
    for i in range(int(math.floor(len(dataFileList) / window))):
        files = []
        for j in range(window):
            files += dataFileList[window*i+j]
    #    print("rdf_dlchau -c " + control + " -o " + outputBaseName+"-%02d"%i + " -f " + " ".join(files))
        subprocess.check_call(["rdf_dlchau", "-c", control, 
                               "-o", outputBaseName + "%02d"%(i+beginIndex),
                               "-f"] + files)

