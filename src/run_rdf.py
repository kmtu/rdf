#!/home/kmtu/bin/python3.2
import sys
import argparse
import subprocess

parser = argparse.ArgumentParser(description='Process data to RDF')
parser.add_argument('dataIndexFile', nargs=1, type=argparse.FileType('r'))
parser.add_argument('window', nargs=1, type=int, help="Window size in ns")
parser.add_argument('outputBaseName', nargs=1,
                     help = "output file name = outputBaseName-01,-02,-03...")
args = parser.parse_args()

dataIndexFile = args.dataIndexFile[0]
window = args.window[0]
control = "../rdf_control"
outputBaseName = args.outputBaseName[0]

currentTime = -1
nextTime = 1
dataFileList = []
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
            dataFileList[currentTime].append(dataDir + "/" + line)
        else:
            if currentTime == nextTime - 1:
                dataFileList.append([])
                nextTime += 1
            else:
                sys.exit("Data index file error: " +
                         "data time is wrong or discontinued")

print("Window size: " + str(window))
for i in dataFileList:
    print(i)

for i in range(len(dataFileList) - window + 1):
    files = []
    for j in range(window):
        files += dataFileList[i+j]
#    print("rdf_dlchau -c " + control + " -o " + outputBaseName+"-%02d"%i + " -f " + " ".join(files))
    subprocess.check_call(["rdf_dlchau", "-c", control, 
                           "-o", outputBaseName + "-%02d"%(i+1),
                           "-f"] + files)

