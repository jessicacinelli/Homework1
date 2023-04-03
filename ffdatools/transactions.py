import glob
import sys

def readfile(files):
    final = []
    for path in files:
        f=open(path, "r")
        nodes = []
        temp = []
        for line in f:
            temp.append(line.split(" ")[2])
        nodes = list(dict.fromkeys(temp))
        final.append(nodes)
    with open(folder+"/output.csv", mode="w") as file:
        for item in final:
            row = (', '.join(item))
            file.write(row+"\n")

if __name__ == '__main__':
    args = len(sys.argv)
    print(sys.argv[1])
    if len(sys.argv)>2:
        print ("\nERROR: incorrect number of arguments")
    else:
        folder = sys.argv[1]
        files = glob.glob(folder + "/*.txt")
        files.remove(folder+"\\interarrivals.txt")
        readfile(files)

