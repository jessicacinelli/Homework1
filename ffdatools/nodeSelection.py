import glob
import sys

def selection(file, node):
    temp = []
    with open(file, mode="r") as file:
        for row in file:
            if node in row:
                temp.append(row)

    with open(node, mode="w") as output:
        for item in temp:
            #row = (', '.join(item))
            output.write(item+"\n")


if __name__ == '__main__':
    args = len(sys.argv)
    if len(sys.argv)>3:
        print ("\nERROR: incorrect number of arguments")
    else:
        file = sys.argv[1]
        node = sys.argv[2]
        selection(file, node)