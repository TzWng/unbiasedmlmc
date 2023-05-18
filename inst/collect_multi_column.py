# This script is to collect all result and save them in one file. Finally, it will output mean value of those results.
import sys
import os
import numpy as np


def read(run_start,run_end):
    estimators = np.empty((0,5),float)
    for i in range(run_start, run_end+1):
        fp = open(str(i) + '.Rout', 'r')
        for line in fp:
            if line[0:3] == '[1]':
                if len(line.split(' ')) > 3: 
                    temp = line.strip('[1]').split()
                    temp_lst = [float(item) for item in temp]
                    estimators = np.append(estimators, np.array([temp_lst]),axis=0)
    np.savetxt('estimators.txt', np.array(estimators))
    
read(1,500)



