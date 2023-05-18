# This script is to collect all result and save them in one file. Finally, it will output mean value of those results.
import sys
import os
import numpy as np

def read(run_start,run_end):
    estimators = []
    for i in range(run_start, run_end+1):
        fp = open(str(i) + '.Rout', 'r')
        for line in fp:
            if line[0:3] == '[1]':
                if len(line.split(' ')) == 2: # omit those are still running
                    estimators.append(float(line.strip().split(' ')[1]))
    np.savetxt('estimators.txt', np.array(estimators))
    
read(1,497)



