import numpy as np
import math
import os

BITS = 8
VARIANCE = 10

def complement(num):
    
    """ Description
    :type num: Int
    :param num: number need to get its complement

    :raises: null

    :rtype: num's complement
    """

    if num < 0:
        return (1 << BITS) - abs(num)
    else:
        return num

def filterG(n, w, c):

    """ Description
    :type n: Int
    :param n: number of filter or ouput channel

    :type w: Int
    :param w: width of filter 

    :type c: Int
    :param c: width of filter

    :raises: null

    :rtype: np.array with (n,w,c)
    """

    filter = np.random.randn(n, w, w, c) * VARIANCE
    return filter.astype(np.int)

def featureG(w,c):

    """ Description
    :type w: Int
    :param w:

    :type c: Int
    :param c:

    :raises: null

    :rtype: np.array with (w,w,c)
    """

    feature = np.random.randn(w,w,c) * VARIANCE
    return feature.astype(np.int)

def filter2mem(filters, path=os.getcwd()+"/filterMEM.hex"):

    """ Description
    :type filters: np.array 4D 
    :param filters: filters need to store

    :type path: String
    :param path: mem file path

    :raises: null

    :rtype: null
    """

    CHANNELMAX = 64
    shape = (filters.shape[0], filters.shape[1], filters.shape[2], math.ceil(filters.shape[3]/CHANNELMAX)*CHANNELMAX)
    filter = np.zeros(shape, dtype=np.int)
    filter[:, :, :, 0: filters.shape[3]] = filters
    filter_split = [filter[:, :, :, i*CHANNELMAX : (i+1)*CHANNELMAX] for i in range(math.ceil(filter.shape[3] / CHANNELMAX))]
    mem = open(path, "w")
    
    for f in filter_split:
        for i in range(f.shape[1] ** 2):
            for n in range(f.shape[0]):
                row = "".join(list(map("{:>02x}".format, list(map(complement, f[n, i // 3, i % 3, :]))))) + "\n"
                mem.write(row)

    mem.close()

def feature2mem(afeature, path=os.getcwd()+"/featureMEM.hex"):
    
    """ Description
    :type feature: np.array 3D
    :param feature:

    :type path:
    :param path:

    :raises:

    :rtype:
    """
    
    ROWMAX = 5
    CHANNELMAX = 64
    mem = open(path, "w")
    shape = (math.ceil(afeature.shape[0] / ROWMAX) * ROWMAX, afeature.shape[1], math.ceil(afeature.shape[2]/CHANNELMAX)*CHANNELMAX)
    feature = np.zeros(shape, dtype=np.int)
    feature[0:afeature.shape[0], :, 0:afeature.shape[2]] = afeature
    feature_split = [feature[:, :, i*CHANNELMAX : (i+1)*CHANNELMAX] for i in range(math.ceil(feature.shape[2] / CHANNELMAX))]
    for f in feature_split:
        for col in range(f.shape[1]):
            for row in range(int(f.shape[0] / ROWMAX)):
                mem_row = "".join(list(map("{:>02x}".format, list(map(complement, f[row*ROWMAX: (row+1)*ROWMAX, col, :].flatten()))))) + "\n"
                mem.write(mem_row)
                