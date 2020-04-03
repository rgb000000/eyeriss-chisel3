import numpy as np
import math
import os

BITS = 8
VARIANCE = 2

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
    FILTERSIZE = 3
    CHANNELMAX = 64
    STEP = ROWMAX - (FILTERSIZE - 1)

    mem = open(path, "w")

    row_need_complement = (STEP - (afeature.shape[0] - ROWMAX) % STEP) % STEP
    print("complement {} rows".format(row_need_complement))
    shape = [afeature.shape[0] + row_need_complement, afeature.shape[1], max(afeature.shape[2], CHANNELMAX)]
    feature = np.zeros(shape, dtype=np.int)
    feature[0: afeature.shape[0], :, 0:afeature.shape[2]] = afeature
    feature_split = [feature[:, :, i*CHANNELMAX : (i+1)*CHANNELMAX] for i in range(math.ceil(feature.shape[2] / CHANNELMAX))]
    for f in feature_split:
        head = 0
        for i in range(1 + int((f.shape[0] - ROWMAX) / STEP)):
            for col in range(f.shape[1]):
                mem_row = "ERROR \n"
                mem_row = "".join(list(map("{:>02x}".format, list(map(complement, f[head:head+ROWMAX, col, :].flatten()))))) + "\n"
                mem.write(mem_row)
            head += STEP
        assert((head - STEP + ROWMAX) == f.shape[0])
    mem.close()