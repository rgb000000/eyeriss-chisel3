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
    filter_split = []
    mem = open(path, "w")
    for i in range(math.ceil(filter.shape[3] / CHANNELMAX)):
        try:
            tmp = filter[:, :, :, i*CHANNELMAX : (i+1)*CHANNELMAX]
        except:
            tmp = filter[:, :, :, i*CHANNELMAX : ]
        filter_split.append(tmp)
    
    for f in filter_split:
        for i in range(f.shape[1] ** 2):
            for n in range(f.shape[0]):
                row = "".join(list(map("{:>02x}".format, list(map(complement, f[n, i // 3, i % 3, :]))))) + "\n"
                mem.write(row)

    mem.close()

def feature2mem(afeature, path=os.getcwd()+"/featureMem.hex"):
    
        """ Description
        :type feature:
        :param feature:
    
        :type path:
        :param path:
    
        :raises:
    
        :rtype:
        """
        
        ROWMAX = 14
        CHANNELMAX = 64
        shape = (afeature)