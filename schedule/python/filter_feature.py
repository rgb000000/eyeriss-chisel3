import numpy as np
import math
from store import *

from config import *

def getSign(num):
    if num >= 0x80:
        return num - (1 << BITS)
    else:
        return num

W_int = []
feature_int = []
def readFile(featureMEMPath, filterMEMPath):

    """ Description
    :type featureMEMPath:
    :param featureMEMPath:

    :type filterMEMPath:
    :param filterMEMPath:

    :raises:

    :rtype:
    """

    with open(featureMEMPath, "r") as f:
        for line in f.readlines():
            line = line.rstrip()
            numbers = [line[i:i+2] for i in range(0, len(line), 2)][::-1]
            feature_int.append(list(map(int, numbers, [16]*len(numbers))))
    with open(filterMEMPath, "r") as f:
        for line in f.readlines():
            line = line.rstrip()
            numbers = [line[i:i+2] for i in range(0, len(line), 2)][::-1]
            W_int.append(list(map(int, numbers, [16]*len(numbers))))

def getW(startAddr, outChannel):

    """ Description
    :type startAddr:    Int
    :param startAddr:   the first of mem address which need to be readed

    :type outChannel:   Int
    :param outChannel:  the number of this conv layer's outChannel, in other word the number of filter

    :type path:         String
    :param path:        filter mem file path

    :raises:

    :rtype:             Filter 3D
    """

    W = np.zeros([FILTERSIZE, FILTERSIZE, CHANNELMAX])
    #W = [ 卷积核大小， 卷积核大小, 卷积核拆分后最大通道数 ]
    n = 0 #读取数据的第几行
    for w in range(FILTERSIZE):
        for h in range(FILTERSIZE):
                W[w, h, :] = W_int[startAddr + n]
                n += outChannel
    shape = W.shape
    W = np.array(list(map(getSign, W.flatten())))
    return W.reshape(shape)

def getFeature(startAddr, inFeatureSize):

    """ Description
    :type startAddr:
    :param startAddr:

    :type inFeatureSize:
    :param inFeatureSize:

    :raises:

    :rtype:
    """

    feature = np.zeros([ROWMAX, inFeatureSize, CHANNELMAX])
    offset = 0
    for col in range(feature.shape[1]):
        col_data = np.array(feature_int[startAddr + offset])
        feature[:, col, :] = col_data.reshape(ROWMAX, CHANNELMAX)
        offset += 1
    shape = feature.shape
    feature = np.array(list(map(getSign, feature.flatten())))
    return feature.reshape(shape)

def conv_forward(feature, filter):

    """ Description:    use for single filter, one outChannel
    :type feature:
    :param feature:

    :type filter:
    :param filter:

    :raises:

    :rtype:
    """

    assert(feature.shape[2] == filter.shape[2])
    shape = [
        feature.shape[0] - filter.shape[0] + 1, 
        feature.shape[1] - filter.shape[1] + 1]
    Z = np.zeros(shape)
    for x in range(shape[0]):
        for y in range(shape[1]):
            Z[x,y] = np.sum(filter * feature[x:x+filter.shape[0], y:y+filter.shape[1], :])
    return Z

def conv4D_forward(feature, filter):

    """ Description:    use for conv 4D filter, outChannel 
    :type feature:
    :param feature:

    :type filter:
    :param filter:

    :raises:

    :rtype:
    """

    assert(feature.shape[2] == filter.shape[3])
    shape = [
        feature.shape[0] - filter.shape[1] + 1, 
        feature.shape[1] - filter.shape[2] + 1,
        filter.shape[0]]
    Z = np.zeros(shape)
    for outchannel in range(filter.shape[0]):
        Z[:, :, outchannel] = conv_forward(feature, filter[outchannel, :, :])
    return Z

def layer(inChannel, outChannel, featureSize):
    
    """ Description:    to simulate a layer
    :type inChannel:
    :param inChannel:   input channel number

    :type outChannel:
    :param outChannel:  output channel number or filter number

    :type featureSize:
    :param featureSize: feature size

    :raises:

    :rtype:             return a layer result
    """

    n = featureSize - FILTERSIZE + 1
    Z = np.zeros((n, n, outChannel))
    for filterNum in range(outChannel):
        for feature_split_num in range(math.ceil(n / (ROWMAX - 3 + 1))):
            Z_split = np.zeros((ROWMAX - FILTERSIZE + 1, n))
            for channel_split_num in range(math.ceil(inChannel / CHANNELMAX)):
                filter_addr = channel_split_num * outChannel * (FILTERSIZE**2) + filterNum
                feature_addr = channel_split_num * (math.ceil((featureSize - ROWMAX)/STEP)+1)*featureSize + feature_split_num * featureSize
                filter = getW(filter_addr, outChannel)
                feature = getFeature(feature_addr, featureSize)
                print("filter addr: {:>08x},   feature addr: {:>08x}".format(filter_addr, feature_addr))
                Z_split += conv_forward(feature, filter)
            if (feature_split_num + 1)*Z_split.shape[0] <= Z.shape[1]:
                Z[feature_split_num*Z_split.shape[0]: (feature_split_num + 1)*Z_split.shape[0], :, filterNum] = Z_split
            else:
                residue = Z.shape[1] - (feature_split_num + 1)*Z_split.shape[0]
                Z[feature_split_num*Z_split.shape[0]: (feature_split_num + 1)*Z_split.shape[0], :, filterNum] = Z_split[0:residue, :]
    return Z

def layerTest():
    
    """ Description: use to test schedule program
    :raises:

    :rtype: return simulation result
    """

    outChanel = 1
    featureSize = 5
    W = filterG(outChanel, FILTERSIZE, CHANNELMAX)
    F = featureG(featureSize, CHANNELMAX)
    sw_Z = conv4D_forward(F, W)
    print(sw_Z.shape)
    filter2mem(W)
    feature2mem(F)
    readFile("./featureMEM.hex", "./filterMEM.hex")
    hw_Z = layer(CHANNELMAX, outChanel, featureSize)
    print(hw_Z.shape)
    print(np.sum(hw_Z != sw_Z))
    np.save("W", W)
    np.save("F", F)
    np.save("Z", hw_Z)
    return W, F, hw_Z

if __name__ == '__main__':
    layerTest()
