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

def getW(startAddr):

    """ Description
    :type startAddr:    Int
    :param startAddr:   the first of mem address which need to be readed

    :raises:

    :rtype:             Filter 3D
    """

    W = np.zeros([FILTERSIZE, FILTERSIZE, inChannel])
    n = 0
    for w in range(FILTERSIZE):
        for h in range(FILTERSIZE):
            for g in range(inChannelGroup):
                W[w, h, CHANNELMAX * g: CHANNELMAX * (g+1)] = W_int[startAddr + n]
                n += 1
    shape = W.shape
    W = np.array(list(map(getSign, W.flatten())))
    return W.reshape(shape)

def getFeature(startAddr):

    """ Description
    :type startAddr:
    :param startAddr:

    :raises:

    :rtype:
    """

    offset = 0
    feature = np.zeros([ROWMAX, featureSize, inChannel])
    for col in range(feature.shape[1]):
        for g in range(inChannelGroup):
            col_data = np.array(feature_int[startAddr + offset])
            feature[:, col, g*CHANNELMAX: (g+1)*CHANNELMAX] = col_data.reshape(ROWMAX, CHANNELMAX)
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
    for feature_split_num in range(math.ceil(n / (ROWMAX - FILTERSIZE + 1))):
        for filterNum in range(outChannel):
            Z_split = np.zeros((ROWMAX - FILTERSIZE + 1, n))
            filter_addr = filterNum * (FILTERSIZE ** 2) * inChannelGroup
            feature_addr = feature_split_num * inChannelGroup * featureSize
            filter = getW(filter_addr)
            feature = getFeature(feature_addr)
            print("filter addr: {:>08x},   feature addr: {:>08x}".format(filter_addr, feature_addr))
            Z_split = conv_forward(feature, filter)
            if (feature_split_num + 1)*Z_split.shape[0] <= Z.shape[1]:
                Z[feature_split_num*Z_split.shape[0]: (feature_split_num + 1)*Z_split.shape[0], :, filterNum] = Z_split
            else:
                residue = Z.shape[1] - (feature_split_num + 1)*Z_split.shape[0]
                Z[feature_split_num*Z_split.shape[0]: (feature_split_num + 1)*Z_split.shape[0], :, filterNum] = Z_split[0:residue, :]
    return Z

def MaxPool(Z):
    shape = Z.shape
    assert(shape[0]%2 == 0)
    shape = [int(shape[0]/2), int(shape[1]/2), int(shape[2])]
    ZZ = np.zeros(shape)
    for i in range(shape[0]):
        for j in range(shape[1]):
            for k in range(shape[2]):
                ZZ[i, j ,k] = Z[i*2:(i+1)*2, j*2:(j+1)*2, k].max()
    return ZZ

def layerTest():
    
    """ Description: use to test schedule program
    :raises:

    :rtype: return simulation result
    """

    W = filterG(outChannel, FILTERSIZE, inChannel)
    F = featureG(featureSize, inChannel)
    filter2mem(W)
    feature2mem(F)
    FPading= np.zeros([featureSize + 2, featureSize + 2, inChannel])
    FPading[1:featureSize+1, 1:featureSize+1, :] = F
    F = FPading
    sw_Z = conv4D_forward(F, W)
    print(sw_Z.shape)
    readFile("./featureMEM.hex", "./filterMEM.hex")
    hw_Z = layer(CHANNELMAX, outChannel, featureSize)
    # print(hw_Z.shape)
    # print(np.sum(hw_Z != sw_Z))
    # assert(np.sum(hw_Z != sw_Z) == 0)
    np.save("W", W)
    np.save("F", F)
    np.save("swZ", sw_Z)
    feature2mem(sw_Z, os.getcwd() +  "/result.hex")
    return W, F, sw_Z

if __name__ == '__main__':
    layerTest()
