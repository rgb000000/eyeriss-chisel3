import numpy as np

BITS = 8
VARIANCE = 10

def getSign(num):
    if num >= 0x80:
        return num - (1 << BITS)
    else:
        return num

def getW(startAddr, inChannel, outChannel, filterSize = 3, path = "./filterMEM.hex"):
    W_int = []
    with open(path, "r") as f:
        for line in f.readlines():
            line = line.rstrip()
            numbers = [line[i:i+2] for i in range(0, len(line), 2)]
            W_int.append(list(map(int, numbers, [16]*len(numbers))))

    channel = 64 # if (inChannel > 64) else inChannel     #卷积核拆分后最大通道数
    W = np.zeros([filterSize, filterSize, channel])
    #W = [ 卷积核大小， 卷积核大小, 卷积核拆分后最大通道数 ]
    n = 0 #读取数据的第几行
    for w in range(filterSize):
        for h in range(filterSize):
                W[w, h, :] = W_int[startAddr + n]
                n += outChannel
    shape = W.shape
    W = np.array(list(map(getSign, W.flatten())))
    return W.reshape(shape)

def getFeature(startAddr, inputWidth, inChannel):
    feature_int = []
    with open("feature.txt", "r") as f:
        for line in f.readlines():
            feature_int.append(list(map(int, line.strip("\n").split(" "))))

    channel = 64 if (inChannel > 64) else inChannel     #feature核拆分后最大通道数
    inputHeight = 16 if (inputWidth > 16) else inputWidth   #feature读取的最大行数
    
    feature = np.zeros([inputHeight, inputWidth, channel])
    #feature = [ feature读取的最大行数， feature列， feature核拆分后最大通道数]
    n = 0   #读取数据的第几行
    for w in range(inputWidth):
        for h in range(inputHeight):
            feature[h, w, :] = feature_int[startAddr + n][h * channel : (h + 1) * channel]  #一次读取64个数据
        n += inputWidth // inputHeight
    
    return feature

def conv_forward(feature, filter):
    pass