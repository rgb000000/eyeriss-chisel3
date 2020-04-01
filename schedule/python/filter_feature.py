import numpy as np

W = np.random.randint(-128, 127, size=(256 * 3 * 3 * 2, 64), dtype=np.int8)
print(W.shape)
np.savetxt("filter.txt", W,fmt='%d',delimiter=' ')

filterSize = 3      #卷积核大小

def getW(startAddr, inChannel, outChannel):
    W_int = []
    with open("filter.txt", "r") as f:
        for line in f.readlines():
            W_int.append(list(map(int, line.strip("\n").split(" "))))

    channel = 64 if (inChannel > 64) else inChannel     #卷积核拆分后最大通道数
    W = np.zeros([filterSize, filterSize, channel])
    #W = [ 卷积核大小， 卷积核大小, 卷积核拆分后最大通道数 ]
    n = 0 #读取数据的第几行
    for w in range(filterSize):
        for h in range(filterSize):
                W[w, h, :] = W_int[startAddr + n]
                n += outChannel

    return W;

w = getW(0, 64, 128)
print(w.shape)

feature = np.random.randint(-128, 127, size=(64 * 64 * 2, 64 * 16), dtype=np.int8)
print(feature.shape)
np.savetxt("feature.txt", feature, fmt='%d', delimiter=' ')

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

feature = getFeature(0, 128, 128)
print(feature.shape)

def conv_forward(feature, filter, stride):
    f = filter.shape[0]
    (n_H_prev, n_W_prev, n_C) = feature.shape
    n_H = int((n_H_prev - f)/stride + 1)
    n_W = int((n_W_prev - f)/stride + 1)
    Z = np.zeros([n_H, n_W, n_C])
    for h in range(n_H):
        for w in range(n_W):
            for c in range(n_C):
                vert_start = h * stride
                vert_end = vert_start + f
                horiz_start = w * stride
                horiz_end = horiz_start + f
                feature_slice = feature[vert_start:vert_end,horiz_start:horiz_end, :]
                Z[h, w, c] = np.sum(np.multiply(feature_slice[:, :, c], filter[:, :, c]))

    return Z
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#feature = np.random.randint(-128, 127, size=(7, 7, 16), dtype=np.int8)
#filter = np.random.randint(-128, 127, size=(3, 3, 16), dtype=np.int8)
#A = conv_forward(feature, filter, 1)
#print(A.shape)

inChannel = 64
outChannel = 128
inputSize = 52
splitFilterNum = (inChannel // 64) if (inChannel > 64) else 1   #卷积拆分个数
splitFeatureNum = int(inputSize / 16 + 0.5) if (inputSize > 16) else 1  #feature拆分行个数
A = np.zeros([14 * (splitFeatureNum + 1), 50, outChannel])
for startFilterAddr in range(outChannel):
        for n in range(splitFilterNum):     #第n个拆分块
            W_split = getW(startFilterAddr + n * filterSize * filterSize * outChannel, inChannel, outChannel)
            #卷积核地址每次增加filterSize * filterSize * outChannel
            for startFeatuteAddr in range(splitFeatureNum + 1):     #一个拆分块在所有feature上卷积
                feature_split = getFeature(startFeatuteAddr * 14 + n * 16 * inputSize * splitFeatureNum, inputSize, inChannel)
                #feature地址每次增加16 * inputSize * splitFeatureNum  交界处卷积计算时，上一次卷积16行中的最后两行，在下次卷积中继续计算
                t = conv_forward(feature_split, W_split, 1)
                A[14 * startFeatuteAddr: 14 * (startFeatuteAddr + 1), :, 64 * n: 64 * (n + 1)] = conv_forward(feature_split, W_split, 1)   #卷积合并
                #print(A)
A = A[0: 50, :, :]