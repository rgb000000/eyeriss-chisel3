import numpy as np
# GRADED FUNCTION: zero_pad

def zero_pad(X, pad):
    """
    Pad with zeros all images of the dataset X. The padding is applied to the height and width of an image, 
    as illustrated in Figure 1.
    
    Argument:
    X -- python numpy array of shape (m, n_H, n_W, n_C) representing a batch of m images
    pad -- integer, amount of padding around each image on vertical and horizontal dimensions
    
    Returns:
    X_pad -- padded image of shape (m, n_H + 2*pad, n_W + 2*pad, n_C)
    """
    
    X_pad = np.pad(X,((0,0), (pad,pad), (pad,pad), (0,0)),"constant",constant_values=(0,0))
    
    return X_pad
    
    # GRADED FUNCTION: conv_single_step

def conv_single_step(a_slice_prev, W, b):
    """
    Apply one filter defined by parameters W on a single slice (a_slice_prev) of the output activation 
    of the previous layer.
    
    Arguments:
    a_slice_prev -- slice of input data of shape (f, f, n_C_prev)
    W -- Weight parameters contained in a window - matrix of shape (f, f, n_C_prev)
    b -- Bias parameters contained in a window - matrix of shape (1, 1, 1)
    
    Returns:
    Z -- a scalar value, result of convolving the sliding window (W, b) on a slice x of the input data
    """

    s = np.multiply(a_slice_prev,W)
    Z = np.sum(s)
    Z = Z + b

    return Z
    
    # GRADED FUNCTION: conv_forward

def conv_forward(A_prev, W, b, hparameters):
    """
    Implements the forward propagation for a convolution function
    
    Arguments:
    A_prev -- output activations of the previous layer, numpy array of shape (m, n_H_prev, n_W_prev, n_C_prev)
    W -- Weights, numpy array of shape (f, f, n_C_prev, n_C)
    b -- Biases, numpy array of shape (1, 1, 1, n_C)
    hparameters -- python dictionary containing "stride" and "pad"
        
    Returns:
    Z -- conv output, numpy array of shape (m, n_H, n_W, n_C)
    cache -- cache of values needed for the conv_backward() function
    """
     
    (m, n_H_prev, n_W_prev, n_C_prev) = A_prev.shape
    
    (f, f, n_C_prev, n_C) = W.shape
    
    stride = hparameters["stride"]
    pad = hparameters["pad"]
    
    n_H = int((n_H_prev - f + 2 * pad)/stride + 1)
    n_W = int((n_W_prev - f + 2 * pad)/stride + 1)
    
    Z = np.zeros((m, n_H, n_W, n_C), dtype=np.int8)
    
    # Create A_prev_pad by padding A_prev
    A_prev_pad = zero_pad(A_prev,pad)
    
    for i in range(m):                               # loop over the batch of training examples
        a_prev_pad = A_prev_pad[i]                              # Select ith training example's padded activation
        
        for h in range(n_H):                           # loop over vertical axis of the output volume
            for w in range(n_W):                       # loop over horizontal axis of the output volume
                for c in range(n_C):                   # loop over channels (= #filters) of the output volume
                    
                    vert_start = h * stride
                    vert_end = vert_start + f
                    horiz_start = w * stride
                    horiz_end = horiz_start + f
                    
                    a_slice_prev = a_prev_pad[vert_start:vert_end,horiz_start:horiz_end,:]
                    
                    Z[i, h, w, c] = conv_single_step(a_slice_prev,W[:,:,:,c],b[:,:,:,c])
                                        
    
    # Making sure your output shape is correct
    assert(Z.shape == (m, n_H, n_W, n_C))
    
    # Save information in "cache" for the backprop
    cache = (A_prev, W, b, hparameters)
    
    return Z
    
    # GRADED FUNCTION: pool_forward

def pool_forward(A_prev, hparameters, mode = "max"):
    """
    Implements the forward pass of the pooling layer
    
    Arguments:
    A_prev -- Input data, numpy array of shape (m, n_H_prev, n_W_prev, n_C_prev)
    hparameters -- python dictionary containing "f" and "stride"
    mode -- the pooling mode you would like to use, defined as a string ("max" or "average")
    
    Returns:
    A -- output of the pool layer, a numpy array of shape (m, n_H, n_W, n_C)
    cache -- cache used in the backward pass of the pooling layer, contains the input and hparameters 
    """
    
    # Retrieve dimensions from the input shape
    (m, n_H_prev, n_W_prev, n_C_prev) = A_prev.shape
    
    # Retrieve hyperparameters from "hparameters"
    f = hparameters["f"]
    stride = hparameters["stride"]
    
    # Define the dimensions of the output
    n_H = int(1 + (n_H_prev - f) / stride)
    n_W = int(1 + (n_W_prev - f) / stride)
    n_C = n_C_prev
    
    # Initialize output matrix A
    A = np.zeros((m, n_H, n_W, n_C))              
    
    for i in range(m):                         # loop over the training examples
        for h in range(n_H):                     # loop on the vertical axis of the output volume
            for w in range(n_W):                 # loop on the horizontal axis of the output volume
                for c in range (n_C):            # loop over the channels of the output volume
                    
                    # Find the corners of the current "slice" 
                    vert_start = h * stride
                    vert_end = vert_start + f
                    horiz_start = w * stride
                    horiz_end = horiz_start + f
                    
                    # Use the corners to define the current slice on the ith training example of A_prev, channel c. 
                    a_prev_slice = A_prev[i,vert_start:vert_end,horiz_start:horiz_end,c]
                    
                    # Compute the pooling operation on the slice. Use an if statment to differentiate the modes. Use np.max/np.mean.
                    if mode == "max":
                        A[i, h, w, c] = np.max(a_prev_slice)
                    elif mode == "average":
                        A[i, h, w, c] = np.mean(a_prev_slice)
    
    
    # Store the input and hparameters in "cache" for pool_backward()
    cache = (A_prev, hparameters)
    
    # Making sure your output shape is correct
    assert(A.shape == (m, n_H, n_W, n_C))
    
    return A
    
def leaky_relu(x):
    return np.where(x>0, x, 0.1*x)
    
def batch_normalization(x, gamma, beta):
    x_mean = np.mean(x)
    x_var = np.var(x)
    x_norm = (x - x_mean) / np.sqrt(x_var + 1e-5)
    out = gamma * x_norm + beta
    return out
    
A_prev = np.random.randint(0, 64, size=(10, 4, 4, 3), dtype=np.int8)
#print(A_prev)
W = np.random.randint(0, 16, size=(2,2,3,8), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,8), dtype=np.int8)
hparameters = {"pad" : 2,
               "stride": 2}

Z = conv_forward(A_prev, W, b, hparameters)
#print(Z.shape)
out = batch_normalization(A_prev, 1, 1)
x = conv_forward(out, W, b, hparameters)
#print(x)

#convolutional
x = np.random.randint(0, 64, size=(1,608,608,3), dtype=np.int8)
W = np.random.randint(0, 16, size=(3,3,3,32), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,32), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#maxpool
x = pool_forward(x, hparameters={"stride": 2, "f": 2})
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,32,64), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,64), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#maxpool
x = pool_forward(x, hparameters={"stride": 2, "f": 2})
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,64,128), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,128), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(1,1,128,64), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,64), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 0, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,64,128), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,128), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#maxpool
x = pool_forward(x, hparameters={"stride": 2, "f": 2})
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,128,256), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,256), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(1,1,256,128), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,128), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 0, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,128,256), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,256), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#maxpool
x = pool_forward(x, hparameters={"stride": 2, "f": 2})
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,256,512), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,512), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(1,1,512,256), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,256), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 0, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,256,512), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,512), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(1,1,512,256), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,256), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 0, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,256,512), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,512), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#maxpool
x = pool_forward(x, hparameters={"stride": 2, "f": 2})
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,512,1024), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,1024), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(1,1,1024,512), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,512), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 0, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,512,1024), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,1024), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(1,1,1024,512), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,512), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 0, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(3,3,512,1024), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,1024), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 1, "stride": 1})
x = batch_normalization(x, 1, 1)
x = leaky_relu(x)
print(x.shape)

#convolutional
W = np.random.randint(0, 16, size=(1,1,1024,425), dtype=np.int8)
b = np.random.randint(0, 16, size=(1,1,1,425), dtype=np.int8)
x = conv_forward(x, W, b, hparameters={"pad": 0, "stride": 1})
#x = batch_normalization(x, 1, 1)
#x = leaky_relu(x)
print(x.shape)

#avergerpool
x = pool_forward(x, hparameters={"stride": 2, "f": 2}, mode="average")
print(x.shape)