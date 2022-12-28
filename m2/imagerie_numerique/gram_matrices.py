import numpy as np
from vgg import Decoder, Encoder
import util
import torch

def gram_matrix(activations):
    # "vectorize" the first two dimensions - do it 
    h = activations.shape[0]
    w = activations.shape[1]
    m_l = h*w
    n_fm = activations.shape[-1]
    F = activations.reshape(m_l,n_fm).T
    G = F @ F.T 

    return G

def mse_gram_matrix(image_1, image_2):
  """Function to compute mse between gram matrices of two images for each layer
  output = loss vector of mse between gram matrices for each layer
  """

  device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

  style_scale=1
  oversize_style=False
  size=512
  # load inputs
  images1 = util.load_styles(image_1, size=size, scale=style_scale, oversize=oversize_style)
  images2 = util.load_styles(image_2, size=size, scale=style_scale, oversize=oversize_style)
  
  mse_gram_layer = np.zeros(5)
  for l in range(5, 0, -1):
    # encode layer to VGG feature space
    with Encoder(l).to(device) as encoder:
        image1_layer = torch.cat([encoder(image1) for image1 in images1])
        print(image1_layer.shape)
        image2_layer = torch.cat([encoder(image2) for image2 in images2])
    
    gram_output_layer = gram_matrix(image1_layer[0])
    gram_style_layer = gram_matrix(image2_layer[0])
    h = image1_layer.shape[0]
    w = image1_layer.shape[1]
    m_l = h*w
    n_l = image1_layer.shape[-1]

    mse_gram_layer[l-1] = torch.sum((gram_output_layer - gram_style_layer)*2)/(4 * n_l2 * m_l*2)

  return mse_gram_layer
