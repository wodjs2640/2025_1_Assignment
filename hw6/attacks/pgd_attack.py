import numpy as np
import torch
from attacks.fgsm_attack import FGSMAttack

class PGDAttack(object):
    def __init__(self, model, epsilon, step_size, num_steps, criterion, device):
        # Arguements
        self.epsilon = epsilon # maximum perturbation
        self.step_size = step_size # step size per iteration
        self.num_steps = num_steps # the number of iterations
        self.criterion = criterion # type of loss function (xent for Cross-Entropy loss, cw for Carlini-Wagner loss)
        self.device = device # device to use: cpu or cuda
        ##########################################
        # TODO: Create an instance of FGSMAttack #
        ##########################################
        
        # YOUR CODE HERE
    
    def perturb(self, image, label):
        ################################################################################################
        # TODO: Given an image and the corresponding label, generate an adversarial image using PGD.   # 
        # Please note that the pixel values of an adversarial image must be in a valid range [0, 255]. #
        ################################################################################################
        
        lower = torch.clamp(image - self.epsilon, 0, 255).to(device=self.device)
        upper = torch.clamp(image + self.epsilon, 0, 255).to(device=self.device)
        adv_image = image.to(device=self.device)
        
        # YOUR CODE HERE
        
        return adv_image

