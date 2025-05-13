import numpy as np
import sys
import torch
from torch.autograd import Variable
import torchvision

from mean_std import mean_torch, std_torch

class FGSMAttack(object):
    def __init__(self, model, epsilon, criterion, device):
        # Arguements
        self.epsilon = epsilon # maximum perturbation
        self.criterion = criterion # type of loss function (xent for Cross-Entropy loss, cw for Carlini-Wagner loss)
        self.device = device

        # Networks
        self.model = model
        
        ####################################################################
        # TODO: Implement Cross-Entropy loss and Carlini-Wagner loss.      #
        # For Carlini-Wagner loss, set the confidence of hinge loss to 50. #
        # Be careful about the sign of loss.                               #
        ####################################################################
        
        # If loss function is Cross-Entropy loss
        if self.criterion == 'xent':
            # TODO: define Cross-Entropy loss
            self.loss = torch.nn.CrossEntropyLoss()

        # If loss function is Carlini-Wagner loss
        elif self.criterion == 'cw':
            def carlini_wagner(logit, label):
                #TODO: implement your Carlini-Wagner loss
                correct_logit = logit[torch.arange(logit.size(0)), label]
                wrong_logit = torch.max(logit - torch.eye(logit.size(1)).to(self.device)[label] * 1e6, dim=1)[0]
                return -torch.sum(correct_logit - wrong_logit)
            self.loss = carlini_wagner

        else:
            print('Loss function must be xent or cw')
            sys.exit()
    
    def perturb(self, image, label):
        ################################################################################################
        # TODO: Given an image and the corresponding label, generate an adversarial image using FGSM.  # 
        # Please note that the pixel values of an adversarial image must be in a valid range [0, 255]. #
        ################################################################################################
        lower = 0
        upper = 255
        
        image = image.to(device=self.device)
        label = label.to(device=self.device)
        
        # YOUR CODE HERE
        image = image.detach().clone()
        image.requires_grad = True
        logits = self.model(image)
        loss = self.loss(logits, label)
        
        self.model.zero_grad()
        loss.backward()
        
        adv_image = image + self.epsilon * torch.sign(image.grad)
        adv_image = torch.clamp(adv_image, lower, upper)
        
        return adv_image
      
