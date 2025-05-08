import numpy as np
import torch
import sys

class NESAttack(object):
    def __init__(self, model, epsilon, step_size, num_steps, criterion, device):
        # Arguements
        self.epsilon = epsilon # maximum perturbation
        self.step_size = step_size # step size per iteration
        self.num_steps = num_steps # the number of iterations
        self.criterion = criterion # type of loss function (xent for Cross-Entropy loss, cw for Carlini-Wagner loss)
        self.device = device # device to use

        # NES
        self.model = model
        self.nes_batch_size = 50 # number of vectors to sample for NES estimation
        self.sigma = 0.25 # noise scale variable for NES estimation

        ####################################################################
        # TODO: Implement Cross-Entropy loss and Carlini-Wagner loss.      #
        # For Carlini-Wagner loss, set the confidence of hinge loss to 50. #
        # Be careful about the sign of loss.                               #
        # Use the almost same implementation from FGSM!                    #
        ####################################################################
        
        # If loss function is Cross-Entropy loss
        if self.criterion == 'xent':
            def cross_entropy(logit, label):
                # TODO: implement Cross-Entropy loss
                return None
            self.loss = cross_entropy
        # If loss function is Carlini-Wagner loss
        elif self.criterion == 'cw':
            def carlini_wagner(logit, label):
                #TODO: implement your Carlini-Wagner loss
                return None
            self.loss = carlini_wagner
        else:
            print('Loss function must be xent or cw')
            sys.exit()

    def grad_est(self, image, label):
        ##########################################################################################################
        # TODO: Estimate the pixelwise gradient of the image regarding to the loss function using NES technique. #
        # Below structure is given for guidance, but it's free to just ignore it and implement your own version. #
        # noise_pos: (self.nes_batch_size) number of random vectors sampled from standard gaussian distribution. #
        # noise: full noise concatenating noise_pos and -noise_pos                                               #
        # image_batch: image added with sigma * noise                                                            #
        # label_batch: label tiled to image_batch size                                                           #
        # grad_est: resulting gradient estimation                                                                #
        ##########################################################################################################

        n, h, w, c = image.shape
        noise_pos = np.random.normal(size=(self.nes_batch_size, h, w, c))
        noise = torch.Tensor(np.concatenate([noise_pos, -noise_pos], axis=0)).to(device=self.device)
        
        # YOUR CODE HERE

        return grad_est.detach()
    
    def perturb(self, image, label):
        ###################################################################################################
        # TODO: Given an image and the corresponding label, generate an adversarial image using black-box #
        # PGD attack with NES gradient estimation.                                                        #
        # Please note that the pixel values of an adversarial image must be in a valid range [0, 255].    #
        ###################################################################################################
        
        lower = torch.clamp(image - self.epsilon, 0, 255).to(device=self.device)
        upper = torch.clamp(image + self.epsilon, 0, 255).to(device=self.device)
        adv_image = torch.clone(image).to(device=self.device)
        
        # YOUR CODE HERE
        
        return adv_image
