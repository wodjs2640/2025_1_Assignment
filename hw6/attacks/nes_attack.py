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
            self.loss = torch.nn.functional.cross_entropy
        # If loss function is Carlini-Wagner loss
        elif self.criterion == 'cw':
            def carlini_wagner(logit, label):
                #TODO: implement your Carlini-Wagner loss
                one_hot = torch.zeros_like(logit).scatter_(1, label.unsqueeze(1), 1)
                other = torch.max(logit * (1 - one_hot), dim=1)[0]
                real = torch.max(logit * one_hot, dim=1)[0]
                return torch.clamp(other - real + 50, min=0)
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

        n, c, h, w = image.shape
        
        # YOUR CODE HERE
        noise_pos = torch.randn(self.nes_batch_size, c, h, w, device=self.device)
        noise = torch.cat([noise_pos, -noise_pos], dim=0)
        
        image_batch = image.repeat(2 * self.nes_batch_size, 1, 1, 1) + self.sigma * noise
        label_batch = label.repeat(2 * self.nes_batch_size).to(device=self.device)
        
        with torch.no_grad():
            logits = self.model(image_batch)
            losses = self.loss(logits, label_batch)

        if losses.dim() == 0:
            losses = losses.unsqueeze(0).repeat(2 * self.nes_batch_size)
        
        grad_est = torch.sum(losses.view(-1, 1, 1, 1) * noise, dim=0) / (2 * self.nes_batch_size * self.sigma)
        
        return grad_est.detach()
    
    def perturb(self, image, label):
        ###################################################################################################
        # TODO: Given an image and the corresponding label, generate an adversarial image using black-box #
        # PGD attack with NES gradient estimation.                                                        #
        # Please note that the pixel values of an adversarial image must be in a valid range [0, 255].    #
        ###################################################################################################
        
        lower = torch.clamp(image - self.epsilon, 0, 255).to(device=self.device)
        upper = torch.clamp(image + self.epsilon, 0, 255).to(device=self.device)
        adv_image = image.clone().detach().to(device=self.device)
        
        # YOUR CODE HERE
        adv_image = adv_image + torch.empty_like(adv_image).uniform_(-self.epsilon, self.epsilon)
        adv_image = torch.clamp(adv_image, 0, 255)
        
        for _ in range(self.num_steps):
            grad = self.grad_est(adv_image, label)
            adv_image = adv_image + self.step_size * torch.sign(grad)
            adv_image = torch.clamp(adv_image, lower, upper)

        return adv_image
