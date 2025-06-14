import numpy as np


class ArgMaxPolicy(object):

    def __init__(self, critic):
        self.critic = critic

    def get_action(self, obs):
        if len(obs.shape) > 3:
            observation = obs
        else:
            observation = obs[None]
        
        #################################################################################
        # TODO return the action that maximizes the Q-value                             #
        # at the current observation as the output.                                     #
        #################################################################################
        # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        actions = None
        # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

        return actions.squeeze()