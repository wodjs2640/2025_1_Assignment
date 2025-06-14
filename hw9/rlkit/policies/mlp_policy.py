import abc
import itertools

import numpy as np
import torch
from torch import nn
from torch.nn import functional as F
from torch import optim
from torch import distributions

from rlkit.infrastructure import pytorch_util as ptu
from rlkit.policies.base_policy import BasePolicy


class MLPPolicy(BasePolicy, nn.Module, metaclass=abc.ABCMeta):

    def __init__(self,
                 ac_dim,
                 ob_dim,
                 n_layers,
                 size,
                 discrete=False,
                 learning_rate=1e-4,
                 training=True,
                 nn_baseline=False,
                 **kwargs
                 ):
        super().__init__(**kwargs)

        # init vars
        self.ac_dim = ac_dim
        self.ob_dim = ob_dim
        self.n_layers = n_layers
        self.discrete = discrete
        self.size = size
        self.learning_rate = learning_rate
        self.training = training
        self.nn_baseline = nn_baseline

        if self.discrete:
            self.logits_na = ptu.build_mlp(input_size=self.ob_dim,
                                           output_size=self.ac_dim,
                                           n_layers=self.n_layers,
                                           size=self.size)
            self.logits_na.to(ptu.device)
            self.mean_net = None
            self.logstd = None
            self.optimizer = optim.Adam(self.logits_na.parameters(),
                                        self.learning_rate)
        else:
            self.logits_na = None
            self.mean_net = ptu.build_mlp(input_size=self.ob_dim,
                                      output_size=self.ac_dim,
                                      n_layers=self.n_layers, size=self.size)
            self.logstd = nn.Parameter(
                torch.zeros(self.ac_dim, dtype=torch.float32, device=ptu.device)
            )
            self.mean_net.to(ptu.device)
            self.logstd.to(ptu.device)
            self.optimizer = optim.Adam(
                itertools.chain([self.logstd], self.mean_net.parameters()),
                self.learning_rate
            )

        if nn_baseline:
            self.baseline = ptu.build_mlp(
                input_size=self.ob_dim,
                output_size=1,
                n_layers=self.n_layers,
                size=self.size,
            )
            self.baseline.to(ptu.device)
            self.baseline_optimizer = optim.Adam(
                self.baseline.parameters(),
                self.learning_rate,
            )
        else:
            self.baseline = None

    ##################################

    def save(self, filepath):
        torch.save(self.state_dict(), filepath)

    ##################################

    # query the policy with observation(s) to get selected action(s)
    def get_action(self, obs: np.ndarray) -> np.ndarray:
        obs = ptu.from_numpy(obs).view(1, -1)
        action_distribution = self.forward(obs)
        action = action_distribution.sample()
        action = ptu.to_numpy(action)
        return action

    # update/train this policy
    def update(self, observations, actions, **kwargs):
        raise NotImplementedError

    # This function defines the forward pass of the network.
    def forward(self, observation: torch.FloatTensor):
        if self.discrete:
            logit = self.logits_na(observation)
            action_distribution = distributions.categorical.Categorical(logits=logit)
        else:
            mean = self.mean_net(observation)
            std = torch.exp(self.logstd)
            action_distribution = distributions.normal.Normal(loc=mean, scale=std)
        return action_distribution


#####################################################
#####################################################

class MLPPolicyPG(MLPPolicy):
    def update(self, paths):
        # preprocess paths
        observations = np.concatenate([path["observation"] for path in paths], axis=0)
        actions = np.concatenate([path["action"] for path in paths], axis=0)
        advantages = np.concatenate([path["advantage"] for path in paths], axis=0)
        returns = np.concatenate([path["return"] for path in paths], axis=0)  

        observations = ptu.from_numpy(observations)
        actions = ptu.from_numpy(actions)
        advantages = ptu.from_numpy(advantages)
        returns = ptu.from_numpy(returns)

        #########################################################################################
        # TODO: compute the policy loss and optimize it.                                        #
        # HINT1: Recall that the expression that we want to MAXIMIZE                            #
        # is the expectation over collected trajectories of:                                    #
        # sum_{t=0}^{T-1} [grad [log pi(a_t|s_t) * (Q_t - b_t)]]                                #
        # HINT2: use the `log_prob` method on the distribution returned by the `forward` method.#
        # HINT3: use self.optimizer to optimize the loss. Remember to 'zero_grad' first.        #
        # HINT4: don't forget that `optimizer.step()` minimizes a loss.                         #
        #########################################################################################
        # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        policy_loss = None
        # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

        if self.nn_baseline:
            for _ in range(10):
                #########################################################################################
                # TODO: compute the baseline loss and optimize it.                                      #
                # HINT1: use `F.mse_loss`.                                                              #
                # HINT2: use self.baseline_optimizer to optimize the loss.                              #
                #########################################################################################
                # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
                baseline_loss = None
                # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        
        train_log = {
            'Policy Loss': ptu.to_numpy(policy_loss),
        }
        if self.nn_baseline:
          train_log['Baseline Loss'] = ptu.to_numpy(baseline_loss)

        return train_log

    def run_baseline_prediction(self, obs):
        """
            Helper function that converts `obs` to a tensor,
            calls the forward method of the baseline MLP,
            and returns a np array

            Input: `obs`: np.ndarray of size [N, 1]
            Output: np.ndarray of size [N]

        """
        obs = ptu.from_numpy(obs)
        predictions = self.baseline(obs)
        return ptu.to_numpy(predictions)[:, 0]
