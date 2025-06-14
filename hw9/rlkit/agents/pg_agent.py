import numpy as np

from rlkit.agents.base_agent import BaseAgent
from rlkit.policies.mlp_policy import MLPPolicyPG
from rlkit.infrastructure import utils


class PGAgent(BaseAgent):
    def __init__(self, env, agent_params):
        super(PGAgent, self).__init__()

        # init vars
        self.env = env
        self.agent_params = agent_params
        self.gamma = self.agent_params['gamma']
        self.standardize_advantages = self.agent_params['standardize_advantages']
        self.nn_baseline = self.agent_params['nn_baseline']

        # actor/policy
        self.actor = MLPPolicyPG(
            self.agent_params['ac_dim'],
            self.agent_params['ob_dim'],
            self.agent_params['n_layers'],
            self.agent_params['size'],
            discrete=self.agent_params['discrete'],
            learning_rate=self.agent_params['learning_rate'],
            nn_baseline=self.agent_params['nn_baseline']
        )

    def train(self, paths):
        """
            Training a PG agent using the given paths.
        """
        # step 1: compute baselines
        if self.nn_baseline:
            self.calculate_baselines(paths)

        # step 2: compute returns
        self.calculate_returns(paths)

        # step 3: compute advantages
        self.calculate_advantages(paths)
        
        # step 4: update the policy
        train_log = self.actor.update(paths)

        return train_log

    def calculate_baselines(self, paths):
        """
            Calculating the predicted values of the baseline on s_0, s_1, ..., s_T.
        """
        for path in paths:
            observations = np.concatenate(
                [path["observation"], path["next_observation"][-1:]], axis=0
            )
            #####################################################################################################################                   
            # TODO: compute the baseline predictions given the observations and append them to the paths.                       #
            ## HINT1: use the `run_baseline_prediction` method on the actor.                                                    #
            ## HINT2: if a path is terminated, then the predicted value of the baseline for the last state s_{T+1} should be 0. #
            #####################################################################################################################                   
            # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
            path["baseline"] = None
            # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

    def calculate_returns(self, paths):
        """
            Calculating the discounted sum of rewards for the given paths.
        """
        for path in paths:
            rewards = path["reward"]
            #########################################################################################################################                   
            # TODO: compute the discounted sum of rewards and append them to the paths.                                             #
            ## HINT1: use the `_discounted_cumsum` method defined below.                                                            #
            ## HINT2: Note that you can (and should) take into account the baselines if self.nn_baseline==True.                     #
            ## This is because there are trajectories that end via 'timeout' and not 'done'.                                        #
            ## For example, if a trajectory ends with timeout, it is incorrect to set the reward-to-go for s_T as r_T since         #
            ## in reality, the agent can keep taking actions after s_T.                                                             #
            ## Refer to '_discounted_cumsum' function for the detailed equation.                                                    #
            ## If not using the baseline, set V(s_{T+1}) to 0.                                                                      #
            #########################################################################################################################                   
            # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
            path["return"] = None
            # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
    
    def calculate_advantages(self, paths):
        """
            Computing advantages by subtracting the baselines from the returns.
        """
        #####################################################################################################################                   
        # TODO: compute the advantages, standardize them, and append them to the paths.                                     #
        # HINT1: don't forget the case when the baseline function is not used.                                              #
        # HINT2: don't forget to standardize advantages.                                                                    #
        #####################################################################################################################                   
        # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        for path in paths:
            path["advantage"] = None

        if self.standardize_advantages:        
            pass
        # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

    #####################################################
    ################## HELPER FUNCTIONS #################
    #####################################################

    def _discounted_cumsum(self, rewards):
        """
            Helper function which takes a list of rewards {r_1, ..., r_t, ... r_T, V(s_{T+1})}
            and returns a list of the same length where the entry in each index t is 
            sum_{t'=t}^T gamma^(t'-t) * r_{t'} + gamma^(T+1-t) * V(s_{T+1}).
        """
        #####################################################################################################################                   
        # TODO: create `list_of_discounted_returns`                                                                         # 
        # HINT1: note that each entry of the output should now be unique,                                                   #
        # because the summation happens over [t, T] instead of [0, T]                                                       #
        # HINT2: it is possible to write a vectorized solution, but a solution                                              #
        # using a for loop is also fine                                                                                     #
        #####################################################################################################################                   
        # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        list_of_discounted_cumsums = []
        # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        return list_of_discounted_cumsums
