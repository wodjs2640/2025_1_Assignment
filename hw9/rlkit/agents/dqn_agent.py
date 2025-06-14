import numpy as np

from rlkit.infrastructure.dqn_utils import MemoryOptimizedReplayBuffer, PiecewiseSchedule
from rlkit.policies.argmax_policy import ArgMaxPolicy
from rlkit.critics.dqn_critic import DQNCritic


class DQNAgent(object):
    def __init__(self, env, agent_params):

        self.env = env
        self.agent_params = agent_params
        self.batch_size = agent_params['batch_size']
        # import ipdb; ipdb.set_trace()
        self.last_obs = self.env.reset()

        self.num_actions = agent_params['ac_dim']
        self.learning_starts = agent_params['learning_starts']
        self.learning_freq = agent_params['learning_freq']
        self.target_update_freq = agent_params['target_update_freq']

        self.replay_buffer_idx = None
        self.exploration = agent_params['exploration_schedule']
        self.optimizer_spec = agent_params['optimizer_spec']

        self.critic = DQNCritic(agent_params, self.optimizer_spec)
        self.actor = ArgMaxPolicy(self.critic)

        lander = agent_params['env_name'].startswith('LunarLander')
        self.replay_buffer = MemoryOptimizedReplayBuffer(
            agent_params['replay_buffer_size'], agent_params['frame_history_len'], lander=lander)
        self.t = 0
        self.num_param_updates = 0

    def add_to_replay_buffer(self, paths):
        pass

    def step_env(self):
        """
            Step the env and store the transition
            At the end of this block of code, the simulator should have been
            advanced one step, and the replay buffer should contain one more transition.
            Note that self.last_obs must always point to the new latest observation.
        """        

        ########################################################################
        # TODO: store the latest observation ("frame") into the replay buffer  #
        # and update the replay buffer writing head (self.replay_buffer_idx).  #                                 
        # HINT1: the replay buffer used here is `MemoryOptimizedReplayBuffer`  #
        # in dqn_utils.py                                                      #
        # HINT2: the current lastest observation can be found at self.last_obs #
        ########################################################################
        # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        self.replay_buffer_idx = None
        # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

        eps = self.exploration.value(self.t)

        ##################################################################################
        # TODO: use epsilon greedy exploration to select the action.                     #
        # HINT 1: take random action with probability eps (see np.random.random())       #
        # OR if your current step number (see self.t) is less than self.learning_starts  #
        # HINT 2: When taking a greedy action,                                           #
        # your actor will take in multiple previous observations ("frames") in order     #
        # to deal with the partial observability of the environment. Get the most recent #
        # `frame_history_len` observations using functionality from the replay buffer,   #
        # and then use those observations as input to your actor.                        #
        ##################################################################################
        # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        perform_random_action = None
        if perform_random_action:
            # take random action
            action = self.env.action_space.sample()
        else:
            # take greedy action
            action = None
        # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        
        #########################################################################################
        # TODO: take a step in the environment using the action from the policy                 #
        # HINT1: use the following useful function:                                             #
        # obs, reward, done, info = env.step(action)                                            #
        # HINT2: remember that self.last_obs must always point to the newest/latest observation #
        #########################################################################################
        # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        self.last_obs = None
        reward = None
        done = None
        info = None
        # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

        #############################################################################################
        # TODO: store the result of taking this action into the replay buffer                       #
        # HINT1: see your replay buffer's `store_effect` function                                   #
        # HINT2: one of the arguments you'll need to pass in is self.replay_buffer_idx from above   #
        #############################################################################################
        # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        
        # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

        #############################################################################################
        # TODO: if taking this step resulted in done, reset the env (and the latest observation)    #
        #############################################################################################
        # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        if done:
            self.last_obs = None
        # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

    def sample(self, batch_size):
        if self.replay_buffer.can_sample(self.batch_size):
            return self.replay_buffer.sample(batch_size)
        else:
            return [],[],[],[],[]

    def train(self, ob_no, ac_na, re_n, next_ob_no, terminal_n):
        log = {}
        if (self.t > self.learning_starts
                and self.t % self.learning_freq == 0
                and self.replay_buffer.can_sample(self.batch_size)
        ):

            #############################################################################################
            # TODO: fill in the call to the update function using the appropriate tensors               #
            #############################################################################################
            # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
            log = self.critic.update(
                None
            )
            # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

            #############################################################################################
            # TODO: update the target network periodically                                              #
            # HINT: your critic already has this functionality implemented                              #
            #############################################################################################
            # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
            if self.num_param_updates % self.target_update_freq == 0:
                pass
            # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

            self.num_param_updates += 1

        self.t += 1
        return log
