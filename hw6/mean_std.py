import numpy as np
import torch

mean = np.array([0.4914, 0.4822, 0.4465])
std = np.array([0.2023, 0.1994, 0.2010])

mean_torch = torch.tensor(mean, dtype=torch.float32).reshape(1,3,1,1)
std_torch = torch.tensor(std, dtype=torch.float32).reshape(1,3,1,1)