import torch
import torch.nn as nn
from torch.nn import functional as F


device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

beta_1 = 0.0001
beta_T = 0.02
num_timesteps = 1000

# TODO: implement here
# *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
betas = None
alphas = None
alphas_bar = None
sigmas = None
# *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****


def ddpm_loss(
    model: nn.Module, x_0: torch.Tensor, t: torch.Tensor
) -> torch.Tensor:
    """
    Compute the DDPM loss for the model.

    Args:
        model (nn.Module): The neural network model.
        x_0 (torch.Tensor): Original tensor.
        t (torch.Tensor): Batched timestep to compute the loss at.
    Returns:
        loss (torch.Tensor): DDPM training loss.
    """
    loss = None

    # TODO: implement here
    # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
    pass
    # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
    return loss


def sample_image(
    model: nn.Module,
    num_samples: int,
    sample_timesteps: int = 1000,
) -> torch.Tensor:
    """
    Sample images from the ddpm model.

    Args:
        model (nn.Module): The ddpm neural network model used for sampling.
        num_samples (int): Number of samples to generate.
        sample_timesteps (int): Number of timesteps to sample.
    Returns:
        x_0 (torch.Tensor): Generated images.
    """
    model.eval()
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    x_T = torch.randn(num_samples, 1, 28, 28).to(device)
    x_0 = None
    x_t = x_T

    # TODO: implement here
    # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
    with torch.no_grad():
        for t in range(sample_timesteps - 1, -1, -1):
            pass
    # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

    x_0 = (x_0 + 1) / 2
    x_0 = x_0.clamp(0, 1)

    return x_0


