import torch
import torch.nn as nn
from torch.nn import functional as F


device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

beta_1 = 0.0001
beta_T = 0.02
num_timesteps = 1000

# TODO: implement here
# *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
# Linear schedule for beta
betas = torch.linspace(beta_1, beta_T, num_timesteps)

# Calculate alphas
alphas = 1 - betas

# Calculate alphas_bar (cumulative product of alphas)
alphas_bar = torch.cumprod(alphas, dim=0)

# Calculate sigmas
sigmas = torch.sqrt(betas)
# *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****


def ddpm_loss(model: nn.Module, x_0: torch.Tensor, t: torch.Tensor) -> torch.Tensor:
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
    # Sample noise
    noise = torch.randn_like(x_0)

    # Get alphas_bar for the current timesteps
    alphas_bar_t = alphas_bar[t].view(-1, 1, 1, 1)

    # Calculate x_t using the reparameterization trick
    x_t = torch.sqrt(alphas_bar_t) * x_0 + torch.sqrt(1 - alphas_bar_t) * noise

    # Predict noise using the model
    predicted_noise = model(x_t, t)
    if hasattr(predicted_noise, "sample"):
        predicted_noise = predicted_noise.sample
    elif isinstance(predicted_noise, dict) and "sample" in predicted_noise:
        predicted_noise = predicted_noise["sample"]

    # Calculate MSE loss between predicted and actual noise
    loss = F.mse_loss(predicted_noise, noise)
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
            # Create timestep tensor for current batch
            t_batch = torch.full((num_samples,), t, device=device, dtype=torch.long)

            # Predict noise using the model
            predicted_noise = model(x_t, t_batch)
            if hasattr(predicted_noise, "sample"):
                predicted_noise = predicted_noise.sample
            elif isinstance(predicted_noise, dict) and "sample" in predicted_noise:
                predicted_noise = predicted_noise["sample"]

            # Get parameters for current timestep
            alpha_t = alphas[t]
            alpha_bar_t = alphas_bar[t]
            beta_t = betas[t]

            # Calculate the mean of p(x_{t-1} | x_t, x_0)
            mean = (1 / torch.sqrt(alpha_t)) * (
                x_t - (beta_t / torch.sqrt(1 - alpha_bar_t)) * predicted_noise
            )

            # Add noise if not the final step
            if t > 0:
                # Sample random noise
                z = torch.randn_like(x_t)
                # Use beta as variance for simplicity
                sigma_t = torch.sqrt(beta_t)
                x_t = mean + sigma_t * z
            else:
                x_t = mean
                x_0 = x_t
    # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

    x_0 = (x_0 + 1) / 2
    x_0 = x_0.clamp(0, 1)

    return x_0
