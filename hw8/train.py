from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import torch
from diffusers import UNet2DModel
from PIL import Image
from torch.utils.data import DataLoader
from torchvision import datasets as dset
from torchvision import transforms as T
from torchvision.utils import make_grid
from tqdm import tqdm
from transformers import set_seed

from todo import ddpm_loss, sample_image

# Hyperparameters
BATCH_SIZE = 128


def main():

    # Set random seeds
    set_seed(42)

    # Set device
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    print(f"Using device: {device}")

    save_dir = Path("./").resolve() / "results"
    save_dir.mkdir(parents=True, exist_ok=True)

    transform = T.Compose(
        [
            T.ToTensor(),
            T.Lambda(lambda x: 2 * x - 1),
        ]
    )
    train_dataset = dset.MNIST("./data", train=True, download=True, transform=transform)
    train_dataloader = DataLoader(
        train_dataset,
        batch_size=BATCH_SIZE,
        shuffle=True,
        num_workers=4,
        drop_last=True,
    )

    model = UNet2DModel(
        (28, 28),
        in_channels=1,
        out_channels=1,
        down_block_types=("DownBlock2D", "AttnDownBlock2D", "AttnDownBlock2D"),
        mid_block_type="UNetMidBlock2D",
        up_block_types=("AttnUpBlock2D", "AttnUpBlock2D", "UpBlock2D"),
        block_out_channels=(32, 64, 128),
        layers_per_block=1,
    )
    model = model.to(device)

    optimizer = torch.optim.Adam(model.parameters(), lr=3e-4)
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=100000, eta_min=1e-6
    )

    print("Training started...")
    for epoch in tqdm(range(100)):

        # Save checkpoint and sampled images
        model.eval()

        samples = sample_image(model, 48, sample_timesteps=1000)

        gen_imgs = make_grid(samples, nrow=8, normalize=True).permute(1, 2, 0).numpy()
        gen_imgs = (gen_imgs * 255).astype(np.uint8)
        gen_imgs = Image.fromarray(gen_imgs)
        gen_imgs.save(save_dir / f"epoch-{epoch}.png")

        model.save_pretrained(save_dir / f"checkpoint-last")

        # Training loop
        model.train()
        for img, _ in tqdm(train_dataloader):
            img = img.to(device)

            batch_size = img.shape[0]

            t = torch.randint(0, 1000, (batch_size,), device=device)
            loss = ddpm_loss(model, img, t)

            optimizer.zero_grad()
            loss.backward()
            optimizer.step()
            scheduler.step()


if __name__ == "__main__":
    main()
