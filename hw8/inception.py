from pathlib import Path

import numpy as np
import torch
import torch.nn.functional as F
from diffusers import UNet2DModel
from PIL import Image
from torch import nn, optim
from torch.utils.data import DataLoader
from torchvision import datasets, transforms
from torchvision.utils import make_grid
from tqdm import tqdm
from transformers import set_seed

from todo import sample_image

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")


# Hyperparameters
BATCH_SIZE = 32
beta_1 = 1e-4
beta_T = 0.02

transform = transforms.Compose([transforms.ToTensor()])


class MLPClassifier(nn.Module):
    def __init__(self):
        super(MLPClassifier, self).__init__()
        self.fc1 = nn.Linear(784, 512)
        self.fc2 = nn.Linear(512, 512)
        self.fc3 = nn.Linear(512, 10)

    def forward(self, x):
        x = torch.flatten(x, start_dim=1)
        x = F.relu(self.fc1(x))
        x = F.dropout(x, 0.2)
        x = F.relu(self.fc2(x))
        x = F.dropout(x, 0.2)
        x = self.fc3(x)
        return x

    def prob(self, x):
        x = self.forward(x)
        prob = F.softmax(x, dim=-1)
        return prob


def train_classifier():
    # ----- Hyperparameters -----
    batch_size = 128
    num_classes = 10
    epochs = 20
    # ----- Data Preparation -----
    train_dataset = datasets.MNIST(
        root="./data", train=True, download=True, transform=transform
    )
    test_dataset = datasets.MNIST(
        root="./data", train=False, download=True, transform=transform
    )

    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    test_loader = DataLoader(test_dataset, batch_size=batch_size)

    # ----- Model Definition -----

    model = MLPClassifier().to(device)
    optimizer = optim.RMSprop(model.parameters(), lr=0.001, alpha=0.9)
    criterion = nn.CrossEntropyLoss()

    # ----- Training -----
    for epoch in range(epochs):
        model.train()
        for batch_x, batch_y in train_loader:
            batch_x = batch_x.to(device)
            batch_y = batch_y.to(device)
            optimizer.zero_grad()
            outputs = model(batch_x)
            loss = criterion(outputs, batch_y)
            loss.backward()
            optimizer.step()
        print(f"Epoch {epoch+1}/{epochs}, Loss: {loss.item():.4f}")

    # ----- Evaluation -----
    model.eval()
    correct = 0
    total = 0
    with torch.no_grad():
        for batch_x, batch_y in test_loader:
            batch_x = batch_x.to(device)
            batch_y = batch_y.to(device)
            outputs = model(batch_x)
            predicted = torch.argmax(outputs, dim=1)
            total += batch_y.size(0)
            correct += (predicted == batch_y).sum().item()

    print("Test accuracy:", correct / total)
    return model


if __name__ == "__main__":
    # Set random seeds
    set_seed(42)

    base_dir = Path("./").resolve() / "results"

    if (base_dir / "classifier.pth").exists():
        classifier = MLPClassifier()
        ckpt = torch.load(base_dir / "classifier.pth", map_location="cpu")
        classifier.load_state_dict(ckpt)
        classifier = classifier.to(device)
    else:
        classifier = train_classifier()
        ckpt = classifier.state_dict()
        torch.save(ckpt, base_dir / "classifier.pth")

    classifier.eval()

    model = UNet2DModel.from_pretrained("results/checkpoint-last")

    model = model.to(device)
    model.eval()

    n_sample = 1000
    n_sample_per_iter = 100

    with torch.no_grad():
        for i in tqdm(range(n_sample // n_sample_per_iter), desc="Sampling"):
            samples = sample_image(model, n_sample_per_iter, sample_timesteps=1000)

            if i == 0:
                all_samples = samples
            else:
                all_samples = torch.cat([all_samples, samples], dim=0)

        inception_score = None

        # TODO: implement here
        # *****START OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****
        # Get class probabilities for all samples using the classifier
        probs = classifier.prob(all_samples)

        # Calculate the marginal distribution p(y) by averaging over all samples
        marginal_probs = torch.mean(probs, dim=0, keepdim=True)

        # Calculate KL divergence: KL(p(y|x) || p(y))
        # KL(P||Q) = sum(P * log(P/Q))
        kl_div = torch.sum(
            probs * torch.log(probs / (marginal_probs + 1e-8) + 1e-8), dim=1
        )

        # Inception Score is exp(E[KL(p(y|x) || p(y))])
        inception_score = torch.exp(torch.mean(kl_div)).item()
        # *****END OF YOUR CODE (DO NOT DELETE/MODIFY THIS LINE)*****

        print(f"Inception Score: {inception_score:.4f}")

    gen_imgs = (
        make_grid(all_samples[:48], nrow=8, normalize=True).permute(1, 2, 0).numpy()
    )
    gen_imgs = (gen_imgs * 255).astype(np.uint8)
    gen_imgs = Image.fromarray(gen_imgs)
    gen_imgs.save(base_dir / "generated_samples.png")
