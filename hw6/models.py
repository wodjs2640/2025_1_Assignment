import torch.nn as nn
import torch.nn.functional as F

import sys, os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))


class BasicBlock(nn.Module):
    '''Pre-activation version of the BasicBlock.'''
    expansion = 1

    def __init__(self, in_planes, planes, stride=1):
        super(BasicBlock, self).__init__()
        self.bn1 = nn.BatchNorm2d(in_planes)
        self.conv1 = nn.Conv2d(in_planes,
                               planes,
                               kernel_size=3,
                               stride=stride,
                               padding=1,
                               bias=False)
        self.bn2 = nn.BatchNorm2d(planes)
        self.conv2 = nn.Conv2d(planes, planes, kernel_size=3, stride=1, padding=1, bias=False)

        if stride != 1 or in_planes != self.expansion * planes:
            self.shortcut = nn.Sequential(
                nn.Conv2d(in_planes,
                          self.expansion * planes,
                          kernel_size=1,
                          stride=stride,
                          bias=False))

    def forward(self, x):
        out = F.relu(self.bn1(x))
        shortcut = self.shortcut(out) if hasattr(self, 'shortcut') else x
        out = self.conv1(out)
        out = self.conv2(F.relu(self.bn2(out)))
        out += shortcut
        return out


class BottleneckBlock(nn.Module):
    '''Pre-activation version of the original Bottleneck module.'''
    expansion = 4

    def __init__(self, in_planes, planes, stride=1):
        super(BottleneckBlock, self).__init__()
        self.bn1 = nn.BatchNorm2d(in_planes)
        self.conv1 = nn.Conv2d(in_planes, planes, kernel_size=1, bias=False)
        self.bn2 = nn.BatchNorm2d(planes)
        self.conv2 = nn.Conv2d(planes, planes, kernel_size=3, stride=stride, padding=1, bias=False)
        self.bn3 = nn.BatchNorm2d(planes)
        self.conv3 = nn.Conv2d(planes, self.expansion * planes, kernel_size=1, bias=False)

        if stride != 1 or in_planes != self.expansion * planes:
            self.shortcut = nn.Sequential(
                nn.Conv2d(in_planes,
                          self.expansion * planes,
                          kernel_size=1,
                          stride=stride,
                          bias=False))

    def forward(self, x):
        out = F.relu(self.bn1(x))
        shortcut = self.shortcut(out) if hasattr(self, 'shortcut') else x
        out = self.conv1(out)
        out = self.conv2(F.relu(self.bn2(out)))
        out = self.conv3(F.relu(self.bn3(out)))
        out += shortcut
        return out


class Model(nn.Module):
    def __init__(self, block, mean, std, num_blocks, initial_channels, num_classes, stride=1):
        super(Model, self).__init__()
        self.in_planes = initial_channels
        self.num_classes = num_classes
        
        self.mean = nn.Parameter(mean)
        self.std = nn.Parameter(std)
        
        self.conv1 = nn.Conv2d(3,
                               initial_channels,
                               kernel_size=3,
                               stride=stride,
                               padding=1,
                               bias=False)
        self.layer1 = self._make_layer(block, initial_channels, num_blocks[0], stride=1)
        self.layer2 = self._make_layer(block, initial_channels * 2, num_blocks[1], stride=2)
        self.layer3 = self._make_layer(block, initial_channels * 4, num_blocks[2], stride=2)
        self.layer4 = self._make_layer(block, initial_channels * 8, num_blocks[3], stride=2)
        self.linear = nn.Linear(initial_channels * 8 * block.expansion, num_classes)

        self.n_feature = 512

    def _make_layer(self, block, planes, num_blocks, stride):
        strides = [stride] + [1] * (num_blocks - 1)
        layers = []
        for stride in strides:
            layers.append(block(self.in_planes, planes, stride))
            self.in_planes = planes * block.expansion
        return nn.Sequential(*layers)

    def compute_h1(self, x):
        out = x
        out = self.conv1(out)
        out = self.layer1(out)
        return out

    def compute_h2(self, x):
        out = x
        out = self.conv1(out)
        out = self.layer1(out)
        out = self.layer2(out)
        return out

    def forward(self, x):
        out = x / 255
        out = (x - self.mean) / self.std
        out = self.conv1(out)
        out = self.layer1(out)
        out = self.layer2(out)
        out = self.layer3(out)
        out = self.layer4(out)
        out = F.avg_pool2d(out, 4)
        out = out.reshape(out.size(0), -1)
        out = self.linear(out)

        return out


def resnet18(mean, std, num_classes=10, dropout=False, stride=1):
    return Model(BasicBlock, mean, std, [2, 2, 2, 2], 64, num_classes, stride=stride)


def resnet34(mean, std, num_classes=10, dropout=False, stride=1):
    return Model(BasicBlock, mean, std, [3, 4, 6, 3], 64, num_classes, stride=stride)


def resnet50(mean, std, num_classes=10, dropout=False, stride=1):
    return Model(BottleneckBlock, mean, std, [3, 4, 6, 3], 64, num_classes, stride=stride)


def resnet101(mean, std, num_classes=10, dropout=False, stride=1):
    return Model(BottleneckBlock, mean, std, [3, 4, 23, 3], 64, num_classes, stride=stride)


def resnet152(mean, std, num_classes=10, dropout=False, stride=1):
    return Model(BottleneckBlock, mean, std, [3, 8, 36, 3], 64, num_classes, stride=stride)


if __name__ == "__main__":
    model = resnet18(100, False, 1)

    num_param = 0
    for k, v in model.named_parameters():
        num_param += v.numel()
    print(num_param / 1e6)
