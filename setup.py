import os
from setuptools import setup, find_packages


setup(
    name="Pearl",
    version="1.0",
    packages=find_packages(where="src", exclude=("tests",)),
    package_dir={"": "src"},
    # package_data={"pearl": ["applications/*/templates/*"]},
)
