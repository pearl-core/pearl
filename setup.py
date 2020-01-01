import os
from setuptools import setup, find_packages

# # Declare your non-python data files:
# # All files within static will be copied into the build preserving the
# # subdirectory structure if they exist.
# data_files = []
# for root, dirs, files in os.walk('static'):
#     data_files.append((os.path.relpath(root, 'static'),
#                        [os.path.join(root, f) for f in files]))

setup(
    name="Pearl",
    version="2.0.0",
    description='Because only in the best shells you will find a Pearl...',
    url='http://github.com/pearl-core/pearl',
    author='Filippo Squillace',
    author_email='feel.sqoox@gmail.com',
    license='GLPv3',
    packages=find_packages(where="src", exclude=("tests",)),
    package_dir={"": "src"},
    package_data={"pearllib": ["static/buava/lib/*"]},
    scripts=['bin/pearl'],
)
