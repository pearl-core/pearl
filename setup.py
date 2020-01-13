from setuptools import setup, find_packages

with open("README.md", "r") as readme_file:
    readme = readme_file.read()

with open('CHANGELOG.md') as changelog_file:
    changelog = changelog_file.read()

with open('VERSION') as version_file:
    version = version_file.read().strip()

with open('requirements-dev.in') as test_requirements_file:
    test_requirements = [line.strip() for line in test_requirements_file.readlines()]

data_files = []

requirements = []

setup(
    # General project information:
    name="pearl",
    url='http://github.com/pearl-core/pearl',
    download_url='https://github.com/pearl-core/pearl/releases',
    version=version,
    keywords=['pearl', 'shell', 'dotfiles', 'package manager'],
    description="Pearl is a lightweight package manager for automating "
                "reproducible environments between different systems (Linux and OSX)."
                "It can be used for dotfiles, plugins, programs and "
                "any form of code accessible via git.",
    long_description=readme + '\n\n' + changelog,
    long_description_content_type="text/markdown",
    author='Filippo Squillace',
    author_email='feel.sqoox@gmail.com',
    license="GNU General Public License v3",
    # https://pypi.org/classifiers/
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Intended Audience :: Developers',
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        'Natural Language :: English',
        'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
        "Operating System :: MacOS",
        "Operating System :: POSIX :: Linux",
        'Topic :: Software Development :: Build Tools',
        "Topic :: System :: Software Distribution",
        "Topic :: System :: Shells",
        "Topic :: Utilities",
    ],

    # Package related arguments:
    packages=find_packages(where="src", exclude=("tests",)),
    package_dir={"": "src"},
    package_data={"pearllib": ["static/buava/lib/*"]},
    include_package_data=True,
    data_files=data_files,
    scripts=['bin/pearl'],

    # Dependency related arguments:
    python_requires='>=3.5',
    setup_requires=[],
    install_requires=requirements,
    test_suite='tests',
    # “optional” dependencies
    extras_require={
        'dev': test_requirements
    },
    # Alternatively, use tests_requires to use the setup.py command "test".
    # tests_require=test_requirements,
)
