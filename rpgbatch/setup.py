# It's suggested to run this with "python3 setup.py develop --user"

from setuptools import setup

setup(
    name='rpgbatch',
    version='0.2.0',
    packages=['rpgbatch'],
    install_requires=[
        'nohrio >= 0.4.*',
    ],
)
