from setuptools import setup

setup(name='wallaroo',
      version='0.0.3',
      description='Client library for the Wallaroo implementation of the Wallaby configuration service',
      url='http://github.com/willb/wallaroo',
      author='William Benton',
      author_email='willb@redhat.com',
      license='Apache 2',
      packages=['wallaroo', 'wallaroo.client'],
      install_requires=[
        'requests',
      ],
      zip_safe=False)