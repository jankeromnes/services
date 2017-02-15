# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

from __future__ import absolute_import

import os

from setuptools import find_packages
from setuptools import setup

here = os.path.dirname(__file__)

with open(os.path.join(here, 'VERSION')) as f:
    version = f.read().strip()


def read_requirements(file_):
    lines = []
    with open(file_) as f:
        for line in f.readlines():
            line = line.strip()
            if line.startswith('-e '):
                lines.append(line.split('#')[1].split('egg=')[1])
            elif line.startswith('#') or line.startswith('-'):
                pass
            else:
                lines.append(line)
    return lines


setup(
    name='shipit_bot_static_analysis',
    version=version,
    description='Listens to bugzilla entries, executes'
    'some static analysis and reports results.',
    author='Mozilla Release Management',
    author_email='release-mgmt@mozilla.com',
    url='https://shipit.mozilla-releng.net',
    tests_require=[
        'flake8',
        'pytest',
    ],
    install_requires=read_requirements('requirements.txt'),
    packages=find_packages(),
    include_package_data=True,
    zip_safe=False,
    license='MPL2',
    entry_points={
        'console_scripts': [
            'shipit-bot-static-analysis = shipit_bot_static_analysis.cli:main',
        ]
    },
)