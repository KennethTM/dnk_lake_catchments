from setuptools import setup
from Cython.Build import cythonize
import numpy as np

#Build command:
#python setup.py build_ext --inplace

setup(
    include_dirs = [np.get_include()],
    ext_modules = cythonize("catchment_cython.pyx", compiler_directives={'language_level' : "3"})
)
