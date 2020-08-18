#!/usr/bin/env python

"""
setup.py file for SWIG miallib
"""

# see also https://docs.python.org/2/distutils/setupscript.html
# see also https://docs.python.org/2/tutorial/modules.html

# nur master.i ist zu editieren !!!

from distutils.core import setup, Extension
import re
import os

pyver = os.environ.get('PYVER', '3.6')


# from setuptools import setup, find_packages


if int(pyver[0]) == 3:
    swig_opts_val = ['-py3', '-v', '-Wall',  '-I../include', '-outdir',
                     './packages/miallib', '-I../include/python',
                     '-I/usr/local/lib/python'+pyver+'/dist-packages/numpy/core/include',
                     '-I/usr/local/lib/python'+pyver,
                     '-I../../core/c',
                     '-I../../core/build/doc/xml/',
                     '-DMCISRG', '-DCLASSIF']
else:
    swig_opts_val = ['-v', '-Wall',  '-I../include', '-outdir',
                     './packages/miallib', '-I../include/python',
                     '-I/usr/local/lib/python'+pyver+'/dist-packages/numpy/core/include',
                     '-I/usr/local/lib/python'+pyver,
                     '-I../../core/c',
                     '-I../../core/build/doc/xml/',
                     '-DMCISRG', '-DCLASSIF']


libraries_val = ['gdal', 'tiff', 'miallib_python']
library_dirs_val = ['../../core/build/lib', '/usr/lib/x86_64-linux-gnu',
                    '/usr/local/lib']
include_dirs_val = ['/usr/local/lib/python'+pyver+'/dist-packages/numpy/core/include',
                    '/usr/local/lib/python'+pyver,
                    '../../core/c/']
define_macros_val = [('MCISRG', None), ('CLASSIF', None)]


_miallib = Extension('_miallib', ['miallib.i'],
                    swig_opts=swig_opts_val,
                    libraries=libraries_val,
                    library_dirs=library_dirs_val,
                    include_dirs=include_dirs_val,
                    define_macros=define_macros_val)


ext_modules_list = ['convolve',
                    'dem',
                    'dist',
                    'erodil',
                    'format',
                    'geometry',
                    'geodesy',
                    'hmt',
                    'imem',
                    'io',
                    'label',
                    'opclo',
                    'pointop',
                    'proj',
                    'segment',
                    'stats']


def createExtension(str):
    return (Extension('_' + str + '_base', [str + '.i'],
                      swig_opts=swig_opts_val,
                      libraries=libraries_val,
                      library_dirs=library_dirs_val,
                      include_dirs=include_dirs_val,
                      define_macros=define_macros_val))


modules_list = []
for idx in ext_modules_list:
    modules_list.append(createExtension(idx))

additional_py_modules = []
for idx in ext_modules_list:
    additional_py_modules.append('miallib/' + idx + '_base')

# sos = []
# for idx in ext_modules_list:
#     sos.append('_' + idx + '_base.so')


# create the interface file for each module
for idx in ext_modules_list:
    if not(os.path.isfile('../include/' + idx + '.i')):
        with open("../include/master.i", "r") as sources:
            lines = sources.readlines()
        with open('../include/'+idx+'.i', "w") as sources:
            for line in lines:
                sources.write(re.sub('master', idx, line))


setup(name="miallib",
      version="0.1",
      author="Pierre Soille",
      author_email="Pierre.Soille@ec.europa.eu",
      copyright="(c) European Commission",
      license="(c) European Commission. Exact licence to be defined",
      description="""Python interface to miallib/jiplib thanks to SWIG""",
      long_description="""Python interface to miallib/jiplib thanks to SWIG: long description""",
      url="http://jeodpp.jrc.ec.europa.eu",
      ext_modules=[_miallib] + modules_list,
      package_dir={'': 'packages'},
      packages=['miallib'],
      # #py_modules = ["miallib/miallib"],
      # py_modules = ['miallib/miallib'] +  additional_py_modules +
      #               ['miallib/format',
      #               'miallib/geometry',
      #               'miallib/geodesy',
      #               'miallib/stats',
      #               'miallib/io',
      #               'miallib/pointop',
      #               'miallib/visu'],
      #data_files=[('bitmaps', ['bm/b1.gif', 'bm/b2.gif'])],
      #
      package_data={'./build/lib.linux-x86_64-'+pyver+'/': ['_miallib.so']},  # + sos
      build_dir={'': './build'},
      build_base={'': './build'}
      )
