#!/usr/bin/env python

"""
setup.py file for SWIG mialib
"""

# see also https://docs.python.org/2/distutils/setupscript.html
# see also https://docs.python.org/2/tutorial/modules.html

from distutils.core import setup, Extension

#from setuptools import setup, find_packages


swig_opts_val =  ['-v', '-Wall',  '-I../include', '-outdir', './packages/mialib',
                  '-I../include/python',
                  '-I/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                  '-I/usr/local/lib/python2.7',
                  '-I../../core/c',
                  '-DMCISRG', '-DCLASSIF']
libraries_val = ['gdal', 'tiff', 'mialib_python']
library_dirs_val = ['../../core/build/lib', '/usr/lib/x86_64-linux-gnu', '/usr/local/lib']
include_dirs_val = ['/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                    '/usr/local/lib/python2.7',
                    '../../core/c/']
define_macros_val  = [('MCISRG', None), ('CLASSIF', None)]


_mialib = Extension('_mialib', ['mialib.i'],
                          swig_opts = swig_opts_val,
                          libraries = libraries_val,
                          library_dirs = library_dirs_val,
                          include_dirs = include_dirs_val,
                          define_macros = define_macros_val)


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
    return (Extension('_'+str+'_base', [str+'.i'],
                          swig_opts = swig_opts_val,
                          libraries = libraries_val,
                          library_dirs = library_dirs_val,
                          include_dirs = include_dirs_val,
                          define_macros = define_macros_val))

modules_list = []
for idx in ext_modules_list:
    modules_list.append(createExtension(idx))

additional_py_modules = []
for idx in ext_modules_list:
    additional_py_modules.append('mialib/'+idx+'_base')

sos = []
for idx in ext_modules_list:
   sos.append('_'+idx+'_base.so')


# create the interface file for each module
import re
import os
for idx in  ext_modules_list:
    if not(os.path.isfile('../include/'+idx+'.i')):
        with open("../include/master.i", "r") as sources:
            lines = sources.readlines()
        with open('../include/'+idx+'.i', "w") as sources:
            for line in lines:
                sources.write(re.sub('master', idx, line))
    


setup (name = "mialib",
       version = "0.1",
       author      = "Pierre Soille",
       author_email      = "Pierre.Soille@jrc.ec.europa.eu",
       copyright = "(c) European Commission",
       license = "(c) European Commission. Exact licence to be defined",
       description = """Python interface to mialib/jiplib thanks to SWIG""",
       long_description = """Python interface to mialib/jiplib thanks to SWIG: long description""",
       url = "http://jeodpp.ec.europa.eu",
       ext_modules = [_mialib ] + modules_list,
       package_dir = {'' : 'packages'},
       packages=['mialib'],
       #py_modules = ["mialib/mialib"],
       py_modules = ['mialib/mialib'] +  additional_py_modules + 
                     ['mialib/format',
                     'mialib/geometry',
                     'mialib/geodesy',
                     'mialib/stats',
                     'mialib/io',
                     'mialib/pointop',
                     'mialib/visu'],
       #data_files=[('bitmaps', ['bm/b1.gif', 'bm/b2.gif'])],
       #
       package_data={'./build/lib.linux-x86_64-2.7/': ['_mialib.so'] + sos },
       build_dir = {'' : '../build'}
       )
