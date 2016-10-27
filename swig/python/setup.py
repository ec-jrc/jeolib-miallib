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
                  '-DMCISRG']
libraries_val = ['gdal', 'tiff', 'mialib_python']
library_dirs_val = ['../../core/build/lib', '/usr/lib/x86_64-linux-gnu', '/usr/local/lib']
include_dirs_val = ['/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                    '/usr/local/lib/python2.7',
                    '../../core/c/']
define_macros_val  = [('MCISRG', None)]




_mialib = Extension('_mialib', ['mialib.i'],
                          swig_opts = swig_opts_val,
                          libraries = libraries_val,
                          library_dirs = library_dirs_val,
                          include_dirs = include_dirs_val,
                          define_macros = define_macros_val)

_pointop_base = Extension('_pointop_base', ['pointop.i'],
                          swig_opts = swig_opts_val,
                          libraries = libraries_val,
                          library_dirs = library_dirs_val,
                          include_dirs = include_dirs_val,
                          define_macros = define_macros_val)

_io_base = Extension('_io_base', ['io.i'],
                          swig_opts = swig_opts_val,
                          libraries = libraries_val,
                          library_dirs = library_dirs_val,
                          include_dirs = include_dirs_val,
                          define_macros = define_macros_val)

_imem_base = Extension('_imem_base', ['imem.i'],
                          swig_opts = swig_opts_val,
                          libraries = libraries_val,
                          library_dirs = library_dirs_val,
                          include_dirs = include_dirs_val,
                          define_macros = define_macros_val)

_geometry_base = Extension('_geometry_base', ['geometry.i'],
                          swig_opts = swig_opts_val,
                          libraries = libraries_val,
                          library_dirs = library_dirs_val,
                          include_dirs = include_dirs_val,
                          define_macros = define_macros_val)

_geodesy_base = Extension('_geodesy_base', ['geodesy.i'],
                          swig_opts = swig_opts_val,
                          libraries = libraries_val,
                          library_dirs = library_dirs_val,
                          include_dirs = include_dirs_val,
                          define_macros = define_macros_val)




setup (name = 'mialib',
       version = '0.1',
       author      = "Pierre Soille",
       author_email      = "Pierre.Soille@jrc.ec.europa.eu",
       copyright = "(c) European Commission",
       license = '(c) European Commission. Exact licence to be defined',
       description = """Python interface to mialib/jiplib thanks to SWIG""",
       long_description = """Python interface to mialib/jiplib thanks to SWIG: long description""",
       url = "http://jeodpp.ec.europa.eu",
       ext_modules = [_mialib,
                      _pointop_base,
                      _io_base,
                      _imem_base,
                      _geometry_base],
       package_dir = {'' : 'packages'},
       packages=['mialib'],
       #py_modules = ["mialib/mialib"],
       py_modules = ['mialib/mialib',
                     'mialib/pointop_base', 'mialib/pointop',
                     'mialib/io_base',
                     'mialib/io',
                     'mialib/imem_base',
                     'mialib/format',
                     'mialib/geometry_base',
                     'mialib/geometry',
                     'mialib/geodesy_base',
                     'mialib/geodesy',
                     'mialib/imstat',
                     'mialib/visu'],
       #data_files=[('bitmaps', ['bm/b1.gif', 'bm/b2.gif'])],
       #
       package_data={'./build/lib.linux-x86_64-2.7/': ['_mialib.so',
                                                        '_pointop_base.so',
                                                        '_io_base.so',
                                                        '_imem_base.so',
                                                        '_geometry_base.so',
                                                        '_geodesy_base.so',
                                                        ]},
       build_dir = {'' : '../build'}
       )
