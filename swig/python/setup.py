#!/usr/bin/env python

"""
setup.py file for SWIG mialib
"""

# see also https://docs.python.org/2/distutils/setupscript.html
# see also https://docs.python.org/2/tutorial/modules.html

from distutils.core import setup, Extension

#from setuptools import setup, find_packages

mialib_module = Extension('_mialib', ['mialib.i'],
                          swig_opts = ['-v', '-Wall',  '-I../include', '-outdir', './packages',
                                       '-I../include/python',
                                       '-I/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                                       '-I/usr/local/lib/python2.7',
                                       '-I../../core/c',
                                       '-DMCISRG'],
                          libraries = ['gdal', 'tiff', 'mialib_python'],
                          library_dirs = ['../../core/build/lib', '/usr/lib/x86_64-linux-gnu', '/usr/local/lib'],
                          include_dirs= ['/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                                          '/usr/local/lib/python2.7',
                                          '../../core/c/'],
                          define_macros = [('MCISRG', None)],)

pointop_module = Extension('_pointop_base', ['pointop.i'],
                          swig_opts = ['-v', '-Wall',  '-I../include', '-outdir', './packages',
                                       '-I../include/python',
                                       '-I/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                                       '-I/usr/local/lib/python2.7',
                                       '-I../../core/c',
                                       '-DMCISRG'],
                          libraries = ['gdal', 'tiff', 'mialib_python'],
                          library_dirs = ['../../core/build/lib', '/usr/lib/x86_64-linux-gnu', '/usr/local/lib'],
                          include_dirs= ['/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                                          '/usr/local/lib/python2.7',
                                          '../../core/c/'],
                          define_macros = [('MCISRG', None)],)

io_module = Extension('_io_base', ['io.i'],
                          swig_opts = ['-v', '-Wall',  '-I../include', '-outdir', './packages',
                                       '-I../include/python',
                                       '-I/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                                       '-I/usr/local/lib/python2.7',
                                       '-I../../core/c',
                                       '-DMCISRG'],
                          libraries = ['gdal', 'tiff', 'mialib_python'],
                          library_dirs = ['../../core/build/lib', '/usr/lib/x86_64-linux-gnu', '/usr/local/lib'],
                          include_dirs= ['/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                                          '/usr/local/lib/python2.7',
                                          '../../core/c/'],
                          define_macros = [('MCISRG', None)],)

imem_module = Extension('_imem_base', ['imem.i'],
                          swig_opts = ['-v', '-Wall',  '-I../include', '-outdir', './packages',
                                       '-I../include/python',
                                       '-I/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                                       '-I/usr/local/lib/python2.7',
                                       '-I../../core/c',
                                       '-DMCISRG'],
                          libraries = ['gdal', 'tiff', 'mialib_python'],
                          library_dirs = ['../../core/build/lib', '/usr/lib/x86_64-linux-gnu', '/usr/local/lib'],
                          include_dirs= ['/usr/local/lib/python2.7/dist-packages/numpy/core/include',
                                          '/usr/local/lib/python2.7',
                                          '../../core/c/'],
                          define_macros = [('MCISRG', None)],)


setup (name = 'mialib',
       version = '0.1',
       author      = "Pierre Soille",
       author_email      = "Pierre.Soille@jrc.ec.europa.eu",
       copyright = "(c) European Commission",
       license = '(c) European Commission. Exact licence to be defined',
       description = """Python interface to mialib/jiplib thanks to SWIG""",
       long_description = """Python interface to mialib/jiplib thanks to SWIG: long description""",
       url = "http://jeodpp.ec.europa.eu",
       ext_modules = [mialib_module,
                      pointop_module,
                      io_module,
                      imem_module],
       package_dir = {'' : 'packages'},
       packages=['mialib'],
       #py_modules = ["mialib/mialib"],
       py_modules = ['mialib/mialib',
                     'mialib/pointop_base', 'mialib/pointop',
                     'mialib/io_base', 'mialib/io',
                     'mialib/imem_base',
                     'mialib/format',
                     'mialib/geodesy',
                     'mialib/geometry',
                     'mialib/imstat',
                     'mialib/visu'],
       #data_files=[('bitmaps', ['bm/b1.gif', 'bm/b2.gif'])],
       #
       package_data={'./build/lib.linux-x86_64-2.7/': ['_mialib.so',
                                                        '_pointop_base.so',
                                                        '_io_base.so',
                                                        '_imem_base.so']},
       build_dir = {'' : '../build'}
       )
