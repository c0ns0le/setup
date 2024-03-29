# -*- coding: utf-8 -*-
#
# Copyright © 2011 Pierre Raybaut
# Licensed under the terms of the MIT License
# (see spyderlib/__init__.py for details)

"""
Scientific Python startup script

Requires NumPy, SciPy, Matplotlib, blpapi, bbg
"""

from __future__ import division, print_function

__has_numpy = True
__has_scipy = True
__has_pandas = True
__has_matplotlib = True
__has_blpapi = True
__has_bbg = True

#==============================================================================
# Pollute the namespace but also provide MATLAB-like experience
#==============================================================================
try:
    from pylab import *  #analysis:ignore
    # Enable Matplotlib's interactive mode:
    ion()
except ImportError:
    pass

# Import modules following official guidelines:
import datetime as dt
try:
    import numpy as np
except ImportError:
    __has_numpy = False

try:
    import scipy as sp
except ImportError:
    __has_scipy = False

try:
    import pandas as pd
except ImportError:
    __has_pandas = False

try:
    import blpapi as bb
except ImportError:
    __has_blpapi = False

try:
    import bbg
    from bbg import (get_data_bbg, get_multidata_bbg, get_sensitivity_bbg,
		     get_histdata_bbg)
except ImportError:
    __has_bbg = False

try:
    import matplotlib as mpl
    import matplotlib.pyplot as plt #analysis:ignore
    if __has_pandas:
        pd.options.display.mpl_style = 'default'
except ImportError:
    __has_matplotlib = False

#==============================================================================
# Print what modules have been imported
#==============================================================================
__imports = ""
if __has_numpy:
    __imports += "Imported NumPy %s" % np.__version__
if __has_scipy:
    __imports += ", SciPy %s" % sp.__version__
if __has_pandas:
    __imports += ", Pandas %s" % pd.__version__
if __has_matplotlib:
    __imports += ", Matplotlib %s" % mpl.__version__
if __has_blpapi:
    __imports += ", blpapi %s" % bb.__version__
if __has_bbg:
    __imports += ", bbg %s" % bbg.__version__

print("")
if __imports:
    print(__imports)

import os
if os.environ.get('QT_API') != 'pyside':
    try:
        import guiqwt
        import guiqwt.pyplot as plt_
        import guidata
        plt_.ion()
        print("+ guidata %s, guiqwt %s" % (guidata.__version__,
                                           guiqwt.__version__))
    except ImportError:
        print()

#==============================================================================
# Add help about the "scientific" command
#==============================================================================
def setscientific():
    """Set 'scientific' in __builtin__"""
    import __builtin__
    from site import _Printer
    infos = ""

    if __has_numpy:
        infos += """
This is a standard Python interpreter with preloaded tools for scientific
computing and visualization. It tries to import the following modules:

>>> import numpy as np  # NumPy (multidimensional arrays, linear algebra, ...)"""

    if __has_scipy:
        infos += """
>>> import scipy as sp  # SciPy (signal and image processing library)"""

    if __has_matplotlib:
        infos += """
>>> import matplotlib as mpl         # Matplotlib (2D/3D plotting library)
>>> import matplotlib.pyplot as plt  # Matplotlib's pyplot: MATLAB-like syntax
>>> from pylab import *              # Matplotlib's pylab interface
>>> ion()                            # Turned on Matplotlib's interactive mode"""

    try:
        import guiqwt  #analysis:ignore
        infos += """
>>> import guidata  # GUI generation for easy dataset editing and display

>>> import guiqwt                 # Efficient 2D data-plotting features
>>> import guiqwt.pyplot as plt_  # guiqwt's pyplot: MATLAB-like syntax
>>> plt_.ion()                    # Turned on guiqwt's interactive mode"""
    except ImportError:
        pass

    if __has_numpy:
        infos += "\n"

#    infos += """
#Within Spyder, this interpreter also provides:
#    * special commands (e.g. %ls, %pwd, %clear)
#    * system commands, i.e. all commands starting with '!' are subprocessed
#      (e.g. !dir on Windows or !ls on Linux, and so on)
#"""
    __builtin__.scientific = _Printer("scientific", infos)


setscientific()
print('Type "scientific" for more details.')

#==============================================================================
# Delete temp vars
#==============================================================================
del setscientific, __has_numpy, __has_scipy, __has_matplotlib, __has_blpapi, __has_bbg, __imports
