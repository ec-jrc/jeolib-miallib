#from miallib import *
#from pointop_base import *  # this makes miallib.arith available


#import pointop_base # this makes miallib.pointop_base.arith available
#from io_base import *

# there is no imem (only imem_base)
# this makes functions available both as miallib.copy_image
# and miallib.imem_base.copy_image
#from imem_base import *


# # uncomment to make all base functions available at top level
# from miallib import *
# from convolve_base import *
# from dem_base import *
# from dist_base import *
# from erodil_base import *
# from format_base import *
# from geodesy_base import *
# from geometry_base import *
# from hmt_base import *
# from imem_base import *
# from io_base import *
# from label_base import *
# from opclo_base import *
# from pointop_base import *
# from proj_base import *
# from segment_base import *
# from stats_base import *
#
#
#
# # uncomment to make all extended functions available at top level
# from format import *
# from geometry import *
# from geodesy import *
# from stats import *
# from io import *
# from pointop import *
# from visu import *
# from dist import *


# packages with base only definitions
from . import convolve_base
from . import dem_base
from . import dist_base
from . import erodil_base
from . import hmt_base
from . import label_base
from . import opclo_base
from . import proj_base
from . import segment_base

# packages with base and extended definitions
from . import format
from . import geometry
from . import geodesy
from . import stats
from . import io
from . import pointop
from . import visu
from . import dist


from . import miallib as base_functions


# use . to secure that the local subpackage is imported
# from .format import *
# from .geodesy import *
# from .geometry import *
#from .io import *
#import io
# from .imstat import *
# from .pointop import *
# from .visu import *


# __all__ = ['format', 'geodesy', 'geometry', 'io',
#            'imstat', 'pointop', 'visu']
