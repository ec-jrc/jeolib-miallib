#from mialib import *
#from pointop_base import *  # this makes mialib.arith available


#import pointop_base # this makes mialib.pointop_base.arith available
#from io_base import *

# there is no imem (only imem_base)
# this makes functions available both as mialib.copy_image
# and mialib.imem_base.copy_image
#from imem_base import *




from mialib import *

# packages with base only definitions
import label_base



# packages with base and extended defintions
import format
import geometry
import geodesy
import imstat
import io
import pointop
import visu




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
