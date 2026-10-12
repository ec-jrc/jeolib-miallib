/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2000-2022 European Union (Joint Research Centre)

This file is part of miallib.

miallib is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

miallib is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with miallib.  If not, see <https://www.gnu.org/licenses/>.
***********************************************************************/

/* processor specific definitions (see endian.h under linux */
#if defined(_WIN32) && !defined(__CYGWIN__) && !defined(__MSYS__)
   /* Native Windows (MSVC, MinGW-w64): all supported CPUs are little-endian */
#  ifndef BYTE_ORDER
#    define LITTLE_ENDIAN 1234
#    define BIG_ENDIAN    4321
#    define PDP_ENDIAN    3412
#    define BYTE_ORDER    LITTLE_ENDIAN
#  endif

#elif defined(__APPLE__) && defined(__MACH__)
   /* macOS (Darwin): BSD-style macros are in <machine/endian.h> */
#  include <machine/endian.h>

#elif defined(__FreeBSD__) || defined(__DragonFly__) || defined(__NetBSD__) \
   || defined(__OpenBSD__)
| --- |
   /* The BSDs: <sys/endian.h> defines the macros natively */
#  include <sys/endian.h>

#else
   /* Linux/glibc, Cygwin, MSYS2 */
#  include <endian.h>
#  ifndef BYTE_ORDER            /* hidden by strict -std= flags on glibc */
#    define BYTE_ORDER    __BYTE_ORDER
#    define LITTLE_ENDIAN __LITTLE_ENDIAN
#    define BIG_ENDIAN    __BIG_ENDIAN
#    define PDP_ENDIAN    __PDP_ENDIAN
#  endif
#endif

/* Safety net: fall back to the macros GCC and Clang predefine on every target. */
#ifndef BYTE_ORDER
#  if defined(__BYTE_ORDER__) && defined(__ORDER_LITTLE_ENDIAN__) \
      && defined(__ORDER_BIG_ENDIAN__)
#    define LITTLE_ENDIAN __ORDER_LITTLE_ENDIAN__
#    define BIG_ENDIAN    __ORDER_BIG_ENDIAN__
#    define PDP_ENDIAN    3412
#    define BYTE_ORDER    __BYTE_ORDER__
#  else
#    error "BYTE_ORDER could not be determined for this platform"
#  endif
#endif

/*
 **	TIFF structures.
 */

struct	mytiff
{
  short int byte_order;         /* for little or big-endian */
  short int version ;		/* TIFF version*/
  int ptr_dir1;		/* Offset to first directory*/
};

struct tag
{
  unsigned short int type;	/*Type of considered tag*/
  short int data_type;		/*Data type to provide tag information*/
  int length;		/*Length of the information*/
  int ValOrPoint;		/*Value or pointer to information*/
};

enum tiff_tags {WIDTH, LENGTH, BPS, PMI, NAME, DESC, SOT, SPP, SBC, XR, YR, SF, N_PLANE, NBR_TAGS};

/* SF=SampleFormat  added on 17-2-2004 */

#define ColorMap N_PLANE


/*
 **VISILOG structure.
 */


struct visi
{
  int magic;		/*must be Ox6931*/
  int gx;			/*pixels per line*/
  int gy;			/*number of lines*/
  int gz;			/*number of planes per image(must be 1)*/
  int rs0;			/*reserved (must be 0)*/
  int rs1;			/*reserved (must be 0)*/
  int grid;		/*grid type (0 ==> rec, 1 ==> hex)*/
  int rs2;			/*reserved (must be 0)*/
  int gcode;		/*arithmetic type*/
  int bstoc;		/*bits per pixel*/
  int rs3;			/*reserved (must be 0)*/
  int ox;			/*X-origin*/
  int oy;			/*Y-origin*/
  int oz;			/*Z-origin (must be 1)*/
  int rs4;			/*reserved (must be 0)*/
  int sizevisihead;	/*visilog header size(must be 76)*/
  int sizeuserhead;	/*user  header size(must be 0)*/
  int rs5;			/*reserved (must be 0)*/
  int sizehead;		/*total header size(must be 76)*/
};


