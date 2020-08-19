/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2000-2020 European Union (Joint Research Centre)

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


/*
 ** FIFO type definition
 */



typedef struct {
  long int *qp;     /* Pointer to circular queue    */
  long int *qps;    /* Pointer to storage position  */
  long int *qpr;    /* Pointer to retrieve position */
  long int *qplast; /* Pointer to last+1 element    */
  long int *qpl;    /* Pointer to look position     */
  long int mod;    /* Modulation factor            */
} FIFO4;

#define  FICT_PIX  1

extern FIFO4 *create_fifo4(long int);
extern void  fifo4_add(FIFO4 *, long int);
extern long int  fifo4_remove(FIFO4 *);
extern long  int fifo4_look(FIFO4 *);
extern void fifo4_lookreset(FIFO4 *);
extern long  int fifo4_empty(FIFO4 *);
extern void  fifo4_increase(FIFO4 *);
extern void  free_fifo4(FIFO4 *);
extern void fifo4_flush(FIFO4 *);
