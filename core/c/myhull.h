/***********************************************************************
Author(s): Pieter Kempeneers
Copyright (C) 2000-2024 European Union (Joint Research Centre)

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
** Header file for myhull.cc
*/

#ifndef _MYHULL_H
#define _MYHULL_H

#include <vector>

/* for point with integer coordinates */
typedef struct {
    int a;
    int b;
} intpair_t;

std::vector<intpair_t> chainHull_2D(std::vector<intpair_t>& points);

#endif /* _MYHULL_H */