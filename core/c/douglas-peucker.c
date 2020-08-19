/***********************************************************************
Author(s): Dominik Brunner and Pierre Soille
Copyright (C) 2004-2020 European Union (Joint Research Centre)

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

/***************************************************************************
                          simplifyLine.c  -  description
                             -------------------
    Implementation of the douglas peucker line simplifiaction algorithm

    begin                : Wed Jun 16 2004
 ***************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>

struct STACK_RECORD {
    int nAnchorIndex, nFloaterIndex;
    struct STACK_RECORD *precPrev;
} *m_pStack;

/*
 *  StackPush:       Push point on the stack
 *
 *  Parameters:
 *
 *    nAnchorIndex   anchor index
 *
 *    nFloaterIndex  floater index
 *
 *  Return values:
 *
 *    none
 */
void StackPush( int nAnchorIndex, int nFloaterIndex )
{
    struct STACK_RECORD *precPrev = m_pStack;
    m_pStack = (struct STACK_RECORD *)malloc( sizeof(struct STACK_RECORD) );
    m_pStack->nAnchorIndex = nAnchorIndex;
    m_pStack->nFloaterIndex = nFloaterIndex;
    m_pStack->precPrev = precPrev;
}

/*
 *  StackPush:       Pop point from the stack
 *
 *  Parameters:
 *
 *    pnAnchorIndex   pointer to an anchor index (result of pop is stored here)
 *
 *    pnFloaterIndex  pointer to a floater index (result of pop is stored here)
 *
 *  Return values:
 *
 *    0               if global stack is NULL
 *
 *    1               if everything is ok
 */
int StackPop( int *pnAnchorIndex, int *pnFloaterIndex )
{
    struct STACK_RECORD *precStack = m_pStack;
    if ( precStack == NULL )
        return 0; //false
    *pnAnchorIndex = precStack->nAnchorIndex;
    *pnFloaterIndex = precStack->nFloaterIndex;
    m_pStack = precStack->precPrev;
    free( precStack );
    return 1; //true
}

/*
 *  ReducePoints:     Reduces points of a line using the douglas peucker algorithm
 *
 *  Parameters:
 *
 *    pPointsX        Array of x coordinates of the line
 *
 *    pPointsY        Array of y coordinates of the line
 *
 *    pPointsCount    Number of points in the line
 *
 *    pnUseFlag       Array of flags whether a point needs to be used in order to display the line or not
 *
 *    tolerance       parameter for the simplification of the line. The higher the value
 *                    the more will the line be simplified.
 *
 *  Return values:
 *
 *    none
 */
void ReducePoints( double *pPointsX, double *pPointsY, int nPointsCount, int *pnUseFlag, double dTolerance)
{
    int nVertexIndex, nAnchorIndex, nFloaterIndex;
    double dSegmentVecLength;
    double dAnchorVecX, dAnchorVecY;
    double dAnchorUnitVecX, dAnchorUnitVecY;
    double dVertexVecLength;
    double dVertexVecX, dVertexVecY;
    double dProjScalar;
    double dVertexDistanceToSegment;
    double dMaxDistThisSegment;
    int nVertexIndexMaxDistance;

    nAnchorIndex = 0;
    nFloaterIndex = nPointsCount - 1;
    StackPush( nAnchorIndex, nFloaterIndex );
    while ( StackPop( &nAnchorIndex, &nFloaterIndex ) ){
        // initialize line segment
        dAnchorVecX = pPointsX[ nFloaterIndex ] - pPointsX[ nAnchorIndex ];
        dAnchorVecY = pPointsY[ nFloaterIndex ] - pPointsY[ nAnchorIndex ];
        dSegmentVecLength = sqrt( dAnchorVecX * dAnchorVecX  + dAnchorVecY * dAnchorVecY );
        dAnchorUnitVecX = dAnchorVecX / dSegmentVecLength;
        dAnchorUnitVecY = dAnchorVecY / dSegmentVecLength;
        // inner loop:
        dMaxDistThisSegment = 0.0;
        nVertexIndexMaxDistance = nAnchorIndex + 1;
        for ( nVertexIndex = nAnchorIndex + 1; nVertexIndex < nFloaterIndex; nVertexIndex++ ){
            //compare to anchor
            dVertexVecX = pPointsX[ nVertexIndex ] - pPointsX[ nAnchorIndex ];
            dVertexVecY = pPointsY[ nVertexIndex ] - pPointsY[ nAnchorIndex ];
            dVertexVecLength = sqrt( dVertexVecX * dVertexVecX
                                     + dVertexVecY * dVertexVecY );
            //dot product:
            dProjScalar = dVertexVecX * dAnchorUnitVecX + dVertexVecY * dAnchorUnitVecY;
            if ( dProjScalar < 0.0 )
                dVertexDistanceToSegment = dVertexVecLength;
            else{
                //compare to floater
                dVertexVecX = pPointsX[ nVertexIndex ] - pPointsX[ nFloaterIndex ];
                dVertexVecY = pPointsY[ nVertexIndex ] - pPointsY[ nFloaterIndex ];
                dVertexVecLength = sqrt( dVertexVecX * dVertexVecX
                                         + dVertexVecY * dVertexVecY );
                //dot product:
                dProjScalar = dVertexVecX * (-dAnchorUnitVecX) + dVertexVecY * (-dAnchorUnitVecY);
                if ( dProjScalar < 0.0 )
                    dVertexDistanceToSegment = dVertexVecLength;
                else //calculate perpendicular distance to line (pythagorean theorem):
                    dVertexDistanceToSegment =
                        sqrt( fabs( dVertexVecLength * dVertexVecLength - dProjScalar * dProjScalar ) );
            } //else
            if ( dMaxDistThisSegment < dVertexDistanceToSegment ){
                dMaxDistThisSegment = dVertexDistanceToSegment;

                nVertexIndexMaxDistance = nVertexIndex;
            } //if
        } //for (inner loop)
        if ( dMaxDistThisSegment <= dTolerance ){ //use line segment
            pnUseFlag[ nAnchorIndex ] = 1;
            pnUseFlag[ nFloaterIndex ] = 1;
        } //if use points
        else{
            StackPush( nAnchorIndex, nVertexIndexMaxDistance );
            StackPush( nVertexIndexMaxDistance, nFloaterIndex );
        } //else
    }
}
