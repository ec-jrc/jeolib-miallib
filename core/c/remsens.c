#include <stdio.h>
#include <stdlib.h>


/* 
      From http://www-numi.fnal.gov/offline_software/srt_public_context/WebDocs/doxygen/loon/html/novas_8c.html#a4004871
      Accessed on 2005-08-09
      This function will compute the Julian date for a given calendar
      date (year, month, day, hour).

   REFERENCES: 
      Fliegel & Van Flandern, Comm. of the ACM, Vol. 11, No. 10, October
      1968, p. 657.

   INPUT
   ARGUMENTS:
      year (short int)
         Year.
      month (short int)
         Month number.
      day (short int)
         Day-of-month.
      hour (double)
         Hour-of-day.

   OUTPUT
   ARGUMENTS:
      None.

   RETURNED
   VALUE:
      (double)
         Julian date.

   GLOBALS
   USED:
      None.

   FUNCTIONS
   CALLED:
      None.

   VER./DATE/
   PROGRAMMER:
      V1.0/06-98/JAB (USNO/AA)

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'juldat'.
      2. This function makes no checks for a valid input calendar
      date.
------------------------------------------------------------------------
*/
double julian_date(short int year, short int month, short int day, double hour)
{
   long int jd12h;

   double tjd;

   jd12h = (long) day - 32075L + 1461L * ((long) year + 4800L
      + ((long) month - 14L) / 12L) / 4L
      + 367L * ((long) month - 2L - ((long) month - 14L) / 12L * 12L)
      / 12L - 3L * (((long) year + 4900L + ((long) month - 14L) / 12L)
      / 100L) / 4L;
   tjd = (double) jd12h - 0.5 + hour / 24.0;

   return (tjd);
}
