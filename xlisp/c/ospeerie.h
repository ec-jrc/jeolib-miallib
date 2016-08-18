/* syntaxe des commandes au niveau de l'interpreteur et fonction
   d'interface associee (v. osptrs.h pour exemple ) */

#ifdef EERIE
/*
   -----------------------------------------
   * -> Non-destructive function
   @ -> Destructive function (i.e., the 1st
        input image is modified and returned
        as output).
   -----------------------------------------
*/

/* functions defined in compute.h */
{   "*SETSTRUCTELEM",   S,  iSetStructElem     },
{   "*SETRANK",         S,  iSetRank           },
{   "*SETORIGIN",       S,  iSetOrigin         },
{   "*SETLINECENTER",   S,  iSetLineCenter     },
{   "*APPLYFILTER",     S,  iApplyFilter       },
{   "*APPLYLINEFILTER", S,  iApplyLineFilter   },

#endif /* ifdef EERIE */
