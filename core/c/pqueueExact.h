/***************************************************************************
                          pqueueExact.h  -  description
                             -------------------
    begin                : Thu Apr 22 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu

    Adapted from http://www.purists.org/georg/pqueue:
    << Binary Heap Priority Queue >>
    As of last Update May 07, 2004

 ***************************************************************************/

#ifndef init_pqueueExact
#define init_pqueueExact

#define PQDATUM   struct node *
#define PQPRIO(p) (p->prio)


/*
 *  Priority queue structure
 */
struct pqueue 
{
	int size, avail, step;
	PQDATUM *d;
};

struct node {
  double prio;   /* priority level */
  long int val;    /* an associated long integer value */
  int offset; /* offset to image origin */
  //int sof;    /* offset to reference seed pixel (used in mssrgcore) */
};

extern struct pqueue * pqExactInit(struct pqueue *, int);
extern int pqExactInsert(struct pqueue *, PQDATUM );
extern int pqExactMinInsert(struct pqueue *, PQDATUM );
extern int pqExactMaxInsert(struct pqueue *, PQDATUM );
extern PQDATUM * pqExactRemove(struct pqueue *, PQDATUM *);
extern PQDATUM * pqExactMinRemove(struct pqueue *, PQDATUM *);
extern PQDATUM * pqExactMaxRemove(struct pqueue *, PQDATUM *);
extern PQDATUM * pqExactPeek(struct pqueue *, PQDATUM *);
extern void freeExact_pq(struct pqueue *);

#endif
