
#define PQDATUM   struct node *
#define PQPRIO(p) (p->prio)


/*
 *  Priority queue structure
 */
struct pqueue 
{
  unsigned int size, avail, step;
  PQDATUM *d;
};

struct node {
  int prio;   /* priority level */
  int val;    /* an associated integer value */
  unsigned long int offset; /* offset to image origin */
  int sof;    /* offset to reference seed pixel (used in mssrgcore) */
};

extern struct pqueue * pqinit(struct pqueue *, int);
extern int pqinsert(struct pqueue *, PQDATUM );
extern int pqmininsert(struct pqueue *, PQDATUM );
extern int pqmaxinsert(struct pqueue *, PQDATUM );
extern PQDATUM * pqremove(struct pqueue *, PQDATUM *);
extern PQDATUM * pqminremove(struct pqueue *, PQDATUM *);
extern PQDATUM * pqmaxremove(struct pqueue *, PQDATUM *);
extern PQDATUM * pqpeek(struct pqueue *, PQDATUM *);
extern void free_pq(struct pqueue *);
extern void reset_pq(struct pqueue *);
