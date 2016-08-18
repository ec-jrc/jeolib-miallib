

typedef struct {
  long int *qp;     /* Pointer to circular queue  */
  long int *qps;    /* Pointer to storage position  */
  long int *qpr;    /* Pointer to retrieve position */
  long int *qplast; /* Pointer to last+1 element  */
  long int *qpl;    /* Pointer to look position  */
  long int qcount;  /* Used to resize the queue  */
  long int qlength;  /* length of the queue  */
} FIFO;


extern FIFO *alloc_fifo(long int);
extern void fifo_add(FIFO *, long int);
extern long int fifo_remove(FIFO *);
extern long int fifo_look(FIFO *);
extern long int fifo_empty(FIFO *);
extern void fifo_increase(FIFO *);
extern void fifo_reset(FIFO *);
extern void clear_fifo(FIFO *);


