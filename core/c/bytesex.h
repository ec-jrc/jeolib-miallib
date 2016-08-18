/* define BYTE_ORDER for BIG_ and LITTLE_ENDIAN architectures */


#if defined(__i386__) || defined(__i486__) || defined(__alpha__) || defined(__TURBOC__)\
	|| (defined(__mips__) && (defined(MIPSEL) || defined (__MIPSEL__)))
#define BYTE_ORDER	1234
#elif defined(__mc68000__) || defined (__sparc__) \
	|| (defined(__mips__) && (defined(MIPSEB) || defined (__MIPSEB__)))
#define BYTE_ORDER	4321
#else
 #error architecture not supported by the Linux C library
#endif
