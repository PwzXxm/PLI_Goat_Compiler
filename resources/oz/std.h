/*
** Header file defining some standard macros.
*/

#ifndef	STD_H
#define	STD_H

#ifndef	NULL
#define	NULL	0
#endif
#ifndef	TRUE
#define	TRUE	((bool) 1)
#endif
#ifndef	FALSE
#define	FALSE	((bool) 0)
#endif
#ifndef	bool
#define	bool	int
#endif

#define	max(a, b)		((a) < (b) ? (b) : (a))
#define	max3(a, b, c)		(max((a), max((b), (c))))

#define	streq(s1, s2)		(strcmp((s1), (s2)) == 0)

#endif
