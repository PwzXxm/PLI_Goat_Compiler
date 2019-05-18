/*
** bison doesn't put these into poz.h
*/

extern	int		ozyyparse(void);
extern	const char	*ozyyfile;
extern	int		ozyylinenum;

/*
** flex doesn't put this into loz.h (it doesn't even create loz.h)
*/

extern	int		ozyylex(void);
extern	FILE		*ozyyin;

/*
** stdio.h defines this only with some options, not with others
*/

extern	int		fileno(FILE *);
