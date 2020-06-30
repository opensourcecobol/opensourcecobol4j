/*
 * Copyright (C) 2002-2009 Keisuke Nishida
 * Copyright (C) 2007-2009 Roger While
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If not write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#ifndef COB_CALL_H
#define COB_CALL_H

#include <setjmp.h>
#include <libcob/common.h>

/* Low level jump structure */
struct cobjmp_buf {
	int	cbj_int[4];
	void	*cbj_ptr[4];
	jmp_buf	cbj_jmp_buf;
	void	*cbj_ptr_rest[2];
};

struct call_stack_list {
	struct call_stack_list	*parent;
	struct call_stack_list	*children;
	struct call_stack_list	*sister;
	char			*name;
};

DECLNORET COB_EXPIMP void	cob_call_error		(void) COB_A_NORETURN;


COB_EXPIMP void		cob_set_cancel		(const char *, void *, void *);
COB_EXPIMP void		*cob_resolve		(const char *);
COB_EXPIMP void		*cob_resolve_1		(const char *);
COB_EXPIMP const char	*cob_resolve_error	(void);

COB_EXPIMP void		*cob_call_resolve	(const cob_field *);
COB_EXPIMP void		*cob_call_resolve_1	(const cob_field *);
COB_EXPIMP void		cob_field_cancel	(const cob_field *);
COB_EXPIMP void		cob_cancel_all		(void);
COB_EXPIMP void		cobcancel		(const char *);
COB_EXPIMP int		cobcall			(const char *, const int, void **);
COB_EXPIMP int		cobfunc			(const char *, const int, void **);
COB_EXPIMP void		*cobsavenv		(struct cobjmp_buf *);
COB_EXPIMP void		*cobsavenv2		(struct cobjmp_buf *, const int);
COB_EXPIMP void		coblongjmp		(struct cobjmp_buf *);

COB_EXPIMP void		cob_push_call_stack_list	(char *);
COB_EXPIMP void		cob_pop_call_stack_list	(void);

#define	cobsetjmp(x)	setjmp (cobsavenv (x))

#endif /* COB_CALL_H */
