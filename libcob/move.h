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
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#ifndef COB_MOVE_H
#define COB_MOVE_H

#include <libcob/common.h>

COB_EXPIMP char		*han2zen	(char *, int, int *);
COB_EXPIMP void		cob_move	(cob_field *, cob_field *);
COB_EXPIMP void		cob_hankaku_move	(cob_field *, cob_field *);
COB_EXPIMP void		cob_set_int	(cob_field *, const int);
COB_EXPIMP int		cob_get_int	(cob_field *);

#ifndef	I18N_UTF8
COB_EXPIMP int		cob_la_anstojis (int);
#endif /*I18N_UTF8*/
COB_EXPIMP int		cob_la_memset	(cob_field *, int);
#endif /* COB_MOVE_H */
