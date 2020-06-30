/*
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

#ifndef COB_LOCAL_H
#define COB_LOCAL_H

/* We use this file to define/prototype things that should not be
   exported to user space
*/

/* Also OK for icc which defines __GNUC__ */

#if	defined(_WIN32) || defined(__CYGWIN__)
#define COB_HIDDEN	extern
#elif	defined(__GNUC__) && defined(linux) && (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 3))
#define COB_HIDDEN	extern __attribute__ ((visibility("hidden")))
#else
#define COB_HIDDEN	extern
#endif

#define COB_ATTR_INIT(v,w,x,y,z)	do { \
	attr.type = v; \
	attr.digits = w; \
	attr.scale = x; \
	attr.flags = y; \
	attr.pic = z; \
	} while (0)

#ifdef	COB_PARAM_CHECK
#define	COB_CHK_PARMS(x, z)	\
	do { \
		if (cob_call_params < z) { \
			cob_runtime_error (parm_msg, #x, z); \
			cob_stop_run (1); \
		} \
	} while (0)
#else
#define	COB_CHK_PARMS(x, z)
#endif

/* I18N_UTF8: Full-width char(s) (utf-8 version). */
#define COB_U8ZERO	"\xef\xbc\x90"
#define COB_U8SPC	"\xe3\x80\x80"
#define COB_U8BLK	"\x20\x20\x20"
#define COB_U8QUOT	"\xe2\x80\x9d"
#define COB_U8SLAS	"\xef\xbc\x8f"
#define COB_U8CSIZ	3

/* I18N_UTF8: Full-width char(s) (sjis version). */
#define COB_SJZERO	"\x82\x4f"
#define COB_SJSPC	"\x81\x40"
#define COB_SJBLK	"\x81\x40"
#define COB_SJQUOT	"\x81\x68"
#define COB_SJSLAS	"\x81\x5e"
#define COB_SJCSIZ	2

#ifdef	I18N_UTF8
#define COB_ZENZERO	COB_U8ZERO
#define COB_ZENSPC	COB_U8SPC
#define COB_ZENBLK	COB_U8BLK
#define COB_ZENQUOT	COB_U8QUOT
#define COB_ZENSLAS	COB_U8SLAS
#define COB_ZENCSIZ	COB_U8CSIZ
#else /*!I18N_UTF8*/
#define COB_ZENZERO	COB_SJZERO
#define COB_ZENSPC	COB_SJSPC
#define COB_ZENBLK	COB_SJBLK
#define COB_ZENQUOT	COB_SJQUOT
#define COB_ZENSLAS	COB_SJSLAS
#define COB_ZENCSIZ	COB_SJCSIZ
#endif /*I18N_UTF8*/

#ifdef	I18N_UTF8
#define COB_U8BYTE_1(c)	((((c)>>7) == 0x00)? 1:		\
			 (((c)>>5) == 0x06)? 2:		\
			 (((c)>>4) == 0x0e)? 3:		\
			 (((c)>>3) == 0x1e)? 4:		\
			 (((c)>>2) == 0x3e)? 5:		\
			 (((c)>>1) == 0x7e)? 6: 0)
#define COB_U8BYTE_N(c)	((c)>>6 == 2)
#endif /*I18N_UTF8*/

COB_HIDDEN int			cob_screen_initialized;
COB_HIDDEN int			cob_got_exception;
COB_HIDDEN unsigned int		cob_orig_line;
COB_HIDDEN const char		*cob_orig_statement;
COB_HIDDEN const char		*cob_orig_program_id;
COB_HIDDEN const char		*cob_orig_section;
COB_HIDDEN const char		*cob_orig_paragraph;

COB_HIDDEN void		cob_memcpy		(cob_field *,
							 unsigned char *,
							 const int);
COB_HIDDEN void		cob_exit_fileio		(void);
COB_HIDDEN void		cob_field_to_string	(const cob_field *, char *);
COB_HIDDEN void		cob_init_numeric	(void);
COB_HIDDEN void		cob_init_termio		(void);
COB_HIDDEN void		cob_init_fileio		(void);
COB_HIDDEN void		cob_init_call		(void);
COB_HIDDEN void		cob_init_intrinsic	(void);
COB_HIDDEN void		cob_init_strings	(void);
COB_HIDDEN void		cob_init_move		(void);
COB_HIDDEN void		cob_screen_terminate	(void);
COB_HIDDEN void		cob_screen_set_mode	(const size_t);
COB_HIDDEN int		cob_real_get_sign	(const cob_field *);
COB_HIDDEN void		cob_real_put_sign	(const cob_field *, const int);
COB_HIDDEN long long	cob_get_long_long	(cob_field *);

#ifdef	I18N_UTF8
COB_HIDDEN int		ascii_to_utf8		(int, unsigned char *);
COB_HIDDEN unsigned char	*cob_national		(const unsigned char *, int);
#endif /*I18N_UTF8*/

COB_HIDDEN char		*cb_get_jisword_buff	(const char *, char *, size_t);
COB_HIDDEN char		*cb_get_jisword		(const char *);

#undef	COB_HIDDEN

#endif /* COB_LOCAL_H */
