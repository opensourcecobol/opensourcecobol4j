/*
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

/*
 * Extracted from glib/gtypes.h in GLIB-2.2.2.
 * Modified by Roger While
 */

#ifndef VB_BYTESWAP_H
#define VB_BYTESWAP_H

#include <sys/types.h>

/* Basic bit swapping functions
 */

#define VB_BSWAP_16_CONSTANT(val)	((unsigned short) (	\
    (unsigned short) ((unsigned short) (val) >> 8) |		\
    (unsigned short) ((unsigned short) (val) << 8)))

#define VB_BSWAP_32_CONSTANT(val)	((unsigned int) (		\
    (((unsigned int) (val) & (unsigned int) 0x000000ffU) << 24) |	\
    (((unsigned int) (val) & (unsigned int) 0x0000ff00U) <<  8) |	\
    (((unsigned int) (val) & (unsigned int) 0x00ff0000U) >>  8) |	\
    (((unsigned int) (val) & (unsigned int) 0xff000000U) >> 24)))

#define VB_BSWAP_64_CONSTANT(val)	((unsigned long long) (	\
    (((unsigned long long) (val) &				\
      (unsigned long long) 0x00000000000000ffULL) << 56) |	\
    (((unsigned long long) (val) &				\
      (unsigned long long) 0x000000000000ff00ULL) << 40) |	\
    (((unsigned long long) (val) &				\
      (unsigned long long) 0x0000000000ff0000ULL) << 24) |	\
    (((unsigned long long) (val) &				\
      (unsigned long long) 0x00000000ff000000ULL) <<  8) |	\
    (((unsigned long long) (val) &				\
      (unsigned long long) 0x000000ff00000000ULL) >>  8) |	\
    (((unsigned long long) (val) &				\
      (unsigned long long) 0x0000ff0000000000ULL) >> 24) |	\
    (((unsigned long long) (val) &				\
      (unsigned long long) 0x00ff000000000000ULL) >> 40) |	\
    (((unsigned long long) (val) &				\
      (unsigned long long) 0xff00000000000000ULL) >> 56)))

/* Arch specific stuff for speed */

#if defined (__GNUC__) && (__GNUC__ >= 2)
#  if defined (__i386__)
#    define VB_BSWAP_16_IA32(val)				\
     (__extension__						\
      ({ register unsigned short int __v, __x = ((unsigned short) (val));	\
	 if (__builtin_constant_p (__x))			\
	   __v = VB_BSWAP_16_CONSTANT (__x);			\
	 else							\
	   __asm__ ("rorw $8, %w0"				\
		    : "=r" (__v)				\
 		    : "0" (__x)					\
 		    : "cc");					\
	 __v; }))
#    define VB_BSWAP_32_IA32(val)				\
       (__extension__						\
	({ register unsigned int __v, __x = ((unsigned int) (val));	\
	   if (__builtin_constant_p (__x))			\
	     __v = VB_BSWAP_32_CONSTANT (__x);			\
	   else							\
	     __asm__ ("bswap %0"				\
		      : "=r" (__v)				\
		      : "0" (__x));				\
	    __v; }))
#    define VB_BSWAP_64_IA32(val)				\
       (__extension__						\
	({ union { unsigned long long __ll;			\
		   unsigned int __l[2]; } __w, __r;		\
	   __w.__ll = ((unsigned long long) (val));		\
	   if (__builtin_constant_p (__w.__ll))			\
	     __r.__ll = VB_BSWAP_64_CONSTANT (__w.__ll);	\
	   else							\
	     {							\
	       __r.__l[0] = VB_BSWAP_32 (__w.__l[1]);		\
	       __r.__l[1] = VB_BSWAP_32 (__w.__l[0]);		\
	     }							\
	   __r.__ll; }))
#    define VB_BSWAP_16(val) (VB_BSWAP_16_IA32 (val))
#    define VB_BSWAP_32(val) (VB_BSWAP_32_IA32 (val))
#    define VB_BSWAP_64(val) (VB_BSWAP_64_IA32 (val))
#  elif defined (__ia64__)
#    define VB_BSWAP_16_IA64(val)				\
       (__extension__						\
	({ register unsigned short __v, __x = ((unsigned short) (val));	\
	   if (__builtin_constant_p (__x))			\
	     __v = VB_BSWAP_16_CONSTANT (__x);			\
	   else							\
	     __asm__ __volatile__ ("shl %0 = %1, 48 ;;"		\
				   "mux1 %0 = %0, @rev ;;"	\
				    : "=r" (__v)		\
				    : "r" (__x));		\
	    __v; }))
#    define VB_BSWAP_32_IA64(val)				\
       (__extension__						\
	 ({ register unsigned int __v, __x = ((unsigned int) (val));	\
	    if (__builtin_constant_p (__x))			\
	      __v = VB_BSWAP_32_CONSTANT (__x);			\
	    else						\
	     __asm__ __volatile__ ("shl %0 = %1, 32 ;;"		\
				   "mux1 %0 = %0, @rev ;;"	\
				    : "=r" (__v)		\
				    : "r" (__x));		\
	    __v; }))
#    define VB_BSWAP_64_IA64(val)				\
       (__extension__						\
	({ register unsigned long long __v,			\
	     __x = ((unsigned long long) (val));		\
	   if (__builtin_constant_p (__x))			\
	     __v = VB_BSWAP_64_CONSTANT (__x);			\
	   else							\
	     __asm__ __volatile__ ("mux1 %0 = %1, @rev ;;"	\
				   : "=r" (__v)			\
				   : "r" (__x));		\
	   __v; }))
#    define VB_BSWAP_16(val) (VB_BSWAP_16_IA64 (val))
#    define VB_BSWAP_32(val) (VB_BSWAP_32_IA64 (val))
#    define VB_BSWAP_64(val) (VB_BSWAP_64_IA64 (val))
#  elif defined (__x86_64__)
#    define VB_BSWAP_16_X86_64(val)				\
     (__extension__						\
      ({ register unsigned short int __v, __x = ((unsigned short) (val));	\
	 if (__builtin_constant_p (__x))			\
	   __v = VB_BSWAP_16_CONSTANT (__x);			\
	 else							\
	   __asm__ ("rorw $8, %w0"				\
		    : "=r" (__v)				\
 		    : "0" (__x)					\
 		    : "cc");					\
	 __v; }))
#    define VB_BSWAP_32_X86_64(val)				\
       (__extension__						\
	 ({ register unsigned int __v, __x = ((unsigned int) (val));	\
	    if (__builtin_constant_p (__x))			\
	      __v = VB_BSWAP_32_CONSTANT (__x);			\
	    else						\
	     __asm__ ("bswapl %0"				\
		      : "=r" (__v)				\
		      : "0" (__x));				\
	    __v; }))
#    define VB_BSWAP_64_X86_64(val)				\
       (__extension__						\
	({ register unsigned long long __v, __x = ((unsigned long long) (val));	\
	   if (__builtin_constant_p (__x))			\
	     __v = VB_BSWAP_64_CONSTANT (__x);			\
	   else							\
	     __asm__ ("bswapq %0"				\
		      : "=r" (__v)				\
		      : "0" (__x));				\
	   __v; }))
#    define VB_BSWAP_16(val) (VB_BSWAP_16_X86_64 (val))
#    define VB_BSWAP_32(val) (VB_BSWAP_32_X86_64 (val))
#    define VB_BSWAP_64(val) (VB_BSWAP_64_X86_64 (val))
#  else /* generic gcc */
#    define VB_BSWAP_16(val) (VB_BSWAP_16_CONSTANT (val))
#    define VB_BSWAP_32(val) (VB_BSWAP_32_CONSTANT (val))
#    define VB_BSWAP_64(val) (VB_BSWAP_64_CONSTANT (val))
#  endif
#  elif defined(_MSC_VER)
#    define VB_BSWAP_16(val) (_byteswap_ushort (val))
#    define VB_BSWAP_32(val) (_byteswap_ulong (val))
#    define VB_BSWAP_64(val) (_byteswap_uint64 (val))
#else /* generic */
#  define VB_BSWAP_16(val) (VB_BSWAP_16_CONSTANT (val))
#  define VB_BSWAP_32(val) (VB_BSWAP_32_CONSTANT (val))
#  define VB_BSWAP_64(val) (VB_BSWAP_64_CONSTANT (val))
#endif /* generic */

#endif /* VB_BYTESWAP_H */
