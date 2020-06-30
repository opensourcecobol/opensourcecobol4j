/*
 * Copyright (C) 2005-2009 Roger While
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

#ifndef COB_INTRINSIC_H
#define COB_INTRINSIC_H

#include <libcob/common.h>

COB_EXPIMP cob_field *cob_intr_binop		(cob_field *, int, cob_field *);
COB_EXPIMP cob_field *cob_intr_current_date		(const int, const int);
COB_EXPIMP cob_field *cob_intr_when_compiled	(const int, const int, cob_field *);
COB_EXPIMP cob_field *cob_intr_exception_file	(void);
COB_EXPIMP cob_field *cob_intr_exception_location	(void);
COB_EXPIMP cob_field *cob_intr_exception_status	(void);
COB_EXPIMP cob_field *cob_intr_exception_statement	(void);
COB_EXPIMP cob_field *cob_intr_char			(cob_field *);
COB_EXPIMP cob_field *cob_intr_ord			(cob_field *);
COB_EXPIMP cob_field *cob_intr_stored_char_length	(cob_field *);
COB_EXPIMP cob_field *cob_intr_combined_datetime	(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_date_of_integer	(cob_field *);
COB_EXPIMP cob_field *cob_intr_day_of_integer	(cob_field *);
COB_EXPIMP cob_field *cob_intr_integer_of_date	(cob_field *);
COB_EXPIMP cob_field *cob_intr_integer_of_day	(cob_field *);
COB_EXPIMP cob_field *cob_intr_test_date_yyyymmdd	(cob_field *);
COB_EXPIMP cob_field *cob_intr_test_day_yyyyddd	(cob_field *);
COB_EXPIMP cob_field *cob_intr_factorial		(cob_field *);

COB_EXPIMP cob_field *cob_intr_exp			(cob_field *);
COB_EXPIMP cob_field *cob_intr_exp10		(cob_field *);
COB_EXPIMP cob_field *cob_intr_abs			(cob_field *);
COB_EXPIMP cob_field *cob_intr_acos			(cob_field *);
COB_EXPIMP cob_field *cob_intr_asin			(cob_field *);
COB_EXPIMP cob_field *cob_intr_atan			(cob_field *);
COB_EXPIMP cob_field *cob_intr_cos			(cob_field *);
COB_EXPIMP cob_field *cob_intr_log			(cob_field *);
COB_EXPIMP cob_field *cob_intr_log10		(cob_field *);
COB_EXPIMP cob_field *cob_intr_sin			(cob_field *);
COB_EXPIMP cob_field *cob_intr_sqrt			(cob_field *);
COB_EXPIMP cob_field *cob_intr_tan			(cob_field *);

COB_EXPIMP cob_field *cob_intr_upper_case		(const int, const int,
							 cob_field *);
COB_EXPIMP cob_field *cob_intr_lower_case		(const int, const int,
							 cob_field *);
COB_EXPIMP cob_field *cob_intr_reverse		(const int, const int,
							 cob_field *);
COB_EXPIMP cob_field *cob_intr_concatenate		(const int, const int,
						 const int, ...);
COB_EXPIMP cob_field *cob_intr_substitute		(const int, const int,
						 const int, ...);
COB_EXPIMP cob_field *cob_intr_substitute_case	(const int, const int,
						 const int, ...);
COB_EXPIMP cob_field *cob_intr_trim			(const int, const int,
						 cob_field *, const int);
COB_EXPIMP cob_field *cob_intr_length		(cob_field *);
COB_EXPIMP cob_field *cob_intr_integer		(cob_field *);
COB_EXPIMP cob_field *cob_intr_integer_part		(cob_field *);
COB_EXPIMP cob_field *cob_intr_fraction_part	(cob_field *);
COB_EXPIMP cob_field *cob_intr_sign			(cob_field *);
COB_EXPIMP cob_field *cob_intr_numval		(cob_field *);
COB_EXPIMP cob_field *cob_intr_numval_c		(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_annuity		(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_mod			(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_rem			(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_sum			(const int, ...);
COB_EXPIMP cob_field *cob_intr_ord_min		(const int, ...);
COB_EXPIMP cob_field *cob_intr_ord_max		(const int, ...);
COB_EXPIMP cob_field *cob_intr_min			(const int, ...);
COB_EXPIMP cob_field *cob_intr_max			(const int, ...);
COB_EXPIMP cob_field *cob_intr_midrange		(const int, ...);
COB_EXPIMP cob_field *cob_intr_median		(const int, ...);
COB_EXPIMP cob_field *cob_intr_mean			(const int, ...);
COB_EXPIMP cob_field *cob_intr_range		(const int, ...);
COB_EXPIMP cob_field *cob_intr_random		(const int, ...);
COB_EXPIMP cob_field *cob_intr_variance		(const int, ...);
COB_EXPIMP cob_field *cob_intr_standard_deviation	(const int, ...);
COB_EXPIMP cob_field *cob_intr_present_value	(const int, ...);
COB_EXPIMP cob_field *cob_intr_year_to_yyyy		(const int, ...);
COB_EXPIMP cob_field *cob_intr_date_to_yyyymmdd	(const int, ...);
COB_EXPIMP cob_field *cob_intr_day_to_yyyyddd	(const int, ...);
COB_EXPIMP cob_field *cob_intr_locale_date		(const int, const int,
						 cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_locale_time		(const int, const int,
						 cob_field *, cob_field *);

COB_EXPIMP cob_field *cob_intr_seconds_past_midnight	(void);
COB_EXPIMP cob_field *cob_intr_lcl_time_from_secs		(const int, const int,
							 cob_field *, cob_field *);

COB_EXPIMP cob_field *cob_intr_seconds_from_formatted_time	(cob_field *,
							 cob_field *);
COB_EXPIMP cob_field *cob_intr_national		(cob_field *);

#endif /* COB_INTRINSIC_H */
