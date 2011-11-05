/*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */
#ifdef __MIRAGE__
#include <os.h>
#else
#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>


#define __XEN_TOOLS__

#define u32 uint32_t
#include <xen/io/xs_wire.h>

#if !HAVE_DECL_XS_RESTRICT
#define XS_RESTRICT 128
#endif

CAMLprim value stub_get_internal_offset(void)
{
	CAMLparam0();
	CAMLreturn(Val_int(XS_RESTRICT));
}

