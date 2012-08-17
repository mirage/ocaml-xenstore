/*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
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

#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#include <xenctrl.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <lwt/lwt_unix.h>

struct job_domain_infolist{
  struct lwt_unix_job job;
  /* Inputs */
  xc_domaininfo_t *result;
  uint32_t lowest_domid;
  uint32_t number_requested;
  /* Outputs */
  int errno_copy;
  const xc_error *error;
  uint32_t number_found;
};

static void worker_domain_infolist(struct job_domain_infolist *job)
{
  xc_interface *xch;
  int ret;

  job->errno_copy = 0;
  job->error = NULL;
  job->number_found = 0;

  xch = xc_interface_open(NULL, NULL, 0);
  job->errno_copy = errno;
  if (xch){
	ret = xc_domain_getinfolist(xch, job->lowest_domid, job->number_requested, job->result);
	if (ret < 0) {
	  job->error = xc_get_last_error(xch);
	} else {
	  job->number_found = ret;
	}
	xc_interface_close(xch);
  }
}

#define ERROR_STRLEN 1024

static value result_domain_infolist(struct job_domain_infolist *job)
{
  static char error_str[ERROR_STRLEN];
  uint32_t number_found = job->number_found;
  int err = job->errno_copy;

  if ((job->errno_copy == 0) && (job->error == NULL)){
	lwt_unix_free_job(&job->job);
	return Val_int(number_found);
  }
  if ((job->error) && (job->error->code != XC_ERROR_NONE)) {
	snprintf(error_str, ERROR_STRLEN, "%d: %s: %s",
			 job->error->code,
			 xc_error_code_to_desc(job->error->code),
			 job->error->message);
  } else {
	snprintf(error_str, ERROR_STRLEN, "%d: %s", job->errno_copy, strerror(job->errno_copy));
  }
  lwt_unix_free_job(&job->job);
  value arg = caml_copy_string(error_str);
  unix_error(err, "xc_domain_getinfolist", arg);
}

CAMLprim value lwt_domain_infolist_job(value lowest_domid, value number_requested, value buf)
{
  struct job_domain_infolist* job =
    (struct job_domain_infolist*)lwt_unix_new(struct job_domain_infolist);
  job->lowest_domid = Int_val(lowest_domid);
  job->number_requested = Int_val(number_requested);
  job->result = Data_bigarray_val(buf);
  job->job.worker = (lwt_unix_job_worker)worker_domain_infolist;
  job->job.result = (lwt_unix_job_result)result_domain_infolist;
  return lwt_unix_alloc_job(&job->job);
}

CAMLprim value ml_sizeof_xc_domaininfo_t(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_int(sizeof(xc_domaininfo_t)));
}

CAMLprim value ml_alloc_page_aligned(value bytes)
{
  CAMLparam1(bytes);
  CAMLlocal1(result);
  int toalloc = Int_val(bytes);
  int ret;
  void *buf;

  toalloc = toalloc | 0xfff;
  ret = posix_memalign((void **) ((void *) &buf), 4096, toalloc);
  if (ret)
	caml_raise_out_of_memory ();

  result = alloc_bigarray_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, buf, bytes);
  CAMLreturn(result);
}

CAMLprim value ml_free_page_aligned(value ba)
{
  CAMLparam1(ba);
  free(Data_bigarray_val(ba));
  CAMLreturn(Val_unit);
}

CAMLprim value ml_domain_infolist_parse(value buf)
{
  CAMLparam1(buf);
  CAMLlocal1(result);
  xc_domaininfo_t *di = Data_bigarray_val(buf);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(di->domain));
  Store_field(result, 1, Val_bool(di->flags & XEN_DOMINF_dying));
  Store_field(result, 2, Val_bool(di->flags & XEN_DOMINF_shutdown));

  CAMLreturn(result);
}

struct job_map_foreign{
  struct lwt_unix_job job;
  void *result;
  int errno_copy;
  uint32_t domid;
  unsigned long mfn;
};

static void worker_map_foreign(struct job_map_foreign *job)
{
  xc_interface *xch;
  job->result = NULL;
  xch = xc_interface_open(NULL, NULL, 0);
  job->errno_copy = errno;
  if (xch){
	job->result = xc_map_foreign_range(xch, job->domid, getpagesize(), PROT_READ|PROT_WRITE, job->mfn);
	job->errno_copy = errno;

	xc_interface_close(xch);
  }
}

static value result_map_foreign(struct job_map_foreign *job)
{
  int err = job->errno_copy;
  if (job->result){
	lwt_unix_free_job(&job->job);
	return alloc_bigarray_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, job->result, getpagesize());	
  }
  lwt_unix_free_job(&job->job);
  value arg = caml_copy_string(NULL);
  unix_error(err, "xc_map_foreign_range", arg);
}

CAMLprim value lwt_map_foreign_job(value domid, value mfn)
{
  struct job_map_foreign* job =
    (struct job_map_foreign*)lwt_unix_new(struct job_map_foreign);
  job->domid = Int_val(domid);
  job->mfn = Nativeint_val(mfn);
  job->job.worker = (lwt_unix_job_worker)worker_map_foreign;
  job->job.result = (lwt_unix_job_result)result_map_foreign;
  return lwt_unix_alloc_job(&job->job);
}

#include <xenctrl.h>
#include <xen/io/xs_wire.h>

static int xs_ring_read(void *buf,
						char *buffer, int len)
{
  struct xenstore_domain_interface *intf = buf;
  XENSTORE_RING_IDX cons, prod;
  int to_read;

  cons = intf->req_cons;
  prod = intf->req_prod;
  xen_mb();
  if (prod == cons)
	return 0;
  if (MASK_XENSTORE_IDX(prod) > MASK_XENSTORE_IDX(cons)) 
	to_read = prod - cons;
  else
	to_read = XENSTORE_RING_SIZE - MASK_XENSTORE_IDX(cons);
  if (to_read < len)
	len = to_read;
  memcpy(buffer, intf->req + MASK_XENSTORE_IDX(cons), len);
  xen_mb();
  intf->req_cons += len;
  return len;
}

static int xs_ring_write(void *buf,
						 char *buffer, int len)
{
  struct xenstore_domain_interface *intf = buf;
  XENSTORE_RING_IDX cons, prod;
  int can_write;

  cons = intf->rsp_cons;
  prod = intf->rsp_prod;
  xen_mb();
  if ( (prod - cons) >= XENSTORE_RING_SIZE )
	return 0;
  if (MASK_XENSTORE_IDX(prod) >= MASK_XENSTORE_IDX(cons))
	can_write = XENSTORE_RING_SIZE - MASK_XENSTORE_IDX(prod);
  else 
	can_write = MASK_XENSTORE_IDX(cons) - MASK_XENSTORE_IDX(prod);
  if (can_write < len)
	len = can_write;
  memcpy(intf->rsp + MASK_XENSTORE_IDX(prod), buffer, len);
  xen_mb();
  intf->rsp_prod += len;
  return len;
}

CAMLprim value ml_interface_read(value interface, value buffer, value ofs, value len)
{
  CAMLparam4(interface, buffer, ofs, len);
  CAMLlocal1(result);
  int res;

  res = xs_ring_read(Data_bigarray_val(interface),
					 String_val(buffer) + Int_val(ofs), Int_val(len));
  if (res == -1)
	caml_failwith("huh");
  result = Val_int(res);
  CAMLreturn(result);
}

CAMLprim value ml_interface_write(value interface, value buffer, value ofs, value len)
{
  CAMLparam4(interface, buffer, ofs, len);
  CAMLlocal1(result);
  int res;

  res = xs_ring_write(Data_bigarray_val(interface),
					  String_val(buffer) + Int_val(ofs), Int_val(len));
  result = Val_int(res);
  CAMLreturn(result);
}

CAMLprim value ml_map_fd(value fd, value len)
{
  CAMLparam2(fd, len);

  void *buf = mmap(NULL, Int_val(len), PROT_READ | PROT_WRITE, MAP_SHARED, Int_val(fd), 0);
  if (buf == MAP_FAILED)
	caml_failwith("map failed");
  
  CAMLreturn(alloc_bigarray_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, buf, Int_val(len)));
}	
