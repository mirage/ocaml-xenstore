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
#include <syslog.h>
#include <xenctrl.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <lwt/lwt_unix.h>

/* xc_domain_getinfolist async binding **************************************/

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
  int ret = 0;

  xch = xc_interface_open(NULL, NULL, 0);
  if (xch){
	ret = xc_domain_getinfolist(xch, job->lowest_domid, job->number_requested, job->result);
	if (ret < 0) {
	  job->error = xc_get_last_error(xch);
	  syslog(LOG_ERR, "xc_domain_getinfolist(%x, %d, %d, %x) = %d:%s", xch, job->lowest_domid, job->number_requested, job->result, job->error->code, job->error->message);
	} else {
	  job->number_found = ret;
	}
	xc_interface_close(xch);
  } else {
	syslog(LOG_ERR, "xc_interface_open: %d:%s", errno, strerror(errno));
	job->errno_copy = errno;
  }
}

#define ERROR_STRLEN 1024

static value result_domain_infolist(struct job_domain_infolist *job)
{
  static char error_str[ERROR_STRLEN];
  uint32_t number_found = job->number_found;

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
  syslog(LOG_ERR, "xc_domain_getinfolist: %s", error_str);
  return Val_int(0);
}

CAMLprim value lwt_domain_infolist_job(value lowest_domid, value number_requested, value cstruct)
{
  LWT_UNIX_INIT_JOB(job, domain_infolist, 0);
  job->lowest_domid = Int_val(lowest_domid);
  job->number_requested = Int_val(number_requested);
  job->result = Data_bigarray_val(Field(cstruct, 0));
  job->errno_copy = 0;
  job->error = NULL;
  job->number_found = 0;
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
  CAMLlocal2(result, some);
  int toalloc = Int_val(bytes);
  int ret;
  void *buf;

  toalloc = toalloc | 0xfff;
  ret = posix_memalign((void **) ((void *) &buf), 4096, toalloc);
  if (ret) {
	syslog(LOG_ERR, "posix_memalign(%x, 4096, %d) = %d:%s", &buf, toalloc, errno, strerror(errno));
	result = Val_int(0);
  } else {
	some = alloc_bigarray_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, buf, bytes);
	result = caml_alloc_tuple(1);
	Store_field(result, 0, some);
  }
  CAMLreturn(result);
}

CAMLprim value ml_free_page_aligned(value ba)
{
  CAMLparam1(ba);
  free(Data_bigarray_val(ba));
  CAMLreturn(Val_unit);
}

CAMLprim value ml_domain_infolist_parse(value cstruct)
{
  CAMLparam1(cstruct);
  CAMLlocal3(result, v_ba, v_ofs);
  unsigned char *addr;
  v_ba = Field(cstruct, 0);
  v_ofs = Field(cstruct, 1);
  addr = Caml_ba_data_val(v_ba) + Int_val(v_ofs);

  xc_domaininfo_t *di = addr;
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(di->domain));
  Store_field(result, 1, Val_bool(di->flags & XEN_DOMINF_dying));
  Store_field(result, 2, Val_bool(di->flags & XEN_DOMINF_shutdown));

  CAMLreturn(result);
}

/* xc_map_foreign_range async binding ***************************************/

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
  LWT_UNIX_INIT_JOB(job, map_foreign, 0);
  job->domid = Int_val(domid);
  job->mfn = Nativeint_val(mfn);
  return lwt_unix_alloc_job(&job->job);
}

CAMLprim value ml_unmap(value ba)
{
  CAMLparam1(ba);
  int ret = munmap(Data_bigarray_val(ba), 4096);
  if (ret != 0)
	syslog(LOG_ERR, "munmap %x = %d:%s", Data_bigarray_val(ba), errno, strerror(errno));

  CAMLreturn(Val_unit);
}

CAMLprim value ml_map_fd(value fd, value len)
{
  CAMLparam2(fd, len);
  CAMLlocal2(result, some);

  void *buf = mmap(NULL, Int_val(len), PROT_READ | PROT_WRITE, MAP_SHARED, Int_val(fd), 0);
  if (buf == MAP_FAILED) {
	syslog(LOG_ERR, "mmap(NULL, %d, PROT_READ | PROT_WRITE, MAP_SHARED, %d, 0) = %d:%s", Int_val(len), Int_val(fd), errno, strerror(errno));
	result = Val_int(0);
  } else {
	result = caml_alloc_tuple(1);
	some = alloc_bigarray_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, buf, Int_val(len));
	Store_field(result, 0, some);
  };
  CAMLreturn(result);
}	

/* /dev/xen/eventchn handling ***********************************************/

#define _H(__h) ((xc_interface *)(__h))

CAMLprim value stub_xc_evtchn_open(void)
{
  CAMLparam0();
  CAMLlocal1(result);

  xc_interface *xce = xc_evtchn_open(NULL, 0);
  if (xce == NULL) {
	syslog(LOG_ERR, "xc_evtchn_open(NULL, 0) = %d:%s", errno, strerror(errno));
	result = Val_int(0);
  } else {
	result = caml_alloc_tuple(1);
	Store_field(result, 0, (value)xce);
  }
  CAMLreturn(result);
}

CAMLprim value stub_xc_evtchn_close(value xce)
{
  CAMLparam1(xce);

  int ret = xc_evtchn_close(_H(xce));
  if (ret == -1)
	syslog(LOG_ERR, "xc_evtchn_close %x = %d:%s", _H(xce), errno, strerror(errno));

  CAMLreturn(Val_unit);
}


CAMLprim value stub_xc_evtchn_fd(value xce)
{
  CAMLparam1(xce);
  CAMLlocal1(result);
  int fd;

  fd = xc_evtchn_fd(_H(xce));
  if (fd == -1) {
	syslog(LOG_ERR, "xc_evtchn_fd(%x) = %d:%s", _H(xce), errno, strerror(errno));
	result = Val_int(0);
  } else {
	result = caml_alloc_tuple(1);
	Store_field(result, 0, Val_int(fd));
  }

  CAMLreturn(result);
}

CAMLprim value stub_xc_evtchn_notify(value xce, value port)
{
  CAMLparam2(xce, port);
  int rc;

  rc = xc_evtchn_notify(_H(xce), Int_val(port));
  if (rc == -1)
	syslog(LOG_ERR, "xc_evtchn_notify(%x, %d) = %d:%s", _H(xce), Int_val(port), errno, strerror(errno));

  CAMLreturn(Val_unit);
}

CAMLprim value stub_xc_evtchn_bind_interdomain(value xce, value domid,
                                               value remote_port)
{
  CAMLparam3(xce, domid, remote_port);
  CAMLlocal1(result);
  evtchn_port_or_error_t rc;

  rc = xc_evtchn_bind_interdomain(_H(xce), Int_val(domid), Int_val(remote_port));
  if (rc == -1) {
	syslog(LOG_ERR, "xc_evtchn_bind_interdomain(%x, %d, %d) = %d:%s", _H(xce), Int_val(domid), Int_val(remote_port), errno, strerror(errno));
	result = Val_int(0);
  } else {
	result = caml_alloc_tuple(1);
	Store_field(result, 0, Val_int(rc));
  }
  CAMLreturn(result);
}

CAMLprim value stub_xc_evtchn_bind_virq_dom_exc(value xce)
{
  CAMLparam1(xce);
  CAMLlocal1(result);
  evtchn_port_or_error_t rc;

  rc = xc_evtchn_bind_virq(_H(xce), VIRQ_DOM_EXC);
  if (rc == -1) {
	syslog(LOG_ERR, "xc_evtchn_bind_virq(%x, VIRQ_DOM_EXC) = %d:%s", _H(xce), errno, strerror(errno));
	result = Val_int(0);
  } else {
	result = caml_alloc_tuple(1);
	Store_field(result, 0, Val_int(rc));
  }
  CAMLreturn(result);
}

CAMLprim value stub_xc_evtchn_unbind(value xce, value port)
{
  CAMLparam2(xce, port);
  int rc;

  rc = xc_evtchn_unbind(_H(xce), Int_val(port));
  if (rc == -1)
	syslog(LOG_ERR, "xc_evtchn_unbind(%x, %d) = %d:%s", _H(xce), Int_val(port), errno, strerror(errno));

  CAMLreturn(Val_unit);
}

CAMLprim value stub_xc_evtchn_pending(value xce)
{
  CAMLparam1(xce);
  CAMLlocal1(result);
  evtchn_port_or_error_t port;

  port = xc_evtchn_pending(_H(xce));
  if (port == -1) {
	syslog(LOG_ERR, "xc_evtchn_pending(%x) = %d:%s", _H(xce), errno, strerror(errno));
	result = Val_int(0);
  } else {
	result = caml_alloc_tuple(1);
	Store_field(result, 0, Val_int(port));
  }

  CAMLreturn(result);
}

CAMLprim value stub_xc_evtchn_unmask(value xce, value _port)
{
  CAMLparam2(xce, _port);
  evtchn_port_t port;

  port = Int_val(_port);
  if (xc_evtchn_unmask(_H(xce), port))
	syslog(LOG_ERR, "xc_evtchn_unmask(%x, %d) = %d:%s", _H(xce), port, errno, strerror(errno));

  CAMLreturn(Val_unit);
}

