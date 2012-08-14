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
