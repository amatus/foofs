/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*- */
/* vim: set expandtab ts=2 sw=2: */
/*
 * main.c
 * Copyright (C) David Barksdale 2011 <amatus.amongus@gmail.com>
 * 
 * foofs is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * foofs is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <microhttpd.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include "storage.h"

char *g_storage_root = "/tmp/test_storage";

char *strprintf(const char *format, ...)
{
  va_list arg;
  int len;
  char *str;

  va_start(arg, format);
  len = vsnprintf(NULL, 0, format, arg);
  va_end(arg);
  str = malloc(len + 1);
  if( NULL == str ) {
    return NULL;
  }
  va_start(arg, format);
  vsprintf(str, format, arg);
  va_end(arg);
  return str;
}

const char *copy_path_elem(const char *path, char **out)
{
  const char *p;
  char *buf;
  size_t n;

  if( NULL == path || NULL == out ) {
    return path;
  }
  for(; *path == '/'; path++) {
  }
  p = strchr(path, '/');
  if( NULL == p ) {
    n = strlen(path);
  } else {
    n = p - path;
  }
  buf = malloc(n + 1);
  if( NULL == buf ) {
    *out = NULL;
    return path;
  }
  memcpy(buf, path, n);
  buf[n] = 0;
  *out = buf;
  return path + n;
}

static int mkdirs(const char *pathname, mode_t mode)
{
  char *path, *elem, *tmp;

  if( NULL == pathname ) {
    return EFAULT;
  }
  if( '/' == pathname[0] ) {
    path = strdup("/.");
  } else {
    path = strdup(".");
  }
  while( 1 ) {
    pathname = copy_path_elem(pathname, &elem);
    if( NULL == elem ) {
      return ENOMEM;
    }
    if( 0 == elem[0] ) {
      return 0;
    }
    tmp = strprintf("%s/%s", path, elem);
    free(path);
    if( NULL == tmp ) {
      return ENOMEM;
    }
    path = tmp;
    if( -1 == mkdir(path, mode) && EEXIST != errno ) {
      return errno;
    }
  }
}

int dir_foreach(DIR *dirp, dir_foreach_cb cb, void *cls)
{
  size_t len;
  void *mem;
  struct dirent *de;
  int count;

  len = offsetof(struct dirent, d_name) + fpathconf(dirfd(dirp), _PC_NAME_MAX)
        + 1;
  mem = malloc(len);
  if( NULL == mem ) {
    return -1;
  }
  for( count = 0; ; count++ ) {
    if( 0 == readdir_r(dirp, mem, &de)) {
      if( NULL == de ) {
        break;
      }
      if( 0 != cb(cls, de->d_name)) {
        break;
      }
    } else {
      count = -1;
      break;
    }
  }
  return count;
}

struct write_blocks_state {
  int fd;
  char *filename;
  char *tmpfile;
};

static void notify_completed(void *cls, struct MHD_Connection *connection,
 void **con_cls, enum MHD_RequestTerminationCode toe)
{
  struct write_blocks_state *s = *con_cls;

  if( NULL == s ) {
    return;
  }
  if( -1 != s->fd ) {
    close(s->fd);
    // TODO: handle EINTR
  }
  if( NULL != s->filename ) {
    free(s->filename);
  }
  if( NULL != s->tmpfile ) {
    unlink(s->tmpfile);
    free(s->tmpfile);
  }
}

static int dh(void *cls, struct MHD_Connection *connection,
 const char *url, const char *method, const char *version,
 const char *upload_data, size_t *upload_data_size, void **con_cls)
{
  struct MHD_Response *response;
  int ret = MHD_NO;
  int status_code = MHD_HTTP_NOT_IMPLEMENTED;

  if( strcmp(method, MHD_HTTP_METHOD_GET) == 0 ) {
    return handle_get(cls, connection, url);
  } else if( strcmp(method, MHD_HTTP_METHOD_PUT) == 0 ) {
    struct write_blocks_state *s = *con_cls;
    const char *gen;
    char *e_hnk, *c_index;
    char *path, *tmp;
    struct stat st;
    ssize_t len;

    if( NULL != s ) {
      if( 0 == *upload_data_size ) {
        // no more data, make sure everything is on disk before giving the OK
        ret = fsync(s->fd);
        if( -1 == ret ) {
          status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
          goto err;
        }
        ret = close(s->fd);
        if( -1 == ret ) {
          // TODO: handle EINTR
          status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
          goto err;
        }
        s->fd = -1;
        ret = rename(s->tmpfile, s->filename);
        if( -1 == ret ) {
          status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
          goto err;
        }
        status_code = MHD_HTTP_OK;
        // not actually an error, but we have no document to return right now.
        // perhaps we could return a checksum or something.
        goto err;
      }
write_again:
      len = write(s->fd, upload_data, *upload_data_size);
      if( -1 == len ) {
        if( EINTR == errno ) {
          goto write_again;
        }
        status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
        goto err;
      }
      *upload_data_size -= len;
      return MHD_YES;
    }
    *con_cls = s = malloc(sizeof(*s));
    if( NULL == s ) {
      status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
      goto err;
    }
    memset(s, 0, sizeof(*s));
    s->fd = -1;
    url = copy_path_elem(url, &e_hnk);
    if( NULL == e_hnk || strlen(e_hnk) < 3 || '.' == e_hnk[0] ) {
      if( NULL != e_hnk ) {
        free(e_hnk);
      }
      status_code = MHD_HTTP_BAD_REQUEST;
      goto err;
    }
    copy_path_elem(url, &c_index);
    if( NULL == c_index || strlen(c_index) < 1 || '.' == c_index[0] ) {
      if( NULL != c_index ) {
        free(c_index);
      }
      free(e_hnk);
      status_code = MHD_HTTP_BAD_REQUEST;
      goto err;
    }
    gen = MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "gen");
    if( NULL == gen || strlen(gen) < 1 || '.' == gen[0] ) {
      free(c_index);
      free(e_hnk);
      status_code = MHD_HTTP_BAD_REQUEST;
      goto err;
    }
    tmp = strprintf("%s/%s/%c%c/%s", g_storage_root, gen, e_hnk[0], e_hnk[1],
                    &e_hnk[2]);
    free(e_hnk);
    if( NULL == tmp ) {
      free(c_index);
      status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
      goto err;
    }
    path = tmp;
    mkdirs(path, 0777);
    tmp = strprintf("%s/%s", path, c_index);
    if( NULL == tmp ) {
      free(path);
      free(c_index);
      status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
      goto err;
    }
    s->filename = tmp;
    // TODO: check for the existance in all generations
    ret = stat(tmp, &st);
    if( 0 == ret ) {
      free(path);
      free(c_index);
      status_code = MHD_HTTP_CONFLICT;
      goto err;
    }
    tmp = strprintf("%s/.%s", path, c_index);
    free(path);
    free(c_index);
    if( NULL == tmp ) {
      status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
      goto err;
    }
    s->tmpfile = tmp;
    s->fd = open(tmp, O_WRONLY | O_CREAT | O_EXCL, 0777);
    if( -1 == s->fd ) {
      // TODO: handle EINTR
      if( EEXIST == errno ) {
        status_code = MHD_HTTP_LOCKED;
      } else {
        status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
      }
      goto err;
    }
    return MHD_YES;
  } else {
    goto err;
  }
  return ret;
err:
  response = MHD_create_response_from_buffer(0, NULL, MHD_RESPMEM_PERSISTENT);
  ret = MHD_queue_response(connection, status_code, response);
  MHD_destroy_response(response);
  return ret;
}

int main(int argc, char **argv)
{
  struct MHD_Daemon *daemon;

  daemon = MHD_start_daemon(
    /*   flags */ MHD_USE_THREAD_PER_CONNECTION
                | MHD_USE_DEBUG, // | MHD_USE_SSL,
    /*    port */ 2322,
    /*     apc */ NULL,
    /* apc_cls */ NULL,
    /*      dh */ dh,
    /*  dh_cls */ NULL,
    /* options */ //MHD_OPTION_HTTPS_MEM_KEY, key_pem,
                  //MHD_OPTION_HTTPS_MEM_CERT, cert_pem,
                  MHD_OPTION_NOTIFY_COMPLETED, &notify_completed, NULL,
                  MHD_OPTION_END);
  if( NULL == daemon )
  {
    fprintf(stderr, "Unable to start daemon\n");
    return 1;
  }
  while(1) sleep(100);
  MHD_stop_daemon(daemon);
  return 0;
}
