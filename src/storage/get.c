/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*- */
/* vim: set expandtab ts=2 sw=2: */
/*
 * get.c
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
#include <errno.h>
#include <fcntl.h>
#include <microhttpd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "storage.h"

struct c_block_list {
  struct c_block_list *next;
  char *path;
  char *c_index;
  off_t size;
  int skip;
};

static struct c_block_list *free_c_block_list_head(struct c_block_list *block)
{
  struct c_block_list *next = block->next;
  
  if( NULL != block->path ) {
    free(block->path);
  }
  if( NULL != block->c_index ) {
    free(block->c_index);
  }
  free(block);
  return next;
}

struct read_blocks_state {
  char *e_hnk;
  char *c_index;
  char *path;
  int fd;
  struct c_block_list *blocks;
};

static void free_read_blocks_state(void *cls)
{
  struct read_blocks_state *s = cls;

  if( NULL != s->e_hnk ) {
    free(s->e_hnk);
  }
  if( NULL != s->c_index ) {
    free(s->c_index);
  }
  if( NULL != s->path ) {
    free(s->path);
  }
  if( -1 != s->fd ) {
    close(s->fd);
  }
  while( NULL != s->blocks ) {
    s->blocks = free_c_block_list_head(s->blocks);
  }
  free(s);
}

static ssize_t read_blocks(void *cls, uint64_t pos, char *buf, size_t max)
{
  struct read_blocks_state *s = cls;
  ssize_t len;

again:
  if( -1 != s->fd ) {
    len = read(s->fd, buf, max);
    if( -1 == len ) {
      if( EINTR == errno ) {
        goto again;
      }
      return MHD_CONTENT_READER_END_OF_STREAM;
    }
    if( len > 0 ) {
      // TODO: don't return just yet if we have more buffer we can fill
      return len;
    }
    close(s->fd);
    // TODO: handle EINTR
    s->fd = -1;
  }
  // open the next file
  if( NULL != s->blocks ) {
    s->fd = open(s->blocks->path, O_RDONLY);
    if( -1 == s->fd ) {
      return MHD_CONTENT_READER_END_OF_STREAM;
    }
    s->blocks = free_c_block_list_head(s->blocks);
    goto again;
  }
  return MHD_CONTENT_READER_END_OF_STREAM;
}

static int get_arg_visitor(void *cls, enum MHD_ValueKind kind, const char *key,
 const char *value)
{
  struct c_block_list *block = cls;

  if(0 != strcmp(key, "skip")) {
    return MHD_YES;
  }
  if(0 != strcmp(value, block->c_index)) {
    return MHD_YES;
  }
  block->skip = 1;
  return MHD_NO;
}

static int foreach_c_block(void *cls, const char *name)
{
  struct read_blocks_state *s = cls;
  struct stat st;
  struct c_block_list *block;
  char *path;
  int ret;

  if( '.' == name[0] ) {
    return 0;
  }
  path = strprintf("%s/%s", s->path, name);
  if( NULL == path ) {
    return 1;
  }
  ret = stat(path, &st);
  if( -1 == ret || !S_ISREG(st.st_mode)) {
    free(path);
    return 0;
  }
  block = malloc(sizeof(*block));
  if( NULL == block ) {
    free(path);
    return 1;
  }
  block->next = s->blocks;
  block->path = path;
  block->c_index = strdup(name);
  block->size = st.st_size;
  block->skip = 0;
  s->blocks = block;
  return 0;
}

static int foreach_gen(void *cls, const char *name)
{
  struct read_blocks_state *s = cls;
  DIR *dir;

  if( '.' == name[0] ) {
    return 0;
  }
  s->path = strprintf("%s/%s/%c%c/%s", g_storage_root, name,
      s->e_hnk[0], s->e_hnk[1], &s->e_hnk[2]);
  if( NULL == s->path ) {
    return 1;
  }
  if( NULL != s->c_index ) {
    int ret;

    ret = foreach_c_block(s, s->c_index);
    free(s->path);
    s->path = NULL;
    return ret;
  }
  dir = opendir(s->path);
  if( NULL == dir ) {
    free(s->path);
    s->path = NULL;
    return 0;
  }
  dir_foreach(dir, foreach_c_block, s);
  free(s->path);
  s->path = NULL;
  return 0;
}

int handle_get(void *cls, struct MHD_Connection *connection,
 const char *url)
{
  struct MHD_Response *response;
  DIR *server_dir;
  struct read_blocks_state *s;
  struct c_block_list *block, **prev;
  char *e_hnk, *c_index;
  int ret;
  int status_code;
  int count;

  s = malloc(sizeof(*s));
  if( NULL == s ) {
    status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
    goto err;
  }
  memset(s, 0, sizeof(*s));
  s->fd = -1;
  url = copy_path_elem(url, &s->e_hnk);
  if( NULL == s->e_hnk || strlen(s->e_hnk) < 3 || '.' == s->e_hnk[0]
      || '.' == s->e_hnk[2]) {
    free_read_blocks_state(s);
    status_code = MHD_HTTP_BAD_REQUEST;
    goto err;
  }
  if( 0 != url[0] ) {
    copy_path_elem(url, &s->c_index);
    if( NULL == s->c_index || 0 == s->c_index[0] || '.' == s->c_index[0] ) {
      free_read_blocks_state(s);
      status_code = MHD_HTTP_BAD_REQUEST;
      goto err;
    }
  }
  server_dir = opendir(g_storage_root);
  if( NULL == server_dir ) {
    free_read_blocks_state(s);
    status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
    goto err;
  }
  ret = dir_foreach(server_dir, foreach_gen, s);
  closedir(server_dir);
  if( ret < 1 ) {
    free_read_blocks_state(s);
    status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
    goto err;
  }
  response = MHD_create_response_from_callback(
    /*       size */ MHD_SIZE_UNKNOWN, 
    /* block_size */ 64 * 1024,
    /*        crc */ &read_blocks,
    /*    crc_cls */ s,
    /*       crfc */ &free_read_blocks_state);
  prev = &s->blocks;
  block = *prev;
  count = 0;
  while( NULL != block ) {
    char *header, *content;

    MHD_get_connection_values(connection, MHD_GET_ARGUMENT_KIND,
      get_arg_visitor, block);
    if( block->skip ) {
      *prev = free_c_block_list_head(block);
      block = *prev;
      continue;
    }
    header = strprintf("X-Block-%u", count);
    if( NULL == header ) {
      MHD_destroy_response(response);
      status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
      goto err;
    }
    content = strprintf("%s %jd", block->c_index, block->size);
    if( NULL == content ) {
      free(header);
      MHD_destroy_response(response);
      status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
      goto err;
    }
    MHD_add_response_header(response, header, content);
    prev = &block->next;
    block = *prev;
    count++;
  }
  if( 0 == count ) {
    MHD_destroy_response(response);
    status_code = MHD_HTTP_NOT_FOUND;
    goto err;
  }
  ret = MHD_queue_response(connection, MHD_HTTP_OK, response);
  MHD_destroy_response(response);
  return ret;
err:
  response = MHD_create_response_from_buffer(0, NULL, MHD_RESPMEM_PERSISTENT);
  ret = MHD_queue_response(connection, status_code, response);
  MHD_destroy_response(response);
  return ret;
}
