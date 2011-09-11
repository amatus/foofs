#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <microhttpd.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

static int parse_url(const char *url, int *gen, char **e_hnk, char **c_index)
{
  int n;
  char *buf, *p;

  if( NULL != gen ) {
    if( sscanf(url, "/%d/%n", gen, &n) != 2 ) {
      return 0;
    }
    url += n;
  } else {
    if( '/' != url[0] ) {
      return 0;
    }
    url++;
  }
  if( NULL != e_hnk ) {
    p = strchr(url, '/');
    if( NULL == p ) {
      n = strlen(url);
    } else {
      n = p - url;
    }
    buf = malloc(n + 1);
    memcpy(buf, url, n);
    buf[n] = 0;
    *e_hnk = buf;
    url += n + 1;
  }
  if( NULL != c_index ) {
    p = strchr(url, '/');
    if( NULL == p ) {
      n = strlen(url);
    } else {
      n = p - url;
    }
    buf = malloc(n + 1);
    memcpy(buf, url, n);
    buf[n] = 0;
    *c_index = buf;
    //url += n + 1;
  }
  return 1;
}

struct read_blocks_state {
  struct MHD_Response *response;
  char *e_hnk;
  char *server_path;
  DIR *server_dir;
  char *e_hnk_path;
  DIR *e_hnk_dir;
  int fd;
  unsigned int count;
};

static void free_read_blocks_state(void *cls)
{
  struct read_blocks_state *s = cls;

  if( NULL != s->response ) {
    MHD_destroy_response(s->response);
  }
  if( NULL != s->e_hnk ) {
    free(s->e_hnk);
  }
  if( NULL != s->server_path ) {
    free(s->server_path);
  }
  if( NULL != s->server_dir ) {
    closedir(s->server_dir);
  }
  if( NULL != s->e_hnk_path ) {
    free(s->e_hnk_path);
  }
  if( NULL != s->e_hnk_dir ) {
    closedir(s->e_hnk_dir);
  }
  if( -1 != s->fd ) {
    close(s->fd);
  }
  free(s);
}

static int do_readdir(DIR *dirp, void **mem, struct dirent **result) {
  size_t len;

  len = offsetof(struct dirent, d_name) + fpathconf(dirfd(dirp), _PC_NAME_MAX)
        + 1;
  *mem = malloc(len);
  if( NULL == *mem ) {
    return 1;
  }
  return readdir_r(dirp, *mem, result);
}

static ssize_t read_blocks(void *cls, uint64_t pos, char *buf, size_t max)
{
  struct read_blocks_state *s = cls;
  struct dirent *de;
  void *mem;
  char *path;
  char *footer;
  char *content;
  struct stat st;
  ssize_t len;
  static const char *footer_prefix = "X-Block-";

again:
  if( -1 != s->fd ) {
    len = read(s->fd, buf, max);
    if( -1 == len ) {
      if( EINTR == errno ) {
        goto again;
      }
      return MHD_CONTENT_READER_END_WITH_ERROR;
    }
    if( 0 < len ) {
      return len;
    }
    close(s->fd);
    s->fd = -1;
  }
  // we don't have an open file, so find one to open
  if( NULL != s->e_hnk_dir ) {
    len = do_readdir(s->e_hnk_dir, &mem, &de);
    if( 0 != len ) {
      if( NULL != mem ) {
        free(mem);
      }
      return MHD_CONTENT_READER_END_WITH_ERROR;
    }
    if( NULL != de ) {
      len = strlen(s->e_hnk_path) + strlen(de->d_name) + 1;
      path = malloc(len + 1);
      if( NULL == path ) {
        free(mem);
        return MHD_CONTENT_READER_END_WITH_ERROR;
      }
      sprintf(path, "%s/%s", s->e_hnk_path, de->d_name);
      len = stat(path, &st);
      if( -1 == len || !S_ISREG(st.st_mode)) {
        free(path);
        free(mem);
        goto again;
      }
      s->fd = open(path, O_RDONLY);
      free(path);
      if( -1 == s->fd ) {
        free(mem);
        goto again;
      }
      len = strlen(de->d_name) + 1 + 21;
      content = malloc(len + 1);
      if( NULL == content ) {
        free(mem);
        return MHD_CONTENT_READER_END_WITH_ERROR;
      }
      sprintf(content, "%s %llu", de->d_name, st.st_size);
      free(mem);
      len = strlen(footer_prefix) + 10;
      footer = malloc(len + 1);
      if( NULL == footer ) {
        free(content);
        return MHD_CONTENT_READER_END_WITH_ERROR;
      }
      sprintf(footer, "%s%u", footer_prefix, s->count);
      s->count++;
      MHD_add_response_footer(s->response, footer, content);
      free(footer);
      free(content);
      goto again;
    }
    free(mem);
    free(s->e_hnk_path);
    s->e_hnk_path = NULL;
    closedir(s->e_hnk_dir);
    s->e_hnk_dir = NULL;
  }
  len = do_readdir(s->server_dir, &mem, &de);
  if( 0 != len ) {
    if( NULL != mem ) {
      free(mem);
    }
    return MHD_CONTENT_READER_END_WITH_ERROR;
  }
  if( NULL != de ) {
    len = strlen(s->server_path) + strlen(de->d_name) + strlen(s->e_hnk) + 3;
    path = malloc(len + 1);
    if( NULL == path ) {
      free(mem);
      return MHD_CONTENT_READER_END_WITH_ERROR;
    }
    sprintf(path, "%s/%s/%c%c/%s", s->server_path, de->d_name, s->e_hnk[0], s->e_hnk[1],
      &s->e_hnk[2]);
    free(mem);
    s->e_hnk_dir = opendir(path);
    if( NULL == s->e_hnk_dir ) {
      free(path);
      goto again;
    }
    s->e_hnk_path = path;
    goto again;
  }
  free(mem);
  return MHD_CONTENT_READER_END_OF_STREAM;
}

static int dh(void *cls, struct MHD_Connection *connection,
 const char *url, const char *method, const char *version,
 const char *upload_data, size_t *upload_data_size, void **con_cls)
{
  struct MHD_Response *response;
  int ret = MHD_NO;
  int status_code = MHD_HTTP_NOT_IMPLEMENTED;

  if( strcmp(method, MHD_HTTP_METHOD_GET) == 0 ) {
    char *e_hnk;
    struct read_blocks_state *s;

    s = malloc(sizeof(*s));
    if( NULL == s ) {
      status_code = MHD_HTTP_INTERNAL_SERVER_ERROR;
      goto err;
    }
    memset(s, 0, sizeof(*s));
    s->fd = -1;
    if( !parse_url(url, NULL, &s->e_hnk, NULL)) {
      free_read_blocks_state(s);
      status_code = MHD_HTTP_NOT_FOUND;
      goto err;
    }
    if( strlen(s->e_hnk) < 3 ) {
      free_read_blocks_state(s);
      status_code = MHD_HTTP_NOT_FOUND;
      goto err;
    }
    s->server_path = strdup("/tmp/test_storage");
    s->server_dir = opendir(s->server_path);
    if( NULL == s->server_dir ) {
      free_read_blocks_state(s);
      status_code = MHD_HTTP_NOT_FOUND;
      goto err;
    }
    // do a callback response which looks for matching blocks and sends them
    // while adding footers which indicate which indexes they were
    s->response = MHD_create_response_from_callback(
      /*       size */ MHD_SIZE_UNKNOWN, 
      /* block_size */ 64 * 1024,
      /*        crc */ &read_blocks,
      /*    crc_cls */ s,
      /*       crfc */ &free_read_blocks_state);
    ret = MHD_queue_response(connection, MHD_HTTP_OK, s->response);
    // XXX does crfc get called if this failed?
  } else if( strcmp(method, MHD_HTTP_METHOD_PUT) == 0 ) {
    int gen;
    char *e_hnk, *c_index;

    if( !parse_url(url, &gen, &e_hnk, &c_index)) {
      status_code = MHD_HTTP_NOT_FOUND;
      goto err;
    }
    free(e_hnk);
    free(c_index);
    goto err;
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
