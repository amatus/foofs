/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*- */
/*
 * storage.h
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
#ifndef _STORAGE_H_
#define _STORAGE_H_

#include <dirent.h>

/* main.c */
typedef int (*dir_foreach_cb)(void *cls, const char *name);

extern char *g_storage_root;

char *strprintf(const char *format, ...)
  __attribute__((format(printf, 1, 2)));

const char *copy_path_elem(const char *path, char **out);

int dir_foreach(DIR *dirp, dir_foreach_cb cb, void *cls);

/* get.c */
int handle_get(void *cls, struct MHD_Connection *connection,
 const char *url);

#endif
