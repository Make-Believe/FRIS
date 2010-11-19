/*
  helloworld -- hash digest functions for R

  Copyright (C) 2003 - 2009  Dirk Eddelbuettel <edd@debian.org>

  $Id: digest.c 24 2009-09-24 12:36:55Z edd $

  This file is part of the digest packages for GNU R.
  It is made available under the terms of the GNU General Public
  License, version 2, or at your option, any later version,
  incorporated herein by reference.

  This program is distributed in the hope that it will be
  useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public
  License along with this program; if not, write to the Free
  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

SEXP helloworld(SEXP cl, data) {
  SEXP result = NULL;

  PROTECT(result=allocVector(STRSXP, 1));
  SET_STRING_ELT(result, 0, mkChar("Hello world!!"));
  UNPROTECT(1);
 
  return result;
}
