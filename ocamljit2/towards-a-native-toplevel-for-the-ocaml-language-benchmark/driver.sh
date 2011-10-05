#!/bin/bash
#
# Copyright (c) 2010-2011, Benedikt Meurer <benedikt.meurer@googlemail.com>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#

OCAML="$HOME/local/ocaml3.12.1/bin/ocaml"
OCAMLJIT2="$HOME/local/ocamljit2/bin/ocaml"
OCAMLNAT="$HOME/local/ocaml3.12.1/bin/ocamlnat"
OCAMLNATJIT="$HOME/local/ocamlnat-jit/bin/ocamlnat"

printf "%-16s %14s %14s %14s %14s\n" "File" "OCaml" "OCamlJIT2" "OCamlNat/ext" "OCamlNat/jit"
for FILE in *.ml; do
  A=`./benchmark "${OCAML}" "${FILE}"` || exit $?
  B=`./benchmark "${OCAMLJIT2}" "${FILE}"` || exit $?
  C=`./benchmark "${OCAMLNAT}" "${FILE}"` || exit $?
  D=`./benchmark "${OCAMLNATJIT}" "${FILE}"` || exit $?
  printf "%-16s %14s %14s %14s %14s\n" "${FILE}" "${A}" "${B}" "${C}" "${D}"
done
