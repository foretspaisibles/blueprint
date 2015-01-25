### Makefile -- Blueprint chart

# Author: Michael Grünewald
# Date: Sun Jan  4 23:01:43 CET 2015

# Blueprint (https://github.com/michipili/blueprint)
# This file is part of Blueprint
#
# Copyright © 2014–2015 Michael Grünewald
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

PROGRAM=		test-chart

SRCS+=			gHelper.ml
SRCS+=			patternSubjectObserver.ml
SRCS+=			card.ml
SRCS+=			chart.ml
SRCS+=			application.ml

EXTERNAL+=		ocaml.findlib:lablgtk2
EXTERNAL+=		ocaml.findlib:lablgtk2.gnomecanvas

test: ${PROGRAM}
	./${PROGRAM}

.include "ocaml.prog.mk"

### End of file `Makefile'
