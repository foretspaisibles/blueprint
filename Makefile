### Makefile -- Blueprint

# Author: Michael Grünewald
# Date: Sun Sep 14 23:57:54 CEST 2014

# Blueprint (https://bitbucket.org/michipili/blueprint)
# This file is part of Blueprint
#
# Copyright © 2014 Michael Grünewald
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

PACKAGE=	blueprint
VERSION=	0.0.2-current
OFFICER=	michipili@gmail.com

MODULE=		mpost.files:base
MODULE+=	mpost.files:pm
MODULE+=	mpost.files:uml
MODULE+=	mpost.files:chart
MODULE+=	mpost.doc:example

post-install-mktexlsr: .PHONY
	mktexlsr ${datarootdir}/texmf-local

post-install: post-install-mktexlsr

.include "generic.project.mk"

### End of file `Makefile'
