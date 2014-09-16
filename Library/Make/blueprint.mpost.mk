### blueprint.mpost.mk -- Blueprint library

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

.include "blueprint.init.mk"

FILESGROUPS+=	${MPOST}

TEXMFDIR?=	${PREFIX}/share/texmf-local
MPOSTDIR=	${TEXMFDIR}/metapost/${PACKAGE}
MPOSTMODE=	${SHAREMODE}
MPOSTOWN=	${SHAREOWN}
MPOSTGRP=	${SHAREGRP}

USE_SWITCH_CREDENTIALS = yes

.include "bps.init.mk"
.include "bps.credentials.mk"
.include "bps.clean.mk"
.include "bps.files.mk"
.include "bps.usertarget.mk"

### End of file `blueprint.mpost.mk'
