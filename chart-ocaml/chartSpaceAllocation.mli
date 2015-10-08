(* ChartSpaceAllocation -- Space allocation for charts

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
class space_allocation :
  ?xmin:float ->
  ?xmax:float ->
  ?ymin:float ->
  ?ymax:float ->
  ?title_height:float ->
  ?title_gap:float ->
  ?legend_height:float ->
  ?legend_gap:float ->
  ?x:float ->
  ?y:float ->
  #GnoCanvas.group ->
  object
    inherit GnoCanvas.group
    method body : GnoCanvas.rect
    method legend : GnoCanvas.rect
    method title : GnoCanvas.rect
  end

val space_allocation :   ?xmin:float ->
  ?xmax:float ->
  ?ymin:float ->
  ?ymax:float ->
  ?title_height:float ->
  ?title_gap:float ->
  ?legend_height:float ->
  ?legend_gap:float ->
  ?x:float ->
  ?y:float ->
  #GnoCanvas.group -> space_allocation
