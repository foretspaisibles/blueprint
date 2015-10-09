(* ChartSpaceAllocation -- Space allocation for charts

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open ChartMVC

class space_allocation
    ?(xmin = -. 10.0)
    ?(xmax =    10.0)
    ?(ymin = -. 10.0)
    ?(ymax =    10.0)
    ?(title_height = 1.0)
    ?(title_gap = 0.25)
    ?(legend_height = 1.0)
    ?(legend_gap = 0.25)
    ?(x = 0.0)
    ?(y = 0.0)
    parent
  =
  let _body =
    GnoCanvas.rect
      ~x1:xmin ~x2:xmax ~y1:ymin ~y2:ymax ~fill_color:"cadet blue" parent
  in
  let _legend =
    GnoCanvas.rect
      ~x1:xmin ~x2:xmax
      ~y1:(ymin -. legend_gap -. legend_height)
      ~y2:(ymin -. legend_gap)
      ~fill_color:"green"
      parent
  in
  let _title =
    GnoCanvas.rect
      ~x1:xmin ~x2:xmax
      ~y1:(ymax +. title_gap)
      ~y2:(ymax +. title_gap +. legend_height)
      ~fill_color:"red"
      parent
  in
  object(self)
    inherit GnoCanvas.group parent#as_group as super
    method body = _body
    method legend = _legend
    method title = _title
    initializer begin
      self#move x y;
      _body#reparent (self :> GnoCanvas.group);
      _legend#reparent (self :> GnoCanvas.group);
      _title#reparent (self :> GnoCanvas.group);
    end
  end

let space_allocation
    ?xmin ?xmax ?ymin ?ymax ?title_height ?title_gap
    ?legend_height ?legend_gap
    ?x ?y
    parent
  =
  new space_allocation
    ?xmin ?xmax ?ymin ?ymax ?title_height ?title_gap
    ?legend_height ?legend_gap
    ?x ?y
    parent
