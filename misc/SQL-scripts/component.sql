SELECT dmudesc,
       compname,
       comppct_r,
       compkind,
       majcompflag,
       localphase,
       drainagecl,
       hydricrating,
       elev_l,
       elev_r,
       elev_h,
       slope_l,
       slope_r,
       slope_h,
       aspectccwise,
       aspectrep,
       aspectcwise,
       map_l,
       map_r,
       map_h,
       airtempa_l  AS maat_l,
       airtempa_r  AS maat_r,
       airtempa_h  AS maat_h,
       soiltempa_r AS mast_r,
       reannualprecip_r,
       ffd_l,
       ffd_r,
       ffd_h,
       tfact,
       wei,
       weg,
       nirrcapcl,
       nirrcapscl,
       nirrcapunit,
       irrcapcl,
       irrcapscl,
       irrcapunit,
       frostact,
       hydricrating,
       hydgrp,
       corcon,
       corsteel,
       taxclname,
       taxorder,
       taxsuborder,
       taxgrtgroup,
       taxsubgrp,
       taxpartsize,
       taxpartsizemod,
       taxceactcl,
       taxreaction,
       taxtempcl,
       taxmoistscl,
       taxtempregime,
       soiltaxedition,
       coiid,
       dmuiid
FROM   datamapunit_view_1 AS dmu
       INNER JOIN component_view_1 AS co
               ON co.dmuiidref = dmu.dmuiid
ORDER  BY dmudesc,
          comppct_r DESC,
          compname ASC; 
		  