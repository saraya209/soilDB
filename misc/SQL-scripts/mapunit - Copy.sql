SELECT ng.grpname,
       areasymbol,
       areatypename,
       liid,
       lmapunitiid,
       nationalmusym,
       muiid,
       musym,
       muname,
       mukind,
       mutype,
       mustatus,
       dmuinvesintens,
       muacres,
       farmlndcl,
       dmuiid,
       pct_component,
       pct_hydric,
       n_component,
       n_majcompflag
FROM   area a
       INNER JOIN legend_view_1 l
               ON l.areaiidref = a.areaiid
       INNER JOIN lmapunit_view_1 lmu
               ON lmu.liidref = l.liid
       INNER JOIN mapunit_view_1 mu
               ON mu.muiid = lmu.muiidref
       INNER JOIN areatype at
               ON at.areatypeiid = areatypeiidref
       INNER JOIN nasisgroup ng
               ON ng.grpiid = mu.grpiidref
       LEFT OUTER JOIN
       --components
       (SELECT cor.muiidref         cor_muiidref,
               dmuiid,
               dmuinvesintens,
               Sum(comppct_r)       pct_component,
               Sum(comppct_r * CASE
                                 WHEN hydricrating = 1 THEN 1
                                 ELSE 0
                               END) pct_hydric,
               Count(*)             n_component,
               Sum(CASE
                     WHEN majcompflag = 1 THEN 1
                     ELSE 0
                   END)             n_majcompflag
        FROM   component_view_1 co
               LEFT OUTER JOIN datamapunit_view_1 dmu
                            ON dmu.dmuiid = co.dmuiidref
               LEFT OUTER JOIN correlation_view_1 cor
                            ON cor.dmuiidref = dmu.dmuiid
                               AND cor.repdmu = 1
        GROUP  BY cor.muiidref,
                  dmuiid,
                  dmuinvesintens) co
                    ON co.cor_muiidref = mu.muiid
WHERE  areatypename IN ( 'Non-MLRA Soil Survey Area', 'MLRA Soil Survey Area' )
ORDER  BY areasymbol,
          musym;