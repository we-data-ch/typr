decl(var('add', empty, public, false, num), any, fn([],[[var('a'),num], [var('b'),num]],num,sequence([]))),
decl(var('add', empty, public, false, int), any, fn([],[[var('a'),int], [var('b'),int]],int,sequence([]))),
decl(var('map', empty, public, false, tarray(ind('n'), gen('t'))), any, fn([],[[var('a'),tarray(ind('n'), gen('t'))], [var('f'),tfn([], [gen('t')], gen('u'))]],tarray(ind('n'), gen('u')),sequence([]))),
decl(var('incr', empty, public, false, num), any, fn([],[[var('a'),num]],num,sequence([]))),
decl(var('seq', empty, public, false, ind('i')), any, fn([],[[var('a'),ind('i')], [var('b'),ind('j')], [var('c'),ind('k')]],tarray(division(minus(ind('j'), ind('i')), ind('k')), int),sequence([])))
