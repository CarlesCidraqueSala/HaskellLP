PRÀCTICA HASKELL HS-DTS. DECISION TREES

El programa dts.hs es un programa en Haskell que permet construir arbres de decisió (decision trees) a partir d'un conjunt de dades, en concret de dades que representen un conjunt de bolets amb 23 atributs. Aquest data set està ubicat en el fitxer agaricus-lepiota.data i, un cop obtingut l’arbre, permet classificar exemples mai vistos per discernir si un bolet és comestible (edible) o verinós (poisonous).

Per executar el programa:
    1) El fitxer dts.hs es compila i es genera l'executable amb la comanda: # ghc dts.hs
    2) Per executar no cal passar-hi cap paràmetre (el programa ja llegeix directament el data set contingut en el fitxer agaricus-lepiota.data)

Utilització:
    1) Primer se'ns pregunta si volem imprimir el decision tree generat (Responem "si" o "no").
    2) Respondre a una serie de preguntes necessàries per tal de navegar a travès del decision tree generat i arribar a una resposta final.

Informació addicional:

    - L'arbre dibuixat està representat de manera que cada nivell te una determinada longitud de linia (en concret el numero de "---+"). Per tant, una linia de l'arbre imprès que conté només un "---+" es l'arrel de l'arbre, el següent nivell ve representat llavors per dos "--+" i així successivament. Cada linia del decision tree ve precedit pel valor de l'atribut que té el seu node pare.

    - La entropia està calculada de la següent forma:
        · Donat un data set D, la entropia de la distribució de probabilitat per un atribut A que té 'na' valors associats (Vai 1 <= i <= na) es definida de la següent manera:
                    E(D, A) = (Sumatori desde i = 1 fins a na)−pi × log2pi

                    d'on pi = Prob(D,A,VAi), Prob(D, A, x) = (num. d'ocurrencies del valor x per a l'atribut A dins de D) / (num. de files de D)

        · El gain per un atribut en particular, donat un data set D, un atribut de classificació C i un atribut de partició P, es calcula de la següent forma:

                    G(D, P, C) = E(D, C) − (Sumatori desde i = 1 fins a np) * Prob(D, P, i) × E(D[P, VP,i], C)
        
        Per a construir el dt s'agafarà el que tingui el màxim information gain, en cas d'empat s'agafa el primer trobat amb aquest màxim.
