
% -----------------------------------------------------------------------
% Trabalho pr�tico: factos de cidades com localiza��o baseada em
% latitude e longitude e predicado auxiliar para calcular a dist�ncia
% entre quaisquer duas destas cidades.
% ------------------------------------------------------------------------

%city(name,latitude,longitude)
%city(brussels,50.8462807,4.3547273).
%city(tirana,41.33165,19.8318).
%city(andorra,42.5075025,1.5218033).
%city(vienna,48.2092062,16.3727778).
%city(minsk,53.905117,27.5611845).
%city(sarajevo,43.85643,18.41342).
%city(sofia,42.6976246,23.3222924).
%city(zagreb,45.8150053,15.9785014).
%city(nicosia,35.167604,33.373621).
%city(prague,50.0878114,14.4204598).
%city(copenhagen,55.6762944,12.5681157).
%city(london,51.5001524,-0.1262362).
%city(tallinn,59.4388619,24.7544715).
%city(helsinki,60.1698791,24.9384078).
city(paris,48.8566667,2.3509871).
%city(marseille,43.296386,5.369954).
%city(tbilisi,41.709981,44.792998).
city(berlin,52.5234051,13.4113999).
%city(athens,37.97918,23.716647).
%city(budapest,47.4984056,19.0407578).
%city(reykjavik,64.135338,-21.89521).
%city(dublin,53.344104,-6.2674937).
city(rome,41.8954656,12.4823243).
%city(pristina,42.672421,21.164539).
%city(riga,56.9465346,24.1048525).
%city(vaduz,47.1410409,9.5214458).
%city(vilnius,54.6893865,25.2800243).
%city(luxembourg,49.815273,6.129583).
%city(skopje,42.003812,21.452246).
%city(valletta,35.904171,14.518907).
%city(chisinau,47.026859,28.841551).
%city(monaco,43.750298,7.412841).
%city(podgorica,42.442575,19.268646).
%city(amsterdam,52.3738007,4.8909347).
%city(belfast,54.5972686,-5.9301088).
%city(oslo,59.9138204,10.7387413).
%city(warsaw,52.2296756,21.0122287).
%city(lisbon,38.7071631,-9.135517).
%city(bucharest,44.430481,26.12298).
%city(moscow,55.755786,37.617633).
%city(san_marino,43.94236,12.457777).
%city(edinburgh,55.9501755,-3.1875359).
%city(belgrade,44.802416,20.465601).
%city(bratislava,48.1483765,17.1073105).
%city(ljubljana,46.0514263,14.5059655).
city(madrid,40.4166909,-3.7003454).
city(stockholm,59.3327881,18.0644881).
city(bern,46.9479986,7.4481481).
city(kiev,50.440951,30.5271814).
city(cardiff,51.4813069,-3.1804979).

%
%  dist_cities(brussels,prague,D).
%  D = 716837.
dist_cities(C1,C2,Dist):-
    city(C1,Lat1,Lon1),
    city(C2,Lat2,Lon2),
    distance(Lat1,Lon1,Lat2,Lon2,Dist).

degrees2radians(Deg,Rad):-
	Rad is Deg*0.0174532925.

% distance(latitude_first_point,longitude_first_point,latitude_second_point,longitude_second_point,distance
% in meters)
distance(Lat1, Lon1, Lat2, Lon2, Dis2):-
	degrees2radians(Lat1,Psi1),
	degrees2radians(Lat2,Psi2),
	DifLat is Lat2-Lat1,
	DifLon is Lon2-Lon1,
	degrees2radians(DifLat,DeltaPsi),
	degrees2radians(DifLon,DeltaLambda),
	A is sin(DeltaPsi/2)*sin(DeltaPsi/2)+ cos(Psi1)*cos(Psi2)*sin(DeltaLambda/2)*sin(DeltaLambda/2),
	C is 2*atan2(sqrt(A),sqrt(1-A)),
	Dis1 is 6371000*C,
	Dis2 is round(Dis1).

% distance(50.8462807,4.3547273,50.0878114,14.4204598,D).
% Online: http://www.movable-type.co.uk/scripts/latlong.html
%





% Predicados
%
% [PROJ2 - EX01] No ficheiro bc_projecto2 comente os factos do tipo city de forma a ficar apenas com 5 cidades. Implemente um predicado tsp1, que seja capaz de determinar o circuito de menor comprimento que dada uma cidade C, de entre um conjunto de outras cidades, permita visitar cada uma das cidades uma �nica vez e voltar � cidade inicial. Na implementa��o do predicado deve seguir uma abordagem for�a bruta (pesquisa exaustiva).


tsp1(Orig, Cam, DT) :-

    findall(C, city(C,_,_), L),
    contar(L, NumAux), Num is NumAux - 1,

    findall((DT,Cam1), (tspAux(Orig, [Orig], Cam1, Num, 0, DT),
           contar(Cam1, Num1), Num2 is Num1 - 1, Num2==NumAux), AllPaths),

    sort(AllPaths, SortedAP),
    SortedAP = [(DT,Cam)|_].

tspAux(Orig, LA, Cam, 0, DA, DT) :-
    LA = [Last|_],

    dist_cities(Last, Orig, DAux),

    DT is DA + DAux,

    CamRev = [Orig|LA],

    reverse(CamRev, Cam).


tspAux(Orig, LA, Cam, Num, DA, DT) :-

    LA = [Act|_],

    city(C,_,_),

    \+ member(C, LA),

    dist_cities(Act, C, DAux),

    DA1 is DA + DAux,

    Num1 is Num - 1,

    tspAux(Orig, [C|LA], Cam, Num1, DA1, DT).


% [PROJ2 - EX02] Identifique qual o n�mero m�ximo de cidades que o predicado anterior tem capacidade para resolver.
%
% [ RESPOSTA ] - 10 cidades.


% [PROJ2 - EX03] Implemente o predicado tsp2, utilizando uma heur�stica greedy, a heur�stica do vizinho mais pr�ximo (ideia base: pr�xima cidade a ser visitada � a mais pr�xima que ainda n�o foi visitada.

% [HEUR�STICA UTILIZADA BestFS Adaptada] - O BestFS sendo um metodo
% baseado no crit�rio local, n�o nos garante resultado �timo, mas por
% outro lado garante-nos uma solu��o rapidamente pois n�o s�o explorados
% multiplas opcoes.

tsp2(Orig, Cam, DT) :-

    findall(C, city(C,_,_), L),
    contar(L, NumAux), Num is NumAux - 1,
    bestfsAux(Orig, [Orig], Cam, DT, 0, Num), !.

bestfsAux(Orig, LA, Cam, DT, DA, 0) :-

    LA = [Last|_],

    dist_cities(Last, Orig, DAux),

    DT is DA + DAux,

    CamRev = [Orig|LA],

    reverse(CamRev, Cam).

bestfsAux(Orig, LA, Cam, DT, DA, Num) :-

    LA = [Act|_],

    % calcular cidades adjacentes e n�o visitadas e
    % guardar tuplo com distancia atual e novo caminho.
    findall( (DistC,[C|LA]),
             (city(C,_,_),  \+ member(C, LA),
                             dist_cities(Act, C, DistC)),
             Novos ),

    %ordenar por dist�ncia
    sort(Novos, NovosOrd),

    % extrair o melhor caminho atual e a sua dist�ncia
    NovosOrd = [(DA1,Melhor)|_],

    % atualizar contadores
    DAux is DA + DA1,
    Num1 is Num - 1,

    % chamada recursiva
    bestfsAux(Orig, Melhor, Cam, DT, DAux, Num1).

% [PROJ2 - EX04] Implemente um predicado tsp3, que com base na solu��o
% encontrada na quest�o 3, implemente uma heur�stica de melhoria da
% solu��o que tem por base o principio da remo��o de cruzamentos.

% verifica se existe cruzamento.
interseccao_cidades(C1, C2, C3, C4) :-
    C1 \== C2,
    C1 \== C3,
    C1 \== C4,
    C2 \== C3,
    C2 \== C4,
    C3 \== C4,
    linearCoord(C1,X1,Y1),
    linearCoord(C2,X2,Y2),
    linearCoord(C3,X3,Y3),
    linearCoord(C4,X4,Y4),
    doIntersect((X1,Y1),(X2,Y2),(X3,Y3),(X4,Y4)).

list_i_j_swapped(As,I,J,Cs) :-
   append(BeforeI,[AtI|PastI],As),
   append(BeforeI,[AtJ|PastI],Bs),
   append(BeforeJ,[AtJ|PastJ],Bs),
   append(BeforeJ,[AtI|PastJ],Cs),
   length(BeforeI,I),
   length(BeforeJ,J).

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

resolverCaminhos(Cam,Q1,Swap,NCam):-
    indexOf(Cam,Q1,IQ1),indexOf(Cam,Swap,ISwap),
    list_i_j_swapped(Cam,IQ1,ISwap,NCam).

tsp3(Orig, NCam, D):-
    tsp2(Orig, Cam,_),
    tsp3Aux(Cam,NCam),
    dist_lista(NCam,D), !.

tsp3Aux(Cam,NCam):-
    testSegmentos(Cam,P1,Q1,P2,Q2),!,
    resolverIntersect(Cam,P1,Q1,P2,Q2,NCam1),
    tsp3Aux(NCam1,NCam).

tsp3Aux(Cam,NCam):-
    NCam = Cam, !.

resolverIntersect(Cam,P1,Q1,P2,Q2,NCam):-
    dist_cities(P1,P2,D1),
    dist_cities(P1,Q2,D2),
    Cam=[Orig|_],
    Q2 \== Orig,
    %dist_cities(Q1,P2,D3),
    %dist_cities(Q1,Q2,D4),
    D1>D2,
    resolverCaminhos(Cam,Q1,Q2,NCam).

resolverIntersect(Cam,_,Q1,P2,_,NCam):-
    resolverCaminhos(Cam,Q1,P2,NCam).


testSegmentos([_|[_|[_|[]]]], _, _ ,_, _):-!,false.

testSegmentos([Act|[Next|Resto]], P1,Q1,P2,Q2):-
    testSegmento(Act,Next,Resto, P1,Q1,P2,Q2),!.

testSegmentos([_|[Next|Resto]],P1,Q1,P2,Q2):-
    testSegmentos([Next|Resto],P1,Q1,P2,Q2),
    !.

testSegmento(_,_,[_|[]],_,_,_,_):-!,false.

testSegmento(P1,Q1,[P2|Resto],C1,C2,C3,C4):-
    Resto=[Q2|_],
    P1 \== Q2,
    interseccao_cidades(P1,Q1,P2,Q2),!,
    C1=P1,
    C2=Q1,
    C3=P2,
    C4=Q2.

testSegmento(P1,Q1,[_|Resto],C1,C2,C3,C4):-
    testSegmento(P1,Q1,Resto,C1,C2,C3,C4),!.

dist_lista([_|[]],0).
dist_lista([Act|Resto], D):-
    Resto = [Next|_],
    dist_lista(Resto,D1),
    dist_cities(Act,Next,D2),
    D is D1+D2,!.

% 5. Implemente o predicado tsp4, recorrendo ao algoritmo simulated annealing.
%

distCam(Cam, DF) :-
    distCam_Aux(Cam, 0, DF).

distCam_Aux([_|[]], Daux, DF) :- DF is Daux, !.
distCam_Aux(Cam, Daux, DF) :-
    Cam = [C1 | Resto],
    Resto = [C2 | _],
    dist_cities(C1, C2, Daux2),
    Daux3 is Daux + Daux2,
    distCam_Aux(Resto, Daux3, DF).

temperature(1).
constant(0.9).
e(2.71828).

acceptanceProbability(Cold,Cnew,T,Ap):- e(E),exp(E,((Cold-Cnew)/T),Ap).

exp(X,Y,R):- R is round(X**Y).

newTemperature(T,NewTemperature):- constant(C), NewTemperature is C*T.


tsp4(Orig,It, CamFinal, DF):-

    tsp3(Orig, Cam, DI),
    temperature(T),
    simAnnealing(Cam, DI, It, T, CamFinal, DF),!.


simAnnealing(Orig,Cold,0,_,CamFinal,DF):-
    CamFinal = Orig,
    DF = Cold,!.

simAnnealing(Orig,Cold,It,T,CamFinal,DF):-
    It2 is It-1,
    newAdjacent(Orig,NewL),
    distCam(NewL,Cnew),
    ((Cnew < Cold),
    newTemperature(T,T1),
    simAnnealing(NewL,Cnew,It2,T1,CamFinal,DF);

    checkMove(Orig,Cold,NewL,Cnew,T,LRes,CRes),
    newTemperature(T,T1),
    simAnnealing(LRes,CRes,It2,T1,CamFinal,DF)),!.


newAdjacent(S1,Sn):-
    length(S1,T1),
    interval(T1,Start,End),
    random_between(Start,End,Pos1),
    random_between(Start,End,Pos2),
    nth1(Pos1,S1,E1),
    nth1(Pos2,S1,E2),
    removeElementPos(Pos1,S1,S2),
    insertElementPos(Pos1,E2,S2,S3),
    removeElementPos(Pos2,S3,S4),
    insertElementPos(Pos2,E1,S4,Sn).

removeElementPos(Pos,List,NewList):-
    nth1(Pos, List, _, NewList),!.
insertElementPos(Pos,Elem,List,NewList):-
    nth1(Pos, NewList, Elem, List),!.

interval(T1,Start,End):-
    Start is 2,
    End is T1 - 1.

checkMove(Orig,Cold,NewLista,Cnew,T,NewL,NewC):-
    random(R),
    acceptanceProbability(Cold,Cnew,T,Ap),
    (((Ap>1; Ap>R), (NewC = Cnew, NewL = NewLista));
    NewL = Orig, NewC = Cold).

% AUXILIARES
%

contar([],0).
contar([_|T],N):- contar(T,N1), N is N1+1.

% Given three colinear points p, q, r, the function checks if
% point q lies on line segment 'pr'
%onSegment(P, Q, R)
onSegment((PX,PY), (QX,QY), (RX,RY)):-
    QX =< max(PX,RX),
    QX >= min(PX,RX),
    QY =< max(PY,RY),
    QY >= min(PY,RY).


% To find orientation of ordered triplet (p, q, r).
% The function returns following values
% 0 --> p, q and r are colinear
% 1 --> Clockwise
% 2 --> Counterclockwise

orientation((PX,PY), (QX,QY), (RX,RY), Orientation):-
	Val is (QY - PY) * (RX - QX) - (QX - PX) * (RY - QY),

	(
		Val == 0, !, Orientation is 0;
		Val >0, !, Orientation is 1;
		Orientation is 2
	).

orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4):-
    orientation(P1, Q1, P2,O1),
    orientation(P1, Q1, Q2,O2),
    orientation(P2, Q2, P1,O3),
    orientation(P2, Q2, Q1,O4).

% The main function that returns true if line segment 'p1q1'
% and 'p2q2' intersect.
doIntersect(P1,Q1,P2,Q2):-
    % Find the four orientations needed for general and
    % special cases
	orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4),

	(
    % General case
    O1 \== O2 , O3 \== O4,!;

    % Special Cases
    % p1, q1 and p2 are colinear and p2 lies on segment p1q1
    O1 == 0, onSegment(P1, P2, Q1),!;

    % p1, q1 and p2 are colinear and q2 lies on segment p1q1
    O2 == 0, onSegment(P1, Q2, Q1),!;

    % p2, q2 and p1 are colinear and p1 lies on segment p2q2
    O3 == 0, onSegment(P2, P1, Q2),!;

     % p2, q2 and q1 are colinear and q1 lies on segment p2q2
    O4 == 0, onSegment(P2, Q1, Q2),!
    ).

linearCoord(City,X,Y):-
    city(City,Lat,Lon),
    geo2linear(Lat,Lon,X,Y).
geo2linear(Lat,Lon,X,Y):-
    degrees2radians(Lat,LatR),
    degrees2radians(Lon,LonR),
    X is round(6371*cos(LatR)*cos(LonR)),
    Y is round(6371*cos(LatR)*sin(LonR)).
