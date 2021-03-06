0 'EIGHT.BA - BY S. BURGOYNE
1 'CLUB 100 LIBRARY - 415/939-1246 BBS,    937-5039 NEWSLETTER, 932-8856 VOICE
2 ' Change line 40 to your choice of default function keys
3 ' Jeu realise par Stephane Burgoyne
4 '   Montreal TRS-80 M-100 BBS
5 '
6 'Voici comment jouer:
7 '  vous jouez contre l'ordinateur
8 '  vous possedez au depart huit cartes choisies au hazard
9 '  vous devez vous debarrasser de vos cartes de la facon suivante:
10 '    1- soit en jouant une carte de meme valeur que la precedente
11 '    2- soit en mettant une carte de meme couleur que la precedente
12 '    3- soit en jouant un huit de n'importe quelle couleur
13 '  vous etes oblige de jouer une carte repondant a une des 3 conditions
14 '  sinon, vous pigez une(des) carte(s) [F4] jusqu'a ce que vous pouvez jouer
15 '  les valets font perdre un tour a l'adversaire
16 '  les deux font piger deux cartes a l'adversaire
17 '  la partie est terminee lorsqu'un des joueurs n'a plus de cartes
18 '  NOTES:  [CAPS LOCK] doit etre presse
19 '          la derniere carte jouee est dans le coin inferieur gauche
20 ' 
21 ' EXEMPLE DE JEU:
22 ' vous avez en main: -AS de PIQUE
23 '			-3  de COEUR
24 '			-7  de TREFLE
25 '			-8  de CARREAU
26 '
27 ' la derniere carte jouee est:
28 '			-7  de PIQUE
29 ' vous devez donc jouer l'AS ou le 7
30 '     ou
31 ' jouer le huit et changer la couleur en COEUR, TREFLE ou CARREAU
32 '
33 'pour jouer l'AS de PIQUE:
34 '   PRESSEZ  [A] et [F8]
35 '
36 'les points sont calcules selon la valeur des cartes qui reste dans les 
37 'mains de chaque joueur
38 '
39 CLEAR400:DIMJO$(35),MO$(35),D$(52),U$(4),C$(13):FORI=1TO4:READA:U$(I)=CHR$(A):NEXT:FORI=1TO13:READC$(I):NEXT:Z$="A2345678910JQK":DATA 156,157,158,159,"A","2","3","4","5","6","7","8","9","10","J","Q","K"
40 CALL16959:CLS:KEY2,"END"+CHR$(13):KEY3,"":KEY4,"DRAW"+CHR$(13):KEY5,CHR$(156)+CHR$(13):KEY6,CHR$(157)+CHR$(13):KEY7,CHR$(158)+CHR$(13):KEY8,CHR$(159)+CHR$(13):FORJ=0TO12:FORI=1TO4:D$(I+J*4)=C$(J+1)+U$(I):NEXTI:NEXTJ
41 FORK=1TO((VAL(RIGHT$(TIME$,2)))*VAL(MID$(DATE$,4,2))/3):I=RND(1):NEXT:JO=8:MO=8:B=1:D=53:FORI=1TO8:GOSUB57:JO$(I)=X$:NEXTI:FORI=9TO35:JO$(I)="":NEXT:FORI=1TO8:GOSUB57:MO$(I)=X$:NEXTI:FORI=9TO35:MO$(I)="":NEXT:GOSUB57:C$=X$:CLS
42 PRINT"The first card is: ";C$:KEY1," "+C$+STRING$(3,8):SCREEN0,1
43 IF(JO=0)OR(MO=0)OR(B=0)THEN45ELSEGOSUB61:PRINT
44 IF(JO<>0)AND(MO<>0)THENGOSUB79:PRINT:GOTO43
45 C1=0:C2=0
46 IFMO=0THEN49ELSECLS:PRINT"I have in hand :";
47 FORI=1TOMO:PRINTMO$(I);" ";:GOSUB58:IFZ>10THENZ=10
48 C1=C1+Z:NEXT:PRINT::CLS
49 IFJO<>0THENPRINT"You have in hand:";:FORI=1TOJO:PRINTJO$(I);" ";:GOSUB51:C2=C2+Z:NEXT:PRINT
50 GOTO52
51 Z=INSTR(1,Z$,LEFT$(JO$(I),1)):IFZ>10THENZ=10:RETURNELSERETURN
52 IFC1=0THEN53ELSEIFC1>=C2THENC1=C1-C2:C2=0:GOTO53ELSEC2=C2-C1:C1=0
53 S1=S1+C1:S2=S2+C2:PRINT"Your points :";C1:PRINT"My points:";C2:IFC2<C1THENPRINT"You have won":N1=N1+1:GOTO55
54 IFC1<C2THENPRINT"I have won"ELSEPRINT"Round over"
55 PRINT:N3=N3+1:KEY2,"":KEY3,"YES"+CHR$(13):KEY4,"NO"+CHR$(13):INPUT"Play again(YES or NO)";RP$:IFINSTR(RP$,"Y")THEN40
56 CALL16964:PRINT:PRINT"In ";N3;" rounds you have won ";N1;".":PRINT"Your total points are ";S1;" mine are ";S2:CALL23164,0,23366:CALL27795:SCREEN0,0:CLEAR:END:CALL17001:PRINT"Press <spacebar> to continue";:CALL17005:CALL24367:RETURN
57 D=D-1:X=INT(D*RND(1)+1):X$=D$(X):D$(X)=D$(D):D$(D)="":RETURN
58 Z=INSTR(1,Z$,LEFT$(MO$(I),1)):RETURN
59 FORW=1TO750:NEXT:RETURN
60 PRINT@0,STRING$(80,32):PRINT@240,STRING$(80,32):RETURN
61 PRINT@80,STRING$(160,32):PRINT@80,"You have in hand: ";:FORI=1TOJO:PRINTJO$(I);" ";:NEXT:PRINT:IFD<1THENKEY4,"NO"+CHR$(13):INPUT"Can you play ";RP$:GOTO63
62 LINEINPUT"Your turn to play: ";RP$
63 GOSUB60:IFRP$="DRAW"ORRP$="NO"THEN64ELSEIFRP$="END"THEN46ELSE68
64 IFD>1THEN67
65 IFD=1THENPRINT@240,STRING$(4,255)+" There are no more cards "+STRING$(4,255):D=0:GOTO61
66 PRINT@240,STRING$(4,255)+" You are blocked "+STRING$(4,255):B=0:GOSUB59:RETURN
67 JO=JO+1:GOSUB57:JO$(JO)=X$:GOTO61
68 IF((LEFT$(RP$,1)=LEFT$(C$,1))OR(RIGHT$(RP$,1)=RIGHT$(C$,1)))OR(LEFT$(RP$,1)="8")THEN70
69 PRINT@240,STRING$(4,255)+" Illegal "+STRING$(4,255):GOTO61
70 FORI=1TOJO:IFRP$=JO$(I)THEN72
71 NEXT:PRINT@240,STRING$(3,255)+" You don't have that card"+STRING$(4,255):GOTO61
72 C$=RP$:KEY1," "+C$+STRING$(4,8):FORJ=ITOJO+1:JO$(J)=JO$(J+1):NEXT:JO=JO-1:Z7$=LEFT$(RP$,1):IFJO=0THENRETURNELSEIFZ7$="8"THEN73ELSEIFZ7$="J"THEN75ELSEIFZ7$="2"THEN76ELSERETURN
73 INPUT"******* What suit?";S9$:FORI=1TO4:IFS9$=U$(I)THEN:C$=S9$:KEY1," "+C$+STRING$(2,8):RETURN
74 NEXTI:PRINT@200,STRING$(7,255)+" ";S9$;" isn't a suit "+STRING$(3,255):CALL16979:GOTO73
75 PRINT@240,STRING$(7,255)+" I lose my turn":GOSUB59:GOSUB60:GOTO61
76 IFD=0THENRETURNELSEPRINT@240,STRING$(7,255)+" I draw two cards":GOSUB59:GOSUB60
77 FORI=1TO2:IFD>1THENMO=MO+1:GOSUB57:MO$(MO)=X$:NEXT:RETURN
78 PRINT@240,"There are no more cards":D=0:GOSUB59:GOSUB60:RETURN
79 IFJO<4THEN92
80 FORI=1TOMO:IF((RIGHT$(MO$(I),1)=RIGHT$(C$,1))AND(LEFT$(MO$(I),1)<>"8"))THEN85ELSENEXT
81 FORI=1TOMO:IF(LEFT$(MO$(I),1)=LEFT$(C$,1))THEN85ELSENEXT
82 FORI=1TOMO:IF(LEFT$(MO$(I),1)="8")THEN85ELSENEXT
83 IFD>1THEN84ELSEPRINT"I cannot play a card.  I am blocked":D=0:GOSUB59:GOSUB60:RETURN
84 MO=MO+1:I=MO:GOSUB57:MO$(MO)=X$:GOTO79
85 B=1:C$=MO$(I):MO$(I)="":FORJ=ITOMO+1:MO$(J)=MO$(J+1):NEXT:MO=MO-1:CLS:PRINT"I have ";MO+1;" cards";:IF(LEFT$(C$,1)<>"8")THENPRINT" and I play :";C$:KEY1," "+C$+STRING$(4,8)ELSE87
86 IF(LEFT$(C$,1)="J")THEN93ELSEIF(LEFT$(C$,1)="2")THEN95ELSERETURN
87 FORI=1TO4:B(I)=0:NEXT:FORI=1TOMO:IF RIGHT$(MO$(I),1)=CHR$(156)THENB(1)=B(1)+1:NEXT:GOTO90
88 IFRIGHT$(MO$(I),1)=CHR$(157)THENB(2)=B(2)+1:NEXT:GOTO90
89 IFRIGHT$(MO$(I),1)=CHR$(158)THENB(3)=B(3)+1:NEXTELSEB(4)=B(4)+1:NEXT
90 E=1:FORI=1TO3:IFB(1)<B(I+1)THENE=I+1:B(1)=B(I+1):NEXTELSENEXT
91 PRINT" and I play :";C$:PRINT"The new suit is: ";U$(E):KEY1," "+U$(E)+STRING$(2,8):C$="8"+U$(E):RETURN
92 CL$=LEFT$(C$,1):CR$=RIGHT$(C$,1):FORI=1TOMO:ML$=LEFT$(MO$(I),1):MR$=RIGHT$(MO$(I),1):IF((MR$=CR$)AND(ML$="2"ORML$="J"))OR((ML$="2"ORML$="J")AND(ML$=CL$))THEN85ELSENEXT:GOTO80
93 IFMO=0THENRETURNELSEIFLEFT$(C$,1)="2"THEN95
94 PRINT@240,STRING$(3,255)+" You lose your turn "+STRING$(3,255):GOSUB59:GOTO79
95 IFMO=0ORD=0THENRETURNELSEPRINT@240,STRING$(7,255)+" You draw two cards":GOSUB59:GOSUB60
96 FORI=1TO2:IFD>1THENJO=JO+1:GOSUB57:JO$(JO)=X$:NEXT:RETURN
97 PRINT@240,"There are no more cards":D=0:RETURN
