0 REM CHANGE 1/10/85
1 CLEAR500:DEFINTA-Y:DIMF$(26),F(26),Z(2):CLS:SCREEN,0:POKE64173,0:CALL23161:E$=CHR$(27):PRINTE$"V"E$"Q";:ONERRORGOTO62:GOSUB12
2 A$="":FORI=148TO155:A$=A$+CHR$(I):NEXT:I=VARPTR(A$):CALL23164,0,PEEK(I+1)+256*PEEK(I+2):R$=CHR$(0)+CHR$(8)+CHR$(13):FORI=20TO32:R$=R$+CHR$(I):NEXT:P=0:L=0:H=0:G=1:K=-1:E=K
3 PRINT@L*40+H*10,E$"p "F$(P);E$"q"
4 ONG+1GOSUB17,19,22,28:A$=INKEY$:IFA$=""THEN4ELSEONINSTR(R$,A$)GOSUB16,6,65,30,41,51,54,55,57,58,60,5,6,8,10,5:GOTO3
5 P=(P+1)MOD(N+1):GOTO11
6 P=P-1:IFP<0THENP=N
7 GOTO11
8 IFP>3THENP=P-4ELSERETURN
9 GOTO11
10 IFP+3<NTHENP=P+4ELSERETURN
11 K=-1:PRINT@L*40+H*10," "F$(L*4+H):L=P\4:H=P-L*4:RETURN
12 P=0:L=0:H=0:FORF=1TO5:READG:FORI=-1694TO-1408STEP11:IF(PEEK(I)AND247)=GTHENN$="":FORJ=3TO10:N$=N$+CHR$(PEEK(I+J)):NEXT:F$(P)=N$:N$=RIGHT$(N$,2):GOSUB14:F(P)=I:P=P+1:GOSUB11
13 NEXT:NEXT:N=P-1:RETURN:DATA176,240,192,128,160
14 IFN$="  "ORN$=" "+CHR$(0)THENA$=" "ELSEA$="."
15 F$(P)=LEFT$(F$(P),6)+A$+N$:RETURN
16 K=-1:E=K:G=(G+1)MOD4:RETURN
17 IFETHENE=0:PRINT@308,E$"l"FRE(0)"Free"
18 PRINT@280,;:CALL23064:RETURN
19 IFETHENE=0:PRINT@280,">Hex >Bin >CA  >CO  Name Kill Invs Menu "
20 IFKTHENK=0:PRINT@310,;:IFPEEK(F(P))AND8THENPRINTE$"pInvs"E$"q"ELSEPRINT"Invs"
21 RETURN
22 IFNOTKTHENRETURNELSEK=0:PRINT@280,E$"K":F=F(P):I=PEEK(F)AND247
23 IFI=240THENPRINT"Alt ROM";ELSEZ=PEEK(F+1)+256*PEEK(F+2):IFI=176THENPRINT"ROM";ELSEIFI=160THENGOSUB53:Z1=Z(1)+6:IFZ(0)=65535ANDZ(2)=63012THENPRINT"CA";:Z(1)=Z(1)-65536ELSEPRINT"CO";ELSEGOSUB26:IFI=128THENPRINT"BA";ELSEPRINT"DO";
24 PRINT" ";:IFI<>240THENPRINT"@"Z;:IFI<>176THENPRINT"+"Z1;:IFI=160THENPRINTSTR$(Z(0))","MID$(STR$(Z(0)+Z(1)-1),2)","MID$(STR$(Z(2)),2)
25 RETURN
26 Z2=PEEK(64434)+256*PEEK(64435):FORJ=0TON:Z3=PEEK(F(J)+1)+256*PEEK(F(J)+2):IFZ3<Z2ANDZ3>ZTHENZ2=Z3
27 NEXT:Z1=Z2-Z:RETURN
28 IFETHENE=0:PRINT@280,"Free"FRE(0)"  HIMEM"HIMEM"  MAXRAM"MAXRAM;E$"K"
29 RETURN
30 F$=F$(P):F=F(P):GOSUB53:IFQANDZ(0)+Z(1)>MAXRAMTHEN61ELSEIFQTHENPOKE-186,PEEK(F+1):POKE-185,PEEK(F+2):Z1=Z(0):GOSUB38ELSEZ1=0:F$="":GOTO32
31 CLEAR500,Z1:ONERRORGOTO62:GOSUB39:LOADMF$:ONERRORGOTO62:GOSUB39:Z=PEEK(-186)+256*PEEK(-185):Z1=HIMEM:Z2=PEEK(Z+2)+256*PEEK(Z+3)+Z1-1:Z3=PEEK(Z+4)+256*PEEK(Z+5)
32 GOSUB37:CLS:PRINTCHR$(27)"W":LINEINPUT"Hex output file:";A$:IFA$=""THENRUNELSEOPENA$FOROUTPUTAS1
33 IFZ1=0THENZ1=HIMEM:PRINT"Top ("Z1") ";:INPUTZ1:Z2=MAXRAM-1:PRINT"End ("Z2") ";:INPUTZ2:Z3=Z1:PRINT"Exe ("Z3") ";:INPUTZ3
34 PRINT#1,Z1;Z2;Z3:ZC=Z1+Z2+Z3:J=0:PRINT"Working...":FORZ=Z1TOZ2:A=PEEK(Z):ZC=ZC+A:X=A\16:Y=AAND15:PRINT#1,MID$(H$,X+1,1)MID$(H$,Y+1,1);:J=J+1:IFJ=39THENJ=0:PRINT#1,
35 NEXT:IFJ<>0THENPRINT#1,
36 PRINT#1,ZC:PRINT#1,CHR$(26);:CLOSE:BEEP:IFF$=""THENRUNELSECLEAR500,MAXRAM:RUN
37 DEFINTA-Y:H$="0123456789ABCDEF":RETURN
38 F$=F$+CHR$(0):FORJ=1TOLEN(F$):POKEJ-185,ASC(MID$(F$,J)):NEXT:RETURN
39 F$="":FORJ=1TO9:A=PEEK(J-185):IFA=0THENJ=9ELSEF$=F$+CHR$(A)
40 NEXT:RETURN
41 F$=F$(P):CLS:PRINTE$"W":IF(PEEK(F(P))AND247)=192THEN43
42 LINEINPUT"Hex input file:";F$:IFF$=""THENRUNELSEIFINSTR(F$,":")=0ANDINSTR(F$,".")=0THENF$=F$+".DO"
43 GOSUB37:OPENF$FORINPUTAS1:INPUT#1,Z1,Z2,Z3:IFZ2>=MAXRAMTHENERROR5ELSEIFZ1>=HIMEMTHEN45ELSECLOSE:GOSUB38:CLEAR500,Z1:ONERRORGOTO62:GOSUB39
44 IFINSTR(F$,":")THENBEEP:PRINT"HIMEM reset... rewind input file,":PRINT"and press ENTER to continue:";:POKE-86,0:A$=INPUT$(1):PRINT:IFA$=CHR$(13)THEN43ELSERUNELSE43
45 LINEINPUT".CO output file:";B$:N=0:ZC=Z1+Z2+Z3:PRINT"Working...":FORZ=Z1TOZ2
46 A$=INPUT$(1,1):I=INSTR(H$,A$)-1:IFI<0THEN46
47 N=NOTN:IFNTHENA=I:GOTO46ELSEA=16*A+I:ZC=ZC+A:POKEZ,A:NEXT:LINEINPUT#1,A$:INPUT#1,Z:CLOSE:IFZ<>ZCTHENGOSUB61:PRINT"Checksum is wrong.":GOTO64
48 BEEP:IFINSTR(F$,":")=0THENPRINT"Kill hex input file? ";:A$=INPUT$(1):PRINTA$:IFA$="Y"ORA$="y"THENKILLF$
49 IFB$=""THENRUNELSEA$="clear500,maxram:run":GOSUB50:SAVEMB$,Z1,Z2,Z3
50 A$=A$+CHR$(13):FORI=1TOLEN(A$):POKE2*I-87,ASC(MID$(A$,I)):POKE2*I-86,0:NEXT:POKE-86,I-1:RETURN
51 F=F(P):GOSUB53:IFNOTQORZ(2)<>0ORZ(0)+Z(1)<>MAXRAMTHEN61ELSEPOKEZ,255:POKEZ+1,255:POKEZ+4,36:POKEZ+5,246:N$="CA"
52 K=-1:POKEF+9,ASC(N$):POKEF+10,ASC(MID$(N$,2)):GOTO14
53 Q=0:IF(PEEK(F)AND247)<>160THENRETURNELSEZ=PEEK(F+1)+256*PEEK(F+2):FORJ=0TO2:Z(J)=PEEK(Z+2*J)+256*PEEK(Z+2*J+1):NEXT:Q=-1:RETURN
54 F=F(P):GOSUB53:IFNOTQORZ(0)<>65535ORZ(2)<>63012THEN61ELSEPOKEZ+4,0:POKEZ+5,0:Z(0)=MAXRAM-Z(1):Y=FIX(Z(0)/256):X=Z(0)-256*Y:POKEZ,X:POKEZ+1,Y:N$="CO":GOTO52
55 K=-1:E=K:F=F(P):PRINT@280,"New name:"E$"K";:LINEINPUTN$:IFN$>""THENMID$(F$(P),1,6)=LEFT$(N$+SPACE$(6),6):FORJ=1TO6:POKEF+J+2,ASC(MID$(F$(P),J)):NEXT
56 PRINT"New extension:"E$"K";:LINEINPUTN$:IFN$>""THENN$=LEFT$(N$+SPACE$(2),2):GOTO52ELSERETURN
57 F=F(P):I=PEEK(F)AND247:IFI=176ORI=240ORF>-1640ANDF<-1606THEN61ELSEK=-1:E=K:PRINT@280,"Kill? "E$"K";:B$=INPUT$(1):PRINTB$:IFB$<>"Y"ANDB$<>"y"THENRETURNELSEA$="run":GOSUB50:KILLF$(P):END
58 K=-1:F=F(P):I=PEEK(F):IFI=240THENPOKEF,0:RUNELSEIFIAND8THENPOKEF,IAND247ELSEPOKEF,IOR8
59 RETURN
60 MENU
61 SOUND9394,8:RETURN
62 GOSUB61:CLS:PRINT:IFERR<23THENI=ERRELSEIFERR<50THENI=21ELSEIFERR<59THENI=ERR-27ELSEI=21
63 I=794+2*I:PRINT"?"CHR$(PEEK(I));CHR$(PEEK(I+1))" Error #"ERR"in line"ERL
64 PRINTCHR$(27)"W":PRINT"Press ENTER to continue:";:POKE-86,0:IFINPUT$(1)=CHR$(13)THENRUNELSEPOKE64173,1:CALL27804:END
65 F$=F$(P):F=F(P):I=PEEK(F)AND247:IFI=160THENGOSUB53:IFZ(2)=0THEN61ELSEIFZ(0)<>65535ANDZ(0)+Z(1)>MAXRAMTHEN61
66 CLS:PRINTE$"W";:POKE64173,1:Z=F+65536:Y=FIX(Z/256):X=Z-256*Y:POKE-589,X:POKE-588,Y:IFI=240THENPOKE-530,0ELSEPOKE-530,9
67 IFI=160ANDZ(0)<HIMEMTHENCLEAR0,Z(0)ELSECLEAR0
68 Z=PEEK(-589)+256*PEEK(-588):CALL22848,PEEK(Z)AND247,Z
