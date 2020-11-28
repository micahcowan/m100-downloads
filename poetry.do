10 REM Inspired by Jay Chidsey, Bit Smitten Part V, 80 Micro, December 1982 (#35) p. 101
20 REM and Margaret Chisman, Producing Computer Poetry, The Best of Creative Computing Volume 2 p. 106
30 REM SETUP
40 CLS:CLEAR 1500
50 R1 = VAL(RIGHT$(TIME$, 1)) 'Tens digit for random seed
60 VOWEL$ = "aeiou"
70 CR$ = CHR$(13) + CHR$(10):PR$ = "." + CR$
80 IF PEEK(1) = 51 THEN PC$ ="TRS-80 Model 100" ELSE IF PEEK(1) = 171 THEN PC$ = "Tandy 200" ELSE PC$ = "Tandy 102"

100 REM Count words
110 READ N$:N=N+1:IF N$<>"end" THEN 110
120 READ F$:F=F+1:IF F$<>"end" THEN 120
130 READ V$:V=V+1:IF V$<>"end" THEN 130
140 READ A$:A=A+1:IF A$<>"end" THEN 140
150 N=N-1:F=F-1:V=V-1:A=A-1 ' sets n,v,a to actual count
160 N$="":F$="":V$="":A$="" ' sets strings to blank
170 DIM N$(N),F$(N),V$(V),A$(A)
180 RESTORE ' data now ready to be reread for content

200 REM words
210 TI$ = "The " + PC$ + " Poet":GOSUB 6500
220 PRINT "Nouns.........."N;" ";
230 PRINT "Feelings......."F
240 PRINT "Verbs.........."V;" ";
250 PRINT "Adjectives....."A
260 IF A$ = "w" THEN 310
270 FOR I=1 TO N:READ N$(I):NEXT:READ EN$ 'peels off 'end'
280 FOR I=1 TO F:READ F$(I):NEXT:READ EN$
290 FOR I=1 TO V:READ V$(I):NEXT:READ EN$
300 FOR I=1 TO A:READ A$(I):NEXT
310 REM start
320 PRINT:PRINT "Press space to wax poetic."
330 GOSUB 6200
340 REM seed randomizer
350 CLS:PRINT "Cogitating..."
360 R2 = VAL(RIGHT$(TIME$, 1)) 'Ones digit for random seed
370 SD = R1*10+R2
380 GOSUB 6630

499 REM loop through random (or not) poetry formats
500 REM makepoetry
510 CLS
520 IF A$="w" THEN 200
530 IF FO = 0 THEN FO = INT(RND(1)*8)+1
540 ON FO GOSUB 1000,1200,1300,1400,1500,1600,1700,1800
550 PRINT PO$
560 FO = 0
570 GOSUB 6600
580 GOTO 500
590 END

999 REM poetry format subroutines
1000 REM nouns and nouns
1010 GOSUB 7000:N1$ = NP$:GOSUB 7100:V1$ = VT$
1020 GOSUB 7000:N2$ = NP$:GOSUB 7100:V2$ = VT$
1030 TI$ = N1$ + " and " + N2$:GOSUB 6100
1040 ST$ = N1$:GOSUB 6300:PO$ = PO$ + V1$ + PR$
1050 ST$ = N2$:GOSUB 6300:PO$ = PO$ + V2$ + PR$
1060 GOSUB 7000:N1$ = NP$:GOSUB 7000:N2$ = NP$:GOSUB 7100
1070 ST$ = N1$:GOSUB 6300:PO$ = PO$ + VT$ + " with " + N2$ + PR$
1080 GOSUB 7100:GOSUB 7300
1090 ST$ = VT$:GOSUB 6300:PO$ = PO$ + AV$ + "!" + CR$
1100 RETURN

1200 REM nouns seldom
1210 GOSUB 6000
1220 TI$ = "The " + NS$:GOSUB 6100
1230 PO$ = PO$ + "How " + AV$ + " " + VI$ + " the " + NS$ + "!" + CR$
1240 PO$ = PO$ + " " + NP$ + " seldom " + VT$ + PR$
1250 GOSUB 6000
1260 PO$ = PO$ + "More " + AJ$ + " are the " + NP$ + "," + CR$
1270 PO$ = PO$ + " as they " + VT$ + PR$
1280 RETURN

1300 REM adjective was the noun
1310 GOSUB 6000:TI$ = AJ$ + " was the " + NS$:GOSUB 6100
1320 ST$ = AJ$:GOSUB 6300:PO$ = PO$ + "was the " + NS$ + "," + CR$
1330 GOSUB 7000:GOSUB 7300
1340 ST$ = AJ$:GOSUB 6300:PO$ = PO$ + "the " + NS$ + PR$
1350 GOSUB 6000
1360 PO$ = PO$ + "The " + NS$ + " " + VI$ + " " + AV$ + PR$
1370 RETURN

1400 REM the noun verbs like a noun
1410 GOSUB 6000:N1$ = NS$:V1$ = VI$:GOSUB 7000:N2$ = NS$:GOSUB 7100:V2$ = VI$
1420 TI$ = "Like a " + N2$:GOSUB 6100
1430 PO$ = PO$ + "The " + N1$ + " " + V1$ + " like a " + N2$ + CR$
1440 PO$ = PO$ + "And " + AV$ + " " + V2$ + CR$
1450 GOSUB 6000
1460 PO$ = PO$ + "While " + AJ$ + " " + NP$ + " " + VT$ + PR$
1470 RETURN

1500 REM nouns verb like nouns
1510 GOSUB 6000:N1$ = NP$:GOSUB 7000:N2$ = NP$
1520 TI$ = N1$ + " " + VT$:GOSUB 6100
1530 ST$ = N1$:GOSUB 6300:PO$ = PO$ + VT$ + " like " + N2$ + CR$
1540 GOSUB 7300:PO$ = PO$ + " " + VT$ + " like " + AJ$ + " " + N2$ + PR$
1550 GOSUB 7300:ST$ = N1$:GOSUB 6300:PO$ = PO$ + VT$ + " " + AV$ + CR$
1560 GOSUB 6000:PO$ = PO$ + " and " + NP$ + " " + VT$ + " " + AV$ + PR$
1570 RETURN

1600 REM God verbs
1610 GOSUB 7100:V1$ = VI$:GOSUB 7100:V2$ = VI$
1620 TI$ = "God " + V1$:GOSUB 6100
1630 PO$ = PO$ + "God " + V1$ + PR$ + "Man " + V2$ + PR$
1640 GOSUB 7000:GOSUB 7100:N1$ = NP$:V1$ = VT$:GOSUB 7000:GOSUB 7100:N2$ = NP$:V2$ = VT$
1650 ST$ = N1$:GOSUB 6300:PO$ = PO$ + V1$ + " with " + N2$ + CR$
1660 PO$ = PO$ + " and " + V2$ + PR$
1670 RETURN

1700 REM verb, noun, verb
1710 GOSUB 6000:A1$ = AV$:GOSUB 7300:A2$ = AV$
1720 TI$ = VT$ + ", " + NS$ + ", " + VT$:GOSUB 6100
1730 ST$ = VT$ + ",":GOSUB 6300:PO$ = PO$ + NS$ + ", " + VT$ + CR$
1740 PO$ = PO$ + " " + VT$ + " " + A1$ + CR$ + " " + VT$ + " " + A2$ + CR$
1750 GOSUB 7300:GOSUB 7000:PO$ = PO$ + " like":W$ = AJ$:GOSUB 6700:PO$ = PO$ + " " + NS$ + ", " + VT$ + PR$
1760 RETURN

1800 REM My NOUN VERBS with FEELINGS
1810 GOSUB 7000:GOSUB 7100:GOSUB 7400
1820 TI$ = "My " + NS$ + " " + VI$ + " with " + F$:GOSUB 6100
1830 PO$ = PO$ + "My " + NS$ + " " + VI$ + " with " + F$ + CR$
1840 GOSUB 7100:GOSUB 7300:ST$ = VG$:GOSUB 6300:PO$ = PO$ + AV$ + CR$
1850 GOSUB 7100:V1$ = VG$:GOSUB 7100:V2$ = VG$:PO$ = PO$ + "Always " + V1$ + " never " + V2$ + PR$
1860 RETURN

1999 REM nouns
2000 DATA barn, book, boy, brook, castle
2010 DATA cat, cloud, comet, cottage, crow
2020 DATA dog, flower, forest, garden
2030 DATA gazelle, hare, horse, lion
2040 DATA manor, mask, moon, mother
2050 DATA mountain, needle, path, pen
2060 DATA planet, plum, poet, rain, ring
2070 DATA river, snow, soldier, spirit
2080 DATA star, stone, thunder, tomb, tree
2090 DATA village, villager, wildflower
2100 DATA end

2999 REM feelings
3000 DATA despair, dread, enchantment
3010 DATA grief, guilt, hate, joy, love
3020 DATA pain, passion, pride, remorse
3030 DATA sadness, sorrow
3040 DATA end

3999 REM verbs
4000 DATA brood, burn, chant, climb, dance
4010 DATA despair, fall, fly, gloom, glow
4020 DATA jump, laugh, leap, live, moan
4030 DATA pierce, play, pray, reflect
4040 DATA ring, ripple, roll, run, see
4050 DATA seek, shout, sin, sing, sink
4060 DATA soar, sparkle, speak, stand
4070 DATA think, throw, wake, walk, write
4080 DATA end

4999 REM adjectives
5000 DATA beautiful, bright, cold
5010 DATA colorful, dark, deep, earnest
5020 DATA forgetful, good, grand, hollow
5030 DATA indecent, light, lone, loud, mad
5040 DATA mortal, painful, peaceful, proud
5050 DATA purple, quick, quiet, regal
5060 DATA regretful, rough, shy, sinful
5070 DATA slow, sorrowful, stern, strange
5080 DATA sweet, wise
5090 DATA end

5999 REM utility subroutines
6000 REM random noun, verb, and adjective
6010 GOSUB 7000
6020 GOSUB 7100
6030 GOSUB 7300
6040 RETURN

6100 REM create title from TI$
6110 TL = LEN(TI$)
6120 TS = 20-LEN(TI$)/2:PO$ = SPACE$(TS)
6130 FOR I = 1 TO TL
6140 	TC$ = MID$(TI$, I, 1)
6150 	IF ASC(TC$) > 96 THEN TC$ = CHR$(ASC(TC$)-32)
6160 	PO$ = PO$ + TC$
6170 NEXT I
6180 PO$ = PO$ + CR$ + CR$
6190 RETURN

6200 REM wait for keypress
6210 A$=INKEY$:IF A$="" THEN 6200
6220 IF A$ = "q" THEN PRINT "Done poeticizing.":MENU 'Quit
6230 IF ASC(A$) = 27 THEN PRINT "Done poeticizing.":END 'ESCape
6240 IF ASC(A$) = 0 THEN 6200 'PRINT key, et. al.
6250 IF A$ = "s" THEN GOSUB 6400:GOTO 6200 'SAVE poem
6260 IF A$ = "p" THEN LCOPY:GOTO 6200 'PRINT poem
6270 IF ASC(A$) >= ASC("1") AND ASC(A$) <= ASC("9") THEN FO = VAL(A$)
6280 RETURN

6300 REM capitalize st$ and add to poem
6310 SC$ = CHR$(ASC(LEFT$(ST$, 1))-32)
6320 SC$ = SC$ + MID$(ST$, 2, LEN(ST$))
6330 PO$ = PO$ + SC$ + " "
6340 RETURN

6400 REM save poem to poems.do
6410 IF PO$ = "" THEN PRINT@240, "No Poem":RETURN
6420 OPEN "RAM:poems.do" FOR APPEND AS 1
6430 PRINT #1, PO$;
6440 PRINT #1, ""
6450 CLOSE 1
6460 PRINT@280, "Saved";
6470 RETURN

6500 REM print a centered title
6510 GOSUB 6100
6520 PRINT PO$
6530 RETURN

6600 REM wait for keypress and then do a little randomizing
6610 GOSUB 6200
6620 SD = VAL(RIGHT$(TIME$, 1)) 'randomize a bit for the next poem
6630 REM seedcheck
6640 IF SD < 1 THEN RETURN
6650 S = RND(1)
6660 SD = SD - 1
6670 GOTO 6630

6700 REM A or AN for W$
6710 IF INSTR(VOWEL$, LEFT$(W$, 1)) <> 0 THEN AR$ = "an" ELSE AR$ = "a"
6720 PO$ = PO$ + " " + AR$ + " " + W$
6730 RETURN

6999 REM random parts of speech
7000 REM randomnoun
7010 I = INT(RND(1)*N)+1
7020 NS$ = N$(I)
7030 NP$ = NS$ + "s"
7040 RETURN

7100 REM randomverb
7110 I = INT(RND(1)*V)+1
7120 REM I/you/we/they
7130 VT$ = V$(I)
7140 REM he/she/it
7150 IF RIGHT$(VT$, 1) = "y" AND INSTR(VOWEL$, MID$(VT$, LEN(VT$)-1, 1)) = 0 THEN VI$ = LEFT$(VT$, LEN(VT$)-1) + "ies" ELSE VI$ = VT$ + "s"
7160 VG$ = VT$
7170 IF INSTR("wy", RIGHT$(VG$, 1)) <> 0 OR RIGHT$(VG$, 2) = "ee" THEN 7200
7180 IF RIGHT$(VG$, 1) = "e" THEN VG$ = LEFT$(VG$, LEN(VG$)-1):GOTO 7200
7190 IF INSTR(VOWEL$, RIGHT$(VG$, 1)) = 0 AND INSTR(VOWEL$, MID$(VG$, LEN(VG$)-1, 1)) <> 0 AND INSTR(VOWEL$, MID$(VG$, LEN(VG$)-2, 1)) = 0 AND LEN(VG$) = 3 THEN VG$ = VG$ + RIGHT$(VG$, 1)
7200 REM adding
7210 VG$ = VG$ + "ing"
7220 RETURN

7300 REM random adjective/adverb
7310 I = INT(RND(1)*A)+1
7320 AJ$ = A$(I)
7330 AV$ = AJ$ + "ly"
7340 RETURN

7400 REM randomfeel
7410 I = INT(RND(1)*F)+1
7420 F$ = F$(I)
7430 RETURN
