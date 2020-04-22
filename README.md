this is a new version of the fxswap calculation

CIX:
3m JPY OIS: (USSOC Curncy - (JYSOC Curncy - (JPY3M CMPL Curncy / JPY CMPL Curncy) * 4)) * 100
3m JPY GC Repo: (USRGCGC ICUS Curncy - (TKRPAV3M Index - (JPY3M CMPL Curncy / JPY CMPL Curncy) * 4)) * 100
3m MXN LIBOR: US0003M Index - MXIB91DT Index + (MXN3M Curncy / MXN Curncy / 100) * 4


•	MXN: 91-day TIIE Rate (benchmark interbank deposit rate), BBG ticker MXIB91DT Index
•	BRL: 84-day OIS, BBG ticker BCSFDPDV Curncy
•	SGD: 3month bill rate,  BBG ticker GTSGD3M Govt
•	KRW: 3month KORIBOR, BBG ticker KRBO3M Index




OIS basis:
(USSOC Curncy - (JYSOC Curncy - (JPY3M CMPL Curncy / JPY CMPL Curncy) * 4)) * 100

USSOC Curncy - JYSOC Curncy + (JPY3M CMPL Curncy / JPY CMPL Curncy) * 4


Govie basis:
(GB3 Govt - (GTJPY3MO Corp - (JPY3M CMPL Curncy / JPY CMPL Curncy) * 4)) * 100


Let R be the gross interest rate

F=S*(R_usd/R_frn)   dollar per foreign ccy, e.g. EUR, has positive points because of higher USD rate
alterantively, define s and f as 1/S and 1/F from above
f=s*(R_frn/R_usd)   foreign ccy per usd, e.g. JPY, has negative points because yen rate is lower than dollar rate

Forward points == P= F-S


so for yen basis per market convention, solve for x in
s+p=s*(R_frn/(R_usd-y)
y=R_usd-R_frn/(1+p/s)


alternatively, solve for x in
s+p=s*((R_frn+x)/(R_usd)
(1+p/s)*R_usd-R_frn=x


In logs
log(S/F)=
x= f − s  + 𝑟_𝑢𝑠𝑑 - 𝑟_𝑦𝑒𝑛


s-f=
(JPY3M CMPL Curncy / JPY CMPL Curncy)


((((CHF Curncy + CHF1W Curncy / 10000) / CHF Curncy) * (1 + USSO1Z Curncy / 100 * (7 / 360)) - 1) * 360 / 7) * 100 - SFSNT1Z Curncy
((((G4 + G3 / 10000) / G4) * (1 + G1 / 100 * (7 / 360)) - 1) * 360 / 7) * 100 - G2


((1+frnior/100*NS_1/360)*(spot)/(spot+fwdptSN/fwdptfactor)-1)*100*(360/NS_1)
((1+D2/100*90/360)*(D4)/(D4+D3/100)-1)*100*(360/90)
